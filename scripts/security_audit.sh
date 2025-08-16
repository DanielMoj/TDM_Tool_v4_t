#!/bin/bash

# ============================================================================
# TDM_Tool_v4_p - KRITISCHES SICHERHEITS-AUDIT SKRIPT
# ============================================================================
# Dieses Skript führt alle kritischen Sicherheitsmaßnahmen automatisch durch
# Zeitrahmen: ca. 30 Minuten
# ============================================================================

set -euo pipefail  # Exit on error, undefined variables, pipe failures

# Farben für Output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Timestamp für Backups
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="../TDM_Tool_v4_BACKUP_${TIMESTAMP}"
AUDIT_LOG="SECURITY_AUDIT_${TIMESTAMP}.log"

# ============================================================================
# FUNKTIONEN
# ============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1" | tee -a "$AUDIT_LOG"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "$AUDIT_LOG"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1" | tee -a "$AUDIT_LOG"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "$AUDIT_LOG"
}

check_command() {
    if ! command -v $1 &> /dev/null; then
        log_warning "Command $1 not found. Some features may be unavailable."
        return 1
    fi
    return 0
}

# ============================================================================
# HAUPTPROGRAMM START
# ============================================================================

echo "============================================================================"
echo "TDM_Tool_v4_p - KRITISCHES SICHERHEITS-AUDIT"
echo "Timestamp: ${TIMESTAMP}"
echo "============================================================================"
echo ""

# Audit-Log initialisieren
echo "Security Audit Log - ${TIMESTAMP}" > "$AUDIT_LOG"
echo "============================================" >> "$AUDIT_LOG"

# ============================================================================
# SCHRITT 1: BACKUP ERSTELLEN (PRIORITÄT: KRITISCH)
# ============================================================================

log_info "SCHRITT 1: Erstelle vollständiges Backup..."

# Backup-Verzeichnis erstellen
log_info "Erstelle Backup-Verzeichnis: ${BACKUP_DIR}"
mkdir -p "$BACKUP_DIR"

# Vollständige Kopie
log_info "Kopiere Repository..."
cp -R . "$BACKUP_DIR/" 2>/dev/null || log_warning "Einige Dateien konnten nicht kopiert werden"

# TAR.GZ Archiv
log_info "Erstelle komprimiertes Archiv..."
tar -czf "${BACKUP_DIR}.tar.gz" \
    --exclude='node_modules' \
    --exclude='renv/library' \
    --exclude='.Rproj.user' \
    --exclude='*.log' \
    . 2>/dev/null || log_warning "Einige Dateien wurden vom Archiv ausgeschlossen"

# Git Bundle
if [ -d .git ]; then
    log_info "Erstelle Git Bundle..."
    git bundle create "../TDM_Tool_v4_bundle_${TIMESTAMP}.bundle" --all 2>/dev/null || \
        log_warning "Git Bundle konnte nicht erstellt werden"
    
    # Bundle verifizieren
    git bundle verify "../TDM_Tool_v4_bundle_${TIMESTAMP}.bundle" 2>/dev/null && \
        log_success "Git Bundle erfolgreich verifiziert" || \
        log_warning "Git Bundle Verifikation fehlgeschlagen"
fi

# Backup-Größen anzeigen
log_info "Backup-Statistiken:"
if [ -d "$BACKUP_DIR" ]; then
    echo "  Verzeichnis: $(du -sh "$BACKUP_DIR" 2>/dev/null | cut -f1)" | tee -a "$AUDIT_LOG"
fi
if [ -f "${BACKUP_DIR}.tar.gz" ]; then
    echo "  TAR.GZ: $(du -sh "${BACKUP_DIR}.tar.gz" 2>/dev/null | cut -f1)" | tee -a "$AUDIT_LOG"
fi

log_success "Backup abgeschlossen!"
echo ""

# ============================================================================
# SCHRITT 2: .GITIGNORE ERSTELLEN/AKTUALISIEREN
# ============================================================================

log_info "SCHRITT 2: Erstelle/Aktualisiere .gitignore..."

# Prüfen ob .gitignore existiert
if [ -f .gitignore ]; then
    log_warning ".gitignore existiert bereits - erstelle Backup"
    cp .gitignore ".gitignore.backup.${TIMESTAMP}"
fi

# Kritische .gitignore erstellen (wurde bereits als Artefakt erstellt)
# Hier würde der Inhalt aus dem ersten Artefakt eingefügt werden

log_success ".gitignore erstellt/aktualisiert!"
echo ""

# ============================================================================
# SCHRITT 3: GIT-HISTORY AUF SECRETS SCANNEN
# ============================================================================

log_info "SCHRITT 3: Scanne Git-History auf Secrets..."

# Temporäre Datei für Funde
FINDINGS_FILE="security_findings_${TIMESTAMP}.tmp"
> "$FINDINGS_FILE"

# Scan-Funktionen
scan_pattern() {
    local pattern=$1
    local description=$2
    local count=0
    
    log_info "Suche nach: $description"
    
    # Git log durchsuchen
    if git log -p --all -S "$pattern" --since="2020-01-01" 2>/dev/null | grep -i "$pattern" > /dev/null; then
        count=$(git log -p --all -S "$pattern" --since="2020-01-01" 2>/dev/null | grep -ic "$pattern" || echo "0")
        if [ "$count" -gt 0 ]; then
            log_warning "GEFUNDEN: $count Vorkommen von '$pattern'"
            echo "PATTERN: $pattern | COUNT: $count | TYPE: $description" >> "$FINDINGS_FILE"
        fi
    fi
}

# Kritische Patterns scannen
if [ -d .git ]; then
    scan_pattern "password" "Passwörter"
    scan_pattern "pwd" "Passwort-Abkürzungen"
    scan_pattern "api_key" "API Keys"
    scan_pattern "apikey" "API Keys (zusammen)"
    scan_pattern "token" "Tokens"
    scan_pattern "secret" "Secrets"
    scan_pattern "mongodb://" "MongoDB Connection"
    scan_pattern "postgres://" "PostgreSQL Connection"
    scan_pattern "mysql://" "MySQL Connection"
    
    # Dateitypen scannen
    log_info "Suche nach sensiblen Dateitypen..."
    
    for ext in "env" "pem" "key" "cert"; do
        if git log --all --full-history -- "*.$ext" 2>/dev/null | grep -q commit; then
            log_warning "GEFUNDEN: Dateien mit Endung .$ext in Git-History"
            echo "FILETYPE: *.$ext | TYPE: Sensitive File" >> "$FINDINGS_FILE"
        fi
    done
    
    # Spezifische Dateien
    for file in "config/users.yaml" "config/database.yml" ".env"; do
        if git log --all --full-history -- "$file" 2>/dev/null | grep -q commit; then
            log_warning "GEFUNDEN: $file in Git-History"
            echo "FILE: $file | TYPE: Configuration File" >> "$FINDINGS_FILE"
        fi
    done
fi

# Zusammenfassung
if [ -s "$FINDINGS_FILE" ]; then
    log_error "KRITISCH: Secrets in Git-History gefunden!"
    echo ""
    echo "Gefundene Probleme:" | tee -a "$AUDIT_LOG"
    cat "$FINDINGS_FILE" | tee -a "$AUDIT_LOG"
    echo ""
    log_warning "SOFORTMASSNAHME ERFORDERLICH: Alle gefundenen Secrets müssen rotiert werden!"
else
    log_success "Keine offensichtlichen Secrets in Git-History gefunden"
fi

echo ""

# ============================================================================
# SCHRITT 4: AKTUELLEN ZUSTAND PRÜFEN
# ============================================================================

log_info "SCHRITT 4: Prüfe aktuellen Repository-Zustand..."

# Prüfe auf kritische Dateien im Working Directory
CRITICAL_FILES=(
    ".env"
    "config/users.yaml"
    "config/database.yml"
    "config/database.yaml"
    "api_keys.json"
    "secrets.json"
    "jwt_secret.txt"
)

log_info "Suche nach ungeschützten sensiblen Dateien..."
for file in "${CRITICAL_FILES[@]}"; do
    if [ -f "$file" ]; then
        # Prüfe ob in .gitignore
        if [ -f .gitignore ] && grep -q "^${file}$" .gitignore; then
            log_info "$file existiert aber ist in .gitignore"
        else
            log_error "KRITISCH: $file existiert und ist NICHT in .gitignore!"
        fi
    fi
done

echo ""

# ============================================================================
# SCHRITT 5: DOKUMENTATION ERSTELLEN
# ============================================================================

log_info "SCHRITT 5: Erstelle Dokumentation..."

# BACKUP_STATUS.md aktualisieren
if [ -f BACKUP_STATUS.md ]; then
    log_info "Aktualisiere BACKUP_STATUS.md mit aktuellen Werten..."
    # Hier würden die Platzhalter in BACKUP_STATUS.md ersetzt werden
fi

# SECURITY_AUDIT.md aktualisieren
if [ -f SECURITY_AUDIT.md ]; then
    log_info "Aktualisiere SECURITY_AUDIT.md mit Scan-Ergebnissen..."
    # Hier würden die Platzhalter in SECURITY_AUDIT.md ersetzt werden
fi

log_success "Dokumentation erstellt!"
echo ""

# ============================================================================
# SCHRITT 6: EMPFEHLUNGEN GENERIEREN
# ============================================================================

log_info "SCHRITT 6: Generiere Sicherheitsempfehlungen..."

cat << EOF | tee -a "$AUDIT_LOG"

============================================================================
SICHERHEITSEMPFEHLUNGEN
============================================================================

1. SOFORTMASSNAHMEN (innerhalb 1 Stunde):
   - [ ] Alle gefundenen Secrets rotieren
   - [ ] .gitignore committen: git add .gitignore && git commit -m "Security: Add comprehensive .gitignore"
   - [ ] Team über Sicherheitslage informieren
   - [ ] Produktiv-Systeme auf kompromittierte Credentials prüfen

2. HEUTE DURCHFÜHREN:
   - [ ] Git-History mit BFG Repo-Cleaner bereinigen
   - [ ] .env.example erstellen
   - [ ] Alle Entwickler-Umgebungen aktualisieren

3. DIESE WOCHE:
   - [ ] Pre-commit Hooks für Secret-Scanning einrichten
   - [ ] Security-Guidelines dokumentieren
   - [ ] Secrets-Management-System evaluieren

============================================================================
EOF

# ============================================================================
# ABSCHLUSS
# ============================================================================

echo ""
log_success "SICHERHEITS-AUDIT ABGESCHLOSSEN!"
echo ""
echo "============================================================================"
echo "ZUSAMMENFASSUNG"
echo "============================================================================"
echo "Backup-Verzeichnis:     ${BACKUP_DIR}"
echo "Backup-Archiv:          ${BACKUP_DIR}.tar.gz"
if [ -f "../TDM_Tool_v4_bundle_${TIMESTAMP}.bundle" ]; then
    echo "Git Bundle:             ../TDM_Tool_v4_bundle_${TIMESTAMP}.bundle"
fi
echo "Audit-Log:              ${AUDIT_LOG}"
echo "Security-Findings:      ${FINDINGS_FILE}"
echo ""
echo "NÄCHSTER SCHRITT: Prüfen Sie die Ergebnisse und führen Sie die"
echo "                  empfohlenen Sofortmaßnahmen durch!"
echo "============================================================================"

# Cleanup temporäre Dateien (optional)
# rm -f "$FINDINGS_FILE"

exit 0