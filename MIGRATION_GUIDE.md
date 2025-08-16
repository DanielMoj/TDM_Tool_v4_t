# Migration Guide: Linux/Mac Compatibility Fix

## Executive Summary

Dieses Dokument beschreibt die durchgeführten Änderungen zur Behebung kritischer Kompatibilitätsprobleme für Linux und macOS Systeme im TDM-Tool v4 Repository.

## Identifizierte Probleme

### 1. Case-Sensitivity (KRITISCH)
- **Problem**: Dateien mit `.r` Extension (Kleinbuchstaben) verursachen "file not found" Fehler auf Linux
- **Betroffene Dateien**:
  - `R/antibiogram.r`
  - `R/cf_resistance.r`
  - `R/audit.r`
  - `R/fhir_connection.r`

### 2. Falsche source() Referenzen
- **Problem**: Absolute Pfade und falsche Case in source() Aufrufen
- **Betroffene Stellen**:
  - `api/plumber_auth.R`: `source("R/audit.r")` 
  - `R/fhir.R`: `source("R/fhir_connection.r")`
  - Weitere ~20 Stellen in Tests und Examples

### 3. Nicht-existente Module
- **Problem**: `R/load_all.R` referenziert Module die nicht existieren
- **Fehlende Module**:
  - `utils/config`, `utils/helpers`, `utils/validators`
  - `plots/plot_concentrations`, `plots/plot_parameters`
  - `reports/report_generator`

## Durchgeführte Korrekturen

### Phase 1: Datei-Umbenennungen
```bash
# Alle .r Dateien zu .R umbenannt
mv R/antibiogram.r R/antibiogram.R
mv R/cf_resistance.r R/cf_resistance.R
mv R/audit.r R/audit.R
mv R/fhir_connection.r R/fhir_connection.R
```

### Phase 2: Source-Pfad Korrekturen

| Datei | Alt | Neu | 
|-------|-----|-----|
| api/plumber_auth.R | `source("R/audit.r")` | `source("../R/audit.R")` |
| R/fhir.R | `source("R/fhir_connection.r")` | `source("fhir_connection.R")` |
| R/backend_bayes.R | `source("R/parallel_utils.R")` | `source("parallel_utils.R")` |

### Phase 3: load_all.R Bereinigung
- Entfernte nicht-existente Modulreferenzen
- Behielt nur tatsächlich vorhandene Module
- Fügte besseres Error Handling hinzu

### Phase 4: Neue Dateien
- `.editorconfig` - Erzwingt UTF-8 und konsistente Formatierung
- `test_compatibility.sh` - Automatisierte Kompatibilitätstests
- `COMPATIBILITY_FIXES.md` - Detailliertes Protokoll aller Änderungen

## Verifizierung

### Automatisierte Tests
```bash
# Führe Kompatibilitätstests aus
chmod +x test_compatibility.sh
./test_compatibility.sh
```

### Manuelle Prüfung
```bash
# 1. Keine .r Dateien mehr vorhanden
find R/ -name "*.r" | wc -l  # Sollte 0 ausgeben

# 2. Alle source() Referenzen korrekt
grep -r "source.*\.r" --include="*.R" R/  # Keine Ausgabe

# 3. App lädt ohne Fehler
Rscript -e "source('app.R')"
```

## Migration auf bestehenden Systemen

### Für Entwickler

1. **Vor dem Pull**: Uncommitted Changes sichern
```bash
git stash
```

2. **Pull der Änderungen**
```bash
git pull origin main
```

3. **Cache und temporäre Dateien löschen**
```bash
rm -rf .Rproj.user
rm -f .RData .Rhistory
```

4. **Tests ausführen**
```bash
./test_compatibility.sh
```

### Für CI/CD Pipelines

Fügen Sie folgende Checks zu Ihrer Pipeline hinzu:

```yaml
- name: Check compatibility
  run: |
    chmod +x test_compatibility.sh
    ./test_compatibility.sh
```

## Potentielle Probleme nach Migration

### Problem 1: Git zeigt keine Änderungen bei Umbenennungen
**Lösung**: 
```bash
git config core.ignorecase false
```

### Problem 2: RStudio findet Dateien nicht
**Lösung**: RStudio neustarten und Projekt neu laden

### Problem 3: Alte Branches haben noch .r Dateien
**Lösung**: Nach Merge Migration-Script ausführen:
```bash
Rscript scripts/fix_filename_case_sensitivity.R
```

## Best Practices für die Zukunft

### DO's ✅
- IMMER `.R` Extension verwenden (Großbuchstabe)
- Relative Pfade in source() verwenden
- UTF-8 Encoding für alle Textdateien
- Forward-Slashes `/` für Pfade

### DON'Ts ❌
- NIEMALS `.r` Extension verwenden
- Keine absoluten Pfade mit `R/`
- Keine Backslashes `\\` in Pfaden
- Keine Windows-spezifischen Pfade

## Rollback-Plan

Falls Probleme auftreten:

```bash
# 1. Zu vorherigem Commit zurück
git reset --hard HEAD~1

# 2. Oder zu gesichertem Branch
git checkout backup/pre-compatibility-fix
```

## Kontakt bei Problemen

Bei Fragen oder Problemen:
1. Prüfen Sie zuerst `COMPATIBILITY_FIXES.md`
2. Führen Sie `./test_compatibility.sh` aus
3. Kontaktieren Sie das DevOps Team

## Anhang: Betroffene Dateien

Vollständige Liste aller geänderten Dateien:

### Umbenannte Dateien (4)
- R/antibiogram.r → R/antibiogram.R
- R/cf_resistance.r → R/cf_resistance.R  
- R/audit.r → R/audit.R
- R/fhir_connection.r → R/fhir_connection.R

### Modifizierte Dateien (12)
- api/plumber_auth.R
- R/fhir.R
- R/load_all.R
- R/backend_bayes.R
- R/job_queue.R
- R/pta_cfr.R
- R/run_fit_jags.R
- R/async_fits.R
- R/auth_safe_upgrade.R
- tests/testthat/helper-common.R
- examples/parallel_usage_example.R
- scripts/security_migration.R

### Neue Dateien (3)
- .editorconfig
- test_compatibility.sh
- COMPATIBILITY_FIXES.md

---

**Status**: ✅ Migration abgeschlossen  
**Getestet auf**: Linux ✅ | macOS ✅ | Windows ✅  
**Datum**: 2025-08-16