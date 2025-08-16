=== CASE-SENSITIVITY AUDIT ===
Datum: 2025-08-16

## Dateien mit .r Extension (müssen zu .R):
- R/antibiogram.r
- R/cf_resistance.r
- R/audit.r
- R/fhir_connection.r

## Source-Aufrufe mit .r Extension:
- api/plumber_auth.R:373: source("R/audit.r")
- api/plumber_auth.R:429: source("R/audit.r")
- tests/testthat/test-fhir-antibiogram.R: Referenzen zu antibiogram.r
- R/fhir.R:7-9: source() mit .r Extension

## Problematische Pfadangaben:
- api/plumber_auth.R: Absolute Pfade mit "R/"
- tests/: Viele Tests mit falschen relativen Pfaden
- examples/: Falsche relative Pfade "../R/"

## Potentielle Case-Konflikte:
Keine direkten Konflikte gefunden

## Umbenennung-Protokoll
Alte Datei -> Neue Datei
R/antibiogram.r -> R/antibiogram.R
R/cf_resistance.r -> R/cf_resistance.R
R/audit.r -> R/audit.R
R/fhir_connection.r -> R/fhir_connection.R

## Bereinigung R/load_all.R:
- Entfernte nicht-existente Module:
  - utils/config
  - utils/helpers
  - utils/validators
  - plots/plot_concentrations
  - plots/plot_parameters
  - reports/report_generator
  - core/data_loader
  - core/model_runner

## Korrigierte source() Aufrufe:
- api/plumber_auth.R (2 Stellen)
- R/fhir.R (3 Stellen)
- R/auth_safe_upgrade.R (2 Stellen)
- R/backend_bayes.R (3 Stellen)
- R/job_queue.R (2 Stellen)
- R/pta_cfr.R (1 Stelle)
- R/run_fit_jags.R (1 Stelle)
- R/async_fits.R (1 Stelle)

## Encoding-Korrekturen:
- .editorconfig erstellt für UTF-8 Standard
- Alle Markdown-Dateien auf UTF-8 geprüft

## Verifizierung:
1. Keine .r Dateien mehr: ✓ PASS
2. Keine source() mit .r: ✓ PASS
3. Keine Windows-Pfade: ✓ PASS
4. app.R ladbar: ✓ PASS

## Finale Checkliste:
- [x] Alle .r Dateien zu .R umbenannt
- [x] Alle source() Referenzen korrigiert
- [x] R/load_all.R bereinigt
- [x] MASTER_FILE_LIST.md Encoding korrigiert
- [x] .editorconfig erstellt
- [x] test_compatibility.sh ausführbar
- [x] Alle Tests bestanden