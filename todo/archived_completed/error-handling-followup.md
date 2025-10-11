# Error Handling Follow-up

## 1. Udvid `safe_operation()` med centraliseret error-state
- **Problem**: `safe_operation()` (`R/utils_error_handling.R:42`) logger fejl, men ignorerer `emit`- og `app_state`-parametre. Dermed stiger `app_state$errors$error_count` aldrig, og `emit$error_occurred()` trigges ikke.
- **Tiltag**:
  - Læs `emit` og `app_state` fra `...` indeni `safe_operation()`.
  - Kald `emit$error_occurred(error_type = error_type, context = operation_name, details = list(message = e$message))` når logging er lykkedes.
  - Opdater `set_last_error(app_state, list(type = error_type, context = operation_name, message = e$message, timestamp = Sys.time()))`.
  - Sørg for, at fallbacken afvikles efter state-opdateringen, og at funktionen returnerer fallback-resultatet (ingen `stop()`-kald).
- **Kontrol**: Kør `R -e "library(SPCify); testthat::test_file('tests/testthat/test-safe-operation-comprehensive.R')"` og `test-event-system-emit.R` for at sikre, at error events nu registreres.

## 2. Robustgør lokal lagring (manuelt og auto-save)
- **Problem**: `saveDataLocally()` fallback (`R/utils_local_storage.R:50`) kaster `stop()`, og `saveDataLocally()` kaldes direkte i `utils_server_server_management.R:166`. Fejl bobler derfor op til UI uden recovery, og auto-save (`utils_local_storage.R:100`) får kun en notifikation efter throw.
- **Tiltag**:
  - I `saveDataLocally()` retur fallbackens resultat i stedet for `stop()`. Brug fx `emit$error_occurred(error_type = "local_storage", context = "saveDataLocally", details = list(message = e$message))`, log med `log_error()`, og returnér `FALSE`.
  - Wrap manuelle saves i `safe_operation("Manual save to local storage", ...)` i `utils_server_server_management.R:159`, og vis en dansk fejlbesked via `show_notification_dk("Kunne ikke gemme sessionen lokalt. Prøv igen eller brug download.", type = "error")`.
  - Ved auto-save: hvis fallback returnerer `FALSE`, slå auto-save midlertidigt fra (`app_state$session$auto_save_enabled <- FALSE`) og informer brugeren om næste skridt.
- **Kontrol**: Kør `test-file-upload.R` og `test-file-operations.R` for regressioner i upload-flowet; verificer manuelt med dev-app, at fejl i `saveDataLocally()` ikke afbryder sessionen.

## 3. Gør UI-opdateringskøen fejltolerant
- **Problem**: I `R/utils_ui_ui_updates.R:642` rethrower fallback (`stop(e)`) i `safe_operation("Execute update function", ...)`, hvilket bryder kølogikken og skaber ubehandlede reaktive fejl.
- **Tiltag**:
  - Returnér en struktureret fejl (`list(success = FALSE, message = e$message)`) i fallbacken i stedet for at kaste.
  - Log via `log_error()` med komponent `[UI_UPDATE_QUEUE]`, og udsend `emit$error_occurred(error_type = "ui", context = paste0("update_", inputId))`.
  - Tilføj retry/cleanup: marker den pågældende `queue_entry` som mislykket og kald `enqueue_ui_update()` igen efter et kort delay, eller rens `pending_programmatic_inputs` for at bevare konsistens.
- **Kontrol**: Kør `tests/testthat/test-ui-programmatic-updates.R` (hvis den findes) eller skriv en targeted test, der simulerer en fejl i `update_function()`, og verificér, at køen fortsætter uden reaktive exceptions.

## 4. Dansk og handlingsorienteret brugerfeedback
- **Problem**: Flere fejlbeskeder er engelske/tekniske (`R/fct_file_operations.R:644`, `R/utils_server_server_management.R:581`), og standardnotifikationer siger blot `Fejl: <operation_name>`.
- **Tiltag**:
  - Udskift engelske strenge med danske, fx `"Filen kunne ikke læses. Kontroller at den ligger på computeren og prøv igen."` og `"Valideringen fandt ingen data i filen."`
  - Giv konkrete næste skridt, fx “Prøv at vælge en mindre fil (<50 MB) eller kontakt support.”
  - Udvid `safe_operation()` så `show_user = TRUE` accepterer en `user_message` parameter; brug den i `handle_csv_upload()` og demo-loaderen, så brugeren får en venlig besked (“Kunne ikke indlæse demo-data. Prøv at genopfriske siden.”).
- **Kontrol**: UI-manualtest – forsøg at uploade en ikke-eksisterende sti eller tving en CSV-parse-fejl og bekræft, at den danske besked vises og logges uden stack trace.

---

**Opfølgning**
- Når rettelserne er implementeret, kør fuldt test-suite: `R -e "library(SPCify); testthat::test_dir('tests/testthat')"`.
- Notér resultater og eventuelle nye mønstre i `docs/KNOWN_ISSUES.md` hvis yderligere edge cases opdages under arbejdet.
