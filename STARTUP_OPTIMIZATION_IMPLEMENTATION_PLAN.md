# 1) Ensret bootflowet: gå 100% package-based + én konfigurationskilde (HØJ PRIORITET)

**Hvorfor**
Dobbelt opstartsvej (package vs. `global.R`), blandede miljøflag (`GOLEM_CONFIG_ACTIVE` vs. `R_CONFIG_ACTIVE`) og parallelle config-loaders skaber divergens, skjulte race conditions og unødvendig opstarts-latens.

**Hvad der skal ændres**

* Drop “double boot”: Begræns `global.R` til ren dev-helper (ingen detach/ressource-sourcing i normal drift). Officiel start er `SPCify::run_app()` / `app.R`.
  *Referencer:* `global.R:7–10, 14–68`, `README/tests`.
* Én miljøkilde: Standardisér på `GOLEM_CONFIG_ACTIVE` og map evt. `R_CONFIG_ACTIVE` dertil ét sted.
  *Referencer:* `R/app_run.R:103`, `R/golem_utils.R:142–168`.
* Én config-loader: Behold **én** vej (fx `config::get` i `R/app_config.R:120–134`) og fjern YAML-læserduplikat (`R/golem_utils.R:394–438`).
* Navnerumskonsistens: “SPCify” overalt (fjern resterende “claudespc” i init/tests/logs).
  *Referencer:* `R/zzz.R:3,7,14–29`, `tests/testthat/test-no-file-dependencies.R:56`.
* Dependencies: Tilføj `excelR` til `Imports`.
  *Referencer:* `R/server_utils_column_management.R:346–402`, `R/ui_app_ui.R:469`.

**Hvordan (nøgletrin)**

* Skift til package-loading i `global.R` (dev-toggle via `options(spc.debug.source_loading = TRUE)`), som i din implementeringsplan (FASE 1).
* Reorganisér filer til golem-konvention (flad `/R/` med `app_*.R`, `mod_*.R`, `utils_*`, `fct_*`).
  *Eksempler:* `server_utils_*` → `utils_server_*`, `ui_utils_*` → `utils_ui_*`, `modules_mod_*` → `mod_*`.

**Acceptkriterier**

* Startup < **100 ms** (mål: 60–80% forbedring ift. tidligere ~400 ms).
* `devtools::check()` uden NOTE/WARN relateret til Collate/namespace.
* Én config-sti i brug; ingen `source('global.R')` i README/tests.

---

# 2) Gør error-handling robust: fix `safe_operation`, undgå eager fallbacks og ensret logging (KRITISK BUGFIX)

**Hvorfor**
`safe_operation()` returnerer pt. fallback-**funktionen** i fejlgren i stedet for at **eksekvere** den → downstream får en closure (fx `readCsvFile`) i stedet for data. Derudover evalueres fallback-braces eager nogle steder og nulstiller guards for tidligt. Uens log-kald vil kaste nye fejl, så snart fallback reelt kører.

**Hvad der skal ændres**

* `safe_operation()` skal **kalde** `fallback(error)` og returnere værdien; giv fallback adgang til `error`.
  *Referencer:* `R/utils_error_handling.R:28`.
* Stop eager fallback: Erstat `{ ... }`-fallbacks med **funktion** der eksekveres **kun ved fejl**; flyt reset/cleanup til `on.exit()` i den primære operation.
  *Referencer:* `R/server_utils_event_system.R:133`, `R/server_utils_server_management.R:137`.
* Logging-hygiejne:

  * Brug konsekvent `log_*` (ikke `cat()/warning()` i boot). *Ref:* `R/app_run.R:58–77`.
  * Alle `log_*` skal have eksplcit `.context`/component. *Ref:* `R/app_server_main.R:25,38,42,106`, `R/server_utils_event_system.R:96,102`.
  * Ret signatur-misbrug: flere steder kaldes `log_error()` med ekstra args; saml budskabet til én streng + separat component. *Ref:* `R/ui_utils_ui_updates.R:135,170,197,265,318 m.fl._

**Tests (skal med)**

* Unit: `safe_operation(success, fallback = function(e) "fallback")` returnerer `"fallback"` og eksponerer `e`.
* Integration: læsefejl i `readCsvFile` ender i korrekt fallback-datastruktur (ikke en closure); guards nulstilles via `on.exit()`; ingen eager reset.
* Log-kontrakt: Snapshot-tests for `.context` til kritiske boot-trin.

**Acceptkriterier**

* Ingen tilfælde hvor en closure bubbles op som “data”.
* Ingen “unused argument” log-fejl ved kørende fallbacks.
* Race-guards ændres kun på succes/`on.exit()` — ikke før opgaven kører.

---

# 3) Ydelses- og hændelsessystem: lazy load, cache ved startup og trim legacy events (HØJ PRIORITET EFTER STABILITET)

**Hvorfor**
Unødige initialiseringer koster opstartstid; blandede/legacy events øger kompleksitet og risiko for nondeterministisk rækkefølge.

**Hvad der skal ændres**

* Lazy loading af tunge moduler/diagnostik, kun on-demand (golem-option gates).
  *Ref:* din FASE 3 “lazy_load_modules()”.
* Let startup-cache for statiske artefakter (branding, config snapshot, observer-priorities) i tempdir.
  *Ref:* `cache_startup_data()` i planen.
* Trim/konsolidér events: fjern legacy/deprecated mønstre, behold prioriteret, deterministisk pipeline (emit → handle → ui-sync).
  *Ref:* `R/server_utils_event_system.R:37–90,105–195`, søgning efter LEGACY/DEPRECATED.
* Integrér eller fjern “død kode” som `manage_app_dependencies()` tidligt i boot.

**Måling og kontrol**

* Microbenchmarks af startup (package vs. source) + profiler for lazy hits.
* Stabil event-sekvens i test mode: `data_loaded` → `auto_detect` → `auto_detection_completed` → `ui_sync_needed` uden overlappende kørsel.

**Acceptkriterier**

* Startup-tid < **100 ms** med package loading + cache aktiv.
* Ingen regressions i event-rækkefølge; samme deterministik som defineret i observer-prioriteter.
* Memory stabil/bedre end baseline.

---

## Quick Wins (kan laves straks)

* Tilføj `excelR` til `DESCRIPTION: Imports`.
* Erstat resterende `message()/print()/cat()/warning()` i boot med `log_*` + `.context`.
* Ret `%||%`-duplikater: behold én implementation (`R/golem_utils.R:614–620` eller `R/utils_logging.R:28`).
* Ret tests der antager “claudespc” namespace.

---

## Leveranceorden (sekventiel, lille risiko)

1. **Bugfix & logging (Pkt. 2)** — stabilitet først.
2. **Bootflow & config (Pkt. 1)** — fjern double boot og ensret konfiguration.
3. **Performance & events (Pkt. 3)** — lazy/caching + trim legacy.
4. **Dokumentation** — opdater `CLAUDE.md` (golem-grænser, package vs. source, performance-krav) og README; tilføj migrations-afsnit.

---

## Done-Definition (tværgående)

* **Tests**: 0 fejl, ny fallback-/race-/log-dækning inkluderet.
* **Performance**: målt forbedring over baseline, dokumenteret i CI-artefakt.
* **Arkitektur**: golem-konform filstruktur, én config-vej, ingen “double boot”.
* **Observability**: alle `log_*` med komponent/.context og konsistent format.


