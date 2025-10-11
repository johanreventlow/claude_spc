# Visualization & Event System Refactor Plan

## Baggrund
Analysen (2024-12-13) identificerede flere vedvarende kompleksitetsproblemer i visualiseringsmodulet og de tilhørende event-registreringer. Denne plan beskriver de konkrete skridt, der skal til for at reducere cyclomatic complexity, fjerne dubleret logik og gøre arkitekturen mere testbar – alt i tråd med CLAUDE.md og TDD-principperne.

## Overblik og Prioritet
- **P1** – Opdel `visualizationModuleServer()` i modulære helpers.
- **P1** – Segmentér navigation- og test-mode events i dedikerede registreringsfunktioner.
- **P2** – Konsolider Anhøj-metodik mellem første beregning og cache-observatør.
- **P2** – Modularisér `setup_visualization()` i mindre byggesten.
- **P3** – Erstat viewport/magic numbers med konfigurationsværdier.
- **P3** – Fjern placeholder-kommentarer i `setup_visualization()`.

## Detaljerede Arbejdspakker

### P1 · Refaktorér `visualizationModuleServer`
- **Filer**: `R/mod_spc_chart_server.R:9`, `R/mod_spc_chart_server.R:95`, `R/mod_spc_chart_server.R:440`, `R/mod_spc_chart_server.R:702`
- **Problem**: ~750 linjer i én funktion, blandet ansvar (cache-init, reaktive pipelines, SPC-beregning, UI-rendering).
- **Handlinger**:
  1. Kortlæg nuværende blokke (cache-init, `chart_config`, `spc_inputs`, `spc_results`, output-rendering).
  2. Ekstrahér helpers i nyt `R/utils_visualization_pipeline.R` eller tilsvarende:
     - `initialize_visualization_cache(app_state, emit)`
     - `build_chart_config_pipeline(app_state, input, session, ns)`
     - `compute_spc_results(app_state, config_pipeline, viewport_provider)`
     - `render_visualization_outputs(results, session, ns)`
  3. Refaktor modulet til at være en tynd orkestrator, der kalder disse helpers.
  4. Flyt `safe_max` og viewport-handling til helpers/egen utility for at muliggøre unit tests.
  5. Tilføj/ajourfør tests i `tests/testthat/test-visualization_module.R`:
     - Unit tests for nye helper-funktioner (mock `app_state`).
     - Integrationstest der sikrer korrekt event-kæde ved `emit$visualization_update_needed`.
  6. Kør `R -e "library(SPCify); testthat::test_file('tests/testthat/test-visualization_module.R')"`.

### P1 · Opdel navigation/test-mode event-registrering
- **Filer**: `R/utils_server_event_listeners.R:338`, `R/utils_server_event_listeners.R:432`, `R/utils_server_event_listeners.R:520`
- **Problem**: `register_navigation_events()` håndterer navigation, session cleanup, test-mode og central error handling i én funktion; svag cohesion.
- **Handlinger**:
  1. Opret helpers (fx `register_session_lifecycle_events()`, `register_test_mode_events()`, `register_error_events()`).
  2. Flyt relevant observerlogik ind i de nye helpers; returnér named lists så `setup_event_listeners()` stadig kan registrere/rydde.
  3. Injicér eksterne afhængigheder via argumenter (fx `track_event`) i stedet for `exists()`-checks.
  4. Opdater `setup_event_listeners()` til at kombinere listerne via `purrr::list_merge`.
  5. Udvid/tilføj tests i `tests/testthat/test-event_system.R` for at sikre:
     - Session-reset rydder QIC/performance-cache.
     - Test-mode events emitteres i forventet rækkefølge.
  6. Kør fuld test-suite for event-systemet.

### P2 · Konsolider Anhøj-resultatbehandling
- **Filer**: `R/mod_spc_chart_server.R:496`, `R/mod_spc_chart_server.R:645`, `R/utils_anhoej_results.R`
- **Problem**: Dubleret udledning af `qic_results` mellem første beregning og cache-hit observer → risiko for divergerende logik.
- **Handlinger**:
  1. Ekstrahér fælles helper, fx `derive_anhoej_results(qic_data, inputs)`.
  2. Kald helperen både i `spc_results`-reaktivet og cache-observatøren; centralisér `update_anhoej_results()`-kald.
  3. Udbyg eksisterende tests i `tests/testthat/test-anhoej-results.R` (eller opret fil) til at dække:
     - Ens output ved cache-hit vs. ny beregning.
     - Handling ved `centerline_changed`.
  4. Kør målrettet testfil.

### P2 · Modularisér `setup_visualization`
- **Filer**: `R/fct_visualization_server.R:10`, `R/fct_visualization_server.R:59`, `R/fct_visualization_server.R:200`
- **Problem**: Funktionen (~280 linjer) blander konfigurationsudtræk, reaktive værdier og output-binding.
- **Handlinger**:
  1. Udpeg logiske sektioner (kolonnekonfiguration, mål-/centerline-reactives, modul-binding, output-eksponering).
  2. Ekstrahér helpers (fx `create_column_config_reactive(app_state, input)`, `build_target_reactives(input, app_state, chart_type_reactive)`).
  3. Lad `setup_visualization()` returnere et objekt med tydelige handles (`list(module = visualization, outputs = list(...))`).
  4. Skriv / opdater tests i `tests/testthat/test-visualization-setup.R`, inklusive edge cases (manual vs. auto config, manglende data).

### P3 · Ekstrahér viewport-konstanter
- **Filer**: `R/mod_spc_chart_server.R:296`, `R/mod_spc_chart_server.R:302`, `R/mod_spc_chart_server.R:709`, `R/config_ui.R`
- **Problem**: Magic numbers (800, 600, 96) hårdkodet i modul → vanskeligt at tilpasse miljøer/branding.
- **Handlinger**:
  1. Tilføj `VIEWPORT_DEFAULTS` i `R/config_ui.R` og eksponer gennem `golem::get_golem_options`.
  2. Erstat magic numbers med konfig-læsning (`get_viewport_defaults()` helper).
  3. Tilføj regressionstest i `tests/testthat/test-viewport-config.R` (sikrer fallback-værdier og override via golem-config).

### P3 · Ryd placeholder-kommentarer i `setup_visualization`
- **Filer**: `R/fct_visualization_server.R:10-105`
- **Problem**: Gentagne “Operation completed” kommentarer uden indhold skygger for logikken.
- **Handlinger**:
  1. Fjern placeholder-kommentarer eller erstat med korte, meningsfulde danske kommentarer.
  2. Ingen tests nødvendige, men kør `lintr` for at sikre stiloverholdelse.

## Test- og Kvalitetskrav
- Brug TDD: skriv tests før refaktorering af hver helper.
- Kør mindst:
  ```r
  R -e "library(SPCify); testthat::test_dir('tests/testthat')"
  R -e "devtools::lint()"
  ```
- Overvåg log-output (via eksisterende log-tests) for at sikre, at event-kæder fortsat logger korrekt kontekst.

## Acceptance Criteria
- Visualiseringsmodulet er opdelt i ≤5 velnavngivne helpers med en tynd orkestrator.
- Navigation/test-mode events er opdelt i separate registreringshelpers uden `exists()`-guards.
- Anhøj-resultater udledes gennem ét code path (ingen dubleret logik).
- `setup_visualization()` består af orkestrering + helpers og er ≤120 linjer.
- Viewport defaults læses fra konfig, og tests dokumenterer både default og override.
- Placeholder-kommentarer er fjernet, og filen er mere læsbar.

## Opgaver der kræver opfølgning
- Opdater ADR med refaktoreringsbeslutninger (fx `docs/adr/ADR-visualization-refactor.md`).
- Efter refaktorering: benchmark `generateSPCPlot` for at sikre uændret performance (`benchmark_qic_cache.R`).

