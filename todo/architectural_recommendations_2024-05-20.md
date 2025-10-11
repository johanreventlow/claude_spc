# SPCify Architectural Follow-Up – 2024-05-20

## 1. Event Bus Completeness (`visualization_update_needed`)
- **Problem**: `visualization_update_needed` is emitted in multiple contexts but is not declared in `create_app_state()`. First emission produces `integer(0)` which halts plot refresh until workaround code resets the value.  
  - `R/state_management.R:66` lacks the event entry.  
  - `R/state_management.R:468` assumes the event exists.  
  - `R/mod_spc_chart_server.R:90` re-initialises the value manually.
- **Resolution Steps**:
  1. Add `visualization_update_needed = 0L` to the `app_state$events` block inside `create_app_state()`.
  2. Remove the ad-hoc initialization logic in `visualizationModuleServer()` (`R/mod_spc_chart_server.R:90-93`). The event bus should be authoritative.
  3. Run affected tests:  
     ```r
     R -e "library(SPCify); testthat::test_dir('tests/testthat', filter = 'event-bus|mod-spc-chart')"
     ```

## 2. Centralize Visualization Viewport State
- **Problem**: `visualizationModuleServer()` stores viewport dimensions in a local `reactiveVal`, bypassing `app_state`, which breaks centralized state governance (`R/mod_spc_chart_server.R:16`).  
- **Resolution Steps**:
  1. Persist viewport metrics under `app_state$visualization$viewport_dims` (e.g., list with `width`, `height`, `timestamp`).
  2. Update read/write helpers to use `safe_operation()` and `shiny::isolate()` as needed.
  3. Remove the local `reactiveVal` and inject dependencies through the shared state helpers.
  4. Extend module integration tests (`tests/testthat/test-mod-spc-chart-integration.R`) to assert viewport state land in `app_state`.

## 3. Enforce Structured Logging Context
- **Problem**: Visualization module logs omit `.context`/`component`, resulting in `[UNSPECIFIED]` messages (e.g., `R/mod_spc_chart_server.R:30`).  
- **Resolution Steps**:
  1. Pass `.context = "[VISUALIZATION]"` (or equivalent component string) to every `log_debug`, `log_info`, `log_error` call in the module.
  2. Verify with existing logging tests or add coverage to ensure context propagation.

## 4. Align UI Sync Throttle with Anti-Race Standard
- **Problem**: UI sync throttling is set to 250 ms contrary to the documented 800 ms debounce standard for high-frequency events (`R/utils_server_event_listeners.R:223`).  
- **Resolution Options**:
  - **Preferred**: Update the throttle to `millis = 800` to match the Hybrid Anti-Race Strategy baseline.
  - **Alternative**: If 250 ms is required, capture the rationale in an ADR and reference it from the code comment block.
  - After change, re-run UI sync integration tests (`tests/testthat/test-event-listener-integration.R`) to confirm non-regression.

---

### Post-Implementation Checklist
- [ ] All recommended changes implemented
- [ ] Relevant tests updated/added and passing
- [ ] ADR added if deviating from standard debounce interval
- [ ] Logging output manually spot-checked in development session
