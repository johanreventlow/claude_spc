# Løsning: Scope Mismatch Problem mellem Auto-Detection og Plot Generation

## Problem Beskrivelse

Auto-detection fungerede korrekt og identificerede kolonner, men SPC plots blev ikke genereret efter auto-detection completerede. Gennem systematisk debugging blev problemet identificeret som en **scope mismatch** mellem auto-detection og visualization modulet.

### Root Cause

1. **Auto-detection kører i hovedapp scope** og opdaterer UI inputs via:
   ```r
   updateSelectizeInput(session, "y_column", choices = col_choices, selected = taeller_col)
   ```

2. **Visualization module's `column_config_reactive()`** lytter til samme inputs:
   ```r
   y_col <- if (!is.null(input$y_column) && input$y_column != "") input$y_column else NULL
   ```

3. **Timing/propagation problem**: UI opdateringer fra `updateSelectizeInput()` propagerer ikke til `input$y_column` i tide til at trigger plot generation.

### Debug Evidence

Debug output viste:
```
DEBUG: auto_detect_and_update_columns() - y_column selected: Tæller
DEBUG: auto_detect_and_update_columns() UI updates completed
DEBUG: chart_config() returning NULL - no y_col found
```

Dette bekræftede at auto-detection satte korrekte værdier, men `column_config_reactive()` så stadig NULL værdier.

## Løsning: Reactive Values Communication

### Concept

I stedet for at være afhængig af UI input propagation timing, implementer direkte kommunikation mellem auto-detection og visualization via reactive values.

### Implementation

#### 1. Auto-Detection Sender (R/fct_data_processing.R)

Efter UI opdateringer, sæt reactive value med auto-detected kolonner:

```r
# SOLUTION: Use reactive values to force column config updates bypassing input propagation delay
# This ensures visualization gets the correct column selections immediately
values$auto_detected_columns <- list(
  x_col = x_col,
  y_col = taeller_col,
  n_col = naevner_col,
  skift_col = skift_col,
  frys_col = frys_col,
  kommentar_col = kommentar_col,
  timestamp = Sys.time()  # Force reactivity even if values are same
)
```

#### 2. Visualization Receiver (R/fct_visualization_server.R)

Prioriter auto-detected værdier over input værdier i `column_config` reactive:

```r
column_config <- reactive({
  # SOLUTION: Check if auto-detected columns are available and use them as priority
  auto_detected <- values$auto_detected_columns
  if (!is.null(auto_detected)) {
    config_result <- list(
      x_col = auto_detected$x_col,
      y_col = auto_detected$y_col,
      n_col = auto_detected$n_col,
      chart_type = get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
    )

    if (!is.null(config_result$y_col)) {
      last_valid_config(config_result)
      return(config_result)
    }
  }

  # Fallback to input values (original behavior for manual selection)
  # ... existing code ...
})
```

### Benefits

1. **Immediate propagation**: Reactive values propagerer øjeblikkeligt
2. **Preserved UI sync**: UI inputs opdateres stadig for visuelt feedback
3. **Backward compatibility**: Fallback til input værdier for manuel selektion
4. **Clean separation**: Auto-detection og manual selection har separate paths

### Files Modified

1. **R/fct_data_processing.R**: Tilføj reactive values communication efter UI opdateringer
2. **R/fct_visualization_server.R**: Prioriter auto-detected værdier i column_config reactive

### Expected Outcome

Efter implementation:
```
DEBUG: auto_detect_and_update_columns() - y_column selected: Tæller
DEBUG: auto_detected_columns reactive value set with y_col: Tæller
DEBUG: column_config() found auto_detected_columns with y_col: Tæller
DEBUG: column_config() using auto-detected y_col: Tæller
DEBUG: chart_config() got valid config with y_col: Tæller
→ ggplot generates successfully
```

## Implementation Notes

- Bevar eksisterende UI opdateringer for brugerfeedback
- Timestamp i reactive value sikrer reactivity selv hvis værdier er ens
- Fallback til input værdier sikrer compatibility med manual kolonne selektion
- Minimal og målrettet ændring - ingen refaktorering af eksisterende logik