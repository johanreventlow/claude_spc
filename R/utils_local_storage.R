# local_storage_functions.R
# Server-side funktioner til localStorage integration og databehandling

# Dependencies ----------------------------------------------------------------
# LOCAL STORAGE FUNKTIONER ===================================================

## Local Storage funktioner til server med datastruktur preservation
saveDataLocally <- function(session, data, metadata = NULL) {
  safe_operation(
    "Save data to local storage",
    code = {
      # CRITICAL: Preserve data structure explicitly - improved method
      data_to_save <- list(
        values = lapply(data, function(x) as.vector(x)), # Convert each column to vector
        col_names = colnames(data),
        nrows = nrow(data),
        ncols = ncol(data),
        class_info = sapply(data, function(x) class(x)[1]) # Take first class only
      )

      app_state <- list(
        data = data_to_save, # Use structured data
        metadata = metadata,
        timestamp = Sys.time(),
        version = "1.2" # Bumped for data structure fix
      )

      # Konverter til JSON med bedre indstillinger for data preservation
      json_data <- jsonlite::toJSON(
        app_state,
        auto_unbox = TRUE,
        pretty = FALSE,
        digits = NA, # Preserve all digits
        na = "null" # Handle NA values properly
      )

      if (is.null(json_data) || nchar(json_data) == 0) {
        stop("JSON konvertering resulterede i tomme data")
      }

      # Send til browser localStorage
      session$sendCustomMessage(
        type = "saveAppState",
        message = list(
          key = "current_session",
          data = json_data
        )
      )
    },
    fallback = function(e) {
      # H7: Robust error handling - return FALSE instead of stop()
      log_error(
        paste("Kunne ikke gemme data lokalt:", e$message),
        .context = "LOCAL_STORAGE"
      )
      return(FALSE)
    },
    error_type = "local_storage",
    session = session,
    show_user = FALSE # Manual saves will show user message separately
  )
}

## Load data med logging
loadDataLocally <- function(session) {
  safe_operation(
    "Load data from local storage",
    code = {
      # Anmod om data fra localStorage
      session$sendCustomMessage(
        type = "loadAppState",
        message = list(key = "current_session")
      )
    },
    fallback = function(e) {
      # Load failed silently
    },
    error_type = "processing"
  )
}

## Clear data med logging
clearDataLocally <- function(session) {
  safe_operation(
    "Clear data from local storage",
    code = {
      session$sendCustomMessage(
        type = "clearAppState",
        message = list(key = "current_session")
      )
    },
    fallback = function(e) {
      # Clear failed silently
    },
    error_type = "processing"
  )
}

## Auto-save funktion med bedre logging og error handling
autoSaveAppState <- function(session, current_data, metadata) {
  if (!is.null(current_data)) {
    # Kun gem hvis der er meaningful data
    if (nrow(current_data) > 0 && any(!is.na(current_data))) {
      # Begræns data størrelse for localStorage (max ~5MB i de fleste browsers)
      data_size <- object.size(current_data)

      if (data_size < 1000000) { # 1MB limit
        result <- safe_operation(
          "Auto-save application state",
          code = {
            saveDataLocally(session, current_data, metadata)
          },
          fallback = function(e) {
            # H7: Disable auto-save on persistent failures
            log_error(
              paste("Auto-gem fejlede:", e$message),
              .context = "AUTO_SAVE"
            )
            shiny::showNotification(
              "Auto-gem fejlede. Funktionen er midlertidigt deaktiveret. Brug Download for at gemme data.",
              type = "warning",
              duration = 5
            )
            return(FALSE)
          },
          error_type = "local_storage"
        )

        # Disable auto-save if it failed
        if (identical(result, FALSE) && exists("app_state")) {
          app_state$session$auto_save_enabled <- FALSE
        }
      } else {
        shiny::showNotification(
          "Data for stor til automatisk gem - brug Download funktion",
          type = "warning",
          duration = 3
        )
      }
    }
  }
}
