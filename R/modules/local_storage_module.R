# R/modules/local_storage_module.R
library(shiny)
library(shinyjs)
library(jsonlite)

# JavaScript funktioner til localStorage
localStorage_js <- "
// Save data to localStorage - FIXED: Konsistent brug af localStorage
window.saveAppState = function(key, data) {
  try {
    console.log('Saving app state for key:', key);
    localStorage.setItem('spc_app_' + key, JSON.stringify(data));
    console.log('App state saved successfully');
    return true;
  } catch(e) {
    console.error('Failed to save to localStorage:', e);
    return false;
  }
};

// Load data from localStorage - FIXED: Konsistent brug af localStorage
window.loadAppState = function(key) {
  try {
    console.log('Loading app state for key:', key);
    var data = localStorage.getItem('spc_app_' + key);
    if (data) {
      console.log('App state loaded successfully');
      return JSON.parse(data);
    } else {
      console.log('No app state found for key:', key);
      return null;
    }
  } catch(e) {
    console.error('Failed to load from localStorage:', e);
    return null;
  }
};

// Clear specific key - FIXED: Konsistent brug af localStorage
window.clearAppState = function(key) {
  try {
    console.log('Clearing app state for key:', key);
    localStorage.removeItem('spc_app_' + key);
    console.log('App state cleared successfully');
    return true;
  } catch(e) {
    console.error('Failed to clear localStorage:', e);
    return false;
  }
};

// Check if data exists - FIXED: Konsistent brug af localStorage
window.hasAppState = function(key) {
  var exists = localStorage.getItem('spc_app_' + key) !== null;
  console.log('Checking app state for key:', key, 'exists:', exists);
  return exists;
};
"

# FIXED: Local Storage funktioner til server med data structure preservation
saveDataLocally <- function(session, data, metadata = NULL) {
  tryCatch({
    cat("DEBUG: Preparing app state for save\n")
    cat("DEBUG: Original data dimensions:", nrow(data), "rows x", ncol(data), "cols\n")
    
    # CRITICAL: Preserve data structure explicitly - improved method
    data_to_save <- list(
      values = lapply(data, function(x) as.vector(x)),  # Convert each column to vector
      col_names = colnames(data),
      nrows = nrow(data),
      ncols = ncol(data),
      class_info = sapply(data, function(x) class(x)[1])  # Take first class only
    )
    
    app_state <- list(
      data = data_to_save,  # Use structured data
      metadata = metadata,
      timestamp = Sys.time(),
      version = "1.2"  # Bumped for data structure fix
    )
    
    # Konverter til JSON med bedre indstillinger for data preservation
    json_data <- jsonlite::toJSON(
      app_state, 
      auto_unbox = TRUE, 
      pretty = FALSE,
      digits = NA,  # Preserve all digits
      na = 'null'   # Handle NA values properly
    )
    
    if (is.null(json_data) || nchar(json_data) == 0) {
      stop("JSON conversion resulted in empty data")
    }
    
    cat("DEBUG: JSON data size:", nchar(json_data), "characters\n")
    
    # Send til browser localStorage
    session$sendCustomMessage(
      type = "saveAppState",
      message = list(
        key = "current_session",
        data = json_data
      )
    )
    
    cat("DEBUG: saveDataLocally completed\n")
    
  }, error = function(e) {
    cat("ERROR in saveDataLocally:", e$message, "\n")
    stop(paste("Failed to save data locally:", e$message))
  })
}

# FIXED: Load data med logging
loadDataLocally <- function(session) {
  tryCatch({
    cat("DEBUG: Requesting data from localStorage\n")
    # Anmod om data fra localStorage
    session$sendCustomMessage(
      type = "loadAppState", 
      message = list(key = "current_session")
    )
    cat("DEBUG: loadDataLocally request sent\n")
  }, error = function(e) {
    cat("ERROR in loadDataLocally:", e$message, "\n")
  })
}

# FIXED: Clear data med logging
clearDataLocally <- function(session) {
  tryCatch({
    cat("DEBUG: Clearing localStorage\n")
    session$sendCustomMessage(
      type = "clearAppState",
      message = list(key = "current_session")
    )
    cat("DEBUG: clearDataLocally request sent\n")
  }, error = function(e) {
    cat("ERROR in clearDataLocally:", e$message, "\n")
  })
}

# FIXED: Auto-save funktion med bedre logging og error handling
autoSaveAppState <- function(session, current_data, metadata) {
  if (!is.null(current_data)) {
    # Kun gem hvis der er meaningful data
    if (nrow(current_data) > 0 && any(!is.na(current_data))) {
      
      # Begræns data størrelse for localStorage (max ~5MB i de fleste browsers)
      data_size <- object.size(current_data)
      cat("DEBUG: Auto-saving data - size:", format(data_size, units = "Kb"), "\n")
      
      if (data_size < 1000000) {  # 1MB limit
        tryCatch({
          saveDataLocally(session, current_data, metadata)
          cat("DEBUG: Auto-save completed successfully\n")
        }, error = function(e) {
          cat("ERROR: Auto-save failed:", e$message, "\n")
          showNotification(
            paste("Auto-gem fejlede:", e$message),
            type = "error",
            duration = 3
          )
        })
      } else {
        cat("WARNING: Data too large for auto-save:", format(data_size, units = "Mb"), "\n")
        showNotification(
          "Data for stor til automatisk gem - brug Download funktion",
          type = "warning",
          duration = 3
        )
      }
    } else {
      cat("DEBUG: Skipping auto-save - no meaningful data\n")
    }
  } else {
    cat("DEBUG: Skipping auto-save - current_data is NULL\n")
  }
}