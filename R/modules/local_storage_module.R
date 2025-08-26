# R/modules/local_storage_module.R
library(shiny)
library(shinyjs)
library(jsonlite)

# JavaScript funktioner til localStorage
localStorage_js <- "
// Save data to localStorage
window.saveAppState = function(key, data) {
  try {
    sessionStorage.setItem('spc_app_' + key, JSON.stringify(data));
    return true;
  } catch(e) {
    console.error('Failed to save to localStorage:', e);
    return false;
  }
};

// Load data from localStorage  
window.loadAppState = function(key) {
  try {
    var data = localStorage.getItem('spc_app_' + key);
    return data ? JSON.parse(data) : null;
  } catch(e) {
    console.error('Failed to load from localStorage:', e);
    return null;
  }
};

// Clear specific key
window.clearAppState = function(key) {
  try {
    localStorage.removeItem('spc_app_' + key);
    return true;
  } catch(e) {
    console.error('Failed to clear localStorage:', e);
    return false;
  }
};

// Check if data exists
window.hasAppState = function(key) {
  return localStorage.getItem('spc_app_' + key) !== null;
};
"

# Local Storage funktioner til server
saveDataLocally <- function(session, data, metadata = NULL) {
  app_state <- list(
    data = data,
    metadata = metadata,
    timestamp = Sys.time(),
    version = "1.0"
  )
  
  # Konverter til JSON
  json_data <- jsonlite::toJSON(app_state, auto_unbox = TRUE, pretty = FALSE)
  
  # Send til browser localStorage
  session$sendCustomMessage(
    type = "saveAppState",
    message = list(
      key = "current_session",
      data = json_data
    )
  )
}

loadDataLocally <- function(session) {
  # Anmod om data fra localStorage
  session$sendCustomMessage(
    type = "loadAppState", 
    message = list(key = "current_session")
  )
}

clearDataLocally <- function(session) {
  session$sendCustomMessage(
    type = "clearAppState",
    message = list(key = "current_session")
  )
}

# Auto-save funktion der kan kaldes når data ændres
autoSaveAppState <- function(session, current_data, metadata) {
  if (!is.null(current_data)) {
    # Kun gem hvis der er meaningful data
    if (nrow(current_data) > 0 && any(!is.na(current_data))) {
      
      # Begræns data størrelse for localStorage (max ~5MB i de fleste browsers)
      if (object.size(current_data) < 1000000) {  # 1MB limit
        saveDataLocally(session, current_data, metadata)
      } else {
        showNotification(
          "Data for stor til automatisk gem - brug Download funktion",
          type = "warning",
          duration = 3
        )
      }
    }
  }
}