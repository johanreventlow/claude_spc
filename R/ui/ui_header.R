# R/ui/ui_header.R
# UI header components including scripts and styles

create_ui_header <- function() {
  tagList(
    waiter::use_waiter(),
    # Enable shinyjs
    shinyjs::useShinyjs(),
    tags$head(
      tags$script(HTML("
      $(document).ready(function() {
        // Function to properly show UI when waiter hides
        window.showAppUI = function() {
          setTimeout(function() {
            $('body').css('opacity', '1');
          }, 100); // Small delay to ensure smooth transition
        };
      });

      // Add to existing JavaScript section
      Shiny.addCustomMessageHandler('showAppUI', function(message) {
        window.showAppUI();
      });
    ")),
      tags$script(HTML(localStorage_js)),
      tags$script(HTML("
      // FIXED: Custom message handlers med bedre error handling
      Shiny.addCustomMessageHandler('saveAppState', function(message) {
        console.log('Received saveAppState message:', message);
        var success = window.saveAppState(message.key, message.data);
        if (!success) {
          console.error('saveAppState failed for key:', message.key);
        }
      });

      Shiny.addCustomMessageHandler('loadAppState', function(message) {
        console.log('Received loadAppState message:', message);
        var data = window.loadAppState(message.key);
        Shiny.setInputValue('loaded_app_state', data, {priority: 'event'});
      });

      Shiny.addCustomMessageHandler('clearAppState', function(message) {
        console.log('Received clearAppState message:', message);
        var success = window.clearAppState(message.key);
        if (!success) {
          console.error('clearAppState failed for key:', message.key);
        }
      });

      // UPDATED: Auto-load existing data ved app start
      if (window.hasAppState('current_session')) {
        setTimeout(function() {
          console.log('Auto-loading saved session data...');
          var data = window.loadAppState('current_session');
          if (data) {
            console.log('Found saved session data, triggering auto-restore');
            Shiny.setInputValue('auto_restore_data', data, {priority: 'event'});
          }
        }, 500); // Reduced delay since no waiter
      }
    ")),

      tags$style(HTML(
        paste0("
        .htContextMenu {
         z-index: 99999 !important;
        }
       .nav-link {padding: .5rem 1rem !important}

       .status-ready { background-color: ", HOSPITAL_COLORS$success, "; }
       .status-warning { background-color: ", HOSPITAL_COLORS$warning, "; }
       .status-error { background-color: ", HOSPITAL_COLORS$danger, "; }
       .status-processing { background-color: ", HOSPITAL_COLORS$primary, "; }

       
        /* --- Excel-ish tema til excelR --- */
  .jexcel_container {
    /*font-family: Calibri, 'Segoe UI', Arial, sans-serif;
    font-size: 13px;*/
    width: none !important;
    height: auto !important;
    padding-bottom: 25px !important;
    position: none !important;
    border: none !important;
  }
  
  .jexcel thead td {
    background: #f3f3f3;
    border-bottom: 2px solid #bfbfbf;
    font-weight: 600;
    white-space: nowrap;
  }

  /* Excel-lignende styling */
  .jexcel tbody tr:nth-child(odd) {
    background-color: #f9f9f9;
  }
  
  .jexcel tbody tr:nth-child(even) {
    background-color: #ffffff;
  }
  
  .jexcel tbody tr:hover {
    background-color: #f0f8ff !important;
  }
  
  .jexcel td {
    border: 1px solid #d9d9d9;
    padding: 4px 8px;
  }
  
  /* Aktiv celle styling */
  .jexcel .highlight {
    background-color: #cce7ff !important;
    border: 2px solid #0066cc !important;
  }
  
  .jexcel_content {
  overflow-y: unset !important; 
  max-height: none !important;
  padding-bottom: 25px !important; 
  }
  
  .jexcel > thead > tr > td {
    position: unset !important;
  }
       
       
       
       
      ")))
    )
  )
}
