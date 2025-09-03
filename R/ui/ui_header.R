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

       .handsontable .htCore th {
        background-color: ", HOSPITAL_COLORS$light, " !important;
        color: ", HOSPITAL_COLORS$dark, " !important;
        font-weight: 600 !important;
       }
       
        /* --- Excel-ish tema til DT --- */
  table.dataTable {
    font-family: Calibri, 'Segoe UI', Arial, sans-serif;
    font-size: 13px;
  }
  table.dataTable thead th {
    background: #f3f3f3;
    border-bottom: 2px solid #bfbfbf;
    font-weight: 600;
    white-space: nowrap;
  }

/* aktiv/redigeret celle: grøn kant som i Excel */
  table.dataTable > tbody > td:focus-within {
    outline: 2px solid #21a366;  /* Excel-grøn */
    outline-offset: -2px;
  }
  /* gør scrolleren visuelt pænere */
  .dataTables_scrollBody {
    border-left: 1px solid #d9d9d9;
    border-right: 1px solid #d9d9d9;
  }

table.dataTable > tbody > tr:nth-child(odd) > td,
table.dataTable > tbody > tr:nth-child(odd) > td > input,
table.dataTable > tbody > tr:nth-child(odd):focus-within > td,
table.dataTable > tbody > tr:nth-child(odd):focus-within > td > input,
table.dataTable > tbody > tr:nth-child(odd) > th {
  background-color: #f3f3f3 !important;
  box-shadow: none !important;
}
table.dataTable > tbody > tr:nth-child(even) > td,
table.dataTable > tbody > tr:nth-child(even) > td > input,
table.dataTable > tbody > tr:nth-child(even):focus-within > td,
table.dataTable > tbody > tr:nth-child(even):focus-within > td > input,
table.dataTable > tbody > tr:nth-child(even) > th {
  background-color: #ffffff !important;
  box-shadow: none !important;
}

table.dataTable > tbody > tr:nth-child(odd):hover > td,
table.dataTable > tbody > tr:nth-child(odd):hover > td > input,
table.dataTable > tbody > tr:nth-child(even):hover > td,
table.dataTable > tbody > tr:nth-child(even):hover > td > input {
   background-color: #f8fbff !important;
}

table.dataTable > tbody > tr > td,
table.dataTable > tbody > tr:focus-within > td {
      border: 1px solid #d9d9d9 !important;   /* gitterlinjer */
}

table.dataTable > tbody > tr > td {
   padding: 5px !important;
}
table.dataTable > tbody > tr:focus-within > td {
   padding: 0px !important;
}


table.dataTable > tbody > tr > td > input,
table.dataTable > tbody > tr:focus-within > td > input {
  border: none !important;
}

table.dataTable > tbody > tr:focus-within > td > input {
  padding: 6px 7px !important;
}
  table.dataTables_scrollBody (overflow: unset !important;
}
       
       
       
       
      ")))
    )
  )
}
