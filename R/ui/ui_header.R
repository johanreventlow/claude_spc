# ui_header.R
# UI header komponenter inklusive scripts og styles

# Dependencies ----------------------------------------------------------------

# UI HEADER KOMPONENTER =======================================================

## Hovedfunktion for UI header
# Opretter alle header komponenter inklusive scripts, styles og waiter
create_ui_header <- function() {
  tagList(
    waiter::use_waiter(),
    # Aktivér shinyjs
    shinyjs::useShinyjs(),
    tags$head(
      tags$script(HTML("
      $(document).ready(function() {
        // Funktion til korrekt at vise UI når waiter skjules
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
      // FIKSET: Tilpassede besked handlers med bedre fejlhåndtering
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

      // OPDATERET: Auto-indlæs eksisterende data ved app start
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

      // Selectize dropup behavior
      $(document).ready(function() {
        // Force dropup behavior for selectize inputs in .selectize-dropup containers
        $('.selectize-dropup').find('select').each(function() {
          if (this.selectize) {
            // Override the dropdown positioning
            var selectize = this.selectize;
            var originalSetup = selectize.setup;
            selectize.setup = function() {
              originalSetup.call(this);
              // Force dropdown to open upward
              this.$dropdown.addClass('dropup-forced');
            };
          }
        });
      });
    ")),
      tags$style(HTML(
        paste0("
       .nav-link {padding: .5rem 1rem !important}

       /* Tab styling - ikke-aktive tabs */
       .nav-tabs .nav-link:not(.active) {
         color: #009ce8 !important;
       }

       /* Tab styling - aktive tabs (behold standard) */
       .nav-tabs .nav-link.active {
         color: inherit;
       }

       .status-ready { background-color: ", HOSPITAL_COLORS$success, "; }
       .status-warning { background-color: ", HOSPITAL_COLORS$warning, "; }
       .status-error { background-color: ", HOSPITAL_COLORS$danger, "; }
       .status-processing { background-color: ", HOSPITAL_COLORS$primary, "; }


        /* --- Excel-lignende tema til excelR --- */
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
  margin-bottom: 25px !important;
  }

  .jexcel > thead > tr > td {
    position: unset !important;
  }

/* Neutraliser bslib spacing omkring textarea wrapper */
        .bslib-grid:has(#indicator-description-wrapper) {
          margin-bottom: 0 !important;
          padding-bottom: 0 !important;
        }

        .bslib-mb-spacing:has(#indicator-description-wrapper) {
          margin-bottom: 0 !important;
        }

        /* Parent container skal være fleksibel */
        #indicator-description-wrapper {
          display: flex !important;
          flex-direction: column !important;
          flex: 1 1 auto !important;
          min-height: 0 !important;
          margin-bottom: 0 !important;
          padding-bottom: 0 !important;
        }

        /* Textarea skal fylde tilgængelig højde */
        #indicator_description {
          flex: 1 1 auto !important;
          min-height: 130px !important;
          height: 100% !important;
          resize: none !important;
          overflow: auto !important;
          margin-bottom: 0 !important;
        }

        /* Fjern margin på form-group omkring textarea */
        #indicator-description-wrapper .form-group,
        #indicator_div {
          margin-bottom: 0 !important;
          flex: 1 1 auto !important;
          display: flex !important;
          flex-direction: column !important;
        }





        /* Selectize dropup styling */
        .selectize-dropup .selectize-control .selectize-dropdown {
          position: absolute !important;
          top: auto !important;
          bottom: 100% !important;
          border-top: 1px solid #d0d7de !important;
          border-bottom: none !important;
          border-radius: 4px 4px 0 0 !important;
          box-shadow: 0 -2px 8px rgba(0, 0, 0, 0.1) !important;
          margin-bottom: 2px !important;
        }

        .selectize-dropup {
          position: relative !important;
        }

        .selectize-dropup .selectize-control {
          position: relative !important;
        }

        .selectize-dropdown {
          max-height: 200px !important;
          overflow-y: auto !important;
          z-index: 1050 !important;
        }
      ")
      ))
    )
  )
}
