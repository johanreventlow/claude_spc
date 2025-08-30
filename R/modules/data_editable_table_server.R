# R/modules/data_editable_table_server.R
# Server logic for editable table module

library(shiny)
library(rhandsontable)

# Editable Table Module Server
editableTableServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      current_data = NULL,
      original_data = NULL,
      history = list(),
      history_position = 0
    )
    
    # Initialize data when available
    observe({
      cat("DEBUG: editableTableServer - observe triggered\n")
      
      incoming_data <- data_reactive()
      
      if (!is.null(incoming_data)) {
        cat("DEBUG: editableTableServer - received data with", nrow(incoming_data), "rows\n")
        
        if(is.null(values$original_data)) {
          cat("DEBUG: editableTableServer - initializing original_data\n")
          values$original_data <- incoming_data
          values$current_data <- incoming_data
          
          # Initialize history
          values$history <- list(incoming_data)
          values$history_position <- 1
          cat("DEBUG: editableTableServer - initialization complete\n")
        }
      } else {
        cat("DEBUG: editableTableServer - received NULL data\n")
      }
    })
    
    # Has data check
    output$has_data <- reactive({
      !is.null(values$current_data)
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)
    
    # Table status
    output$table_status <- renderUI({
      if(is.null(values$current_data)) {
        div(
          span(class = "status-indicator status-warning"),
          "Ingen data tilgængelig",
          style = "font-size: 1rem;"
        )
      } else {
        changes_made <- !identical(values$current_data, values$original_data)
        status_class <- if(changes_made) "status-processing" else "status-ready"
        status_text <- if(changes_made) {
          "Data modificeret - ændringer er midlertidige"
        } else {
          "Data er synkroniseret med original"
        }
        
        div(
          span(class = paste("status-indicator", status_class)),
          status_text,
          style = "font-size: 1rem;"
        )
      }
    })
    
    # Render editable table
    output$editable_table <- rhandsontable::renderRHandsontable({
      req(values$current_data)
      
      data <- values$current_data
      
      hot <- rhandsontable::rhandsontable(
        data,
        width = 500, 
        stretchH = "all",
        contextMenu = TRUE,
        manualColumnResize = TRUE,
        manualRowResize = TRUE,
        fillHandle = list(direction = "vertical", autoInsertRow = TRUE)
      ) %>%
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE,
          allowColEdit = TRUE,
          allowReadOnly = FALSE,
          allowComments = FALSE
        ) %>%
        rhandsontable::hot_table(
          highlightCol = TRUE, 
          highlightRow = TRUE,
          stretchH = "all"
        )
      
      return(hot)
    })
    
    # Handle table changes
    observeEvent(input$editable_table, {
      req(input$editable_table)
      
      new_data <- rhandsontable::hot_to_r(input$editable_table)
      values$current_data <- new_data
      
      # Add to history
      values$history_position <- values$history_position + 1
      values$history <- c(values$history[1:(values$history_position - 1)], list(new_data))
      
      if(length(values$history) > 50) {
        values$history <- values$history[-1]
        values$history_position <- values$history_position - 1
      }
    })
    
    # Undo functionality
    observeEvent(input$undo, {
      if (values$history_position > 1) {
        values$history_position <- values$history_position - 1
        values$current_data <- values$history[[values$history_position]]
      }
    })
    
    # Redo functionality
    observeEvent(input$redo, {
      if (values$history_position < length(values$history)) {
        values$history_position <- values$history_position + 1
        values$current_data <- values$history[[values$history_position]]
      }
    })
    
    # Add row
    observeEvent(input$add_row, {
      req(values$current_data)
      
      new_row <- values$current_data[1, ]
      new_row[1, ] <- NA
      
      values$current_data <- rbind(values$current_data, new_row)
      
      showNotification("Ny række tilføjet", type = "message", duration = 2)
    })
    
    # Delete row
    observeEvent(input$delete_row, {
      req(values$current_data, nrow(values$current_data) > 1)
      
      values$current_data <- values$current_data[-nrow(values$current_data), ]
      
      showNotification("Række slettet", type = "message", duration = 2)
    })
    
    # Reset to original
    observeEvent(input$reset, {
      req(values$original_data)
      
      values$current_data <- values$original_data
      values$history <- list(values$original_data)
      values$history_position <- 1
      
      showNotification("Data nulstillet til original", type = "message", duration = 3)
    })
    
    # Validation messages for edited data
    output$edit_validation_messages <- renderUI({
      req(values$current_data)
      
      validation <- validateDataStructure(values$current_data)
      
      if(length(validation$warnings) > 0 || length(validation$errors) > 0) {
        
        messages <- tagList()
        
        if(length(validation$errors) > 0) {
          messages <- tagList(messages,
                              div(
                                class = "alert alert-danger",
                                style = "margin-bottom: 15px;",
                                icon("exclamation-triangle"),
                                strong(" Fejl i redigeret data:"),
                                tags$ul(
                                  lapply(validation$errors, function(err) tags$li(err))
                                )
                              )
          )
        }
        
        if(length(validation$warnings) > 0) {
          messages <- tagList(messages,
                              div(
                                class = "alert alert-warning", 
                                style = "margin-bottom: 15px;",
                                icon("exclamation-circle"),
                                strong(" Advarsler for redigeret data:"),
                                tags$ul(
                                  lapply(validation$warnings, function(warn) tags$li(warn))
                                )
                              )
          )
        }
        
        return(messages)
      }
      
      return(NULL)
    })
    
    # Return current data
    return(
      reactive(values$current_data)
    )
  })
}
