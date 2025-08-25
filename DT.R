library(shiny)
library(DT)
library(dplyr)
library(tibble)

ui <- fluidPage(
  h3("DT: redigerbar tabel"),
  fluidRow(
    column(8, DTOutput("tbl")),
    column(4,
           actionButton("add_row", "Tilføj række"),
           actionButton("del_rows", "Slet markerede"),
           br(), br(),
           verbatimTextOutput("debug")
    )
  )
)

server <- function(input, output, session) {
  data_rv <- reactiveVal(
    tibble(
      dato = as.Date("2025-01-01") + 0:4,
      taeller = c(10,12,9,11,13),
      naevner = c(100,110,95,105,120)
    )
  )
  
  output$tbl <- renderDT({
    datatable(
      data_rv(),
      selection = "multiple",
      editable = "all",
      options = list(pageLength = 5, dom = "tip")
    )
  }, server = FALSE)
  
  # Håndter celle-redigering
  observeEvent(input$tbl_cell_edit, {
    info <- input$tbl_cell_edit
    df <- data_rv()
    i <- info$row; j <- info$col + 1  # +1 fordi DT tæller fra 0
    # Validering (eksempel): taeller/naevner skal være numeriske og ikke negative
    if (names(df)[j] %in% c("taeller","naevner")) {
      val <- suppressWarnings(as.numeric(info$value))
      if (is.na(val) || val < 0) return()  # afvis ugyldig opdatering
      df[i, j] <- val
    } else if (names(df)[j] == "dato") {
      val <- try(as.Date(info$value), silent = TRUE)
      if (inherits(val, "try-error") || is.na(val)) return()
      df[i, j] <- val
    } else {
      df[i, j] <- info$value
    }
    data_rv(df)
    replaceData(dataTableProxy("tbl"), df, resetPaging = FALSE, rownames = FALSE)
  })
  
  # Tilføj række
  observeEvent(input$add_row, {
    df <- add_row(data_rv(), dato = Sys.Date(), taeller = 0, naevner = 1)
    data_rv(df)
    replaceData(dataTableProxy("tbl"), df, resetPaging = FALSE, rownames = FALSE)
  })
  
  # Slet markerede rækker
  observeEvent(input$del_rows, {
    sel <- input$tbl_rows_selected
    if (length(sel) == 0) return()
    df <- data_rv() |> slice(-sel)
    data_rv(df)
    replaceData(dataTableProxy("tbl"), df, resetPaging = FALSE, rownames = FALSE)
  })
  
  output$debug <- renderPrint({
    df <- data_rv() |> mutate(procent = ifelse(naevner > 0, taeller/naevner, NA_real_))
    df
  })
}

shinyApp(ui, server)