# app.R
library(shiny)
library(DT)
library(tibble)

ui <- fluidPage(
  tags$head(tags$style(HTML("
  /* --- Excel-ish tema til DT --- */
  .excel-theme table.dataTable {
    font-family: Calibri, 'Segoe UI', Arial, sans-serif;
    font-size: 13px;
  }
  .excel-theme table.dataTable thead th {
    background: #f3f3f3;
    border-bottom: 2px solid #bfbfbf;
    font-weight: 600;
    white-space: nowrap;
  }
  .excel-theme table.dataTable thead th,
  .excel-theme table.dataTable tbody td {
    border: 1px solid #d9d9d9 !important;   /* gitterlinjer */
  }
  .excel-theme table.dataTable tbody tr:hover td {
    background-color: #f8fbff !important;   /* diskret hover */
  }
  /* aktiv/redigeret celle: grøn kant som i Excel */
  .excel-theme table.dataTable tbody td:focus-within {
    outline: 2px solid #21a366;  /* Excel-grøn */
    outline-offset: -2px;
    background: #fff;
  }
  /* gør scrolleren visuelt pænere */
  .excel-theme .dataTables_scrollBody {
    border-left: 1px solid #d9d9d9;
    border-right: 1px solid #d9d9d9;
  }
  "))),
  h3("DT som Excel-agtig editor"),
  DTOutput("tbl", height = "520px")
)

server <- function(input, output, session) {
  # Eksempeldatasæt (med 'model' som første kolonne i stedet for rownames)
  df0 <- tibble::rownames_to_column(mtcars[1:10, 1:5], "model")
  rv  <- reactiveVal(df0)
  
  output$tbl <- renderDT({
    datatable(
      rv(),
      rownames  = FALSE,
      selection = "none",                 # undgå konflikt mellem klik og redigering
      editable  = "all",  # "alle celler" redigerbare
      options   = list(pageLength = 10, dom = "t"),
      # Gør redigering aktiv ved ét klik (simulerer dblclick)
      callback  = JS("
        table.on('click', 'tbody td', function() {
          $(this).dblclick();
        });
      ")
    )
  })
  
  proxy <- dataTableProxy("tbl")
  
  # Gem ændringer i den underliggende data.frame
  observeEvent(input$tbl_cell_edit, {
    info <- input$tbl_cell_edit
    i <- info$row
    j <- info$col + 1       # JS er 0-indekseret; R er 1-indekseret
    v <- info$value
    
    df <- rv()
    df[i, j] <- DT::coerceValue(v, df[i, j])  # bevar kolonnens datatype
    rv(df)
    replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)
  })
}

shinyApp(ui, server)
