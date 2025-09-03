library(shiny)
library(DT)

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
  rv <- reactiveVal({
    df <- head(mtcars, 15)
    df$note <- ""                 # tekst-kolonne
    df
  })

  output$tbl <- renderDT({
    datatable(
      rv(),
      class = "excel-theme cell-border compact hover",
      rownames = FALSE,
      editable = list(target = "cell"),
      extensions = c("FixedHeader", "FixedColumns", "KeyTable"),
      options = list(
        scrollX = TRUE,
        scrollY = "420px",
        paging  = FALSE,
        fixedHeader = TRUE,                    # frys top-række
        fixedColumns = list(leftColumns = 1),  # frys 1. kolonne
        keys = TRUE,                           # piletaster-fokus
        ordering = FALSE,
        columnDefs = list(
          list(className = "dt-right", targets = which(sapply(rv(), is.numeric)) - 1) # højrejustér tal
        )
      ),
      # Enter -> gem & hop ned til næste række i samme kolonne
      callback = JS("
        table.on('keydown', 'tbody td input', function(e){
          if(e.key === 'Enter'){
            e.preventDefault();
            var td = $(this).closest('td');
            $(this).blur(); // trigger Shiny update
            var cell = table.cell(td);
            var idx  = cell.index();
            var nextRow = idx.row + 1;
            if(nextRow < table.rows({page:'current'}).count()){
              var nextCell = table.cell(nextRow, idx.column).node();
              // lille delay så Shiny når at opdatere inden næste edit
              setTimeout(function(){ $(nextCell).click(); }, 10);
            }
          }
        });
      ")
    )
  })

  # modtag ændringer fra DT og skriv tilbage
  observeEvent(input$tbl_cell_edit, {
    info <- input$tbl_cell_edit
    d <- rv()
    d[info$row, info$col] <- DT::coerceValue(info$value, d[info$row, info$col])
    rv(d)
  })
}

shinyApp(ui, server)
