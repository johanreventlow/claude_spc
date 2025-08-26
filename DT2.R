# app.R
library(shiny)
library(DT)
library(tibble)

ui <- fluidPage(
  tags$head(tags$style(HTML("
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
      editable  = "all",  
      # options   = list(pageLength = 10, dom = "t"),
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
