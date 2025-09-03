# BFH UTH Browser – Optimeret Shiny-app ---------------------------------
# Performance forbedringer og kode cleanup

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(lubridate)
library(DT)
library(janitor)
library(plotly)
library(rio)
library(purrr)
library(data.table)
library(munsell)

# Hjælpefunktion: Værdibokse ----------------------------------------------
value_boxes_ui <- function(id_prefix = "") {
  layout_column_wrap(
    width = 1 / 2,
    heights_equal = "row",
    # max_height = "360px",
    min_height = "360px",
    value_box(
      title = "Antal sager",
      value = textOutput(paste0(id_prefix, "antal_sager")),
      showcase = bsicons::bs_icon("stack"),
      theme_color = "primary",
      showcase_layout = "left center",
      full_screen = FALSE,
      fill = TRUE,
      height = "120px"
    ),
    value_box(
      title = "Maksimum serielængde",
      value = textOutput(paste0(id_prefix, "antal_uafsluttede")),
      showcase = bsicons::bs_icon("shuffle"),
      theme_color = "warning",
      showcase_layout = "left center",
      full_screen = FALSE,
      fill = TRUE,
      height = "120px"
    ),
    
    value_box(
      title = "Uafsluttede ≤ 90 dage",
      value = textOutput(paste0(id_prefix, "antal_nye")),
      showcase = bsicons::bs_icon("calendar-check"),
      theme_color = "success",
      showcase_layout = "left center",
      full_screen = FALSE,
      fill = TRUE,
      height = "120px"
    ),
    
    

    value_box(
      # title = "Antal kryds",
      title = layout_column_wrap(width = 1 / 3,"Antal Kryds", "Forventet", "Faktisk"),
      value = layout_column_wrap(width = 1 / 3,"", "20", "10"),
      
      
      
      
      # showcase = bsicons::bs_icon("hash"),
      theme_color = "danger",
      # showcase_layout = "left center",
      full_screen = FALSE,
      fill = TRUE,
      height = "120px"
    ),
    

    
    
    value_box(
      title = "Antal sager",
      value = textOutput(paste0(id_prefix, "antal_sager")),
      showcase = bsicons::bs_icon("stack"),
      theme_color = "secondary",
      showcase_layout = "left center",
      full_screen = FALSE,
      fill = TRUE,
      height = "120px"
    ),
    value_box(
      title = "Obs. uden for kontrolgrænse",
      value = textOutput(paste0(id_prefix, "antal_uafsluttede")),
      showcase = bsicons::bs_icon("asterisk"),
      theme_color = "info",
      showcase_layout = "left center",
      full_screen = FALSE,
      fill = TRUE,
      height = "120px"
    ),
  )
}

# UI ----------------------------------------------------------------------
my_theme <- bs_theme(bootswatch = "flatly")

ui <- page_navbar(
  title = tagList(
    img(
      src = "Logo_Bispebjerg_og Frederiksberg_RGB_neg.png",
      height = "40px",
      style = "margin-right: 10px;"
    ),
    div("UTH DASHBOARD", style = "position: absolute; right: 20px; top: 20px; font-weight: bold;")
  ),
  theme = my_theme,
  inverse = FALSE,
  
  sidebar = sidebar(
    fileInput(
      "filvalg",
      "Vælg datafil (Excel eller CSV)",
      accept = c(".xlsx", ".xls", ".csv", ".CSV")
    ),
    actionButton("nulstil", "Nulstil filtre", icon = icon("undo")),
    uiOutput("filafhaengige_input")
  ),
  
  nav_panel(
    "Overblik",
      condition = "output.dataLoaded != 'TRUE'",
      layout_columns(
        # col_widths = c(12, 6, 6, 6, 6),
        col_widths = c(6, 6, 6, 6),
        height = "auto",
        max_height = "100%",
        # value_boxes_ui("overblik_"),
        card(card_header(div(icon("sliders-h")," Indstillinger",)), full_screen = TRUE, plotlyOutput("plot_status")),
        card(card_header(div(icon("chart-line")," SPC Graf",)), full_screen = TRUE, plotlyOutput("plot_tid")),
        card(card_header(div(icon("table")," Data",)),full_screen = TRUE, plotlyOutput("plot_afdeling")),
        # card(full_screen = TRUE, plotlyOutput("plot_sagsbehandling"))
        value_boxes_ui("overblik_")
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100*1024^2)
  
  # Theme farver cache
  theme_colors <- list(
    primary = bs_get_variables(my_theme, "primary") |> as.character(),
    secondary = bs_get_variables(my_theme, "secondary") |> as.character(),
    success = bs_get_variables(my_theme, "success") |> as.character(),
    info = bs_get_variables(my_theme, "info") |> as.character(),
    warning = bs_get_variables(my_theme, "warning") |> as.character(),
    danger = bs_get_variables(my_theme, "danger") |> as.character()
  )
  
  # PERFORMANCE BOOST 1: Cache raw data --------------------------------
  raw_data <- reactiveVal(NULL)
  
  # Compatibility wrapper for original code
  dpsd_data <- reactive({ raw_data() })
  
  # PERFORMANCE BOOST 2: Efficient data loading ------------------------
  observeEvent(input$filvalg, {
    req(input$filvalg)
    ext <- tools::file_ext(input$filvalg$name) |> str_to_lower()
    validate(need(ext %in% c("xlsx", "xls", "csv"), "Filtype ikke understøttet."))
    
    withProgress(message = "Indlæser data...", value = 0, {
      tryCatch({
        incProgress(0.3, detail = "Læser fil...")
        
        if (ext == "csv") {
          df <- data.table::fread(
            input$filvalg$datapath,
            sep = ";", quote = "\"", skip = 3, encoding = "Latin-1",
            fill = TRUE, strip.white = TRUE, na.strings = c("", "NA")
          )
        } else {
          df <- rio::import(input$filvalg$datapath, skip = 3, col_types = "text")
        }
        
        incProgress(0.4, detail = "Behandler data...")
        
        # Efficient data processing
        df <- df |>
          clean_names() |>
          mutate(
            across(contains("dato"), ~ {
              if (ext == "csv") {
                dmy(.)
              } else {
                suppressWarnings(excel_numeric_to_date(as.numeric(.)))
              }
            })
          ) |>
          arrange(desc(opret_dato)) |>
          mutate(
            across(c(sagsstatus, samlet_alvorlighedskategori, dpsd_hovedgruppe, 
                     dpsd_problem, dpsd_proces, afdeling), 
                   ~ replace_na(as.character(.), "(ikke angivet)")),
            status_afsluttet = case_when(
              sagsstatus %in% c("Lukket", "Afvist", "Slettet") ~ "Afsluttet",
              TRUE ~ "Uafsluttet"
            ),
            status_afsluttet = factor(status_afsluttet, levels = c("Afsluttet", "Uafsluttet"))
          )
        
        incProgress(0.3, detail = "Færdiggør...")
        raw_data(df)
        
      }, error = function(e) {
        showNotification(paste("Fejl ved indlæsning:", e$message), type = "error")
        raw_data(NULL)
      })
    })
  })
  
  # PERFORMANCE BOOST 3: Debounced text search -------------------------
  fritekst_debounced <- reactive(input$fritekst) %>% debounce(500)
  
  # PERFORMANCE BOOST 4: Efficient filtering ---------------------------
  filtreret_data <- reactive({
    df <- raw_data()
    req(df)
    
    # Dato filtering
    if (!is.null(input$dato)) {
      df <- df |> filter(opret_dato >= input$dato[1], opret_dato <= input$dato[2])
    }
    
    # Categorical filters - batch processing
    filters <- list(
      afdeling = input$afdeling,
      sagsstatus = input$sagsstatus,
      samlet_alvorlighedskategori = input$samlet_alvorlighedskategori,
      dpsd_hovedgruppe = input$hovedgruppe
    )
    
    for (col in names(filters)) {
      if (!is.null(filters[[col]]) && length(filters[[col]]) > 0) {
        df <- df |> filter(.data[[col]] %in% filters[[col]])
      }
    }
    
    # Text search - optimized
    fritekst <- fritekst_debounced()
    if (!is.null(fritekst) && nzchar(fritekst)) {
      søgeord <- str_split(tolower(fritekst), "\\s+")[[1]]
      tekstfelter <- c("titel", "haendelsesbeskrivelse", "hvordan_kunne_det_ske",
                       "forslag_til_forebyggelse", "beskrivelse_af_sagsopfolgning", "laering_og_tiltag")
      
      # More efficient text search - keeping original OR logic
      if (all(tekstfelter %in% names(df))) {
        df <- df |> 
          rowwise() |>
          filter({
            samlet_tekst <- tolower(paste(c_across(all_of(tekstfelter)), collapse = " "))
            any(map_lgl(søgeord, ~ str_detect(samlet_tekst, fixed(.x))))
          }) |>
          ungroup()
      }
    }
    
    df
  })
  
  # PERFORMANCE BOOST 5: Cached summary statistics ---------------------
  summary_stats <- reactive({
    df <- filtreret_data()
    req(nrow(df) > 0)
    
    list(
      total = nrow(df),
      uafsluttede = sum(df$status_afsluttet == "Uafsluttet"),
      nye = sum(df$status_afsluttet == "Uafsluttet" & df$opret_dato >= Sys.Date() - 90),
      gamle = sum(df$status_afsluttet == "Uafsluttet" & df$opret_dato < Sys.Date() - 90)
    )
  })
  
  # Data loading status
  output$dataLoaded <- renderText({
    if (is.null(raw_data())) "FALSE" else "TRUE"
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  output$harSagsdetaljer <- renderText({
    df <- raw_data()
    req(df)
    required_vars <- c("titel", "haendelsesbeskrivelse", "hvordan_kunne_det_ske",
                       "forslag_til_forebyggelse", "beskrivelse_af_sagsopfolgning", "laering_og_tiltag")
    if (all(required_vars %in% names(df))) "TRUE" else "FALSE"
  })
  outputOptions(output, "harSagsdetaljer", suspendWhenHidden = FALSE)
  
  # UI inputs
  output$filafhaengige_input <- renderUI({
    df <- raw_data()
    req(df)
    
    tagList(
      textInput("fritekst", "Fritekstsøgning", value = ""),
      dateRangeInput(
        "dato", "Oprettelsesdato",
        start = min(df$opret_dato, na.rm = TRUE),
        end = max(df$opret_dato, na.rm = TRUE),
        startview = "year", weekstart = 1, language = "da", separator = "til"
      ),
      actionButton("seneste7", "Vis kun seneste 7 dage", icon = icon("calendar")),
      selectInput("sagsstatus", "Sagsstatus", choices = sort(unique(df$sagsstatus)), 
                  selected = NULL, multiple = TRUE),
      selectInput("afdeling", "Afdeling", choices = sort(unique(df$afdeling)), 
                  selected = NULL, multiple = TRUE),
      selectInput("samlet_alvorlighedskategori", "Samlet alvorlighed", 
                  choices = sort(unique(df$samlet_alvorlighedskategori)), selected = NULL, multiple = TRUE),
      selectInput("hovedgruppe", "DPSD Hovedgruppe", choices = sort(unique(df$dpsd_hovedgruppe)), 
                  selected = NULL, multiple = TRUE)
    )
  })
  
  # Filter reset actions
  observeEvent(input$seneste7, {
    df <- raw_data()
    req(df)
    updateDateRangeInput(session, "dato",
                         start = max(df$opret_dato, na.rm = TRUE) - 6,
                         end = max(df$opret_dato, na.rm = TRUE))
  })
  
  observeEvent(input$nulstil, {
    df <- raw_data()
    req(df)
    updateSelectInput(session, "afdeling", selected = character(0))
    updateSelectInput(session, "sagsstatus", selected = character(0))
    updateSelectInput(session, "samlet_alvorlighedskategori", selected = character(0))
    updateSelectInput(session, "hovedgruppe", selected = character(0))
    updateDateRangeInput(session, "dato",
                         start = min(df$opret_dato, na.rm = TRUE),
                         end = max(df$opret_dato, na.rm = TRUE))
    updateTextInput(session, "fritekst", value = "")
  })
  
  # Period formatting
  format_periode <- reactive({
    if (is.null(input$dato)) return("(Periode ikke valgt)")
    sprintf("(Periode: %s – %s)",
            format(input$dato[1], "%d. %b %Y"),
            format(input$dato[2], "%d. %b %Y"))
  })
  
  # PERFORMANCE BOOST 6: Unified value box outputs ---------------------
  # Generate all value box outputs efficiently
  output$overblik_antal_sager <- output$alvorlighed_antal_sager <- 
    output$hovedgruppe_antal_sager <- output$tabel_antal_sager <- renderText({
      stats <- summary_stats()
      req(stats)
      stats$total
    })
  
  output$overblik_antal_uafsluttede <- output$alvorlighed_antal_uafsluttede <- 
    output$hovedgruppe_antal_uafsluttede <- output$tabel_antal_uafsluttede <- renderText({
      stats <- summary_stats()
      req(stats)
      stats$uafsluttede
    })
  
  output$overblik_antal_nye <- output$alvorlighed_antal_nye <- 
    output$hovedgruppe_antal_nye <- output$tabel_antal_nye <- renderText({
      stats <- summary_stats()
      req(stats)
      stats$nye
    })
  
  output$overblik_antal_gamle <- output$alvorlighed_antal_gamle <- 
    output$hovedgruppe_antal_gamle <- output$tabel_antal_gamle <- renderText({
      stats <- summary_stats()
      req(stats)
      stats$gamle
    })
  
  # PERFORMANCE BOOST 7: Optimized plot functions -------------------
  plot_stable_bar <- function(data, x_var, title) {
    df <- data %>%
      count({{ x_var }}, status_afsluttet, name = "n") %>%
      mutate({{ x_var }} := fct_reorder({{ x_var }}, n, .fun = sum))
    
    x_sym <- rlang::enquo(x_var)
    x_name <- rlang::as_name(x_sym)
    
    df %>%
      mutate(label = .data[[x_name]]) %>%
      plot_ly(x = ~n, y = ~reorder(label, n), color = ~status_afsluttet,
              colors = c("Uafsluttet" = theme_colors$secondary, "Afsluttet" = theme_colors$primary),
              type = "bar", orientation = "h") %>%
      config(modeBarButtonsToRemove = c('zoom2d','pan2d','lasso2d','select','resetScale',
                                        'hoverClosestCartesian','hoverCompareCartesian'),
             displaylogo = FALSE) %>%
      layout(title = list(text = paste0(title, '<br>', '<sup>', format_periode(), '</sup>')),
             xaxis = list(title = "Antal"), yaxis = list(title = ""),
             barmode = "stack", legend = list(title = list(text = "Afsluttet")),
             margin = list(t = 35, pad = 5))
  }
  
  plot_afdeling_top25 <- function(df) {
    df <- df |>
      count(afdeling, status_afsluttet, name = "antal") |>
      group_by(afdeling) |> mutate(total = sum(antal)) |> ungroup()
    
    top_afdelinger <- df |> 
      summarise(total = sum(antal), .by = afdeling) |> 
      slice_max(total, n = 25)
    
    df <- df |>
      mutate(afdeling = if_else(afdeling %in% top_afdelinger$afdeling, afdeling, "[Øvrige afdelinger]")) |>
      group_by(afdeling, status_afsluttet) |> 
      summarise(antal = sum(antal), .groups = "drop") |>
      group_by(afdeling) |> mutate(total = sum(antal)) |> ungroup() |>
      mutate(afdeling = fct_reorder(afdeling, total))
    
    plot_ly(data = df, x = ~antal, y = ~reorder(afdeling, total), color = ~status_afsluttet,
            colors = c("Uafsluttet" = theme_colors$secondary, "Afsluttet" = theme_colors$primary),
            type = "bar", orientation = "h") %>%
      config(modeBarButtonsToRemove = c('zoom2d','pan2d','lasso2d','select','resetScale',
                                        'hoverClosestCartesian','hoverCompareCartesian'),
             displaylogo = FALSE) %>%
      layout(title = list(text = paste0('Sager pr. afdeling<br><sup>', format_periode(), '</sup>')),
             xaxis = list(title = "Antal"), yaxis = list(title = ""),
             barmode = "stack", legend = list(title = list(text = "Afsluttet")),
             margin = list(t = 35, pad = 5))
  }
  
  # Reactive value to track if plots have been rendered initially
  plots_initialized <- reactiveVal(FALSE)
  
  # Force initial rendering of all plots when data is loaded
  observe({
    req(nrow(filtreret_data()) > 0)
    if (!plots_initialized()) {
      # Trigger all plot outputs to render initially across all tabs
      outputOptions(output, "plot_status", suspendWhenHidden = FALSE)
      outputOptions(output, "plot_tid", suspendWhenHidden = FALSE) 
      outputOptions(output, "plot_afdeling", suspendWhenHidden = FALSE)
      outputOptions(output, "plot_sagsbehandling", suspendWhenHidden = FALSE)
      outputOptions(output, "plot_alvorlighed", suspendWhenHidden = FALSE)
      outputOptions(output, "plot_faktisk_konsekvens", suspendWhenHidden = FALSE)
      outputOptions(output, "plot_mulig_konsekvens", suspendWhenHidden = FALSE)
      outputOptions(output, "plot_hovedgruppe", suspendWhenHidden = FALSE)
      plots_initialized(TRUE)
    }
  })
  
  # Plot outputs - with conditional caching after initial render
  output$plot_status <- renderPlotly({
    req(nrow(filtreret_data()) > 0)
    plot_stable_bar(filtreret_data(), sagsstatus, "Sager pr. status")
  })
  
  output$plot_tid <- renderPlotly({
    req(nrow(filtreret_data()) > 0)
    
    # Definer periode for de seneste 12 måneder
    seneste_12_maaneder <- floor_date(Sys.Date() %m-% months(11), "month")
    
    # Data for oprettede sager
    oprettede <- filtreret_data() |>
      mutate(maaned = floor_date(opret_dato, "month")) |>
      filter(maaned >= seneste_12_maaneder) |>
      count(maaned, name = "oprettede")
    
    # Data for afsluttede sager (kun dem der er afsluttet)
    afsluttede <- filtreret_data() |>
      filter(!is.na(sagsafslutning_dato)) |>
      mutate(maaned = floor_date(sagsafslutning_dato, "month")) |>
      filter(maaned >= seneste_12_maaneder) |>
      count(maaned, name = "afsluttede")
    
    # Kombinér data
    df <- full_join(oprettede, afsluttede, by = "maaned") |>
      replace_na(list(oprettede = 0, afsluttede = 0)) |>
      arrange(maaned)
    
    plot_ly(data = df) %>%
      add_trace(x = ~maaned, y = ~oprettede, type = "scatter", mode = "lines+markers",
                name = "Oprettede", line = list(color = theme_colors$success),
                marker = list(color = theme_colors$success)) %>%
      add_trace(x = ~maaned, y = ~afsluttede, type = "scatter", mode = "lines+markers", 
                name = "Afsluttede", line = list(color = theme_colors$primary),
                marker = list(color = theme_colors$primary)) %>%
      config(modeBarButtonsToRemove = c('zoom2d','pan2d','lasso2d','select','resetScale',
                                        'hoverClosestCartesian','hoverCompareCartesian'),
             displaylogo = FALSE) %>%
      layout(title = list(text = paste0('Antal sager pr. måned<br><sup>', format_periode(), '</sup>')),
             xaxis = list(title = "", type = "date"),
             yaxis = list(title = "Antal", rangemode = "tozero"),
             legend = list(title = list(text = "")),
             margin = list(t = 35, pad = 5))
  })
  
  output$plot_afdeling <- renderPlotly({
    req(nrow(filtreret_data()) > 0)
    plot_afdeling_top25(filtreret_data())
  })
  
  output$plot_sagsbehandling <- renderPlotly({
    req(nrow(filtreret_data()) > 0)
    df <- filtreret_data() |>
      mutate(
        afslutningsdato = if_else(status_afsluttet == "Afsluttet", sagsafslutning_dato, Sys.Date()),
        behandlingsdage = as.numeric(afslutningsdato - opret_dato),
        afslut_maaned = if_else(status_afsluttet == "Afsluttet", 
                                floor_date(sagsafslutning_dato, "month"), as.Date(NA))
      ) |>
      mutate(gruppe = if_else(is.na(afslut_maaned), "Uafsluttede", format(afslut_maaned, "%b %Y"))) |>
      filter(gruppe == "Uafsluttede" | afslut_maaned >= floor_date(Sys.Date() %m-% months(11), "month"))
    
    df <- df |> 
      mutate(
        gruppe = factor(gruppe, levels = c(format(seq(floor_date(Sys.Date() %m-% months(11), "month"), 
                                                      floor_date(Sys.Date(), "month"), by = "1 month"), "%b %Y"), "Uafsluttede")),
        gruppe_num = as.numeric(gruppe)
      )
    
    plot_ly(data = df, x = ~jitter(gruppe_num, amount = 0.25), y = ~behandlingsdage,
            color = ~status_afsluttet,
            colors = c("Uafsluttet" = theme_colors$secondary, "Afsluttet" = theme_colors$primary),
            text = ~paste("Sag:", sagsnummer, "<br>Oprettet:", format(opret_dato, "%d-%m-%Y"), 
                          "<br>Afsluttet:", ifelse(status_afsluttet == "Afsluttet", 
                                                   format(sagsafslutning_dato, "%d-%m-%Y"), "Uafsluttet"), 
                          "<br>Behandlingsdage:", behandlingsdage),
            hoverinfo = "text", type = "scatter", mode = "markers",
            marker = list(size = 6, opacity = 0.4)) %>%
      config(modeBarButtonsToRemove = c('zoom2d','pan2d','lasso2d','select','resetScale',
                                        'hoverClosestCartesian','hoverCompareCartesian'),
             displaylogo = FALSE) %>%
      layout(title = list(text = paste0("Sagsbehandlingstid<br><sup>", format_periode(), "</sup>")),
             xaxis = list(title = "Afslutningsmåned", type = "linear", tickmode = "array",
                          tickvals = 1:length(levels(df$gruppe)), ticktext = levels(df$gruppe)),
             yaxis = list(title = "Antal dage"),
             shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 90, y1 = 90, yref = "y",
                                line = list(dash = "dot", width = 1, color = "gray"))),
             annotations = list(list(x = 1, xref = "paper", y = 90, yref = "y", text = "90 dages frist",
                                     showarrow = FALSE, xanchor = "left", font = list(size = 11, color = "gray"))))
  })
  
  output$plot_alvorlighed <- renderPlotly({
    req(nrow(filtreret_data()) > 0)
    plot_stable_bar(filtreret_data(), samlet_alvorlighedskategori, "Sager pr. samlet alvorlighed")
  })
  
  output$plot_faktisk_konsekvens <- renderPlotly({
    req(nrow(filtreret_data()) > 0)
    plot_stable_bar(filtreret_data(), faktisk_konsekvens, "Sager pr. faktisk konsekvens")
  })
  
  output$plot_mulig_konsekvens <- renderPlotly({
    req(nrow(filtreret_data()) > 0)
    plot_stable_bar(filtreret_data(), mulig_konsekvens, "Sager pr. mulig konsekvens")
  })
  
  output$plot_hovedgruppe <- renderPlotly({
    req(nrow(filtreret_data()) > 0)
    plot_stable_bar(filtreret_data(), dpsd_hovedgruppe, "Sager pr. DPSD hovedgruppe")
  })
  
  # PERFORMANCE BOOST 8: Efficient data table -------------------------
  output$sagstabel <- renderDT({
    df <- filtreret_data()
    req(nrow(df) > 0)
    
    df |> 
      select(Sagsnummer = sagsnummer, Oprettelsesdato = opret_dato, Sagsstatus = sagsstatus,
             Titel = titel, `Samlet alvorlighed` = samlet_alvorlighedskategori, Afdeling = afdeling)
  }, selection = 'single', server = TRUE, rownames = FALSE,
  options = list(pageLength = 10, scrollY = "calc(100vh - 200px)", scrollCollapse = TRUE, searching = FALSE))
  
  # Modal dialog for case details
  current_index <- reactiveVal(1)
  
  observeEvent(input$sagstabel_rows_selected, {
    selected <- input$sagstabel_rows_selected
    req(length(selected) == 1)
    current_index(selected)
    
    showModal(
      modalDialog(
        title = uiOutput("modal_title"),
        uiOutput("modal_content"),
        footer = tagList(
          actionButton("prev_case", label = tagList(bsicons::bs_icon("arrow-left-short"), "Forrige")),
          span(textOutput("case_position", inline = TRUE), style = "margin: 0 10px;"),
          actionButton("next_case", label = tagList("Næste", bsicons::bs_icon("arrow-right-short"))),
          modalButton("Luk")
        ),
        easyClose = TRUE, size = "l"
      )
    )
  })
  
  output$modal_title <- renderUI({
    index <- current_index()
    data <- filtreret_data()
    if (nrow(data) >= index && index >= 1) {
      sag <- data[index, ]
      h4(sag$titel)
    }
  })
  
  output$modal_content <- renderUI({
    index <- current_index()
    data <- filtreret_data()
    if (nrow(data) >= index && index >= 1) {
      sag <- data[index, ]
      tagList(
        fluidRow(
          column(6,
                 p(strong("Sagsnummer:"), HTML(as.character(case_when(
                   sag$sagsstatus %in% c("Lukket", "Afvist", "Slettet") ~ as.character(tags$span(
                     a(sag$sagsnummer, href = paste0("https://dpsd.csc.dsdn.dk/Form/Closed.aspx?file=", sag$sagsnummer, "&form=DPSD_Sagsbehandling"), target = "_blank"),
                     HTML("&nbsp;"), bsicons::bs_icon("box-arrow-up-right")
                   )),
                   sag$sagsstatus %in% c("Åben", "Accepteret") ~ as.character(tags$span(
                     a(sag$sagsnummer, href = paste0("https://dpsd.csc.dsdn.dk/Form/Managing.aspx?file=", sag$sagsnummer, "&form=DPSD_Sagsbehandling"), target = "_blank"),
                     HTML("&nbsp;"), bsicons::bs_icon("box-arrow-up-right")
                   )),
                   sag$sagsstatus %in% c("Oprettet", "Videresendt") ~ as.character(tags$span(
                     a(sag$sagsnummer, href = paste0("https://dpsd.csc.dsdn.dk/Form/Receiving.aspx?file=", sag$sagsnummer, "&form=DPSD_Initialmodtagelse"), target = "_blank"),
                     HTML("&nbsp;"), bsicons::bs_icon("box-arrow-up-right")
                   )),
                   TRUE ~ sag$sagsnummer
                 )))),
                 p(strong("Sagsstatus:"), sag$sagsstatus),
                 p(strong("Oprettelsesdato:"), format(sag$opret_dato, "%d-%m-%Y")),
                 if (!is.na(sag$sagsafslutning_dato)) p(strong("Afslutningsdato:"), format(sag$sagsafslutning_dato, "%d-%m-%Y"))
          ),
          column(6,
                 p(strong("Mulig konsekvens:"), sag$mulig_konsekvens),
                 p(strong("Faktisk konsekvens:"), sag$faktisk_konsekvens),
                 p(strong("Samlet alvorlighed:"), sag$samlet_alvorlighedskategori)
          )
        ),
        p(strong("Afdeling:"), sag$afdeling),
        div(
          strong("Beskrivelse og opfølgning:"),
          div(
            style = "height: 400px; overflow-y: auto; border: 1px solid #ccc; padding: 10px; margin-top: 5px; background-color: #f9f9f9;",
            HTML(paste(
              unlist(Map(function(navn, tekst) {
                if (!is.null(tekst) && !is.na(tekst) && nzchar(tekst)) {
                  paste0("<b>", navn, ":</b><br>", gsub("\n", "<br>", htmltools::htmlEscape(tekst)))
                } else {
                  NULL
                }
              },
              navn = c("Hændelsesbeskrivelse", "Hvordan kunne det ske?", "Forslag til forebyggelse",
                       "Beskrivelse af sagsopfølgning", "Læring og tiltag"),
              tekst = c(sag$haendelsesbeskrivelse, sag$hvordan_kunne_det_ske, sag$forslag_til_forebyggelse,
                        sag$beskrivelse_af_sagsopfolgning, sag$laering_og_tiltag))),
              collapse = "<br><br>"
            ))
          )
        )
      )
    }
  })
  
  output$case_position <- renderText({
    index <- current_index()
    total <- nrow(filtreret_data())
    paste0("Sag ", index, " af ", total)
  })
  
  observeEvent(input$prev_case, {
    index <- current_index()
    if (index > 1) current_index(index - 1)
  })
  
  observeEvent(input$next_case, {
    index <- current_index()
    if (index < nrow(filtreret_data())) current_index(index + 1)
  })
}

shinyApp(ui, server)

