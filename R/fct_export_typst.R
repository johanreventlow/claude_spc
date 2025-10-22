# R/fct_export_typst.R
# Typst PDF Export Functions
# Provides R integration for generating PDF reports via Typst/Quarto

# EXPORT CHART AS PNG =========================================================

#' Export SPC Chart as PNG for Typst Embedding
#'
#' Eksporterer en ggplot SPC chart som PNG i korrekt format til indlejring
#' i Typst dokumenter. Optimeret til A4 landscape layout.
#'
#' Title og subtitle fjernes automatisk fra plottet, da disse vises
#' separat i PDF layoutet. Plot margin sættes til 0mm for tight embedding.
#'
#' @param plot_object ggplot object. SPC chart der skal eksporteres.
#' @param output_path Character. Sti til output PNG fil.
#' @param width Numeric. Bredde i mm (default: 200mm, passende til A4 landscape).
#' @param height Numeric. Højde i mm (default: 120mm).
#' @param dpi Numeric. DPI opløsning (default: 300 for high quality).
#'
#' @return Character. Path til den genererede PNG fil.
#'
#' @examples
#' \dontrun{
#' plot <- create_spc_plot(data)
#' png_path <- export_chart_for_typst(plot, "chart.png")
#' }
#'
#' @family export_functions
#' @export
export_chart_for_typst <- function(
  plot_object,
  output_path,
  width = 200, # mm
  height = 120, # mm
  dpi = 300
) {
  # Validér inputs
  if (!inherits(plot_object, "ggplot")) {
    stop("plot_object must be a ggplot object", call. = FALSE)
  }

  if (is.null(output_path) || !is.character(output_path)) {
    stop("output_path must be provided as a character string", call. = FALSE)
  }

  # Log operation
  log_debug(
    component = "[TYPST_EXPORT]",
    message = "Eksporterer SPC chart til PNG",
    details = list(
      output = output_path,
      dimensions = sprintf("%gmm x %gmm", width, height),
      dpi = dpi
    )
  )

  # Konvertér mm til inches (ggplot2 bruger inches)
  width_in <- width / 25.4
  height_in <- height / 25.4

  # Fjern title og subtitle fra plot (vises allerede i PDF layout)
  # Sæt margin til 0 for tight embedding i PDF
  plot_clean <- plot_object +
    ggplot2::labs(title = NULL, subtitle = NULL) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "mm"))

  # Eksportér plot
  tryCatch(
    {
      ggplot2::ggsave(
        filename = output_path,
        plot = plot_clean,
        width = width_in,
        height = height_in,
        dpi = dpi,
        units = "in",
        device = "png"
      )

      log_info(
        component = "[TYPST_EXPORT]",
        message = "Chart eksporteret succesfuldt",
        details = list(
          output = output_path,
          size_kb = round(file.size(output_path) / 1024, 1)
        )
      )

      return(output_path)
    },
    error = function(e) {
      log_error(
        component = "[TYPST_EXPORT]",
        message = "Chart eksport fejlede",
        details = list(error = e$message)
      )
      stop("PNG export failed: ", e$message, call. = FALSE)
    }
  )
}

# CREATE TYPST DOCUMENT =======================================================

#' Create Typst Document from R Data
#'
#' Genererer en .typ fil med dynamisk indhold fra R data structures.
#' Bruger bfh-diagram2 template med fuld parametrisering.
#'
#' @param template Character. Template navn (kun "bfh-diagram2" supporteret).
#' @param output_path Character. Sti til output .typ fil.
#' @param hospital Character. Hospital navn.
#' @param department Character. Afdeling/afsnit navn (optional).
#' @param title Character. Chart titel.
#' @param analysis Character. Analyse tekst med findings (optional).
#' @param details Character. Periode info, gennemsnit, niveau (optional).
#' @param chart_image_path Character. Sti til chart PNG fil.
#' @param spc_stats List. SPC statistikker (runs, crossings, outliers).
#' @param data_definition Character. Datadefinition tekst (optional).
#' @param author Character. Forfatter navn (optional).
#' @param date Date. Rapport dato (default: Sys.Date()).
#'
#' @return Character. Path til den genererede .typ fil.
#'
#' @examples
#' \dontrun{
#' create_typst_document(
#'   template = "bfh-diagram2",
#'   output_path = "report.typ",
#'   hospital = "BFH",
#'   title = "SPC Chart",
#'   chart_image_path = "chart.png",
#'   spc_stats = list(runs_expected = 12, runs_actual = 10)
#' )
#' }
#'
#' @family export_functions
#' @export
create_typst_document <- function(
  template = c("bfh-diagram2"),
  output_path,
  hospital,
  department = NULL,
  title,
  analysis = NULL,
  details = NULL,
  chart_image_path,
  spc_stats = list(),
  data_definition = NULL,
  author = NULL,
  date = Sys.Date()
) {
  # Validér template
  template <- match.arg(template)

  if (template != "bfh-diagram2") {
    stop("Invalid template. Only 'bfh-diagram2' is currently supported.", call. = FALSE)
  }

  # Validér required parameters
  if (missing(output_path) || missing(hospital) || missing(title) || missing(chart_image_path)) {
    stop("Required parameters: output_path, hospital, title, chart_image_path", call. = FALSE)
  }

  log_debug(
    component = "[TYPST_EXPORT]",
    message = "Genererer Typst dokument",
    details = list(
      template = template,
      output = output_path,
      hospital = hospital
    )
  )

  # Hjælpefunktion: Konvertér R værdi til Typst syntax
  to_typst <- function(value, bracket = FALSE) {
    if (is.null(value)) {
      return("none")
    } else if (is.numeric(value)) {
      return(as.character(value))
    } else if (is.character(value)) {
      # Escape special characters (BEFORE markdown conversion)
      # Only escape backslashes and quotes, NOT asterisks (needed for markdown)
      escaped <- gsub("\\\\", "\\\\\\\\", value)
      escaped <- gsub("\"", "\\\\\"", escaped)

      # Convert CommonMark markdown to Typst markdown when in bracket mode
      # CommonMark (ggplot/marquee) -> Typst conversion:
      #   **bold** -> *bold*
      #   *italic* -> _italic_
      #   _italic_ -> _italic_ (same)
      if (bracket) {
        # Step 1: Protect already-converted bold by temporarily replacing * with placeholder
        # Convert CommonMark bold (**text**) to placeholder (##BOLD##text##/BOLD##)
        escaped <- gsub("\\*\\*([^*]+)\\*\\*", "##BOLD##\\1##/BOLD##", escaped)

        # Step 2: Convert CommonMark italic (*text*) to Typst italic (_text_)
        # Now we can safely match single * without conflicting with bold
        escaped <- gsub("\\*([^*]+?)\\*", "_\\1_", escaped)

        # Step 3: Convert placeholders back to Typst bold (*text*)
        escaped <- gsub("##BOLD##([^#]+)##/BOLD##", "*\\1*", escaped)

        # Handle line breaks for Typst
        # In Typst, line breaks need explicit markup: backslash followed by space or newline
        # We use "\ " (backslash-space) for hard line breaks
        lines <- strsplit(escaped, "\n", fixed = TRUE)[[1]]
        lines <- trimws(lines)
        # Join with backslash-space (Typst hard line break operator)
        escaped <- paste(lines, collapse = " \\\n")

        return(sprintf("[%s]", escaped))
      } else {
        return(sprintf('"%s"', escaped))
      }
    } else if (inherits(value, "Date")) {
      return(sprintf(
        "datetime(year: %d, month: %d, day: %d)",
        as.integer(format(value, "%Y")),
        as.integer(format(value, "%m")),
        as.integer(format(value, "%d"))
      ))
    } else {
      return("none")
    }
  }

  # Byg Typst dokument
  typst_content <- c(
    '#import "bfh-template/bfh-template.typ" : *',
    "",
    "// Auto-generated Typst document from SPCify R package",
    sprintf("// Generated: %s", Sys.time()),
    "",
    "#show: chart => bfh-diagram2(",
    sprintf("  hospital: %s,", to_typst(hospital)),
    sprintf("  department: %s,", to_typst(department, bracket = TRUE)),
    sprintf("  title: %s,", to_typst(title, bracket = TRUE)),
    sprintf("  details: %s,", to_typst(details, bracket = TRUE)),
    sprintf("  analysis: %s,", to_typst(analysis, bracket = TRUE)),
    sprintf("  data_definition: %s,", to_typst(data_definition, bracket = TRUE)),
    "",
    "  // SPC Statistics",
    sprintf("  runs_expected: %s,", to_typst(spc_stats$runs_expected)),
    sprintf("  runs_actual: %s,", to_typst(spc_stats$runs_actual)),
    sprintf("  crossings_expected: %s,", to_typst(spc_stats$crossings_expected)),
    sprintf("  crossings_actual: %s,", to_typst(spc_stats$crossings_actual)),
    sprintf("  outliers_expected: %s,", to_typst(spc_stats$outliers_expected)),
    sprintf("  outliers_actual: %s,", to_typst(spc_stats$outliers_actual)),
    "",
    sprintf("  author: %s,", to_typst(author)),
    sprintf("  date: %s,", to_typst(date)),
    "",
    "  chart",
    ")",
    "",
    "// Chart image",
    sprintf('#image("%s", height: 115mm)', chart_image_path)
  )

  # Skriv fil
  tryCatch(
    {
      writeLines(typst_content, output_path)

      log_info(
        component = "[TYPST_EXPORT]",
        message = "Typst dokument genereret",
        details = list(
          output = output_path,
          lines = length(typst_content)
        )
      )

      return(output_path)
    },
    error = function(e) {
      log_error(
        component = "[TYPST_EXPORT]",
        message = "Typst dokument generation fejlede",
        details = list(error = e$message)
      )
      stop("Typst document creation failed: ", e$message, call. = FALSE)
    }
  )
}

# COMPILE TYPST TO PDF ========================================================

#' Compile Typst to PDF via Quarto
#'
#' Kompilerer en .typ fil til PDF ved hjælp af Quarto's bundled Typst CLI.
#' Kræver Quarto >= 1.4 installeret på systemet (som inkluderer Typst 0.13+).
#'
#' @param typst_file Character. Sti til input .typ fil.
#' @param output_pdf Character. Sti til output PDF (optional, auto-generated hvis NULL).
#'
#' @return Character. Path til den genererede PDF fil.
#'
#' @examples
#' \dontrun{
#' compile_typst_to_pdf("report.typ", "report.pdf")
#' }
#'
#' @family export_functions
#' @export
compile_typst_to_pdf <- function(
  typst_file,
  output_pdf = NULL
) {
  # Validér input file eksisterer
  if (!file.exists(typst_file)) {
    stop("Typst file not found: ", typst_file, call. = FALSE)
  }

  # Check Quarto tilgængelighed
  quarto_path <- Sys.which("quarto")
  if (quarto_path == "") {
    log_error(
      component = "[TYPST_EXPORT]",
      message = "Quarto CLI ikke fundet",
      details = list(suggestion = "Installér Quarto fra https://quarto.org")
    )
    stop(
      "Quarto CLI not found. Please install Quarto from https://quarto.org",
      call. = FALSE
    )
  }

  # Check Quarto version (kræver >= 1.4 for Typst)
  version_output <- system2("quarto", "--version", stdout = TRUE, stderr = TRUE)
  version_num <- tryCatch(
    as.numeric_version(version_output[1]),
    error = function(e) NULL
  )

  if (!is.null(version_num) && version_num < "1.4.0") {
    log_warn(
      component = "[TYPST_EXPORT]",
      message = "Quarto version for gammel til Typst",
      details = list(current = version_output[1], required = ">= 1.4.0")
    )
    stop(
      "Quarto version >= 1.4.0 required for Typst format. Current: ",
      version_output[1],
      call. = FALSE
    )
  }

  log_debug(
    component = "[TYPST_EXPORT]",
    message = "Quarto version check OK",
    details = list(version = version_output[1])
  )

  # Set output path hvis ikke angivet
  if (is.null(output_pdf)) {
    output_pdf <- sub("\\.typ$", ".pdf", typst_file)
  }

  # Compile via Quarto's bundled Typst CLI
  log_info(
    component = "[TYPST_EXPORT]",
    message = "Starter Quarto Typst compilation",
    details = list(input = typst_file, output = output_pdf)
  )

  tryCatch(
    {
      # Kald quarto typst compile (bruger Quarto's bundled Typst CLI)
      # Dette er mere direkte end 'quarto render' og kræver ikke YAML-header
      result <- system2(
        "quarto",
        c("typst", "compile", shQuote(typst_file), shQuote(output_pdf)),
        stdout = TRUE,
        stderr = TRUE
      )

      # Check om PDF blev genereret
      expected_pdf <- output_pdf

      if (!file.exists(expected_pdf)) {
        stop("PDF file was not generated. Quarto Typst output: ", paste(result, collapse = "\n"))
      }

      log_info(
        component = "[TYPST_EXPORT]",
        message = "PDF genereret succesfuldt",
        details = list(
          output = output_pdf,
          size_kb = round(file.size(output_pdf) / 1024, 1)
        )
      )

      return(output_pdf)
    },
    error = function(e) {
      log_error(
        component = "[TYPST_EXPORT]",
        message = "Quarto Typst compilation fejlede",
        details = list(error = e$message, input = typst_file)
      )
      stop("PDF compilation failed: ", e$message, call. = FALSE)
    }
  )
}

# HIGH-LEVEL WRAPPER ==========================================================

#' Export SPC to Typst PDF
#'
#' High-level wrapper der orkestrerer hele PDF export flowet:
#' 1. Eksportér SPC chart til PNG
#' 2. Generér Typst dokument med metadata
#' 3. Kompilér til PDF via Quarto
#' 4. Cleanup temp filer
#'
#' @param plot_object ggplot object. SPC chart der skal eksporteres.
#' @param metadata List. Metadata (hospital, department, title, analysis, etc.).
#' @param spc_statistics List. SPC statistikker fra Anhøj rules.
#' @param output_path Character. Sti til output PDF fil.
#'
#' @return Character. Path til den genererede PDF fil.
#'
#' @examples
#' \dontrun{
#' plot <- create_spc_plot(data)
#' pdf <- export_spc_to_typst_pdf(
#'   plot_object = plot,
#'   metadata = list(
#'     hospital = "BFH",
#'     title = "SPC Chart",
#'     analysis = "Analysis text"
#'   ),
#'   spc_statistics = list(runs_expected = 12, runs_actual = 10),
#'   output_path = "report.pdf"
#' )
#' }
#'
#' @family export_functions
#' @export
export_spc_to_typst_pdf <- function(
  plot_object,
  metadata = list(),
  spc_statistics = list(),
  output_path
) {
  # Validér required parameters
  if (is.null(plot_object) || !inherits(plot_object, "ggplot")) {
    stop("plot_object is required and must be a ggplot object", call. = FALSE)
  }

  if (is.null(output_path) || !is.character(output_path)) {
    stop("output_path is required and must be a character string", call. = FALSE)
  }

  log_info(
    component = "[TYPST_EXPORT]",
    message = "Starter komplet Typst PDF export workflow",
    details = list(output = output_path)
  )

  # Opret temp directory til arbejdsfiler
  temp_dir <- tempfile("typst_export_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Kopier Typst template til temp directory
  # Dette er nødvendigt fordi .typ filen importerer template med relativ path
  template_source <- system.file(
    "templates/typst/bfh-template",
    package = "SPCify"
  )

  if (!dir.exists(template_source)) {
    stop("Typst template not found in package installation", call. = FALSE)
  }

  template_dest <- file.path(temp_dir, "bfh-template")
  dir.create(template_dest, recursive = TRUE)

  # Kopier alle filer fra template directory
  template_files <- list.files(template_source, recursive = TRUE, full.names = TRUE)
  for (file in template_files) {
    rel_path <- sub(paste0("^", template_source, "/"), "", file)
    dest_file <- file.path(template_dest, rel_path)

    # Opret subdirectories hvis nødvendigt
    dest_dir <- dirname(dest_file)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
    }

    # Kopier fil
    if (!file.info(file)$isdir) {
      file.copy(file, dest_file, overwrite = TRUE)
    }
  }

  log_debug(
    component = "[TYPST_EXPORT]",
    message = "Template kopieret til temp directory",
    details = list(
      template_source = template_source,
      template_dest = template_dest,
      files_copied = length(template_files)
    )
  )

  tryCatch(
    {
      # Step 1: Eksportér chart til PNG
      chart_png <- file.path(temp_dir, "chart.png")
      export_chart_for_typst(
        plot_object = plot_object,
        output_path = chart_png,
        width = 200,
        height = 120,
        dpi = 300
      )

      # Step 2: Generér Typst dokument
      typst_file <- file.path(temp_dir, "document.typ")
      create_typst_document(
        template = "bfh-diagram2",
        output_path = typst_file,
        # hospital = metadata$hospital %||% "Bispebjerg og Frederiksberg Hospital",
        hospital = "Bispebjerg og Frederiksberg Hospital",
        department = metadata$department,
        title = metadata$title %||% "Skriv en kort titel, eller tilføj en konklusion,\n**der tydeligt opsummerer, hvad grafen fortæller**",
        analysis = metadata$analysis,
        details = metadata$details,
        chart_image_path = basename(chart_png), # Relative path fra .typ fil
        spc_stats = spc_statistics,
        data_definition = metadata$data_definition,
        author = metadata$author,
        date = metadata$date %||% Sys.Date()
      )

      # Step 3: Kompilér til PDF
      pdf_result <- compile_typst_to_pdf(
        typst_file = typst_file,
        output_pdf = output_path
      )

      # Step 4: Cleanup sker automatisk via on.exit()

      log_info(
        component = "[TYPST_EXPORT]",
        message = "Komplet PDF export workflow succesfuldt",
        details = list(
          output = output_path,
          size_kb = round(file.size(output_path) / 1024, 1)
        )
      )

      return(pdf_result)
    },
    error = function(e) {
      log_error(
        component = "[TYPST_EXPORT]",
        message = "PDF export workflow fejlede",
        details = list(error = e$message, output = output_path)
      )
      stop("Export workflow failed: ", e$message, call. = FALSE)
    }
  )
}

# HELPER UTILITIES ============================================================

# Null coalescing operator (hjælper)
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
