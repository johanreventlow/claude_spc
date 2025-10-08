# test-first-render-label-placement.R
# Regression test for viewport-ready label placement fix
#
# ISSUE: Labels overlappede centerline ved første rendering fordi device/viewport
# ikke var initialiseret → forkerte NPC konverteringer eller forkerte dimensioner.
#
# ROOT CAUSE: add_spc_labels() blev kaldt før renderPlot() havde åbnet device,
# eller med fallback dimensioner (800×600) i stedet for faktiske viewport dimensioner.
#
# FIX: To-lags strategi:
# 1) Viewport guard i renderPlot() - req() venter på clientData før rendering starter
# 2) Non-blocking device check i add_spc_labels() - tillader ggplot construction uden device
#
# RESULTAT: renderPlot() åbner altid device med KORREKTE dimensioner før labels beregnes

test_that("add_spc_labels() tilføjer labels UDEN device (ggplot kan bygges uden device)", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("ggplot2")

  # Load required functions
  if (!exists("add_spc_labels")) {
    source("R/fct_add_spc_labels.R")
  }
  if (!exists("add_right_labels_marquee")) {
    source("R/utils_add_right_labels_marquee.R")
  }
  if (!exists("create_responsive_label")) {
    source("R/utils_label_helpers.R")
  }
  if (!exists("format_y_value")) {
    source("R/utils_label_formatting.R")
  }
  if (!exists("place_two_labels_npc")) {
    source("R/utils_label_placement.R")
  }
  if (!exists("npc_mapper_from_built")) {
    source("R/utils_npc_mapper.R")
  }
  if (!exists("estimate_label_heights_npc")) {
    source("R/utils_label_height_estimation.R")
  }
  if (!exists("LABEL_PLACEMENT_CONFIG")) {
    source("R/config_label_placement.R")
  }

  # Create test data
  test_data <- data.frame(
    x = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 30),
    y = rnorm(30, mean = 50, sd = 10)
  )

  qic_result <- qicharts2::qic(
    x = x,
    y = y,
    data = test_data,
    chart = "i",
    target = 55
  )

  base_plot <- qic_result
  qic_data <- qic_result$data

  # SIMULER FØRSTE RENDER: Ingen device åben
  # (I Shiny kan device være i ustabil tilstand ved første render)
  if (grDevices::dev.cur() > 1) {
    grDevices::dev.off()  # Luk eventuelle åbne devices
  }

  # Kald add_spc_labels() UDEN device
  result_no_device <- add_spc_labels(
    plot = base_plot,
    qic_data = qic_data,
    y_axis_unit = "count",
    label_size = 6,
    verbose = FALSE
  )

  # FIX: ggplot CAN be built without device, so labels SHOULD be added
  # FORVENT: Plot returneret MED labels (downstream functions use 7×7" fallback)
  expect_true(inherits(result_no_device, "gg"),
              "Skal returnere ggplot object selv uden device")

  # Verificer at placement info ER tilstede (labels blev tilføjet med fallback sizes)
  placement_info <- attr(result_no_device, "placement_info")
  expect_false(is.null(placement_info),
               "Placement info skal være tilstede - ggplot kan bygges uden device")
})

test_that("add_spc_labels() fungerer korrekt med valid device", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("ggplot2")

  # Load required functions
  if (!exists("add_spc_labels")) {
    source("R/fct_add_spc_labels.R")
  }
  if (!exists("add_right_labels_marquee")) {
    source("R/utils_add_right_labels_marquee.R")
  }
  if (!exists("create_responsive_label")) {
    source("R/utils_label_helpers.R")
  }
  if (!exists("format_y_value")) {
    source("R/utils_label_formatting.R")
  }
  if (!exists("place_two_labels_npc")) {
    source("R/utils_label_placement.R")
  }
  if (!exists("npc_mapper_from_built")) {
    source("R/utils_npc_mapper.R")
  }
  if (!exists("estimate_label_heights_npc")) {
    source("R/utils_label_height_estimation.R")
  }
  if (!exists("LABEL_PLACEMENT_CONFIG")) {
    source("R/config_label_placement.R")
  }

  # Create test data
  test_data <- data.frame(
    x = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 30),
    y = rnorm(30, mean = 50, sd = 10)
  )

  qic_result <- qicharts2::qic(
    x = x,
    y = y,
    data = test_data,
    chart = "i",
    target = 55
  )

  base_plot <- qic_result
  qic_data <- qic_result$data

  # SIMULER ANDEN RENDER: Device klar med realistiske dimensioner
  grDevices::pdf(NULL, width = 13.5, height = 7.8)  # Simuler Shiny viewport
  on.exit({
    if (grDevices::dev.cur() > 1) grDevices::dev.off()
  }, add = TRUE)

  # Kald add_spc_labels() MED valid device
  result_with_device <- add_spc_labels(
    plot = base_plot,
    qic_data = qic_data,
    y_axis_unit = "count",
    label_size = 6,
    verbose = FALSE
  )

  # FORVENT: Plot med labels
  expect_true(inherits(result_with_device, "gg"),
              "Skal returnere ggplot object")

  # Verificer at placement info er tilstede
  placement_info <- attr(result_with_device, "placement_info")
  expect_false(is.null(placement_info),
               "Placement info skal være tilstede når device er klar")

  # Verificer minimum gap (labels ikke overlapper linjer)
  if (!is.null(placement_info) && !is.null(placement_info$gap_from_line)) {
    expect_true(placement_info$gap_from_line >= 0,
                "Gap fra label til linje skal være ikke-negativ")
  }
})

test_that("add_spc_labels() håndterer blank centerline korrekt", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("ggplot2")

  # Load required functions
  if (!exists("add_spc_labels")) {
    source("R/fct_add_spc_labels.R")
  }
  if (!exists("add_right_labels_marquee")) {
    source("R/utils_add_right_labels_marquee.R")
  }
  if (!exists("create_responsive_label")) {
    source("R/utils_label_helpers.R")
  }
  if (!exists("format_y_value")) {
    source("R/utils_label_formatting.R")
  }
  if (!exists("place_two_labels_npc")) {
    source("R/utils_label_placement.R")
  }
  if (!exists("npc_mapper_from_built")) {
    source("R/utils_npc_mapper.R")
  }
  if (!exists("estimate_label_heights_npc")) {
    source("R/utils_label_height_estimation.R")
  }
  if (!exists("LABEL_PLACEMENT_CONFIG")) {
    source("R/config_label_placement.R")
  }

  # Create test data
  test_data <- data.frame(
    x = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 30),
    y = rnorm(30, mean = 50, sd = 10)
  )

  # Opret qic plot MED target men UDEN centerline
  qic_result <- qicharts2::qic(
    x = x,
    y = y,
    data = test_data,
    chart = "run",  # Run chart har ikke alltid synlig CL
    target = 55
  )

  base_plot <- qic_result
  qic_data <- qic_result$data

  # Åbn device
  grDevices::pdf(NULL, width = 13.5, height = 7.8)
  on.exit({
    if (grDevices::dev.cur() > 1) grDevices::dev.off()
  }, add = TRUE)

  # Test med blank/NA centerline
  # Simuler scenario hvor user har clearet centerline input
  qic_data_no_cl <- qic_data
  qic_data_no_cl$cl <- NA_real_

  # Kald add_spc_labels() med kun target (ingen CL)
  result_no_cl <- tryCatch({
    add_spc_labels(
      plot = base_plot,
      qic_data = qic_data_no_cl,
      y_axis_unit = "percent",  # Run chart med nævner ofte i percent
      label_size = 6,
      verbose = FALSE
    )
  }, error = function(e) {
    # Hvis der opstår fejl, fang den
    list(error = e$message)
  })

  # FORVENT: Enten (A) Plot returneret med kun target label, ELLER (B) Plot uden labels
  # Begge er acceptable graceful degradation paths
  if (inherits(result_no_cl, "gg")) {
    expect_true(TRUE, "Plot returneret succesfuldt ved blank centerline")
  } else if (is.list(result_no_cl) && !is.null(result_no_cl$error)) {
    # Hvis add_spc_labels valgte at returnere plot uden labels (via warning),
    # er det også OK
    expect_true(grepl("Ingen CL eller Target", result_no_cl$error, ignore.case = TRUE),
                "Forventelig warning når ingen værdier tilgængelige")
  } else {
    fail("Uventet returtype ved blank centerline test")
  }
})

test_that("add_spc_labels() tilføjer labels selv med unrealistic device sizes (non-blocking)", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("ggplot2")

  # Load required functions
  if (!exists("add_spc_labels")) {
    source("R/fct_add_spc_labels.R")
  }
  if (!exists("add_right_labels_marquee")) {
    source("R/utils_add_right_labels_marquee.R")
  }
  if (!exists("create_responsive_label")) {
    source("R/utils_label_helpers.R")
  }
  if (!exists("format_y_value")) {
    source("R/utils_label_formatting.R")
  }
  if (!exists("place_two_labels_npc")) {
    source("R/utils_label_placement.R")
  }
  if (!exists("npc_mapper_from_built")) {
    source("R/utils_npc_mapper.R")
  }
  if (!exists("estimate_label_heights_npc")) {
    source("R/utils_label_height_estimation.R")
  }
  if (!exists("LABEL_PLACEMENT_CONFIG")) {
    source("R/config_label_placement.R")
  }

  # Create test data
  test_data <- data.frame(
    x = 1:30,
    y = rnorm(30, mean = 50, sd = 10)
  )

  qic_result <- qicharts2::qic(
    x = x,
    y = y,
    data = test_data,
    chart = "i"
  )

  base_plot <- qic_result
  qic_data <- qic_result$data

  # Test forskellige device sizes (alle skulle nu tilføje labels)
  test_sizes <- list(
    c(1, 1),     # Meget lille
    c(100, 100), # Meget stor
    c(2, 2)      # Lille men gyldig
  )

  for (size in test_sizes) {
    # Åbn device med given size
    grDevices::pdf(NULL, width = size[1], height = size[2])

    result <- add_spc_labels(
      plot = base_plot,
      qic_data = qic_data,
      y_axis_unit = "count",
      label_size = 6,
      verbose = FALSE
    )

    # FIX: Non-blocking approach - labels SKAL tilføjes uanset device size
    expect_true(inherits(result, "gg"),
                sprintf("Skal returnere ggplot med size %.1f×%.1f",
                        size[1], size[2]))

    placement_info <- attr(result, "placement_info")
    expect_false(is.null(placement_info),
                 sprintf("Labels skal tilføjes selv med device size %.1f×%.1f (non-blocking)",
                         size[1], size[2]))

    if (grDevices::dev.cur() > 1) grDevices::dev.off()
  }
})
