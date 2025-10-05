# ==============================================================================
# COMPREHENSIVE VIEWPORT SCENARIO TESTING
# ==============================================================================
#
# Tester label placement på tværs af forskellige viewport-konfigurationer
# for at identificere edge cases og fejlsituationer.
#
# FOKUS: Lave, brede viewports hvor problemer kan opstå
#
# ==============================================================================

library(ggplot2)
library(marquee)
library(grid)

# Source functions (only if not already sourced)
if (!exists("estimate_label_height_npc", mode = "function")) {
  source("utils_standalone_label_placement.R")
  source("R/config_label_placement.R")
}

# ==============================================================================
# SCENARIO DEFINITIONS
# ==============================================================================

scenarios <- list(
  list(
    name = "Standard (square-ish)",
    width = 7,
    height = 7,
    description = "Normal quadratisk viewport"
  ),
  list(
    name = "Bred, lav (wide screen)",
    width = 12,
    height = 4,
    description = "Bred, lav viewport (typisk widescreen)"
  ),
  list(
    name = "Meget bred, meget lav",
    width = 14,
    height = 3,
    description = "Ekstremt bred, meget lav viewport"
  ),
  list(
    name = "Lav, smal",
    width = 4,
    height = 3,
    description = "Lille viewport"
  ),
  list(
    name = "Høj, smal (portrait)",
    width = 4,
    height = 10,
    description = "Portrait-orienteret viewport"
  ),
  list(
    name = "Facet-lignende (lille)",
    width = 3,
    height = 2.5,
    description = "Typisk facet panel størrelse"
  )
)

# Test labels
test_label_cl <- "{.8 **CL**}  \n{.24 **45.2%**}"
test_label_target <- "{.8 **Target**}  \n{.24 **50.0%**}"

test_style <- marquee::modify_style(
  marquee::classic_style(),
  "p",
  margin = marquee::trbl(0),
  align = "right"
)

# ==============================================================================
# TEST FUNCTION
# ==============================================================================

test_scenario <- function(scenario, cl_value = 45, target_value = 50) {

  cat("\n", strrep("=", 80), "\n")
  cat("SCENARIO:", scenario$name, "\n")
  cat("Viewport:", scenario$width, "x", scenario$height, "inches\n")
  cat(scenario$description, "\n")
  cat(strrep("=", 80), "\n")

  # Opret test plot
  df <- data.frame(
    x = 1:20,
    y = seq(40, 55, length.out = 20)
  )

  p <- ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    geom_hline(yintercept = cl_value, color = "blue", linetype = "dashed") +
    geom_hline(yintercept = target_value, color = "red", linetype = "dashed") +
    coord_cartesian(ylim = c(35, 60)) +
    theme_minimal()

  # Mål faktisk panel height
  panel_h_measured <- tryCatch({
    measure_panel_height_inches(p)
  }, error = function(e) {
    cat("⚠️  FEJL ved panel height måling:", e$message, "\n")
    return(NULL)
  })

  if (is.null(panel_h_measured)) {
    cat("❌ Kunne ikke måle panel height - springer scenario over\n")
    return(invisible(NULL))
  }

  cat(sprintf("\n📏 Målt panel højde: %.2f inches\n", panel_h_measured))
  cat(sprintf("   Aspect ratio: %.2f (width/height)\n", scenario$width / scenario$height))

  # Beregn label heights med NY API
  height_cl <- tryCatch({
    estimate_label_height_npc(
      test_label_cl,
      style = test_style,
      panel_height_inches = panel_h_measured,
      return_details = TRUE
    )
  }, error = function(e) {
    cat("⚠️  FEJL ved label height estimation (CL):", e$message, "\n")
    return(NULL)
  }, warning = function(w) {
    # Muffle warnings men fortsæt execution
    invokeRestart("muffleWarning")
  })

  height_target <- tryCatch({
    estimate_label_height_npc(
      test_label_target,
      style = test_style,
      panel_height_inches = panel_h_measured,
      return_details = TRUE
    )
  }, error = function(e) {
    cat("⚠️  FEJL ved label height estimation (Target):", e$message, "\n")
    return(NULL)
  }, warning = function(w) {
    # Muffle warnings men fortsæt execution
    invokeRestart("muffleWarning")
  })

  if (is.null(height_cl) || is.null(height_target)) {
    cat("❌ Kunne ikke estimere label heights - springer scenario over\n")
    return(invisible(NULL))
  }

  # Debug output
  cat(sprintf("\n[DEBUG] height_cl class: %s\n", paste(class(height_cl), collapse = ", ")))
  cat(sprintf("[DEBUG] height_target class: %s\n", paste(class(height_target), collapse = ", ")))

  if (!is.list(height_cl)) {
    cat("[DEBUG] height_cl is NOT a list! Value:", height_cl, "\n")
    cat("❌ height_cl har forkert type - springer scenario over\n")
    return(invisible(NULL))
  }

  if (!is.list(height_target)) {
    cat("[DEBUG] height_target is NOT a list! Value:", height_target, "\n")
    cat("❌ height_target har forkert type - springer scenario over\n")
    return(invisible(NULL))
  }

  cat(sprintf("📐 Label heights:\n"))
  cat(sprintf("   CL:     NPC=%.4f, inches=%.4f\n", height_cl$npc, height_cl$inches))
  cat(sprintf("   Target: NPC=%.4f, inches=%.4f\n", height_target$npc, height_target$inches))
  cat(sprintf("   Label/Panel ratio: %.1f%%\n", (height_cl$npc * 100)))

  # Check om labels er for store relativt til panel
  if (height_cl$npc > 0.3) {
    cat("⚠️  WARNING: Labels optager >30% af panel højde!\n")
  }

  # Opret NPC mapper
  mapper <- npc_mapper_from_plot(p)

  # Konverter line positions til NPC
  cl_npc <- mapper$y_to_npc(cl_value)
  target_npc <- mapper$y_to_npc(target_value)

  cat(sprintf("\n🎯 Line positions:\n"))
  cat(sprintf("   CL:     %.1f (%.4f NPC)\n", cl_value, cl_npc))
  cat(sprintf("   Target: %.1f (%.4f NPC)\n", target_value, target_npc))
  cat(sprintf("   Line gap: %.4f NPC (%.1f%% af panel)\n",
              abs(target_npc - cl_npc), abs(target_npc - cl_npc) * 100))

  # Beregn forventet gaps
  gap_line_factor <- get_label_placement_param("relative_gap_line")  # 0.08
  gap_labels_factor <- get_label_placement_param("relative_gap_labels")  # 0.30

  expected_gap_line_inches <- height_cl$inches * gap_line_factor
  expected_gap_line_npc <- expected_gap_line_inches / panel_h_measured

  expected_gap_labels_inches <- height_cl$inches * gap_labels_factor
  expected_gap_labels_npc <- expected_gap_labels_inches / panel_h_measured

  min_center_gap_npc <- height_cl$npc + expected_gap_labels_npc

  cat(sprintf("\n📊 Expected gaps (ny API):\n"))
  cat(sprintf("   gap_line:   %.4f inches → %.4f NPC (%.1f%% af panel)\n",
              expected_gap_line_inches, expected_gap_line_npc, expected_gap_line_npc * 100))
  cat(sprintf("   gap_labels: %.4f inches → %.4f NPC (%.1f%% af panel)\n",
              expected_gap_labels_inches, expected_gap_labels_npc, expected_gap_labels_npc * 100))
  cat(sprintf("   min_center_gap: %.4f NPC (%.1f%% af panel)\n",
              min_center_gap_npc, min_center_gap_npc * 100))

  # Check om der er nok plads
  available_space_npc <- abs(target_npc - cl_npc)
  space_needed_npc <- min_center_gap_npc + 2 * expected_gap_line_npc

  cat(sprintf("\n🔍 Space analysis:\n"))
  cat(sprintf("   Available space: %.4f NPC\n", available_space_npc))
  cat(sprintf("   Space needed:    %.4f NPC\n", space_needed_npc))
  cat(sprintf("   Ratio: %.2f (>1.0 = nok plads)\n", available_space_npc / space_needed_npc))

  if (available_space_npc < space_needed_npc) {
    cat("⚠️  WARNING: Ikke nok plads til optimal placering!\n")
  }

  # Test label placement
  cat("\n🎨 Testing label placement...\n")

  result <- tryCatch({
    place_two_labels_npc(
      yA_npc = cl_npc,
      yB_npc = target_npc,
      label_height_npc = height_cl,  # Brug ny API
      pref_pos = c("under", "under")
    )
  }, error = function(e) {
    cat("❌ FEJL ved label placement:", e$message, "\n")
    cat("Stacktrace:\n")
    print(e)
    return(NULL)
  })

  if (is.null(result)) {
    cat("❌ Label placement fejlede fuldstændigt\n")
    return(invisible(NULL))
  }

  # Analyser resultatet
  cat(sprintf("\n✅ Placement result:\n"))
  cat(sprintf("   CL label:     %.4f NPC (%s)\n", result$yA, result$sideA))
  cat(sprintf("   Target label: %.4f NPC (%s)\n", result$yB, result$sideB))
  cat(sprintf("   Quality: %s\n", result$placement_quality))

  if (length(result$warnings) > 0) {
    cat("\n⚠️  Warnings:\n")
    for (w in result$warnings) {
      cat(sprintf("   - %s\n", w))
    }
  }

  # Verificer at labels ikke overlapper
  label_gap_achieved <- abs(result$yA - result$yB)
  cat(sprintf("\n🔬 Verification:\n"))
  cat(sprintf("   Label gap achieved: %.4f NPC\n", label_gap_achieved))
  cat(sprintf("   Min required gap:   %.4f NPC\n", height_cl$npc))

  if (label_gap_achieved < height_cl$npc) {
    cat("❌ FEJL: Labels overlapper! (gap < label_height)\n")
  } else {
    cat("✓ Labels overlapper ikke\n")
  }

  # Verificer line gaps
  gap_A_to_line <- abs(result$yA - cl_npc)
  gap_B_to_line <- abs(result$yB - target_npc)

  cat(sprintf("   Gap CL label → line:     %.4f NPC (min: %.4f)\n",
              gap_A_to_line, expected_gap_line_npc + height_cl$npc/2))
  cat(sprintf("   Gap Target label → line: %.4f NPC (min: %.4f)\n",
              gap_B_to_line, expected_gap_line_npc + height_target$npc/2))

  # Overall vurdering
  cat("\n")
  if (result$placement_quality == "optimal") {
    cat("✅ SUCCES: Optimal placering opnået\n")
  } else if (result$placement_quality == "acceptable") {
    cat("⚠️  ACCEPTABLE: Placering er OK men ikke optimal\n")
  } else if (result$placement_quality == "suboptimal") {
    cat("⚠️  SUBOPTIMAL: Placering er degraderet\n")
  } else {
    cat("❌ DEGRADED/FAILED: Placering er betydeligt kompromitteret\n")
  }

  return(result)
}

# ==============================================================================
# RUN ALL SCENARIOS
# ==============================================================================

cat("\n")
cat(strrep("#", 80), "\n")
cat("# COMPREHENSIVE VIEWPORT SCENARIO TESTING\n")
cat(strrep("#", 80), "\n")

results <- list()

for (i in seq_along(scenarios)) {
  scenario <- scenarios[[i]]

  # Open device med scenario's dimensions
  temp_file <- tempfile(fileext = ".pdf")
  pdf(file = temp_file, width = scenario$width, height = scenario$height)

  result <- test_scenario(scenario)
  results[[scenario$name]] <- result

  dev.off()
  unlink(temp_file)
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat(strrep("#", 80), "\n")
cat("# SUMMARY\n")
cat(strrep("#", 80), "\n\n")

for (i in seq_along(scenarios)) {
  scenario <- scenarios[[i]]
  result <- results[[scenario$name]]

  cat(sprintf("%-30s", scenario$name))

  if (is.null(result)) {
    cat(" ❌ FEJLEDE\n")
  } else {
    quality_symbol <- switch(result$placement_quality,
                            optimal = "✅",
                            acceptable = "⚠️ ",
                            suboptimal = "⚠️ ",
                            degraded = "❌",
                            failed = "❌",
                            "❓")

    cat(sprintf(" %s %s", quality_symbol, result$placement_quality))

    if (length(result$warnings) > 0) {
      cat(sprintf(" (%d warnings)", length(result$warnings)))
    }
    cat("\n")
  }
}

cat("\n")
cat("Test completed. Gennemgå output ovenfor for detaljeret analyse.\n")
