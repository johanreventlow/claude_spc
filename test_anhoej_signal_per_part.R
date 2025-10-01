# Test anhoej.signal calculation per part
library(qicharts2)
library(tidyverse)

# Load data
data <- read_csv2("inst/extdata/spc_exampledata_utf8.csv")
data$Dato <- dmy(data$Dato)

# Kør qic() med part ved punkt 14
qic_result <- qic(
  x = Dato,
  y = Tæller,
  n = Nævner,
  data = data,
  chart = "run",
  freeze = c(3),
  part = c(14),  # Skift ved punkt 14
  multiply = 100,
  return.data = TRUE
)

qic_data <- qic_result

cat("=== Original qic() output ===\n")
cat("Total punkter:", nrow(qic_data), "\n")
cat("Parts i data:", unique(qic_data$part), "\n\n")

# Vis runs.signal fra qicharts2
cat("runs.signal (fra qicharts2) per row:\n")
print(data.frame(
  row = 1:nrow(qic_data),
  part = qic_data$part,
  runs.signal = qic_data$runs.signal
))

cat("\n=== Anvend logik fra fct_spc_plot_generation.R ===\n\n")

# Brug runs.signal direkte fra qicharts2 (allerede per-række)
runs_sig_col <- if ("runs.signal" %in% names(qic_data)) {
  qic_data$runs.signal
} else {
  rep(FALSE, nrow(qic_data))
}

# Beregn crossings signal per part
crossings_sig_col <- rep(FALSE, nrow(qic_data))
if ("n.crossings" %in% names(qic_data) && "n.crossings.min" %in% names(qic_data) && "part" %in% names(qic_data)) {
  # For hver part, check om crossings signal er brudt
  for (p in unique(qic_data$part)) {
    part_rows <- which(qic_data$part == p)
    if (length(part_rows) > 0) {
      part_data <- qic_data[part_rows, ]
      n_cross <- max(part_data$n.crossings, na.rm = TRUE)
      n_cross_min <- max(part_data$n.crossings.min, na.rm = TRUE)
      has_crossing_signal <- !is.na(n_cross) && !is.na(n_cross_min) && n_cross < n_cross_min

      cat("Part", p, ":\n")
      cat("  Rows:", part_rows[1], "-", part_rows[length(part_rows)], "\n")
      cat("  n.crossings:", n_cross, "\n")
      cat("  n.crossings.min:", n_cross_min, "\n")
      cat("  has_crossing_signal:", has_crossing_signal, "\n\n")

      crossings_sig_col[part_rows] <- has_crossing_signal
    }
  }
}

# Kombinér: TRUE hvis ENTEN runs ELLER crossings signal for den pågældende række
qic_data$anhoej.signal <- runs_sig_col | crossings_sig_col

cat("=== Final anhoej.signal column ===\n")
print(data.frame(
  row = 1:nrow(qic_data),
  part = qic_data$part,
  runs.signal = runs_sig_col,
  crossings.signal = crossings_sig_col,
  anhoej.signal = qic_data$anhoej.signal
))

cat("\n=== Summary ===\n")
cat("Part 1 (rows 1-13):\n")
cat("  anhoej.signal TRUE count:", sum(qic_data$anhoej.signal[1:13]), "\n")
cat("  anhoej.signal FALSE count:", sum(!qic_data$anhoej.signal[1:13]), "\n")

cat("\nPart 2 (rows 14-36):\n")
cat("  anhoej.signal TRUE count:", sum(qic_data$anhoej.signal[14:36]), "\n")
cat("  anhoej.signal FALSE count:", sum(!qic_data$anhoej.signal[14:36]), "\n")
