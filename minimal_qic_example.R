# Minimalt eksempel på brug af qic() med part-kolonne
# Loader eksempeldata og sætter part ved punkt 14

library(qicharts2)
library(tidyverse)

# Load data
data <- read_csv2("inst/extdata/spc_exampledata_utf8.csv")

# Konverter dato til Date format
data$Dato <- dmy(data$Dato)

# Opret part-kolonne: 1 for første 13 rækker, 2 for resten (skift ved punkt 14)
# data$part <- ifelse(seq_len(nrow(data)) <= 13, 1, 2)

# Beregn andel
# data$andel <- data$Tæller / data$Nævner

# Kør qic() med part
qic_result <- qic(
  x = Dato,
  y = Tæller,
  n = Nævner,
  data = data,
  chart = "run",
  # freeze = c(3),
  # part = c(14),
  multiply = 100,  # Vis som procent
  return.data = TRUE
)

# Udtræk qic_data (data med beregninger)
qic_data <- qic_result

# Vis resultater
cat("QIC beregninger med part-kolonne:\n")
cat("Total antal punkter:", nrow(qic_data), "\n")
cat("Part 1 (første 13):", sum(qic_data$part == 1, na.rm = TRUE), "punkter\n")
cat("Part 2 (fra punkt 14):", sum(qic_data$part == 2, na.rm = TRUE), "punkter\n\n")

# Vis metrics for hele datasættet
cat("Hele datasæt:\n")
cat("Faktisk serelængde:", max(qic_data$longest.run, na.rm = TRUE), "\n")
cat("Forventet serielængde max:", max(qic_data$longest.run.max, na.rm = TRUE), "\n")
cat("Antal kryds:", max(qic_data$n.crossings, na.rm = TRUE), "\n")
cat("Antal kryds min:", max(qic_data$n.crossings.min, na.rm = TRUE), "\n\n")

# Filtrer til første part (part 1)
first_part <- min(qic_data$part, na.rm = TRUE)
qic_data_first <- qic_data[qic_data$part == first_part & !is.na(qic_data$part), ]

cat("Kun første part (part", first_part, "):\n")
cat("Faktisk serielængde:", max(qic_data_first$longest.run, na.rm = TRUE), "\n")
cat("Forventet serielængde max:", max(qic_data_first$longest.run.max, na.rm = TRUE), "\n")
cat("Antal kryds:", max(qic_data_first$n.crossings, na.rm = TRUE), "\n")
cat("Antal kryds min:", max(qic_data_first$n.crossings.min, na.rm = TRUE), "\n\n")

# Filtrer til seneste part (part 2)
latest_part <- max(qic_data$part, na.rm = TRUE)
qic_data_latest <- qic_data[qic_data$part == latest_part & !is.na(qic_data$part), ]

cat("Kun seneste part (part", latest_part, "):\n")
cat("Faktisk serielængde:", max(qic_data_latest$longest.run, na.rm = TRUE), "\n")
cat("Forventet serielængde max:", max(qic_data_latest$longest.run.max, na.rm = TRUE), "\n")
cat("Antal kryds:", max(qic_data_latest$n.crossings, na.rm = TRUE), "\n")
cat("Antal kryds min:", max(qic_data_latest$n.crossings.min, na.rm = TRUE), "\n")
