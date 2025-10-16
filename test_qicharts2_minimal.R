# Minimal test to debug qicharts2::qic() error
# Test different ways of calling qicharts2::qic()

library(qicharts2)

# Create simple test data
test_data <- data.frame(
  Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
  Komplikationer = c(12, 15, 18, 14, 16, 19, 17, 15, 18, 20,
                     22, 21, 19, 23, 25, 24, 22, 26, 28, 27),
  Procedurer = rep(100, 20),
  stringsAsFactors = FALSE
)

cat("=== Test 1: qic() with vectors (NO data parameter) ===\n")
result1 <- tryCatch({
  qicharts2::qic(
    x = test_data$Dato,
    y = test_data$Komplikationer,
    n = test_data$Procedurer,
    chart = "p",
    return.data = TRUE
  )
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  traceback()
  return(NULL)
})

if (!is.null(result1)) {
  cat("✓ Test 1 SUCCESS\n")
  cat(sprintf("  Returned %d rows\n", nrow(result1)))
} else {
  cat("✗ Test 1 FAILED\n\n")
}

cat("\n=== Test 2: qic() with NSE (WITH data parameter) ===\n")
result2 <- tryCatch({
  qicharts2::qic(
    x = Dato,
    y = Komplikationer,
    n = Procedurer,
    data = test_data,
    chart = "p",
    return.data = TRUE
  )
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  traceback()
  return(NULL)
})

if (!is.null(result2)) {
  cat("✓ Test 2 SUCCESS\n")
  cat(sprintf("  Returned %d rows\n", nrow(result2)))
} else {
  cat("✗ Test 2 FAILED\n\n")
}

cat("\n=== Test 3: do.call() with vectors (like our code) ===\n")
qic_params <- list(
  x = test_data$Dato,
  y = test_data$Komplikationer,
  n = test_data$Procedurer,
  chart = "p",
  return.data = TRUE
)

result3 <- tryCatch({
  do.call(qicharts2::qic, qic_params)
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  traceback()
  return(NULL)
})

if (!is.null(result3)) {
  cat("✓ Test 3 SUCCESS\n")
  cat(sprintf("  Returned %d rows\n", nrow(result3)))
} else {
  cat("✗ Test 3 FAILED\n")
}
