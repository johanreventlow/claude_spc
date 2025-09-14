# lint_and_style.R
# Development script til code quality checking og formatting

library(lintr)
library(styler)

# Paths til at analysere
paths_to_check <- c(
  "global.R",
  "server.R", 
  "ui.R",
  "app.R",
  "R/"
)

# LINTING ======================================================================

cat("🔍 Kører lintr code quality check...\n")

# Kør lintr på specifikke filer og mapper
lint_results <- list()

for (path in paths_to_check) {
  if (file.exists(path)) {
    cat("Checker:", path, "\n")
    if (dir.exists(path)) {
      # For mapper: scan alle .R filer
      r_files <- list.files(path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
      for (file in r_files) {
        lint_results[[file]] <- lint(file)
      }
    } else {
      # For enkelte filer
      lint_results[[path]] <- lint(path)
    }
  }
}

# Vis resultater
total_issues <- sum(sapply(lint_results, length))
cat("\n📊 Lintr resultater:\n")
cat("Total issues fundet:", total_issues, "\n")

if (total_issues > 0) {
  cat("\n⚠️  Issues fundet:\n")
  for (file in names(lint_results)) {
    issues <- lint_results[[file]]
    if (length(issues) > 0) {
      cat("\n", file, ":\n")
      print(issues)
    }
  }
} else {
  cat("✅ Ingen linting issues fundet!\n")
}

# STYLING ======================================================================

cat("\n🎨 Kører styler code formatting...\n")

# Style hele R/ mappen og hovedfiler
for (path in paths_to_check) {
  if (file.exists(path)) {
    cat("Styling:", path, "\n")
    if (dir.exists(path)) {
      style_dir(path, recursive = TRUE)
    } else {
      style_file(path)
    }
  }
}

cat("✅ Code styling færdig!\n")

# SUMMARY ======================================================================

cat("\n📋 SAMMENFATNING:\n")
cat("- Lintr issues:", total_issues, "\n")
cat("- Styling: Komplet\n")

if (total_issues > 0) {
  cat("\n🔧 Næste trin: Ret lintr issues manuelt\n")
} else {
  cat("\n🎉 Code quality check bestået!\n")
}