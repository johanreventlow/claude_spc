# lint_and_style.R
# Development script til code quality checking og formatting
# Supports danske kommentarer og følger SPC app conventions

library(lintr)
library(styler)
library(here)

# Paths til at analysere
paths_to_check <- c(
  "global.R",
  "global_packaged.R",
  "app.R",
  "R/"
)

# Konfiguration
lintr_config_file <- here::here(".lintr")
styler_config <- list(
  scope = "tokens",
  strict = FALSE,  # Don't force changes that break danish comments
  indent_by = 2,
  start_comments_with_one_space = FALSE,  # Allow danish comment style
  reindention = tidyverse_reindention(),
  math_token_spacing = tidyverse_math_token_spacing()
)

# LINTING ======================================================================

cat("🔍 Kører lintr code quality check...\n")

# Verify lintr configuration exists
if (file.exists(lintr_config_file)) {
  cat("Bruger lintr config:", lintr_config_file, "\n")
} else {
  cat("Advarsel: Ingen .lintr fil fundet, bruger defaults\n")
}

# Kør lintr på specifikke filer og mapper
lint_results <- list()

for (path in paths_to_check) {
  if (file.exists(path)) {
    cat("Checker:", path, "\n")
    if (dir.exists(path)) {
      # For mapper: scan alle .R filer (undtag golem_utils og dev/)
      r_files <- list.files(path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
      r_files <- r_files[!grepl("golem_utils\\.R$", r_files)]
      r_files <- r_files[!grepl("^dev/", r_files)]

      for (file in r_files) {
        cat("  Linting:", basename(file), "\n")
        lint_results[[file]] <- lintr::lint(file)
      }
    } else {
      # For enkelte filer
      lint_results[[path]] <- lintr::lint(path)
    }
  }
}

# Vis resultater
total_issues <- sum(sapply(lint_results, length))

# Kategoriser issues efter type
critical_issues <- 0
style_issues <- 0
warning_issues <- 0

for (file_issues in lint_results) {
  for (issue in file_issues) {
    if (grepl("error", tolower(issue$type))) {
      critical_issues <- critical_issues + 1
    } else if (grepl("warning", tolower(issue$type))) {
      warning_issues <- warning_issues + 1
    } else {
      style_issues <- style_issues + 1
    }
  }
}

cat("\n📊 Lintr resultater:\n")
cat("Total issues fundet:", total_issues, "\n")
cat("  - Kritiske errors:", critical_issues, "\n")
cat("  - Warnings:", warning_issues, "\n")
cat("  - Style issues:", style_issues, "\n")

if (total_issues > 0) {
  cat("\n⚠️  Issues fundet:\n")
  for (file in names(lint_results)) {
    issues <- lint_results[[file]]
    if (length(issues) > 0) {
      cat("\n📁", basename(file), ":\n")
      print(issues)
    }
  }
} else {
  cat("✅ Ingen linting issues fundet!\n")
}

# STYLING ======================================================================

cat("\n🎨 Kører styler code formatting...\n")

# Style hele R/ mappen og hovedfiler med custom config
files_styled <- 0

for (path in paths_to_check) {
  if (file.exists(path)) {
    cat("Styling:", path, "\n")
    if (dir.exists(path)) {
      # For mapper: style alle .R filer (undtag golem_utils)
      r_files <- list.files(path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
      r_files <- r_files[!grepl("golem_utils\\.R$", r_files)]

      for (file in r_files) {
        cat("  Styling:", basename(file), "\n")
        result <- styler::style_file(
          file,
          scope = styler_config$scope,
          strict = styler_config$strict,
          indent_by = styler_config$indent_by,
          start_comments_with_one_space = styler_config$start_comments_with_one_space,
          reindention = styler_config$reindention,
          math_token_spacing = styler_config$math_token_spacing
        )
        if (any(result$changed)) {
          files_styled <- files_styled + 1
        }
      }
    } else {
      # For enkelte filer
      result <- styler::style_file(
        path,
        scope = styler_config$scope,
        strict = styler_config$strict,
        indent_by = styler_config$indent_by,
        start_comments_with_one_space = styler_config$start_comments_with_one_space,
        reindention = styler_config$reindention,
        math_token_spacing = styler_config$math_token_spacing
      )
      if (any(result$changed)) {
        files_styled <- files_styled + 1
      }
    }
  }
}

cat("✅ Code styling færdig!\n")
cat("📝 Filer ændret af styler:", files_styled, "\n")

# SUMMARY ======================================================================

cat("\n📋 SAMMENFATNING:\n")
cat("- Lintr issues:", total_issues, "(", critical_issues, "kritiske,", warning_issues, "warnings,", style_issues, "style)\n")
cat("- Styling: Komplet (", files_styled, "filer ændret)\n")

# Exit codes for automation
exit_code <- 0
if (critical_issues > 0) {
  cat("\n❌ FEJL: Kritiske lintr errors fundet - skal rettes!\n")
  exit_code <- 1
} else if (warning_issues > 0) {
  cat("\n⚠️  ADVARSEL: Warnings fundet - bør rettes\n")
  exit_code <- 2
} else if (style_issues > 0) {
  cat("\n💡 INFO: Kun style issues - ikke kritisk\n")
  exit_code <- 0
} else {
  cat("\n🎉 Code quality check bestået!\n")
  exit_code <- 0
}

# Guidance
if (total_issues > 0) {
  cat("\n🔧 Næste trin:\n")
  if (critical_issues > 0) {
    cat("  1. Ret kritiske errors (blokkerer commit)\n")
  }
  if (warning_issues > 0) {
    cat("  2. Overvej at rette warnings\n")
  }
  if (style_issues > 0) {
    cat("  3. Style issues kan ignoreres eller rettes\n")
  }
}

if (files_styled > 0) {
  cat("\n📝 Styler har ændret", files_styled, "filer - husk at stage dem til git!\n")
}

# Return exit code for scripts/hooks
quit(status = exit_code)