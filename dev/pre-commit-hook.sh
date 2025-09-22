#!/bin/bash
# Pre-commit hook for SPC App
#
# INSTALLATION:
# cp dev/pre-commit-hook.sh .git/hooks/pre-commit
# chmod +x .git/hooks/pre-commit
#
# Dette script kører automatisk ved hver git commit og:
# - Tjekker kun ændrede R filer
# - Kører lintr for code quality
# - Kører styler for formatting
# - Blokerer commit hvis kritiske errors findes
# - Kræver re-staging hvis styler ændrer filer

echo "🔍 Running pre-commit checks for SPC App..."

# Get list of staged R files (excluding dev/ and golem_utils)
staged_r_files=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(R|r)$' | grep -v 'dev/' | grep -v 'golem_utils.R')

if [ -z "$staged_r_files" ]; then
    echo "✅ No R files changed - skipping code quality checks"
    exit 0
fi

echo "📁 Checking staged R files:"
echo "$staged_r_files"

# Check if we have R and required packages
if ! command -v Rscript &> /dev/null; then
    echo "❌ Rscript not found - skipping code quality checks"
    echo "   Install R to enable automatic code quality checking"
    exit 0
fi

# Verify required packages are available
echo "🔧 Checking required R packages..."
if ! Rscript -e "library(lintr); library(styler); library(here)" 2>/dev/null; then
    echo "⚠️  Missing required packages (lintr, styler, here)"
    echo "   Install with: install.packages(c('lintr', 'styler', 'here'))"
    echo "   Skipping code quality checks"
    exit 0
fi

# Run lintr and styler script
echo ""
echo "🎯 Running lintr and styler..."
if ! Rscript dev/lint_and_style.R; then
    exit_code=$?
    echo ""
    if [ $exit_code -eq 1 ]; then
        echo "❌ COMMIT BLOCKED: Critical lintr errors found!"
        echo "   Fix critical errors and try again"
        echo "   Run manually: Rscript dev/lint_and_style.R"
        exit 1
    elif [ $exit_code -eq 2 ]; then
        echo "⚠️  Warnings found, but commit allowed"
        echo "   Consider fixing warnings: Rscript dev/lint_and_style.R"
    fi
fi

# Check if styler made changes to any files
files_changed=$(git diff --name-only)
if [ ! -z "$files_changed" ]; then
    echo ""
    echo "📝 Styler has modified files:"
    echo "$files_changed"
    echo ""
    echo "🔄 Please stage the styled files and commit again:"
    echo "   git add ."
    echo "   git commit"
    exit 1
fi

echo ""
echo "✅ Pre-commit checks passed!"
exit 0