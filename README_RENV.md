# renv Dependency Management

Dette projekt bruger `renv` til at låse alle R package dependencies.

## Hvorfor renv?

Vi kræver pakken `marquee`, der leverer `geom_marquee()` til deterministisk placering af CL/Target labels. renv sikrer at den præcise version altid installeres.

## Locked Dependencies

`renv.lock` filen indeholder alle CRAN dependencies inkl. `marquee` med præcise versioner.

## Installation

### Første gang setup:
```r
# Install renv hvis ikke allerede installeret
install.packages("renv")

# Restore alle locked dependencies
renv::restore()
```

### Opdatering af dependencies:

⚠️ **VIGTIGT**: `marquee` skal bevares ved opdateringer!

```r
# ✅ SAFE: Opdater alle pakker
renv::update()
testthat::test_dir()     # Test changes
renv::snapshot()         # Lock new versions

# 🎯 BEDST: Brug automated script
source(".renv_update_recipe.R")  # Guided update workflow
```

**Installer nye pakker:**
```r
install.packages("pakkenavn")
renv::snapshot()  # Lock den nye pakke

# Eller GitHub pakker
remotes::install_github("user/repo@branch")
renv::snapshot()
```

### Verificer marquee installation:
```r
library(marquee)
"geom_marquee" %in% getNamespaceExports("marquee")  # Skal være TRUE
```

## Version Konflikt Håndtering

### Problem: `marquee` mangler

**Automatisk detektion:**
- SPCify viser warning ved opstart hvis `marquee::geom_marquee` mangler
- Plot-funktioner checker før brug og giver klare fejlbeskeder

**Løsning for brugere:**
```r
# Option 1: Brug renv (anbefalet)
renv::restore()  # Installerer automatisk korrekt version
# Genstart R session

# Option 2: Manuel installation
install.packages('marquee')
# Genstart R session
```

### Sikkerhed mod package collision

renv isolerer SPCify's dependencies i projekt-bibliotek:
- Projektets egne pakkeversioner i `renv/library/` → **BRUGES af SPCify**
- Ingen konflikt mellem globale versioner og projektversioner

## Vedligehold

1. Hold `marquee` ajour via renv-workflow
2. Opdater lock: `renv::snapshot()` efter installationer
3. Commit og push opdateret `renv.lock`

## .gitignore Setup

renv filer inkluderet i git:
- ✅ `renv.lock` - Dependency lockfile (COMMIT)
- ✅ `renv/activate.R` - Bootstrap script (COMMIT)
- ✅ `.renvignore` - Ignore patterns (COMMIT)
- ❌ `renv/library/` - Package binaries (IGNORE)
- ❌ `renv/staging/` - Temp staging (IGNORE)

## Troubleshooting

### Problem: "geom_marquee not found"
```r
# Verificer korrekt source
renv::status()  # Check for discrepancies

# Reinstaller marquee
install.packages("marquee")
renv::snapshot()
```

### Problem: "Connection timeout"
```r
# Øg timeout
options(timeout = 300)
renv::restore()
```

## Best Practices

1. **Altid commit `renv.lock`** efter dependency ændringer
2. **Kør `renv::status()`** regelmæssigt for at tjekke sync
3. **Test efter restore** at kritiske features (som geom_marquee) virker
5. **Pin kritiske dependencies** med eksakte SHA's (som vi gør med ggrepel)
