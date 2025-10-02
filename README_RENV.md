# renv Dependency Management

Dette projekt bruger `renv` til at låse alle R package dependencies, inklusive custom GitHub branches.

## Hvorfor renv?

Vi bruger en custom fork af `ggrepel` (`teunbrand/ggrepel@marquee_repel`) der indeholder `geom_marquee_repel()` funktionalitet, som endnu ikke er merged ind i main ggrepel pakken. renv sikrer at denne præcise version altid installeres.

## Locked Dependencies

`renv.lock` filen indeholder:
- **ggrepel**: Version 0.9.6.9999 fra `teunbrand/ggrepel@marquee_repel`
- **RemoteSha**: `0bbdee8174712929f297da6f0e4170059b0a9344` (exact commit)
- Alle andre CRAN dependencies med præcise versioner

## Installation

### Første gang setup:
```r
# Install renv hvis ikke allerede installeret
install.packages("renv")

# Restore alle locked dependencies
renv::restore()
```

### Opdatering af dependencies:
```r
# Installer nye pakker normalt
install.packages("pakkenavn")

# Eller GitHub pakker
remotes::install_github("user/repo@branch")

# Lock den nye tilstand
renv::snapshot()
```

### Verificer ggrepel installation:
```r
library(ggrepel)
packageVersion("ggrepel")  # Skal være 0.9.6.9999
"geom_marquee_repel" %in% getNamespaceExports("ggrepel")  # Skal være TRUE
```

## Version Konflikt Håndtering

### Problem: Anden bruger har standard ggrepel installeret

Hvis nogen prøver at bruge SPCify med standard CRAN ggrepel (uden marquee support):

**Automatisk detektion:**
- SPCify viser warning ved opstart hvis geom_marquee_repel mangler
- Plot-funktioner checker før brug og giver klare fejlbeskeder

**Løsning for brugere:**
```r
# Option 1: Brug renv (anbefalet)
renv::restore()  # Installerer automatisk korrekt fork
# Genstart R session

# Option 2: Manuel installation
remotes::install_github('teunbrand/ggrepel@marquee_repel')
# Genstart R session
```

### Sikkerhed mod package collision

renv isolerer SPCify's dependencies i projekt-bibliotek:
- Standard ggrepel i brugerens globale library → **IKKE brugt**
- Custom ggrepel fork i `renv/library/` → **BRUGES af SPCify**
- Ingen konflikt mellem versioner

## Hvad sker der når marquee_repel bliver merged?

Når `geom_marquee_repel` bliver merged ind i officiel ggrepel:
1. Opdater DESCRIPTION: Fjern `Remotes:` linje, opdater version
2. Installer fra CRAN: `install.packages("ggrepel")`
3. Fjern version check fra `R/zzz_ggrepel_check.R`
4. Opdater lock: `renv::snapshot()`
5. Commit og push opdateret `renv.lock`

## .gitignore Setup

renv filer inkluderet i git:
- ✅ `renv.lock` - Dependency lockfile (COMMIT)
- ✅ `renv/activate.R` - Bootstrap script (COMMIT)
- ✅ `.renvignore` - Ignore patterns (COMMIT)
- ❌ `renv/library/` - Package binaries (IGNORE)
- ❌ `renv/staging/` - Temp staging (IGNORE)

## Troubleshooting

### Problem: "ggrepel version conflict"
```r
# Fjern cached installation
renv::purge("ggrepel")

# Re-restore fra lock
renv::restore(packages = "ggrepel")
```

### Problem: "geom_marquee_repel not found"
```r
# Verificer korrekt source
renv::status()  # Check for discrepancies

# Force reinstall fra GitHub
remotes::install_github("teunbrand/ggrepel@marquee_repel", force = TRUE)
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
3. **Dokumentér custom sources** i DESCRIPTION `Remotes:` field
4. **Test efter restore** at kritiske features (som geom_marquee_repel) virker
5. **Pin kritiske dependencies** med eksakte SHA's (som vi gør med ggrepel)
