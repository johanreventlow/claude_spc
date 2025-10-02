# Dependency Management Strategy

## Nuværende Tilgang: Full renv Lock ✅ (Anbefalet)

**Status:** Alle 101 pakker er låst i `renv.lock`

**Fordele:**
- ✅ **Reproducerbarhed**: Samme environment på alle maskiner
- ✅ **Stabilitet**: Ingen overraskelser fra upstream breaking changes
- ✅ **Testbarhed**: CI/CD kan recreate exact environment
- ✅ **Debugging**: Nemmere at reproducere bugs
- ✅ **Compliance**: Vigtig for sundhedssoftware med audit trails

**Ulemper:**
- ⚠️ Manuel opdatering af dependencies med `renv::update()`
- ⚠️ Kan gå glip af bugfixes i dependencies

**Use case:** Produktion, stabilitet-kritisk software, healthcare apps

---

## Alternativ: Selective Lock (Kun ggrepel)

**Koncept:** Lås kun kritiske custom dependencies, lad resten være "latest"

### Implementering:

#### Option A: Minimal renv.lock
```r
# Deactivate renv completely
renv::deactivate()

# Remove renv infrastructure
unlink("renv", recursive = TRUE)
unlink(".Rprofile")
unlink("renv.lock")

# Keep only DESCRIPTION with Remotes field
# DESCRIPTION already specifies:
#   Remotes: teunbrand/ggrepel@marquee_repel
#   ggrepel (>= 0.9.6.9999)

# Installation for users:
remotes::install_deps()  # Installs latest CRAN + custom ggrepel fork
```

**Fordele:**
- ✅ Altid seneste CRAN versioner
- ✅ Automatiske bugfixes fra upstream
- ✅ Simplere workflow (ingen renv overhead)

**Ulemper:**
- ❌ Breaking changes fra dependencies kan bryde appen
- ❌ Svært at reproducere bugs fra produktionsmiljø
- ❌ CI/CD kan fejle hvis upstream introducerer breaking changes
- ❌ Ikke reproducerbart mellem udviklere

#### Option B: Hybrid - renv med update policy
```r
# Keep renv, men tillad opdateringer undtagen for kritiske pakker
renv::settings$snapshot.type("explicit")

# I DESCRIPTION, kun angiv kritiske dependencies:
Depends:
    R (>= 4.0.0)
Config/renv/profiles: default

# Kun lock ggrepel, resten hentes som latest:
renv::snapshot(packages = "ggrepel")
```

**Fordele:**
- ✅ Låser kun ggrepel fork
- ✅ Andre pakker opdateres frit
- ⚠️ Middelvej mellem stabilitet og freshness

**Ulemper:**
- ⚠️ Kompleks renv configuration
- ⚠️ Stadig risiko for breaking changes
- ⚠️ Sværere at debugge version-specific bugs

---

## Anbefaling for SPCify

### 🏥 Healthcare Software = Full Lock (Nuværende)

**Hvorfor:**
1. **Patient safety**: Ingen overraskelser i produktion
2. **Audit compliance**: Præcis version tracking
3. **Regulatory requirements**: FDA/CE krav til software validation
4. **Reproducerbarhed**: Critical for bug investigation

**Workflow:**
```r
# Kvartalsvis dependency review
renv::status()           # Check for updates
renv::update()           # Test updates
testthat::test_dir()     # Run full test suite
# Manual QA testing
renv::snapshot()         # Lock tested versions
git commit -m "chore(deps): quarterly dependency update"
```

### 🚀 Rapid Development = Selective Lock

**Kun hvis:**
- Ikke healthcare/kritisk software
- Team har resources til hyppig dependency testing
- Breaking changes er acceptable tradeoff

**Workflow:**
```r
# Behold kun Remotes: field i DESCRIPTION
# Ingen renv
remotes::install_deps()  # Hent seneste hver gang
```

---

## Decision Matrix

| Kriterie | Full Lock | Selective Lock | No Lock |
|----------|-----------|----------------|---------|
| Reproducerbarhed | ✅ Excellent | ⚠️ Partial | ❌ Poor |
| Stabilitet | ✅ High | ⚠️ Medium | ❌ Low |
| Freshness | ❌ Manual | ✅ Automatic | ✅ Always latest |
| Debugging | ✅ Easy | ⚠️ Moderate | ❌ Hard |
| Healthcare compliance | ✅ Yes | ⚠️ Maybe | ❌ No |
| CI/CD reliability | ✅ High | ⚠️ Medium | ❌ Low |

---

## Migration til Selective Lock (Hvis ønsket)

### Step 1: Backup current state
```bash
git add renv.lock
git commit -m "chore: backup full renv lock before switching to selective"
git tag "v1.0-full-lock"
```

### Step 2: Switch to selective
```r
# Deactivate renv
renv::deactivate()

# Remove renv files (keep .gitignore entry commented out)
rm -rf renv/
rm .Rprofile
rm renv.lock
rm .renvignore

# DESCRIPTION already has Remotes: field, så ggrepel fork er specified
```

### Step 3: Update documentation
```bash
# Update README with new installation instructions
echo "remotes::install_deps()" >> README.md
rm README_RENV.md  # No longer needed
```

### Step 4: Test installation
```r
# Fresh R session
remove.packages("SPCify")
remotes::install_deps()
devtools::load_all()
testthat::test_dir()
```

---

## Min anbefaling for dig

**Behold full lock** af følgende grunde:

1. **Healthcare context**: Du arbejder med kvalitetsdata fra hospital
2. **Stabilitet prioritet**: Breaking changes fra ggplot2/shiny kan ødelægge plots
3. **Reproducerbarhed**: Vigtig for audit og quality assurance
4. **Team onboarding**: Nye udviklere får exact environment
5. **Low maintenance cost**: Kvartalsvis opdatering er tilstrækkeligt

**Kompromis:**
Hvis du vil have freshness, kør quarterly dependency updates:
```r
# Hver 3. måned
renv::update()         # Hent seneste versioner
# Test grundigt
renv::snapshot()       # Lock nye versioner
```

Dette giver dig:
- ✅ Stabilitet mellem opdateringer
- ✅ Kontrollerede opdateringer med testing
- ✅ Reproducerbarhed
- ✅ Latest features (delayed by max 3 months)
