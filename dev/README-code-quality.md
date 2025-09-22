# Code Quality Setup - SPC App

Dette dokument beskriver hvordan code quality tools (lintr, styler) er konfigureret i SPC appen.

## 🎯 Formål

- **Lintr**: Tjekker R kode for potentielle problemer og style issues
- **Styler**: Formaterer R kode automatisk efter tidyverse style guide
- **Git hooks**: Sikrer at code quality tjekkes automatisk ved commits

## 📁 Filer

```
dev/
├── lint_and_style.R           # Hovedscript til lintr + styler
├── pre-commit-hook.sh         # Manuel git hook (backup)
├── setup-precommit.sh         # Setup script til pre-commit framework
└── README-code-quality.md     # Denne fil

.lintr                         # Lintr konfiguration
.pre-commit-config.yaml        # Pre-commit framework config
.git/hooks/pre-commit         # Aktiv git hook (auto-genereret)
```

## 🔧 Setup Valgmuligheder

### Option 1: Manuel Git Hook (Standard - allerede aktivt)

```bash
# Allerede installeret og virker automatisk
git commit -m "din besked"  # Hook kører automatisk
```

Fordele:
- ✅ Kører automatisk uden ekstra dependencies
- ✅ Tilpasset specifikt til SPC app
- ✅ Håndterer danske kommentarer korrekt

### Option 2: Pre-commit Framework

```bash
# Install pre-commit framework
pip install pre-commit

# Setup hooks
./dev/setup-precommit.sh

# Disable manuel hook hvis ønsket
rm .git/hooks/pre-commit
```

Fordele:
- ✅ Mere avancerede hooks (YAML check, merge conflict detection)
- ✅ Automatisk updates af hooks
- ✅ Integration med mange andre tools

## 🎮 Usage

### Manuel kørsel
```bash
# Kør lintr + styler på alle filer
Rscript dev/lint_and_style.R

# Med pre-commit framework
pre-commit run --all-files
```

### Automatisk ved commit
```bash
git commit -m "din besked"
# → Kører automatisk code quality checks
# → Blokerer commit hvis kritiske errors
# → Formaterer kode automatisk hvis nødvendigt
```

## ⚙️ Konfiguration

### Lintr (.lintr)
- Line length: 120 tegn
- Tillader danske kommentarer
- Undtager `golem_utils.R` og `dev/` mapper
- Tjekker library() calls (foretrækker pkg::function())

### Styler (dev/lint_and_style.R)
- Følger tidyverse style guide
- Bevarer danske kommentar-style
- Formaterer kun når nødvendigt

### Exclusions
- `R/golem_utils.R` - Golem genereret kode
- `dev/` - Development scripts
- `tests/` - Test filer (kun for nogle checks)

## 🚫 Fejlhåndtering

### Exit codes (fra dev/lint_and_style.R)
- `0`: Alt OK
- `1`: Kritiske errors (blokerer commit)
- `2`: Warnings (tillader commit)

### Typiske fejl
```bash
# Kritiske lintr errors fundet
❌ FEJL: Kritiske lintr errors fundet - skal rettes!

# Styler ændrede filer
📝 Styler har ændret 3 filer - husk at stage dem til git!
git add .
git commit
```

## 🔄 Maintenance

### Opdater hooks
```bash
# Pre-commit framework
pre-commit autoupdate

# Manuel hook - geninstallér
cp dev/pre-commit-hook.sh .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit
```

### Test setup
```bash
# Test manuel script
Rscript dev/lint_and_style.R

# Test pre-commit
pre-commit run --all-files

# Test git hook
echo "# test" >> R/app_config.R
git add R/app_config.R
git commit -m "test"
```

## 🎯 Best Practices

1. **Commit tidligt og ofte** - hooks kører hurtigere på små ændringer
2. **Fix lintr errors først** - kritiske errors blokerer commits
3. **Lad styler gøre arbejdet** - formater ikke manuelt
4. **Ignorer style warnings** - fokusér på functionality først
5. **Bevar danske kommentarer** - tooling understøtter det

## 🛠️ Troubleshooting

### Hook kører ikke
```bash
# Tjek hook eksisterer og er eksekverbar
ls -la .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit
```

### R packages mangler
```bash
# Install required packages
install.packages(c("lintr", "styler", "here"))
```

### Performance issues
```bash
# Kør kun på ændrede filer
git add specific_file.R
git commit -m "small change"
```