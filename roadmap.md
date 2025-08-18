# SPC App - Udviklings Roadmap Trin-for-Trin

## 🎯 Overordnet Udviklingsstrategi

**Princip**: Build → Test → Validate → Iterate
- Hver fase har klare **succeskriterier** og **testpunkter**
- **Løbende kvalitetssikring** med rigtige data
- **Modulær udvikling** - et modul ad gangen
- **Early prototyping** for at fange UX-problemer tidligt

---
  
## Phase 1: Foundation & Core Infrastructure (Uge 1-2)
  
### 1.1 Project Setup & Struktur

```r
# Mappestruktur
spc_app/
├── R/
│   ├── app.R              # Main app entry
│   ├── global.R           # Global variables & functions
│   ├── modules/           # Shiny modules
│   ├── utils/             # Helper functions
│   └── data/              # Test data
├── www/                   # Static assets (logo, CSS)
├── reports/               # R Markdown templates
├── tests/                 # Unit tests
└── docs/                  # Documentation
```

**Deliverables:**
- [ ] RStudio projekt setup med renv
- [ ] Git repository med .gitignore
- [ ] Basic package dependencies (shiny, bslib, qicharts2, rhandsontable)
- [ ] Global.R med hospital branding variabler

**Test:** App starter og viser tom UI

### 1.2 Basic UI Framework
```r
# Basal layout med bslib tema
ui <- bslib::page_sidebar(
  sidebar = sidebar(),      # Tom sidebar
  mainPanel()              # Tom main panel
)
```

**Deliverables:**
- [ ] Responsive layout med bslib
- [ ] Hospital branding (farver, fonts)
- [ ] Navigation struktur (faner)
- [ ] Basic CSS styling

**Test:** UI ser professionel ud og virker på desktop/tablet

---
  
## Phase 2: Data Module (Uge 2-3)
  
### 2.1 File Upload & Preview
**Focus:** Få data ind i appen

```r
# Første iteration: Simpel CSV upload
dataModuleUI <- function(id) {
  fileInput(NS(id, "file"), "Upload CSV")
  # + preview tabel
}
```

**Deliverables:**
- [ ] File upload (CSV/XLSX)
- [ ] Data preview tabel
- [ ] Basic data validation og fejlmeddelelser
- [ ] Import indstillinger (separator, decimal, encoding)

**Test med rigtige data:**
- [ ] Hospital infektionsdata (anonymiseret)
- [ ] Ventetider fra ambulatorium
- [ ] Test fejlhåndtering med korrupte filer

### 2.2 Editable Data Table
**Focus:** rhandsontable integration

```r
# Redigérbar tabel med validering
output$editable_table <- renderRHandsontable({
  rhandsontable(data()) %>%
    hot_validate_numeric(cols = "værdi")
})
```

**Deliverables:**
- [ ] rhandsontable implementation
- [ ] Cell validation med advarsler
- [ ] Add/delete rows funktionalitet
- [ ] Basic undo/redo

**Test:**
- [ ] Redigér eksisterende data og verificér ændringer
- [ ] Test store datasæt (500+ rækker)
- [ ] Test performance ved real-time beregninger

**Kvalitetssikring checkpoints:**
- ✅ Kan uploade hospital CSV-filer uden fejl
- ✅ Kan redigere data og se ændringer med det samme
- ✅ Validering fanger typiske dataproblemer

---
  
  ## Phase 3: Visualization Core (Uge 3-4)
  
  ### 3.1 Basic qicharts2 Integration
**Focus:** Få første grafer til at virke

```r
# Minimalist graf-generation
output$plot <- renderPlot({
  qicharts2::qic(
    x = data()$tid,
    y = data()$værdi,
    chart = input$chart_type
  )
})
```

**Deliverables:**
- [ ] Run chart (default)
- [ ] I-chart og MR-chart
- [ ] P-chart for proportioner
- [ ] Basic plot styling

**Test med hospital data:**
- [ ] Månedlige infektionsrater
- [ ] Gennemsnitlige ventetider
- [ ] Andel patienter behandlet indenfor målsætning

### 3.2 Anhøj Rules Implementation
**Focus:** Korrekt signaldetektion

```r
# Test Anhøj reglerne
test_anhoej_rules <- function(data) {
  # Implementér og test mod kendte eksempler
}
```

**Deliverables:**
- [ ] Automatisk runs-test
- [ ] Crossings-test
- [ ] Visual highlighting af signals
- [ ] Signal-rapportering tekst

**Kritisk test:**
- [ ] Sammenlign resultater med qicharts2 reference
- [ ] Test med datasæt hvor du kender det forventede resultat
- [ ] Verificér matematik bag signal-detektion

**Kvalitetssikring checkpoints:**
- ✅ qicharts2 grafer vises korrekt
- ✅ Anhøj regler giver samme resultater som reference
- ✅ Signals markeres tydeligt og forklares

---
  
  ## Phase 4: Export & Branding (Uge 4-5)
  
  ### 4.1 PNG Export
**Focus:** High-quality graf export

```r
# PNG eksport med branding
export_png <- function(plot, filename) {
  # Hospital logo, footer, styling
  ggsave(filename, plot, dpi = 300, ...)
}
```

**Deliverables:**
- [ ] PNG eksport med hospital logo
- [ ] Konsistent branding (farver, fonts)
- [ ] Konfigurérbar størrelse og DPI
- [ ] Automatisk footer med metadata

**Test:**
- [ ] Export til forskellige størrelser
- [ ] Print-kvalitet output
- [ ] Logo placering og synlighed

### 4.2 Basic PDF Report
**Focus:** R Markdown rapport

```r
# Simpel rapport template
---
  title: "SPC Analyse - [Afdeling]"
output: pdf_document
---
  ```

**Deliverables:**
- [ ] R Markdown template med hospital branding
- [ ] Automatisk plot inclusion
- [ ] Basic metadata (dato, afdeling, etc.)
- [ ] PDF generation pipeline

**Test:**
- [ ] Generér rapport med rigtige hospital data
- [ ] Verificér branding konsistens
- [ ] Test forskellige graf-typer i rapport

**Kvalitetssikring checkpoints:**
- ✅ PNG eksport med korrekt hospital branding
- ✅ PDF rapport ser professionel ud
- ✅ Metadata og footer er korrekte

---
  
  ## Phase 5: Enhanced Features (Uge 5-6)
  
  ### 5.1 Multiple Chart Types
**Focus:** P', U', C, G charts

```r
# Udvid chart type support
supported_charts <- c("run", "i", "mr", "p", "pp", "u", "up", "c", "g")
```

**Deliverables:**
- [ ] Alle qicharts2 chart types
- [ ] Automatisk chart type forslag baseret på data
- [ ] Proper handling af count vs. rate data

### 5.2 Faceting & Multi-Unit
**Focus:** Småmultipler per afdeling

```r
# Facetted plots med individuelle kontrolgrænser
qic(..., facets = ~ afdeling)
```

**Deliverables:**
- [ ] Facetting by afdeling/enhed
- [ ] Individuelle kontrolgrænser
- [ ] Sammenlignelig akse-skalering

**Test med multi-afdeling data:**
- [ ] Forskellige afdelinger på samme hospital
- [ ] Sammenlignelig data på tværs af enheder

**Kvalitetssikring checkpoints:**
- ✅ Alle chart types fungerer korrekt
- ✅ Facetting viser meningsfulde sammenligninger
- ✅ Kontrolgrænser beregnes korrekt per enhed

---
  
  ## Phase 6: Polish & Production Ready (Uge 6-7)
  
  ### 6.1 Error Handling & User Experience
**Focus:** Robust production app

```r
# Comprehensive error handling
tryCatch({
  # App logic
}, error = function(e) {
  showNotification("Der opstod en fejl...", type = "error")
})
```

**Deliverables:**
- [ ] Comprehensive error handling
- [ ] Loading indicators og progress bars
- [ ] Input validation og user feedback
- [ ] Help tooltips og documentation

### 6.2 Performance Optimization
**Focus:** Handle realistiske datasæt

```r
# Optimize for larger datasets
reactive_data <- reactive({
  req(input$file)
  # Debounced og cached computations
}) %>% bindCache(input$file)
```

**Deliverables:**
- [ ] Debounced reactives for store datasæt
- [ ] Caching af beregnede resultater
- [ ] Memory management
- [ ] Concurrent user handling

**Final stress test:**
- [ ] 1000+ datapoint datasets
- [ ] Multiple concurrent users
- [ ] Edge cases og error scenarios

---
  
  ## 🔍 Løbende Kvalitetssikringsproces
  
  ### Weekly Quality Checks:
  1. **Funktional test** med rigtige hospital data
2. **Performance test** med realistiske datasæt
3. **UX review** med potentielle brugere
4. **Code review** og refactoring

### Test Data Sources:
- **Infektionsdata**: MRSA, C.diff rater
- **Ventetider**: Ambulatorie, akut modtagelse
- **Kvalitetsindikatorer**: Komplikationsrater
- **Edge cases**: Missing data, få datapunkter, outliers

### Validation Framework:
```r
# Unit tests for hver fase
testthat::test_that("Anhøj rules work correctly", {
  result <- test_anhoej_implementation(known_dataset)
  expect_equal(result$signals, expected_signals)
})
```

---
  
  ## 📝 Success Criteria Per Phase
  
**Phase 1**: ✅ App starter, UI ser professionel ud
**Phase 2**: ✅ Kan importere og redigere hospital data
**Phase 3**: ✅ Korrekte SPC grafer med signal-detektion
**Phase 4**: ✅ Professional eksport med hospital branding
**Phase 5**: ✅ Komplet chart type support og multi-unit analysis
**Phase 6**: ✅ Production-ready med robust error handling

## 🚀 Deployment Strategy

1. **Development**: Lokalt RStudio miljø
2. **Testing**: Shiny server eller RStudio Connect
3. **Production**: Hospital IT infrastructure
4. **Backup**: Docker container for reproducibility

---
  
*Dette roadmap giver dig en solid foundation til at bygge og kvalitetssikre SPC appen systematisk, med hyppige testpunkter og validation mod rigtige hospital data.*