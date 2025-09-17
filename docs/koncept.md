# R Shiny SPC App - Komplet Specifikation

## Executive Summary

En R Shiny-applikation til sundhedsvæsenet, der gør det muligt for brugere at producere standardiserede seriediagrammer (run charts) og SPC-kontrolkort på mindre end 5 minutter - fra datainput til færdig rapport med korrekt branding og signaldetektion baseret på Anhøj-reglerne.

---

## 1. Formål og Målgruppe

### Formål
En standardiseret og robust platform til at producere seriediagrammer og relevante SPC-kontrolkort med mulighed for simpel dataredigering direkte i appen, dokumentation af regler (Anhøj-regler), samt eksport af grafer og rapporter.

### Primære brugere
- **Kvalitets- og forbedringsteams**: Hurtigt at generere seriediagrammer og kontrolkort til ugentlige/månedlige opfølgninger
- **Afdelinger/kliniske ledere**: Overblik over indikatorer (andel, rate, tid mellem hændelser)
- **Analytikere**: Reproducerbar konfiguration, kode-eksport og batch-kørsel

### Brugsscenarier
- Monitering af infektionsrater, medicinfejl, ventetider
- Før/efter-analyser af kvalitetsforbedringsindsatser
- Månedlig rapportering til ledelse med standardiserede grafer
- Træning i SPC-metoder med indbygget dokumentation

---

## 2. Teknisk Arkitektur

### Frontend
- **Framework**: Shiny med bslib-tema for moderne UI
- **Struktur**: Modulær opbygning med Shiny modules:
  - `dataModule`: Upload, import og redigering
  - `visualModule`: Graf-generering og konfiguration
  - `analysisModule`: SPC-regler og diagnostik
  - `exportModule`: Rapporter og downloads
  - `settingsModule`: Branding og konfiguration

### Backend
- **SPC-motor**: `qicharts2::qic()` som kernebibliotek til seriediagrammer og Shewhart-kort (I, MR, Xbar, S, T, C, U, U', P, P', G)
- **Datatabel**: Editérbar tabel via `rhandsontable` med validering, dropdowns, copy–paste, multi-select, tooltips, kolonneformatering og undo/redo
- **State-håndtering**: `reactiveValues()` + mulighed for at gemme/indlæse konfigurationer (JSON)
- **Privatliv**: Standard kører lokalt (ingen upload til eksterne services)

### Datamodel
```
Minimum kolonner:
- tid (dato/tidsstempel eller sekvens)
- værdi (numerisk)

Valgfrie kolonner:
- tæller/nævner (til P-/U-kort)
- gruppe/fase
- enhed/afdeling
- kommentarer
```

---

## 3. Datainput og Håndtering

### Input-metoder
1. **Upload**: CSV/XLSX med preview og konfiguration (skilletegn, decimalseparator, encoding, datokolonner med auto-detektion + manuel override, NA-håndtering)
2. **Copy-paste**: Direkte indsættelse fra Excel/andre kilder
3. **Eksempeldata**: Prædefinerede datasæt til træning/demo

### Direkte redigering (rhandsontable)
- **Grundfunktioner**: Tilføj/slet rækker, redigér celler, batch-fill
- **Validering**: Numerisk format, datoformat, ikke-negative værdier, nævner ≥ tæller
- **Avanceret**: Undo/redo, "reset til original", beregnede felter (procent = tæller/nævner; rate pr. 1000; rullende median/mean)

---

## 4. Databehandling og Transformationer

### Måletype-kategorier
- **Måling**: Kontinuerlige data (I/MR, Xbar/S)
- **Andel**: Proportioner (P/P')
- **Rate**: Hændelser pr. enhed (U/U')
- **Tid mellem hændelser**: G-kort
- **Tælling**: C-kort

### Subgruppering og filtrering
- **Subgruppering**: Uge/måned/kvartal med opsummeringsfunktion (mean, sum, rate pr. X, andel = sum(tæller)/sum(nævner))
- **Filtre**: Enhed, periode, kategori, inkl. hurtigfilter til seneste N perioder
- **Outlier-håndtering**: Label men ikke auto-fjern; mulighed for at ekskludere datapunkter bevidst (med audit trail)
- **Faseændringer**: Markér start for ny fase, automatisk genberegning

---

## 5. Visualisering og UI

### Layout-struktur
```
├── Sidebar (kontrolpaneler)
│   ├── Data-indstillinger
│   ├── Graf-konfiguration
│   ├── Analyse-parametre
│   └── Eksport-muligheder
└── Main panel (faner)
    ├── Graf
    ├── Tabel (editérbar)
    ├── Diagnostik
    └── Kommentarer
```

### Seriediagram (Run chart)
- **Visning**: Punkt-linje-plot med medianlinje
- **Konfiguration**: Median eller mean som reference; vælg antal datapunkter, ekskludér punkter på median
- **Annotations**: Mål-/målsætning-linje, tolerancebånd, annotationsværktøj

### Shewhart-kontrolkort
- **Korttyper**: I/MR, Xbar/S, P/P', U/U', C, G
- **Beregninger**: Automatisk CL, UCL, LCL baseret på qicharts2
- **Visning**: Kontrolgrænser, centerline, markering af out-of-control punkter

### Avancerede features
- **Facettering**: Småmultipler pr. enhed/afdeling
- **Sammenligning**: Baseline vs. forbedringsfase overlay
- **Interaktivitet**: Hover-tooltips, zoom/pan, klik-for-annotation

---

## 6. Analyse og Regler - VIGTIG AFGRÆNSNING

### Anhøj-regler (KUN for run charts)
- **Automatisk beregning** af antal krydsninger og længste run relativt til N (ekskl. punkter på median)
- **Dynamiske kritiske værdier** med markering af *special cause*
- **Visning** af hvilke punkter/perioder udløser signalet
- **Rapportlinje**: "Signal: Ja/Nej" + hvilken regel + kort forklaring

### Kontrolkort-regler (KUN basale grænsebrud)
- **Kun basale grænsebrud**: Punkter uden for kontrolgrænser
- **INGEN udvidede regler**: Western Electric/Champions-regler indgår IKKE

### Datakrav og advarsler
- Soft-advarsel ved få datapunkter (fx <16 brugbare punkter til runs)
- Flag for mange punkter på median der kan reducere styrken af runs-test

---

## 7. Kommentarer og Dokumentation

### Kommentarer
- **Inline-kommentarer**: Bundet til datapunkter/faser
- **Global note**: Til hele graf/figur
- **Audit trail**: Log over ændringer (valgfrit)

### Indbygget hjælp
- **SPC-teori**: Forklaring af Anhøj-regler og kontrolkort-principper
- **Tooltips**: Kontekstuel hjælp til UI-elementer
- **Eksempler**: Gennemgang af typiske anvendelsescases

---

## 8. Eksport og Rapportering

### Graf-eksport (must-have)
- **Formater**: PNG og PDF med konfigurérbar bredde/højde og DPI
- **Branding**: 
  - Automatisk branded footer (titel, datakilde, dato, regelsæt)
  - Organisationslogo
  - Standardiseret ggplot/qicharts2-tema (farver/typografi) i tråd med organisationens brand

### Rapport-generering (must-have)
- **PDF via R Markdown/Quarto** med branded skabelon:
  - Forside med logo og organisationsinfo
  - Standardiseret layout, farver og sidefod
- **Indhold**:
  - Grafer med høj opløsning
  - Diagnoseafsnit (Anhøj-analyse)
  - Metode-beskrivelse
  - Datasammendrag og kommentarer

### Data og kode-eksport
- **Data**: CSV/XLSX med alle redigeringer bevaret
- **Konfiguration**: JSON-fil til genskabelse af analyse
- **Kode-eksport**: Reproducerbar `qicharts2::qic()`-kode

---

## 9. Indstillinger og Konfiguration

### Branding-indstillinger
- **Tema/branding**: Organisationens farver, logo, typografi og standardiseret sidefod på grafer/rapporter
- **Regler**: KUN Anhøj-regler for run charts (ingen brugerdefinerede); på kontrolkort kun basale grænsebrud
- **Privatliv**: "Gem aldrig data"-tilstand

### Sprog og lokalisering
- **Dansk/Engelsk**: Komplet interface-oversættelse
- **Dato-formater**: Regional konfiguration

---

## 10. Roadmap og Udvidelser

### Version 1 (MVP)
- Core SPC-funktionalitet (run charts + basale kontrolkort)
- Dataredigering med rhandsontable
- PNG/PDF eksport med branding
- Anhøj-regler implementation

### Version 2
- **Batch-rapportering**: Multiple datasæt og automatiseret kørsel
- **Udvidede kontrolkort**: X-mR for individuelle værdier
- **API-mode**: Programmatisk adgang til funktionalitet

### Version 3
- **Shinylive/offline**: Browser-baseret kørsel uden R-server (roadmap - ikke krav i v1)
- **Brugerprofiler**: Personlige indstillinger og favoritter
- **Integration**: Excel-skabeloner, SQL-databaser, FHIR-standarden

---

## 11. Succeskriterier og KPI'er

### Performance-mål
- **<5 minutter** fra import til run chart eller Shewhart-kort med Anhøj-diagnose og PNG/PDF-eksport med korrekt branding
- **<10 sekunder** graf-generering for datasæt op til 1000 punkter

### Funktionalitets-mål
- **Korrekt signaldetektion**: 100% korrekthed i Anhøj-diagnose sammenlignet med qicharts2
- **Stabil dataredigering**: rhandsontable understøtter stabil redigering (undo/redo), validering og bevarer formater ved eksport
- **Reproducerbar kode**: Kode-eksport (R) genskaber samme figur med samme indstillinger

### Brugeroplevelse
- **Intuitiv workflow**: Nye brugere kan producere første graf inden for 15 minutter
- **Konsistent interface**: Alle Shiny modules følger samme design-mønstre

---

## 12. Tekniske Overvejelser

### Dependencies
- **Core**: shiny, bslib, qicharts2, rhandsontable
- **Visualisering**: ggplot2, plotly (til interaktivitet)
- **Export**: rmarkdown/quarto, webshot2 (PNG), pagedown (PDF)
- **Utilities**: DT, shinycssloaders, shinyWidgets

### Performance og sikkerhed
- **Reaktivitet**: Debounced inputs til store datasæt
- **Memory management**: Effektiv håndtering af store Excel-filer
- **Input validation**: Omfattende tjek af uploadede filer
- **File size limits**: Rimelige grænser for upload-størrelse

---

*Dette dokument danner grundlag for udvikling af en robust, brugervenlig SPC-applikation til sundhedsvæsenet med fokus på korrekt statistisk analyse og professionel præsentation.*