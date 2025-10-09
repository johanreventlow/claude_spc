# SPC App - Brugervejledning

Statistical Process Control (SPC) applikation til kvalitetsovervågning og procesanalyse i klinisk praksis.

## Indholdsfortegnelse

- [Kom i gang](#kom-i-gang)
- [Upload af data](#upload-af-data)
- [Kolonne konfiguration](#kolonne-konfiguration)
- [Chart typer](#chart-typer)
- [Fortolkning af resultater](#fortolkning-af-resultater)
- [Avancerede funktioner](#avancerede-funktioner)
- [Fejlfinding](#fejlfinding)

## Kom i gang

### Start applikationen

```r
# Fra R konsol
source('global.R')
```

Appen åbner i din standard webbrowser på `http://localhost:3838`.

### Første gang

Ved første opstart:
1. Appen loader nødvendige pakker (~5 sekunder)
2. Hospital branding indlæses automatisk
3. Du ser en tom main screen med "Upload Data" prompt

## Upload af data

### Understøttede filformater

- **CSV** (.csv) - Kommasepareret
- **Excel** (.xlsx, .xls) - Microsoft Excel
- **TSV** (.tsv, .txt) - Tab-separeret

### Data krav

Minimum kolonner der anbefales:
- **Dato/Tid kolonne** - Tidspunkt for observation (format: YYYY-MM-DD eller DD/MM/YYYY)
- **Værdi kolonne** - Målte værdier (numerisk)
- **Nævner kolonne** - For proportions/rater (valgfri, numerisk)
- **Kommentar kolonne** - Noter/annotations (valgfri, tekst)

**Eksempel data struktur:**

| Dato       | Antal_Fald | Patienter | Kommentar        |
|------------|-----------|-----------|------------------|
| 2024-01-01 | 3         | 150       |                  |
| 2024-01-02 | 1         | 145       |                  |
| 2024-01-03 | 5         | 160       | Weekend spike    |
| 2024-01-04 | 2         | 155       |                  |

### Upload proces

1. **Klik "Upload Data"** knappen i sidebar
2. **Vælg fil** fra din computer
3. **Auto-detection kører** - Appen genkender automatisk kolonnetyper
4. **Data tabel vises** - Tjek at data ser korrekt ud
5. **SPC chart genereres** - Plot vises automatisk

### Encoding og specialtegn

Appen understøtter:
- ✅ Danske tegn (æ, ø, å)
- ✅ UTF-8 encoding
- ✅ ISO-8859-1 (Latin-1)
- ✅ Windows-1252

Hvis specialtegn ikke vises korrekt, gem filen som UTF-8 i Excel/din editor.

## Kolonne konfiguration

### Automatisk detektion

Appen detekterer automatisk:
- **X-akse (Dato)** - Søger efter dato-lignende kolonner
- **Y-akse (Værdi)** - Numeriske værdier
- **Nævner (N)** - For rater/proportioner
- **Kommentarer** - Tekst kolonner

**Confidence scores** vises for hver detektion (høj/medium/lav).

### Manuel justering

Hvis auto-detection ikke er korrekt:

1. **Åbn "Kolonne Indstillinger"** i sidebar
2. **Vælg korrekt kolonne** fra dropdown for:
   - X-akse (Dato/Tid)
   - Y-akse (Målværdi)
   - Nævner (hvis relevant)
   - Kommentar (valgfri)
3. **Plot opdateres automatisk** efter ændringer

### Skift/Fase konfiguration

For at vise procesændringer:

1. **Aktiver "Vis Skift"** checkbox
2. **Vælg "Skift Kolonne"** - kolonne med fase-indikatorer
3. **Vertikal linje** vises ved hver fase-overgang
4. **Kontrol limits** genberegnes for hver fase

## Chart typer

### Run Chart (Standardvalg)

**Hvornår:** Enkel trendvisning uden statistiske kontrol limits.

**Fordele:**
- Simpel at fortolke
- Kræver ingen antagelser om datafordeling
- Viser trend og variation klart

**Anhøj's rules detekterer:**
- Shifts (flere punkter konsekutivt på samme side)
- Trends (stigende/faldende mønstre)
- Too few crossings (for lidt variation)

### I-chart (Individual Chart)

**Hvornår:** Kontinuerlige målinger med enkeltobservationer.

**Eksempler:**
- Ventetider (minutter)
- Blodtryk målinger
- Temperatur målinger

**Kontrol limits:** Baseret på bevægelig range (mR).

### P-chart (Proportion Chart)

**Hvornår:** Proportioner eller procentsatser med variabel nævner.

**Eksempler:**
- Andel af fald pr. 100 patienter
- Procentdel af korrekte medicindoseringer
- Infektionsrate pr. 1000 patienter

**Krav:**
- Y-kolonne: Antal events (tæller)
- N-kolonne: Total muligheder (nævner)
- Nævner kan variere mellem observationer

### U-chart (Rate Chart)

**Hvornår:** Rater hvor events kan forekomme flere gange pr. enhed.

**Eksempler:**
- Antal fejl pr. 1000 recepter
- Fald pr. 1000 sengedage
- Komplikationer pr. 100 procedurer

**Forskel fra P-chart:** Events kan overstige nævner (>100%).

### PP-chart (Standardized P-chart)

**Hvornår:** P-chart med standardiseret Y-akse for lettere sammenligning.

**Fordele:**
- Lettere at sammenligne på tværs af forskellige perioder
- Kontrol limits er konstante (±3 sigma)

### UP-chart (Standardized U-chart)

**Hvornår:** U-chart med standardiseret Y-akse.

## Fortolkning af resultater

### Kontrol limits (UCL/LCL)

**Upper Control Limit (UCL)** - Øvre grænse (rød stiplet linje)
**Lower Control Limit (LCL)** - Nedre grænse (rød stiplet linje)
**Centerline (CL)** - Gennemsnit (blå linje)

**Fortolkning:**
- Punkter **inden for limits** → Almindelig variation (common cause)
- Punkter **uden for limits** → Særlig årsag (special cause) - kræver undersøgelse

### Anhøj's Rules (Non-Random Variation)

#### 1. Shifts
**Definition:** For mange punkter på samme side af centerline.

**Signal:**
- ≥8 konsekutive punkter på én side
- Indikerer systematisk ændring i proces

**Handling:** Undersøg hvad der ændrede sig omkring shift-punktet.

#### 2. Too Few Crossings
**Definition:** For få krydsninger af centerline.

**Signal:**
- Færre crossings end forventet
- Indikerer ujævn variation eller faser

**Handling:** Tjek om data er stratificeret eller har skjulte faser.

#### 3. Too Many Runs
**Definition:** For mange korte sekvenser.

**Signal:**
- Over-mixing af data
- Kan indikere sampling bias

**Handling:** Verificer dataindsamlingsmetode.

### Anhoej Results Panel

Appen viser automatisk:
- **Longest Run:** Længste sekvens på samme side (max værdi i rødt hvis signal)
- **N Crossings:** Antal centerline krydsninger (min værdi i rødt hvis for få)
- **Out of Control:** Antal punkter uden for kontrol limits
- **Interpretation:** Tekst-beskrivelse af fund

**Eksempel output:**
```
Longest Run: 12 / 8 (SIGNAL - Shift detected)
N Crossings: 5 / 9 (SIGNAL - Too few crossings)
Out of Control: 2 points
Interpretation: Special cause variation detected - investigate process changes
```

### Practical Interpretation

| Observation | Betydning | Handling |
|------------|-----------|----------|
| Alle punkter inden for limits, ingen signals | Stabil proces | Fortsæt overvågning |
| Punkt(er) over UCL | Usædvanlig høj værdi | Undersøg årsag - positiv eller negativ? |
| Punkt(er) under LCL | Usædvanlig lav værdi | Undersøg årsag - positiv eller negativ? |
| Shift signal | Proces niveau ændret | Identificer hvornår + hvorfor |
| Too few crossings | Stratificering eller faser | Overvej at opdele data i perioder |

## Avancerede funktioner

### Target og Centerline

**Brugerdefineret Target:**
1. Aktiver "Brug Target" checkbox
2. Indtast ønsket målværdi
3. Rød horizontal linje vises på chart

**Manuel Centerline:**
1. Aktiver "Brug Manuel Centerline"
2. Indtast ønsket centerline værdi
3. Kontrol limits beregnes omkring denne værdi

**Anvendelse:**
- Sammenlign aktuel performance med target
- Sæt goals for forbedringsarbejde

### Frys kontrol limits

**Hvornår:** Når du vil låse kontrol limits efter baseline-periode.

**Fremgangsmåde:**
1. Upload data med "Frys" kolonne (0 eller 1)
2. Sæt 1 for baseline-punkter, 0 for nye data
3. Vælg "Frys Kolonne" i indstillinger
4. Kontrol limits beregnes kun fra baseline

**Anvendelse:**
- PDSA cycles - test af ændringer
- Før/efter sammenligning
- Implementering af ny standard

### Kommentarer og annotations

**Tilføj kontekst til datapunkter:**

1. Inkluder "Kommentar" kolonne i data
2. Skriv noter ved relevante datapunkter
3. Vælg "Kommentar Kolonne" i indstillinger
4. Kommentarer vises ved datapunkter på chart

**Eksempel:**
```
Dato       | Værdi | Kommentar
2024-01-15 | 8     | Ny procedure implementeret
2024-01-20 | 12    | Ferieperiode - reduceret bemanding
```

### Export funktionalitet

**Download plot:**
1. Højreklik på plot
2. Vælg "Save image as..."
3. Gem som PNG (høj opløsning)

**Download data:**
- CSV med SPC beregninger (kommer snart)
- Inkluderer: rådata + kontrol limits + signals

## Fejlfinding

### Data loader ikke

**Problem:** "Error: kunne ikke læse fil"

**Løsninger:**
- Tjek filformat (CSV/Excel)
- Verificer encoding (skal være UTF-8)
- Prøv at åbne i Excel og gem som "CSV UTF-8"

### Plot vises ikke

**Problem:** Tomt plot område efter upload

**Løsninger:**
1. Tjek at data har mindst 2 gyldige datapunkter
2. Verificer at Y-kolonne indeholder numeriske værdier
3. Tjek konsol for fejlmeddelelser (F12 i browser)
4. Genindlæs siden og prøv igen

### Auto-detection fejler

**Problem:** Forkerte kolonner valgt automatisk

**Løsning:** Brug manuel kolonne-selektion i sidebar under "Kolonne Indstillinger".

### Kontrol limits mangler

**Problem:** Kun centerline vises, ingen UCL/LCL

**Mulige årsager:**
- For få datapunkter (kræver minimum ~10)
- Kun ét unikt tal i data (ingen variation)
- Chart type ikke egnet til data (skift til Run Chart)

### Anhøj signals virker ikke

**Problem:** Anhøj results viser "Afventer data"

**Check:**
1. Er "Vis Anhøj Resultater" aktiveret?
2. Er der mindst 10 datapunkter?
3. Er data numerisk og ikke-konstant?

### Performance problemer

**Problem:** Langsom opdatering ved store datasæt

**Optimering:**
- Reducer antal datapunkter (filter til relevant periode)
- Maksimum anbefalet: ~500 punkter pr. chart
- Split store datasæt i separate perioder

### Encoding problemer

**Problem:** Æ, ø, å vises som �

**Løsning:**
1. Åbn CSV i notepad/text editor
2. "Save As" → Encoding: "UTF-8"
3. Upload igen

## Tips og best practices

### Data forberedelse

✅ **Gør:**
- Brug konsistent datoformat (YYYY-MM-DD anbefales)
- Tjek for duplikerede datoer før upload
- Fjern tomme rækker nederst i Excel
- Navngiv kolonner tydeligt (undgå "Column1", "X", "Y")

❌ **Undgå:**
- Mellemrum i kolonnenavne (brug underscore: "Antal_Fald")
- Specialtegn i kolonnenavne (#, %, /, *, etc.)
- Formler i Excel celler (konverter til værdier)
- Merged cells i Excel

### Chart valg

**Run Chart** → Start altid her for simpel visualisering

**I-Chart** → Kontinuerlige målinger (ventetid, temperatur, blodtryk)

**P-Chart** → Binære outcomes (ja/nej, fald/ikke-fald, succes/fejl)

**U-Chart** → Events der kan ske flere gange (fejl, komplikationer, fald)

### Datamængde

- **Minimum:** 10-15 punkter for meningsfuld SPC analyse
- **Optimalt:** 20-30 punkter for stabile kontrol limits
- **Maximum:** 500 punkter (split større datasæt i perioder)

### Fortolkning

1. **Se på det store billede først** - Overordnet trend
2. **Identificer special cause variation** - Punkter uden for limits
3. **Undersøg Anhøj signals** - Shifts og mønstre
4. **Kontekstualisér** - Hvad skete der omkring signifikante punkter?
5. **Handl på insights** - Dokumentér læring og juster proces

## Support og yderligere hjælp

### Yderligere ressourcing

- **SPC teori:** "Understanding Variation" af Donald Wheeler
- **Anhøj's Rules:** [Original artikel](https://doi.org/10.1371/journal.pone.0113825)
- **qicharts2 dokumentation:** [CRAN](https://cran.r-project.org/package=qicharts2)

### Rapportering af fejl

Hvis du oplever problemer:
1. Noter præcis hvad du gjorde
2. Tag screenshot af fejlmeddelelse
3. Gem fejllog fra R konsol
4. Kontakt support med ovenstående info

## Version

Denne vejledning er for SPC App version 1.0 (Sprint 5 complete).

Senest opdateret: Januar 2025
