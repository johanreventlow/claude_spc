# Claude Code Instruktioner – SPC App

## 1) Projektoversigt

Dette er en **R Shiny** applikation til **Statistical Process Control (SPC)** med **qicharts2**. Appen anvendes i klinisk kvalitetsarbejde og skal forblive stabil, forståelig og på dansk.

---

## 2) Vigtige regler (baseline)

* **ALDRIG** ændre globale konfigurationer uden eksplicit aftale.
* `TEST_MODE_AUTO_LOAD` skal være **TRUE** under udvikling.
* Bevar **dansk interface** og **danske kommentarer**.
* Brug kun **godkendte commit-beskeder på dansk**.
* Brug **git commit hash `f05a97f`** som reference for stabil version.

---

## 3) Arkitektur (fil- og mappeoversigt)

* `/R/modules/` – Shiny-moduler (visualisering, status mv.).
* `/R/server/` – Server-logik, opdelt i filer.
* `/R/ui/` – UI-komponenter.
* `/R/data/` – Eksempeldata og testfiler.

*Ret kun de sektioner/filer, der er nødvendige for den konkrete opgave.*

---

## 4) Debugging

* Brug **`cat()`** til minimal, målrettet debug-output.
* Tjek altid **console-output** ved fejl.
* Ved **frys/loops**: gennemgå **reactive dependencies** (især `reactive`, `observe`, `observeEvent`, `req`, `isolate`).

---

## 5) Arbejdsflow

1. Test altid ændringer på **ny port** først.
2. Bevar **backup** af fungerende commits (branch eller tag).
3. Sammenlign mod **`f05a97f`** før merge.
4. Små, afgrænsede PRs med tydelig diff.
5. **Commit-forslag løbende:** Efter hver lille, logisk ændring skal der **foreslås** et commit (ingen auto-commit). Se “Commit-beskeder”.

---

## 6) Forbud

* Ingen **automatiske commits** uden eksplicit aftale (kun **forslag** om commit).
* Ingen **stor refaktorering** uden godkendelse.
* Ingen ændringer af **`brand.yml`** eller **hospitalskonfiguration**.
* Ingen nye **afhængigheder** (pakker) uden godkendelse.
* **Ingen ændringer af CSV-filer** hvad angår **encoding**, **linjeafslutninger (CRLF)**, **delimiter** eller **BOM** (CSV bruges i Windows-kontekst). Bevar eksisterende format uændret.

---

## 7) Kodningsprincipper (for at holde LLM’en på sporet)

### 7.1 Afgrænsning & plan

* Start hver opgave med én linjes problemformulering: **“Jeg løser: …”**
* Notér evt. antagelser kort; vælg altid **mindst indgribende** løsning.
* Oplist **hvilke filer/sektioner** du ændrer og **hvorfor** (maks 3 punkter).

### 7.2 Minimal & lokal ændring

* Skriv **den absolutte minimumsmængde kode** der kræves.
* **Ingen omfattende ændringer**, **ingen urelaterede rettelser**.
* **Rør kun** ved nødvendige linjer – undgå kosmetik (whitespace/format) uden formål.
* Bevar eksisterende **API’er/funktionssignaturer** medmindre opgaven kræver andet.

### 7.3 Stil & konvention

* Følg projektets **eksisterende stil** (navngivning, mappe-/filstruktur).
* Kommentér kort **hvorfor** (ikke det åbenlyse **hvad**).
* Gør koden **præcis, modulær, testbar**, og hold funktioner **enkeltansvarlige**.

### 7.4 Data & filer (CSV m.m.)

* **CSV:** Bevar **encoding, CRLF, delimiter og BOM** præcis som i repo.
* Undgå at åbne/lagre CSV via værktøjer, der ændrer disse egenskaber implicit.
* Ændr ikke decimalseparator eller kolonneordener uden udtrykkeligt krav.

### 7.5 Sikkerhed & robusthed

* Ingen **hårdkodede hemmeligheder**, nøgler eller stier.
* **Valider input** og håndtér fejl dér, hvor de opstår; ingen tavs fejlslukning.
* Log ikke følsomme data; brug eksisterende mønstre for log/fejl.

### 7.6 Ydelse & kompatibilitet

* Ingen “optimeringer” uden **målbar** begrundelse.
* Bevar **bagudkompatibilitet** og eventuelle **feature flags**.
* Introducér ikke global tilstand; undgå utilsigtede sideeffekter.

---

## 8) Test & verifikation

* Tilføj/tilpas **små enhedstests** for ny/ændret adfærd (hvor relevant).
* Alle eksisterende tests skal **bestå**; hvis en test brydes, forklar **hvorfor** og ret **minimalt**.
* Medtag en **kort verifikationsguide**:

  1. Hvilken side/komponent åbnes
  2. Hvilket input/flow bruges
  3. Hvilket output forventes

---

## 9) Leveranceformat

* Vis ændringer som **diff/patch** eller tydelige kodeblokke med **fil- og linjereferencer**.
* Medtag en **ultrakort CHANGELOG-linje** (imperativ, én sætning).
* Marker alt, som mennesker skal gøre, som **\[MANUELT TRIN]** (fx migrering, genstart, env-variabler).

**Commit-beskeder (på dansk, uden omtale af Claude/AI/autoværktøjer):**

* Format: `type(scope): kort handle-orienteret beskrivelse`

  * Eksempler:

    * `fix(qi-plot): undgå NA i centerlinje ved tom nævner`
    * `feat(ui): tilføj kompakt visning af SPC-kort i modul X`
    * `chore(test): opdater snapshot for procentformat`

**Commit-forslag (skabelon i svar/PR):**

* **Commit-forslag:** `fix(spc): håndter nævner=0 uden fejl i p-chart`

  * Omfatter filer: `/R/modules/spc_plot.R` (guard), `/R/ui/plot_panel.R` (tooltip)
  * Tests: opdateret `tests/testthat/test_rates.R`
  * Bemærk: ingen ændringer i CSV-filer eller encoding.

---

## 10) Afvigelser & begrænsninger

* Kræver opgaven større refaktorering for at blive korrekt? **Stop** og foreslå:

  * (a) **Minimal midlertidig løsning**, og
  * (b) **Kort plan** for trinvis refaktorering (milepæle, risiko, test).
* Kan opgaven ikke løses uden store ændringer? Lever en **klar begrænsningsforklaring**.

---

## 11) Kommunikation til mennesket (brugeren)

* Brug korte, handlingsorienterede beskeder: **“Gør X i fil Y, linje Z …”**
* Marker **alt** jeg skal gøre manuelt med **\[MANUELT TRIN]**.
* Ingen fyld; hold dig til **fakta, diff og tjeklister**.
* Hvis jeg skal gøre noget, **sig det klart**.

---

## 12) Sanity-check før aflevering (LLM-tjekliste)

* [ ] Løst **netop** den beskrevne opgave – og **kun** den.
* [ ] Ingen nye afhængigheder.
* [ ] Eksisterende tests består; nye tests dækker ændringen.
* [ ] Ingen utilsigtede formatteringsændringer.
* [ ] **CSV:** encoding, CRLF, delimiter og BOM uændret.
* [ ] Klar verifikationsguide + **\[MANUELT TRIN]** oplistet.
* [ ] Diff viser **små, målrettede** ændringer.
* [ ] Commit-forslag inkluderet, **uden omtale af Claude/AI**.

---

## 13) Eksempel på opgave-skabelon (som svar/PR-tekst)

**Jeg løser:** “Fejl i procentformat på p-chart når nævner=0.”
**Antagelser:** Bevarer eksisterende API; viser “—” ved nævner=0.
**Ændringer (filer/sektioner):**

1. `/R/modules/spc_plot.R` – Guard mod nævner=0 i `compute_rate()`.
2. `/R/ui/plot_panel.R` – Tilføj kort tooltip ved “—”.

**Diff:** (indsæt minimal diff her)
**Tests:** Tilføjet test for nævner=0 i `tests/testthat/test_rates.R`.
**Verifikation:**

1. Åbn “SPC” → vælg dataset “Eksempel A”.
2. Sæt måned M hvor nævner=0 → graf viser “—”, ingen fejl i console.
   **CHANGELOG:** `fix(spc): håndter nævner=0 uden fejl i p-chart.`
   **Commit-forslag:** `fix(spc): håndter nævner=0 uden fejl i p-chart`
   **\[MANUELT TRIN]:** Ingen.

---

### Husk

* Fokuser **kun** på den opgave, du er på.
* **Ingen sweeping changes.**
* **Ingen urelaterede edits.**
* **Gør koden præcis, modulær, testbar.**
* **Ødelæg ikke eksisterende funktionalitet.**
* Foreslå **hyppige commits** – men **ingen auto-commits** – og **aldrig** nævn Claude/AI i beskeder.
* **Rør aldrig CSV-encoding (Windows-kontekst), CRLF, delimiter eller BOM.**
