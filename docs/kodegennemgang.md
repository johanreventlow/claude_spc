# Kodegennemgang – claude_spc

## Samlet vurdering
Shiny-appen har en omfattende struktur med adskilte UI-, server- og hjælpefunktioner samt et stort testsæt. Arkitekturen er imidlertid delvist overgangen fra en ældre tilgang til en ny “centralized state”, hvilket giver dobbelt håndtering og betydelig kodegentagelse. Der er flere kritiske best practice-afvigelser fra golem/pakke-tilgangen, og tekstfiler lider af mojibake på grund af en inkompatibel tegnsætning. Dokumentationen er delvis gennemarbejdet (udvidede planer og README), men kodedokumentation og generelle standarder er uens.

## Kvalitet af kildekode

### Styrker
- Klar modulopdeling i `R/` med fokus på særskilte ansvarsområder (UI, server, utils).
- Omfangsrigt testapparat i `tests/testthat/` dækker mange arbejdsgange og edge cases.
- Logging- og debug-infrastruktur (`R/utils_logging.R`, `R/utils_advanced_debug.R`) giver mulighed for sporbarhed.

### Problemer
- **Encoding-fejl (kritisk):** Flere filer indeholder fejllede danske tegn og emojis, fx `app.R:5`, `R/run_app.R:66`, `README.md:5`. Ved kørsel på UTF-8-server vil labels, dokumentation og UI-tekster fremstå korrupte.
- **Runtime `source()`-kald (kritisk):** Centrale entrypoints (`R/app_server.R:26`, `R/run_app.R:14`, `R/app_ui.R:5`, `global.R:28`) reader filer ved sessionsstart. Det bryder golem/pakke-arkitekturen, skaber overhead pr. session og vanskeliggør enhedstestning.
- **Dual state kopi- og indsæt-guard (betydeligt):** Overgangslaget mellem `values` og `app_state` gentages i dusinvis af `if (exists("use_centralized_state") && ...)`-blokke (`R/utils_server_management.R:16`, `R/utils_session_helpers.R:17`). Dette er svært at vedligeholde og øger risiko for inkonsekvens.
- **Direkte `cat()` i produktionskode (betydeligt):** Fx `global.R:552` skriver udenom logningsfacaden og støjer i konsollen, selv når DEBUG er slået fra.
- **Fejlhåndtering af testtilstand:** Testdata autoload i `R/app_server.R` antager, at filveje er tilgængelige, og logik for centraliseret state kan efterlade flag i udefineret tilstand ved fejl.
- **Manglende `req()`-vagter og isolering flere steder:** Nogle observers i `R/utils_server_management.R` og `R/utils_session_helpers.R` læser reaktive værdier uden `req()`/`isolate()`, hvilket kan trigge unødvendigt mange reaktive re-runs.
- **Uklar afhængighedsstyring:** Flere filer kalder `library()` uden namespace-kvalificering inde i modul-filer (fx `R/mod_spc_chart.R:6`), hvilket reducerer forudsigelighed og gør pakkeopbygning svær.

## Dokumentation

### Styrker
- Omfattende roadmap og arkitekturplaner (fx `TECHNICAL_IMPROVEMENTS_PLAN.md`, `STATE_MANAGEMENT_PLAN.md`).
- README giver overblik over funktionalitet og opsætning.

### Problemer
- **Manglende roxygen på eksporterede funktioner:** Centrale helpers (`R/app_ui.R:12 create_ui_header`, `R/utils_server_management.R:10 setup_session_management`, m.fl.) mangler roxygen, hvilket hindrer automatisk dokumentation og korrekt NAMESPACE.
- **Testfiler uden beskrivelser:** Flere testfiler har overlappende kontekst uden klar forklaring, fx `test-name-only-detection*.R`.
- **Encoding-problem påvirker dokumenter:** Markdown- og Roxygen-kommentarer indeholder mojibake, hvilket reducerer læsbarheden på tværs af platforme.
- **Manglende udviklerguide:** Der er ikke en tydelig instruktion i, hvordan udviklere bedst kører appen via golem eller hvordan state-migrationen er tænkt afsluttet.

## Overholdelse af Shiny best practices

| Emne                          | Vurdering | Noter |
|------------------------------|-----------|-------|
| App-struktur (golem)         | ⚠️        | Klassisk golem-konvention er ikke fulgt pga. runtime `source()` i stedet for pakke-loading. |
| Afkobling af UI og server    | ✅        | `app_ui()` og `app_server()` er separate og modulopdelt. |
| Modules og genbrug           | ✅        | Modulstruktur med `mod_spc_chart` m.fl. giver separation og genbrug. |
| Reaktiv state-håndtering     | ⚠️        | Dual state migrering giver kompleksitet og stor risiko for race conditions. |
| Fejl/logging                 | ⚠️        | Logningssystem eksisterer, men omgås flere steder med direkte `cat()`/`showNotification` uden fallback. |
| Ressourcehåndtering          | ✅        | Der anvendes `session$onSessionEnded`, `setup_session_cleanup` og waiter-handlers. |
| Testdækning                  | ✅        | Stor testmængde tyder på omfattende dækning. |

## Overflødige eller duplikerede dele

- **Duplikerede tests:** Seks variationer af `test-name-only-detection*.R` tester stort set samme funktionalitet og bør konsolideres til én autoritativ suite.
- **State-synkroniseringskode:** Gentagne blokke for at holde `values` og `app_state` synkroniseret bør abstrakteres eller fjernes efter migrering.
- **Redundante console logs:** `cat()`-linjer, der duplikerer logoutput, er overflødige når `log_*()` allerede eksisterer.

## Forslag til forbedringer

1. **Global UTF-8 standardisering**
   - Re-encod alle `.R`- og `.md`-filer til UTF-8 og verificer specialtegn.
   - Sikr, at `save/app data` håndterer UTF-8 for både læsning og skrivning.

2. **Refaktorer init-lag til pakke-style**
   - Flyt `source()`-kæder til `R/`-pakken og indlæs via `load_all()`/`library(claudeSpc)`.
   - Opdel `global.R` i mindre filer, så konfiguration kan indlæses via R-pakken i stedet for runtime-sourcing.

3. **Afslut central state migrering**
   - Vælg en enkelt datakilde (`app_state` eller `values`), skriv adapter-funktioner, og fjern dobbelt-koden.
   - Introducer værktøjer som `with_app_state()` til at kapsle tilgang til state.

4. **Stram logging og debugging**
   - Erstat alle `cat()`/`print()`-debug med `log_debug()` eller `log_info()` og brug log-levels konsekvent.
   - Gennemgå `initialize_advanced_debug()` for at sikre, at den ikke kører i produktion uden flag.

5. **Skrive roxygen & udviklerdocs**
   - Tilføj roxygen til alle eksportede moduler og helpers.
  - Udarbejd en kort “CONTRIBUTING.md” eller sektion i README, der beskriver build/run-flow med golem.

6. **Konsolider tests**
   - Fjern redundant testfiler og samle navnekontrol-tests i én fil med tydelig kontekst.
   - Brug fælles helper-funktioner for at undgå gentagelse af test-setup.

7. **Sikre reaktive vagter**
   - Tilføj `req()` og `isolate()` hvor input kan være `NULL` for at reducere unødig reaktiv belastning.
   - Overvej `eventReactive()` for workflows som autoload/restore.

---

*Udarbejdet af: Codex (GPT-5)  
Dato: 17-09-2025*
