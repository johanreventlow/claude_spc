# ADR-015: Migrering til BFHchart for SPC-visualisering

## Status
Proposed

## Kontekst
SPCify anvender i dag `qicharts2::qic()` til at generere SPC-diagrammer (run, I, P m.fl.) i Shiny-applikationen. Implementeringen er udbygget med omfattende hjælpefunktioner (`R/fct_spc_plot_generation.R`, `R/utils_qic_preparation.R`) til datarensning, state-håndtering, caching og strukturering af Anhøj-signaler. Denne løsning er moden, men giver begrænsninger:

- **Udvidelser**: Flere ønskede forbedringer (eksempelvis forbedret fasevisning og annotationsstyring) kræver dybe hooks i qicharts2.
- **Vedligehold**: Mange interne abstractions er skræddersyet til qicharts2’s output og parametre, hvilket gør videreudvikling og refaktorering tung.
- **Observability**: qicharts2 fungerer som et sort bokssystem; det er vanskeligt at instrumentere for detaljerede metrics og logging uden at wrappe store dele af biblioteket.

BFHchart-biblioteket tilbyder moderne SPC-rendering med tydeligere API, bedre kontrol over layout og annotationer, samt mulighed for tættere integrering med vores event-baserede arkitektur. Migreringen kræver dog en helhedsplan for testdækning, caching og Shiny-integration.

## Beslutning
Vi migrerer SPC-visualiseringen fra qicharts2 til BFHchart gennem en kontrolleret flertrinsplan:

1. **Feasibility & krav**: Analysér BFHchart og dokumentér feature-paritet og gaps (Issue 1 i `docs/issues/bfhchart-transition-issues.md`).
2. **Dependency bootstrap**: Planlæg introduktion af BFHchart i miljøet uden at berøre `NAMESPACE`, herunder lazy loading og golem-konfiguration (Issue 2).
3. **Ny renderingsservice**: Udarbejd et internt API (`compute_spc_results_bfh()`) der producerer de samme strukturerede dataobjekter, suppleret af `safe_operation()` og `app_state`-integration (Issue 3).
4. **Shiny-integration**: Opdater modul- og event-bus flows til at bruge BFHchart-resultater med korrekte prioriteter og guards (Issue 4).
5. **Caching & performance**: Revider caching, logging og benchmarks til at understøtte BFHchart (Issue 5).
6. **Oprydning & dokumentation**: Fjern qicharts2-rester, opdater dokumentation og kør fuld regressionstest-suite (Issue 6).

Beslutningen fastholder TDD-kravet: hver fase introducerer først tests (forventet BFHchart-adfærd), hvorefter implementeringen følger.

## Konsekvenser

### Fordele
- **Kontrol og fleksibilitet**: BFHchart giver bedre mulighed for at styre layout, annotationer og dansk tilpasning uden workarounds.
- **Simplere arkitektur**: Vi kan reducere qicharts2-specifikke hjælpefunktioner og i stedet have et mere generisk renderingslag.
- **Observability**: Eget service-lag oven på BFHchart gør logging og målinger enklere (bedre integration med `log_*` API’et).
- **Fremtidssikring**: Åbner for hurtigere videreudvikling af SPC-funktionalitet (fx nye charttyper eller custom regelsæt).

### Ulemper
- **Migreringsomkostning**: Kræver flere iterationer med tests, refaktorering og dokumentationsændringer.
- **Læringskurve**: Teamet skal blive fortrolig med BFHchart’s API og modellering.
- **Kompatibilitet**: Risiko for subtle forskelle i beregninger eller visning sammenlignet med qicharts2, især omkring Anhøj-regler og fasehåndtering.

### Mitigations
- **TDD & regressionstests**: Udvider eksisterende test-suites (inkl. `tests/testthat/test-spc-plot-generation-comprehensive.R` og `shinytest2`) til at fange regressions.
- **Feature-paritet dokuments**: Issue 1 kræver eksplicit gap-analyse, så potentielle afvigelser identificeres tidligt.
- **Fallback-plan**: qicharts2-koden forbliver urørt indtil BFHchart-servicen er fuldt valideret; vi kan rulle tilbage ved at bevare feature flag i `golem::get_golem_options()`.

## Relaterede Beslutninger
- **ADR-001**: UI sync throttling – relevant for event-prioritering når nyt plot trækkes ind.
- **ADR-014**: Depreker optimeret event-pipeline – integreres for at sikre at BFHchart følger den nye event-struktur.
- **docs/issues/bfhchart-transition-issues.md**: Indeholder den detaljerede issueplan som denne ADR refererer til.

## Implementation Notes
- Overvej feature flag `FEATURE_FLAG_USE_BFHCHART` til gradvis aktivering i `app_state`.
- Udnyt eksisterende `safe_operation()` mønster til at wrappe BFHchart-kald og sikre struktureret logging (`component = "[SPC_RENDERER]"`).
- Opdater caching moduler (`R/utils_qic_caching.R`) til et bibliotek-agnostisk navn før selve BFHchart integreres, for at minimere diff-støj.
- Justér eksport- og rapporteringsfunktioner så de kan generere BFHchart-baserede plots (export pipeline beskrives i Issue 6).

## Referencer
- BFHchart dokumentation (intern/ekstern link tilføjes i Issue 1).
- `docs/issues/bfhchart-transition-issues.md` – GitHub issuepakke for migreringen.
- `todo/spc_plot_modernization_plan.md` – eksisterende noter om plotmodernisering.

## Dato
2025-10-14
