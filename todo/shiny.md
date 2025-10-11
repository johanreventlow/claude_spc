 OVERSIGT

  - To kritiske regressions i det nye autodetect-flow; yderligere forbedringspunkter omkring konfiguration, global
    tilstand og sprogkonsistens.

  KRITISKE PROBLEMER

  - app_state$columns$auto_detect$last_run gemmes nu som en liste (session, timestamp m.m.), men guard-logikken omkring
    difftime() forventer fortsat en POSIXct (se R/fct_autodetect_unified.R:73 og R/fct_autodetect_unified.R:173). Efter
    første succesfulde kørsel vil næste trigger give Fejl i as.POSIXct.default(time2) og stoppe hele autodetect-kæden,
    hvilket jeg kunne reproducere med et minimalt R-kald.
  - Når autodetect ikke finder en kolonne (fx manglende nævner i nyt datasæt), bliver den gamle mapping stående,
    fordi update_all_column_mappings() kun overskriver felter, der ikke er NULL (R/fct_autodetect_unified.R:395-437).
    Resultatet er, at ratio-charts bruger en kolonne, som ikke findes i de aktuelle data, filtrerer alt væk og ender i
    en “For få gyldige datapunkter”-fejl. Det rammer både funktionalitet og brugerforståelse.

  ANBEFALINGER

  - Funktionalitet
      - sanitize_spc_config() logger og returnerer uændret konfiguration når config$y_col == character(0) (R/
        utils_spc_data_processing.R:17-32), så sanitiseringsløftet holdes ikke. Overvej at nulstille feltet i stedet for
        at stop() inden i safe_operation.
  - Reaktiv arkitektur
      - QIC-kald tælles i .GlobalEnv (R/fct_spc_plot_generation.R:601-609), hvilket introducerer tværsessions-tilstand i
        en funktion der ellers opererer på session-isolering. Flyt tælleren til app_state eller en pr.-session cache.
  - Performance
      - Ingen yderligere observationer.
  - Struktur
      - Ingen yderligere observationer.
  - Fejlhåndtering
      - Når vi retter last_run, bør vi samtidig sikre at guard-blokken håndterer manglende timestamp elegant (fx via
        is.list()/$timestamp) for tydeligere fallback-logning.
  - Stil og konventioner
      - Fallback-tekst “SPC Chart” i process_chart_title() er engelsksproget (R/utils_spc_data_processing.R:64-70); bør
        lokaliseres til dansk for konsistens.

  KONKRETE FORSLAG

  - I autodetect_engine() opdater last_run til enten at være en ren Sys.time() (som tidligere) eller tilpas guard-
    logikken til at bruge last_run$timestamp og håndtere manglende felt uden at kalde difftime() på en liste.
  - Udvid update_all_column_mappings() med eksplicit nulstilling: hvis results$n_col er NULL, så sæt
    app_state$columns$mappings$n_column <- NULL osv., så gamle mappings ikke hænger ved.
  - Flyt QIC-call-counter til app_state$cache$qic eller en anden pr.-session struktur, og nulstil den i
    setup_session_cleanup() for at undgå global deling.
  - Justér sanitize_spc_config() til at returnere en faktisk saniteret liste (fx config$y_col <- NULL) i stedet for at
    rejse stop() inde i safe_operation.
  - Skift fallback-tekst til fx "SPC-diagram - ...", så standardbeskeder forbliver på dansk.

  POSITIVE ASPEKTER

  - Event-busset er flot modulariseret med register_*_events() og konsekvent brug af OBSERVER_PRIORITIES, hvilket gør
    prioriterings‑ og cleanup-strategien gennemskuelig (R/utils_server_event_listeners.R).
  - autodetect_engine() har stærke guard-patterns og on.exit‑cleanup, så vi undgår hængende in_progress-flag ved fejl.
  - Logging følger centralt API (log_debug_kv, log_debug_block) med kontekst-tags, hvilket styrker observabiliteten
    under kliniske audits.