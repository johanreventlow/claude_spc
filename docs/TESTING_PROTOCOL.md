# SPC App Testing Protocol & Development Approach

## 🎯 **Testing Philosophy**

Denne dokument beskriver den tilgang vi anvender til at sikre at SPC app'ens funktionalitet **faktisk virker** for brugeren, ikke kun eksisterer i backend koden.

---

## 📋 **Test-Driven User Experience (TDUE) Approach**

### **Roller og Ansvar**

**🧑‍💻 Bruger/Tester (Johan):**
- Interagerer med UI'en som en reel bruger
- Uploader filer, klikker knapper, udfylder felter
- Observerer UI response og debug output
- Rapporterer konkrete problemer med præcis feedback

**🔧 Udvikler/Problemløser (Claude):**
- Analyserer bruger feedback og debug output
- Identificerer UI-server disconnects og reactive chain problemer
- Implementerer fixes med focus på brugerflow
- Sikrer backend funktionalitet bliver tilgængelig gennem UI

### **Workflow**

1. **Test Specific UI Function**
   - Bruger tester én specifik funktion ad gangen
   - F.eks. "Upload fil og observer autodetection"

2. **Observe & Document**
   - Noter hvad der sker vs. hvad der burde ske
   - Kopier relevant debug output
   - Beskriv præcist hvor UI'en fejler

3. **Report with Context**
   - Format: "Jeg klikkede X, forventede Y, men fik Z"
   - Inkluder relevant debug statements
   - Specificer browser/session context hvis relevant

4. **Analyze & Fix**
   - Claude analyserer feedback og debug data
   - Identificerer root cause (UI binding, reactive chain, etc.)
   - Implementerer targeted fixes

5. **Re-test & Verify**
   - Bruger tester samme funktion igen
   - Verificer at fix løser problemet
   - Fortsæt til næste UI funktion

---

## 🛠 **Development Standards**

### **Code Quality Requirements**

**✅ Best Practice Kodning:**
- Single Responsibility Principle for alle funktioner
- Explicit error handling med `tryCatch()` og `safe_operation()`
- Defensive programming med input validation
- Immutable data flow patterns
- Consistent naming conventions (Danish comments, English code)

**✅ Test-First Development:**
- Skriv tests FØR implementering
- Alle tests skal bestå før commit
- Test coverage for critical paths skal være 100%
- Integration tests for UI-server connections
- Regression tests for tidligere fundne fejl

**✅ Debug-First Approach:**
- Detaljerede debug statements overalt:
  ```r
  cat("DEBUG: [COMPONENT] ===========================================\n")
  cat("DEBUG: [COMPONENT] Specific action description\n")
  cat("DEBUG: [COMPONENT] Variable name:", variable_value, "\n")
  cat("DEBUG: [COMPONENT] ✅ Success message\n")
  ```
- Strukturerede debug kategorier:
  - `[UI_INTERACTION]` - User interface events
  - `[REACTIVE_CHAIN]` - Reactive execution flow
  - `[DATA_FLOW]` - Data processing steps
  - `[AUTO_DETECT]` - Autodetection functionality
  - `[QIC_PROCESSING]` - SPC calculations
  - `[PLOT_GENERATION]` - ggplot building

**✅ Hyppige Commits:**
- Commit efter hver successful fix
- Descriptive commit messages på dansk:
  ```
  fix(ui): retter file upload til autodetection connection

  - Fikser observeEvent binding for fil upload
  - Tilføjer debug statements til reactive chain
  - Tester verificeret gennem bruger feedback

  🤖 Generated with [Claude Code](https://claude.ai/code)
  ```
- Atomic commits - én logisk ændring per commit
- Tests skal bestå før commit

---

## 🔍 **Testing Areas**

### **Phase 1: Core Data Flow**
1. **File Upload & Recognition**
   - Upload CSV/Excel fil
   - Verificer data parsing og import
   - Observer debug output for file processing

2. **Autodetection Activation**
   - Triggering af autodetection efter upload
   - UI sync - dropdown opdateringer
   - Column matching feedback til bruger

3. **Manual Column Selection**
   - Dropdown functionality
   - Column validation feedback
   - Error messages for invalid selections

### **Phase 2: SPC Calculations**
1. **Chart Type Selection**
   - Dropdown med chart type options
   - Verification af chart type propagation til backend
   - Conditional UI elements (N-column for P/U charts)

2. **QIC Integration**
   - Actual qic() execution med user data
   - SPC calculation results
   - Error handling for invalid data configurations

3. **Plot Generation & Display**
   - ggplot rendering til browser
   - Plot responsiveness og interactivity
   - Download/export functionality

### **Phase 3: Advanced Features**
1. **Target Lines & Centerlines**
   - Input fields for target/centerline values
   - Visual representation på plots
   - Validation af numeriske inputs

2. **Phase Management**
   - Skift/Frys column functionality
   - Phase separation visualization
   - Baseline freeze features

3. **Comments & Annotations**
   - Comment column integration
   - ggrepel labels på plots
   - Text truncation og formatting

---

## 📊 **Success Criteria**

**For hver test iteration:**
- ✅ UI responds som forventet
- ✅ Debug output viser korrekt execution flow
- ✅ Ingen JavaScript errors i browser console
- ✅ Backend functionalitet aktiveres gennem UI interaction
- ✅ User feedback er meningsfuld og helpful

**For overall app:**
- ✅ Complete user journey fra upload til plot generation
- ✅ All autodetection scenarios working
- ✅ All chart types genererer korrekte plots
- ✅ Robust error handling med user-friendly messages
- ✅ Consistent performance under normal usage

---

## 🚀 **Implementation Principles**

### **Reactive Architecture**
- Event-driven patterns over timing-based solutions
- Explicit dependency management med `req()` guards
- Observer priorities for correct execution order
- Race condition prevention through proper sequencing

### **State Management**
- Centralized app state med unified schema
- Immutable state updates
- Scope-safe variable access med `exists()` guards
- Migration-safe patterns during architecture changes

### **Error Resilience**
- Graceful degradation when components fail
- User-friendly error messages
- Fallback behaviors for edge cases
- Comprehensive logging for debugging

---

## 📝 **Documentation Requirements**

**For hver fix:**
- Document what was broken
- Explain the root cause
- Describe the solution approach
- Include before/after behavior description

**For major changes:**
- Update this testing protocol if needed
- Add new test scenarios to coverage
- Document any new debug categories
- Update user workflow documentation

---

*Dette dokument sikrer at vi bygger en SPC app der ikke kun har god backend kode, men som faktisk fungerer perfekt for brugeren.*