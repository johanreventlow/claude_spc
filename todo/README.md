# TODO - Active Development Tasks

**Status**: Post-Remediation Phase
**Sidste opdatering**: 2025-10-11

Dette er active TODO-mappen for SPC App efter gennemførelse af REMEDIATION_MASTER_PLAN (alle Fase 1-4 opgaver completed).

---

## 📁 Active Documents

### 1. **MAJOR_VISUALIZATION_REFACTOR.md**
**Status**: PLANNING (Afventer ≥90% test coverage)
**Effort**: 15-20 timer
**Beskrivelse**: Comprehensive plan for refactoring visualizationModuleServer() (1237 linjer) i testbare, modulære komponenter.

**Key Phases**:
- Fase 1: Test infrastructure (prerequisites)
- Fase 2-4: Extract helpers (data/computation/UI)
- Fase 5: Event registration split
- Fase 6: Integration & polish

**Dependencies**:
- Test coverage ≥90% (current ~60-70%)
- Performance baseline documented
- M4-M6 tidyverse migration (optional)

**Reference**: REMEDIATION_MASTER_PLAN opgaver M12-M15

---

### 2. **test-coverage.md**
**Status**: ACTIVE - Continuous improvement
**Beskrivelse**: Test coverage analysis med identificerede gaps og action items.

**Key Focus Areas**:
- Visualization module integration tests (CRITICAL for M12-M15)
- State accessor unit tests
- Startup optimization test harnesses
- Flaky test stabilization

**Note**: Denne fil opdateres kontinuerligt som tests tilføjes.

---

### 3. **spc_plot_modernization_plan.md**
**Status**: BLOCKED - Afventer ≥90% test coverage
**Effort**: 5-6 timer
**Beskrivelse**: Tidyverse migration plan for plot generation code (M4-M6).

**Key Tasks**:
- M4: Tidyverse kommentar-mapping (1.5h)
- M5: Tidyverse QIC args cleanup (2h)
- M6: Replace base-R i plot enhancements (2h)

**Why Blocked**: High-risk refactor kræver solid test foundation først.

**Reference**: REMEDIATION_MASTER_PLAN opgave M4-M6

---

## 📦 Archived Documents

Completed remediation documents er flyttet til `/archived_completed/`:

### Fra REMEDIATION_MASTER_PLAN (Completed):
- ✅ `architectural_recommendations_2024-05-20.md` → H11 (UI sync throttle)
- ✅ `configuration_remediation.md` → H1-H5 (config cleanup)
- ✅ `error-handling-followup.md` → H7-H9 (error handling)
- ✅ `legacy_remediation_plan.md` → H10, diverse
- ✅ `loggin.md` → Logging guidelines (dokumenteret i praksis)
- ✅ `performance_review.md` → K7, H13-H16 (cache optimization)
- ✅ `security_auditor.md` → K2, K5, K6 (security fixes)
- ✅ `shiny.md` → K1, K4, M1-M2 (runtime bugs)
- ✅ `technical-debt-remediation.md` → M7-M8, H12 (cleanup)
- ✅ `visualization_event_refactor_plan.md` → Superseded by MAJOR_VISUALIZATION_REFACTOR.md

---

## 🎯 Recommended Next Steps

### Option A: Test Coverage First (RECOMMENDED)
1. Work through `test-coverage.md` action items
2. Achieve ≥90% coverage on critical paths
3. Then proceed to Major Visualization Refactor
4. Finally tackle Tidyverse migration

**Rationale**: Solid test foundation reduces risk for upcoming refactors

### Option B: Tidyverse Migration First
1. Complete M4-M6 from `spc_plot_modernization_plan.md`
2. Then Major Visualization Refactor
3. Parallelt work on test coverage

**Rationale**: Reduces duplicate work (refactor once with modern code)

### Option C: Parallel Tracks
1. **Track 1**: Test coverage improvements (ongoing)
2. **Track 2**: Tidyverse migration M4-M6 (short sprint)
3. **Track 3**: Major Visualization Refactor (when prerequisites met)

**Rationale**: Maximizes throughput, requires coordination

---

## 📊 Current Status Summary

### ✅ Completed (via REMEDIATION_MASTER_PLAN)
- 39 af 39 planlagte opgaver (K1-K7, H1-H19, M1-M3, M7-M11)
- Zero known critical bugs
- Test coverage forbedret (kritiske stier)
- 2 ADR'er dokumenteret (ADR-001, ADR-014)

### 🔄 In Progress
- Test coverage improvements (kontinuerlig)

### ⏸️ Blocked
- Major Visualization Refactor (afventer test coverage ≥90%)
- Tidyverse migration M4-M6 (afventer test coverage ≥90%)

### 📅 Backlog
- M12-M15: Major Visualization Refactor (15-20t)
- M4-M6: Tidyverse migration (5-6t)
- Further test coverage improvements (ongoing)

---

## 🔗 Related Documentation

- **Master Plan**: `/docs/REMEDIATION_MASTER_PLAN.md` - Completed remediation roadmap
- **Development Guide**: `/CLAUDE.md` - Development principles og guidelines
- **Configuration**: `/docs/CONFIGURATION.md` - Config management patterns
- **ADRs**: `/docs/adr/` - Architectural Decision Records

---

**Note**: Hold dette README opdateret når opgaver flyttes mellem active/archived eller status ændres.
