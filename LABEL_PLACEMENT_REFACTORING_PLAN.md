# 🎯 LABEL PLACEMENT REFACTORING - Implementation Tracker

**Start dato**: 2025-10-05
**Status**: 🚧 In Progress
**Fase**: FASE 1 - Fix strukturelle bugs

---

## 📊 INTEGRERET ANALYSE (3 LLM reviews)

### 🔴 Kritiske strukturelle bugs
1. **`propose_single_label()` respekterer ikke bounds** (LLM3)
   - Bruger `clamp01()` i flip-scenarios → labels presses uden for pad_top/pad_bot
   - **Impact**: Labels kan placeres uden for panel padding

2. **`estimate_label_height_npc()` antager fast panelstørrelse** (Alle 3)
   - Hardcoded pt→NPC factor (0.0033) antager ~150mm panel
   - **Impact**: Upræcis på små paneler (facetter) og store plots
   - **Root cause**: Estimering i stedet for måling

### ⚠️ Konfigurationsproblemer
3. **Magic numbers spredt gennem kode** (Min + LLM2)
   - Collision thresholds: 0.001, 0.5, [0.5, 0.3, 0.15]
   - Height calculation: 0.0033, 0.015, 1.1, 0.02, 0.3

4. **Inkonsistente defaults** (Min + LLM2)
   - `place_two_labels_npc()`: Absolutte defaults (0.015, 0.03)
   - `add_right_labels_marquee()`: Relative defaults (8%, 30%)
   - **Documentation mismatch**: Kommentar siger 15%, kode bruger 8%

5. **Ubrugte parametre** (LLM2)
   - `base_size` i `estimate_label_height_npc()` ikke anvendt
   - `x_npc` i `add_right_labels_marquee()` dokumenteret men ubrugt

### ✅ Designproblemer
6. **Tema-integration mangler** (LLM2)
   - Hardcoded farver (#009CE8, #565656) i flere steder
   - Hardcoded font ("Roboto Medium")
   - Burde komme fra `brand.yml` / `get_package_theme()`

7. **Logging ikke efter CLAUDE.md** (Min)
   - Bruger `message()` i stedet for `log_debug()`/`log_info()`

8. **Type validation mangler** (Min)

9. **Performance** (LLM2)
   - `ggplot_build(p)` kaldes flere gange uden caching

---

# 🚀 IMPLEMENTERINGSPLAN (3 Faser)

## **FASE 1: FIX STRUKTURELLE BUGS** (Kritisk)

### ✅ Task 1.1: Fix `propose_single_label()` bounds problem
**Status**: ✅ COMPLETED
**Commit**: `fix(label-placement): respekter bounds ved label flip`

**Ændringer**:
- [x] Tilføj `clamp_to_bounds()` helper function
- [x] Opdater `propose_single_label()` til at bruge bounds-aware clamping
- [x] Opdater begge flip-branches (under→over og over→under)

**Filer**:
- `utils_standalone_label_placement.R`: linjer 80-91 (ny helper), 330-353 (opdateret funktion)

**Tests**:
- [x] Opret `tests/testthat/test-label-placement-bounds.R`
- [x] Test: Store labels respekterer bounds ved flip ✅
- [x] Test: clamp_to_bounds() fungerer korrekt ✅
- [x] Manual verification passed ✅

---

### ✅ Task 1.2: Implementer præcis `estimate_label_height_npc()` med grob-måling
**Status**: ⬜ Not started
**Commit**: `feat(label-placement): implementer præcis højdemåling via grob`

**Ændringer**:
- [ ] Erstat eksisterende `estimate_label_height_npc()` med grob-baseret implementation
- [ ] Tilføj `style` parameter for faktisk marquee rendering
- [ ] Tilføj `panel_height_inches` parameter (optional)
- [ ] Fjern alle magic numbers (pt_to_npc_factor, line_spacing, etc.)
- [ ] Behold fallback for robusthed

**Filer**:
- `utils_standalone_label_placement.R`: Erstat linjer 233-295

**Tests**:
- [ ] Opret `tests/testthat/test-label-height-estimation.R`
- [ ] Test: Måling fra faktisk grob er præcis
- [ ] Test: Større base_size giver proportionalt større højde
- [ ] Test: Fallback fungerer ved fejl
- [ ] Test: Forskellige markup patterns

---

### ✅ Task 1.3: Opdater `add_right_labels_marquee()` til ny implementation
**Status**: ⬜ Not started
**Commit**: `refactor(label-placement): brug grob-baseret højdemåling i add_right_labels_marquee`

**Ændringer**:
- [ ] Opret `right_aligned_style` tidligt i funktionen (DRY)
- [ ] Opdater auto-beregning til at bruge grob-baseret måling
- [ ] Send `style` parameter til `estimate_label_height_npc()`
- [ ] Genbruge style-objektet til marquee rendering
- [ ] Opdater verbose logging

**Filer**:
- `bfh_layout_reference_dev.R`: linjer 194-206, 295-300

---

### ✅ Task 1.4: Integration tests for Fase 1
**Status**: ⬜ Not started
**Commit**: `test(label-placement): tilføj precision tests for forskellige panel-størrelser`

**Tests**:
- [ ] Opret `tests/testthat/test-label-placement-precision.R`
- [ ] Test: Labels præcise på små facetterede paneler
- [ ] Test: Labels præcise på store paneler (base_size 24+)
- [ ] Test: Labels respekterer bounds ved extreme cases
- [ ] Test: Regression test - eksisterende plots unchanged

---

### ✅ Task 1.5: Kør alle tests og verificer
**Status**: ⬜ Not started

**Checklist**:
- [ ] Kør `R -e "source('global.R'); testthat::test_dir('tests/testthat')"`
- [ ] Alle tests grønne ✅
- [ ] Manuel test: Kør `bfh_layout_reference_dev.R`
- [ ] Verificer: Labels placeres korrekt
- [ ] Verificer: Ingen labels uden for bounds
- [ ] Verificer: Auto-beregnet højde realistisk

---

## **FASE 2: CENTRALISÉR KONFIGURATION** (Medium prioritet)

### ✅ Task 2.1: Opret centraliseret konfigurationsfil
**Status**: ⬜ Not started
**Commit**: `feat(config): centralisér label placement konstanter`

**Ændringer**:
- [ ] Opret `R/config_label_placement.R`
- [ ] Definer `LABEL_PLACEMENT_CONFIG` list
- [ ] Implementer `get_label_placement_param()` helper
- [ ] Dokumenter alle konstanter med rationale

**Filer**:
- `R/config_label_placement.R` (ny)

**Konstanter at inkludere**:
- `relative_gap_line = 0.08` (8% af label_height_npc)
- `relative_gap_labels = 0.30` (30% af label_height_npc)
- `pad_top = 0.01`, `pad_bot = 0.01`
- `coincident_threshold_factor = 0.1`
- `tight_lines_threshold_factor = 0.5`
- `gap_reduction_factors = c(0.5, 0.3, 0.15)`
- `shelf_center_threshold = 0.5`
- `marquee_size_factor = 6`
- `marquee_lineheight = 0.9`
- `height_safety_margin = 1.05`
- `height_fallback_npc = 0.13`

---

### ✅ Task 2.2: Opdater `place_two_labels_npc()` til at bruge config
**Status**: ⬜ Not started
**Commit**: `refactor(label-placement): brug centraliseret config i place_two_labels_npc`

**Ændringer**:
- [ ] Ændre defaults til `NULL` for gap_line, gap_labels, pad_top, pad_bot
- [ ] Beregn defaults fra config i function body
- [ ] Opdater coincident_threshold til at bruge config factor
- [ ] Opdater tight_lines_threshold til at bruge config factor
- [ ] Opdater gap_reduction_factors til at bruge config
- [ ] Opdater shelf logic til at bruge config

**Filer**:
- `utils_standalone_label_placement.R`: Opdater `place_two_labels_npc()` function

---

### ✅ Task 2.3: Opdater `add_right_labels_marquee()` til at bruge config
**Status**: ⬜ Not started
**Commit**: `refactor(label-placement): brug config i add_right_labels_marquee`

**Ændringer**:
- [ ] Hent `relative_gap_line` fra config
- [ ] Hent `relative_gap_labels` fra config
- [ ] Hent `marquee_size_factor` fra config
- [ ] Hent `marquee_lineheight` fra config
- [ ] Opdater defaults i params list

**Filer**:
- `bfh_layout_reference_dev.R`: Opdater defaults (linjer 212-213, 303, 316)

---

### ✅ Task 2.4: Fix documentation mismatch
**Status**: ⬜ Not started
**Commit**: `docs(label-placement): ret kommentar fra 15% til 8% gap_line`

**Ændringer**:
- [ ] Ret kommentar linje 494 i `bfh_layout_reference_dev.R`
- [ ] Verificer at alle kommentarer matcher faktisk kode

**Filer**:
- `bfh_layout_reference_dev.R`: linje 494

---

### ✅ Task 2.5: Tests for Fase 2
**Status**: ⬜ Not started
**Commit**: `test(config): verificer config integration`

**Tests**:
- [ ] Test: Config defaults anvendes korrekt
- [ ] Test: Manuelle parameter overrides fungerer
- [ ] Test: get_label_placement_param() fejler gracefully ved invalid keys
- [ ] Kør alle tests igen - verificer ingen regression

---

## **FASE 3: POLISH & BEST PRACTICES** (Lav prioritet)

### ✅ Task 3.1: Fjern ubrugte parametre
**Status**: ⬜ Not started
**Commit**: `refactor(api): fjern ubrugte parametre x_npc og base_size`

**Ændringer**:
- [ ] Fjern `x_npc` fra `add_right_labels_marquee()` signature
- [ ] Fjern `x_npc` fra documentation (@param)
- [ ] Fjern `x_npc` fra example calls
- [ ] ~~Fjern `base_size` fra `estimate_label_height_npc()`~~ (BEHOLD - bruges nu!)

**Filer**:
- `bfh_layout_reference_dev.R`: linjer 126-127, 184, 503

---

### ✅ Task 3.2: Integrer med tema/branding
**Status**: ⬜ Not started
**Commit**: `feat(theme): hent label farver og font fra tema/branding`

**Ændringer**:
- [ ] Opret `R/utils_theme_integration.R`
- [ ] Implementer `get_label_color()` med fallback
- [ ] Implementer `get_label_font()` med fallback
- [ ] Opdater `add_right_labels_marquee()` defaults til at bruge tema
- [ ] Opdater marquee rendering til at bruge tema-font

**Filer**:
- `R/utils_theme_integration.R` (ny)
- `bfh_layout_reference_dev.R`: linjer 182-183, 271-272, 317

---

### ✅ Task 3.3: Implementer proper logging
**Status**: ⬜ Not started
**Commit**: `refactor(logging): brug struktureret logging istedet for message()`

**Ændringer**:
- [ ] Erstat alle `message()` calls med `log_debug()`
- [ ] Erstat `warning()` med `log_warn()`
- [ ] Tilføj `component = "[LABEL_PLACEMENT]"` til alle logs
- [ ] Tilføj struktureret `details` list hvor relevant

**Filer**:
- `bfh_layout_reference_dev.R`: linjer 204, 244
- `utils_standalone_label_placement.R`: linje 292

---

### ✅ Task 3.4: Type validation
**Status**: ⬜ Not started
**Commit**: `feat(validation): tilføj input validation til public functions`

**Ændringer**:
- [ ] Tilføj `stopifnot()` checks i `place_two_labels_npc()`
- [ ] Tilføj `stopifnot()` checks i `estimate_label_height_npc()`
- [ ] Tilføj range warnings for out-of-range NPC values
- [ ] Test: Validation fungerer og giver nyttige fejlbeskeder

**Filer**:
- `utils_standalone_label_placement.R`: Alle public functions

---

### ✅ Task 3.5: Performance - Cache ggplot_build
**Status**: ⬜ Not started
**Commit**: `perf(label-placement): cache ggplot_build for bedre performance`

**Ændringer**:
- [ ] Opdater `npc_mapper_from_plot()` til at acceptere `built` parameter
- [ ] I `add_right_labels_marquee()`: Call `ggplot_build()` én gang
- [ ] Send cached build til `npc_mapper_from_plot()`
- [ ] Genbruge cached build til x_range extraction
- [ ] Benchmark: Verificer performance forbedring

**Filer**:
- `utils_standalone_label_placement.R`: `npc_mapper_from_plot()`
- `bfh_layout_reference_dev.R`: `add_right_labels_marquee()`

---

### ✅ Task 3.6: Final testing og documentation
**Status**: ⬜ Not started
**Commit**: `docs(label-placement): opdater dokumentation og README`

**Ændringer**:
- [ ] Opdater function documentation (roxygen)
- [ ] Opdater examples i documentation
- [ ] Kør `devtools::document()` for at regenerere documentation
- [ ] Kør fuld test suite en sidste gang
- [ ] Manuel smoke test på forskellige plot typer

---

## ✅ SUCCESS CRITERIA

### Fase 1:
- [ ] Labels respekterer altid pad_top/pad_bot, også ved flip
- [ ] Label højde præcis på paneler fra 50mm til 300mm højde
- [ ] Alle tests grønne
- [ ] Ingen regression i eksisterende plots

### Fase 2:
- [ ] Alle magic numbers fjernet fra core logic
- [ ] Defaults konsistente på tværs af API
- [ ] Documentation korrekt
- [ ] Config kan overrides ved behov

### Fase 3:
- [ ] Logging følger CLAUDE.md standarder
- [ ] Type validation på alle public functions
- [ ] Farver/fonts fra tema i stedet for hardcoded
- [ ] ggplot_build kaldes max 1 gang pr. plot
- [ ] Dokumentation komplet og opdateret

---

## 📝 COMMIT LOG

| Task | Commit Hash | Date | Notes |
|------|-------------|------|-------|
| 1.1  | -           | -    | -     |
| 1.2  | -           | -    | -     |
| 1.3  | -           | -    | -     |
| 1.4  | -           | -    | -     |
| 1.5  | -           | -    | -     |
| 2.1  | -           | -    | -     |
| 2.2  | -           | -    | -     |
| 2.3  | -           | -    | -     |
| 2.4  | -           | -    | -     |
| 2.5  | -           | -    | -     |
| 3.1  | -           | -    | -     |
| 3.2  | -           | -    | -     |
| 3.3  | -           | -    | -     |
| 3.4  | -           | -    | -     |
| 3.5  | -           | -    | -     |
| 3.6  | -           | -    | -     |

---

## 🎯 CURRENT STATUS

**Nuværende task**: Task 1.1 - Fix propose_single_label() bounds problem
**Næste milestone**: Fase 1 completion
**Blokkere**: Ingen

---

**Sidst opdateret**: 2025-10-05
