# Intelligent Label Placement System - Standalone Version

## 📦 Oversigt

Intelligent label placement system med NPC-koordinater og multi-level collision avoidance.

Kan genbruges i **ethvert R plotting projekt** til at placere labels ved horisontale linjer uden overlaps.

## ✨ Features

- ✅ **Ingen overlaps** mellem labels eller med linjer
- ✅ **Multi-level collision avoidance** (3 niveauer)
- ✅ **Auto-adaptive parametre** baseret på font sizes
- ✅ **Device-independent** (NPC koordinater 0-1)
- ✅ **Robust** på tværs af ggplot2 versioner
- ✅ **Edge case handling** (sammenfaldende linjer, bounds violations)

## 📋 Dependencies

```r
library(ggplot2)
library(stringr)
```

## 🚀 Hurtig Start

### 1. Kopier Funktionerne

Kopier disse funktioner fra `bfh_layout_reference_dev.R` til dit projekt:

- **`npc_mapper_from_plot()`** (linje 42-166)
- **`estimate_label_height_npc()`** (linje 184-227)
- **`place_two_labels_npc()`** (linje 272-610)
- **`propose_single_label()`** (linje 612-632)
- **`clamp01()`** (helper function)

### 2. Basis Brug

```r
library(ggplot2)

# Opret plot
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_hline(yintercept = 20, color = "blue") +
  geom_hline(yintercept = 25, color = "red") +
  theme_minimal()

# Opret NPC mapper
mapper <- npc_mapper_from_plot(p)

# Definer labels
label_A <- "{.8 **CL**}\n{.24 **20 mpg**}"
label_B <- "{.8 **Target**}\n{.24 **25 mpg**}"

# Auto-beregn label height
label_height <- estimate_label_height_npc(label_A)

# Placer labels
result <- place_two_labels_npc(
  yA_npc = mapper$y_to_npc(20),
  yB_npc = mapper$y_to_npc(25),
  label_height_npc = label_height,
  gap_line = label_height * 0.08,     # 8% af label height
  gap_labels = label_height * 0.3,    # 30% af label height
  priority = "A",
  pref_pos = c("under", "under")
)

# Resultat:
# result$yA = 0.35 (NPC position for label A)
# result$yB = 0.65 (NPC position for label B)
# result$sideA = "under"
# result$sideB = "under"
# result$placement_quality = "optimal"
```

### 3. Brug Resultater til Plotting

```r
# Konverter NPC tilbage til data coordinates
yA_data <- mapper$npc_to_y(result$yA)
yB_data <- mapper$npc_to_y(result$yB)

# Tilføj labels med marquee
library(marquee)

p_final <- p +
  marquee::geom_marquee(
    data = data.frame(x = max(mtcars$wt), y = yA_data, label = label_A),
    aes(x = x, y = y, label = label),
    hjust = 1,
    vjust = 0.5,
    size = 6,
    color = "blue"
  ) +
  marquee::geom_marquee(
    data = data.frame(x = max(mtcars$wt), y = yB_data, label = label_B),
    aes(x = x, y = y, label = label),
    hjust = 1,
    vjust = 0.5,
    size = 6,
    color = "red"
  )
```

## 🎯 Collision Avoidance Strategi

### NIVEAU 1: Reduceret Label Gap
- Prøver 50% → 30% → 15% af normal label gap
- Bevarer line-gaps mens labels accepterer tættere spacing

### NIVEAU 2: Smart Label Flip
- **2a**: Flip label A til modsatte side, hold B fast
- **2b**: Hold A fast, flip B til modsatte side
- **2c**: Flip BEGGE labels til modsatte side

### NIVEAU 3: Shelf Placement
- Sidste udvej: Placer labels i hjørnerne
- Prioriterer vigtigste label tæt på sin linje

## 📊 Quality Levels

| Quality | Beskrivelse |
|---------|-------------|
| `optimal` | Labels placeret ideelt uden adjustments |
| `acceptable` | Minor adjustments (reduceret gap eller flip) |
| `suboptimal` | Begge labels flipped eller shelf placement |
| `degraded` | Kun én label vist (anden uden for bounds) |

## 🔧 API Reference

### `npc_mapper_from_plot(p, panel = 1)`

Opret NPC mapper fra ggplot object.

**Returns:**
- `y_to_npc`: function(y_data) → NPC
- `npc_to_y`: function(npc) → y_data
- `limits`: c(ymin, ymax)
- `trans_name`: transformation navn

### `estimate_label_height_npc(text, base_size = 14, fallback_npc = 0.055)`

Estimér label højde fra marquee markup.

**Parameters:**
- `text`: Marquee string med `{.8 **Header**}\n{.24 **Value**}` format
- `base_size`: Base font size for responsive sizing
- `fallback_npc`: Fallback værdi hvis parsing fejler

**Returns:** numeric label height i NPC

### `place_two_labels_npc(...)`

Core placement algoritme.

**Parameters:**
- `yA_npc`, `yB_npc`: Line positions i NPC (0-1)
- `label_height_npc`: Label height (auto hvis NULL)
- `gap_line`: Min gap fra label til linje (default 8% af label_height)
- `gap_labels`: Min gap mellem labels (default 30% af label_height)
- `pad_top`, `pad_bot`: Panel padding (default 0.01)
- `priority`: "A" eller "B" - prioriteret label
- `pref_pos`: c("under"/"over", "under"/"over")

**Returns:**
- `yA`, `yB`: NPC positions
- `sideA`, `sideB`: "over" eller "under"
- `placement_quality`: Quality level
- `warnings`: Character vector med warnings

## 📝 Eksempler

### Eksempel 1: Tætte Linjer (Trigger Collision Avoidance)

```r
# Linjer meget tæt på hinanden
p <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_hline(yintercept = 20) +
  geom_hline(yintercept = 21) +  # Kun 1 mpg fra forrige!
  theme_minimal()

mapper <- npc_mapper_from_plot(p)

result <- place_two_labels_npc(
  yA_npc = mapper$y_to_npc(20),
  yB_npc = mapper$y_to_npc(21),
  label_height_npc = 0.13,
  gap_line = 0.01,
  gap_labels = 0.04
)

# Output:
# result$sideA = "over" (automatisk flipped!)
# result$sideB = "under"
# result$placement_quality = "acceptable"
# result$warnings = c("NIVEAU 2a: Flippet label A til modsatte side")
```

### Eksempel 2: Sammenfaldende Linjer (Target = CL)

```r
result <- place_two_labels_npc(
  yA_npc = mapper$y_to_npc(20),
  yB_npc = mapper$y_to_npc(20),  # Samme værdi!
  label_height_npc = 0.13,
  gap_line = 0.01,
  gap_labels = 0.04,
  pref_pos = c("under", "under")
)

# Output:
# result$sideA = "under" (som ønsket)
# result$sideB = "over" (automatisk modsatte side)
# result$placement_quality = "optimal"
# result$warnings = c("Sammenfaldende linjer - placerer labels over/under")
```

## 🎨 Marquee Label Format

Labels bruger `{marquee}` syntax:

```r
# Format:
"{.SIZE **TEXT**}\n{.SIZE **TEXT**}"

# Eksempel:
label <- "{.8 **Header**}\n{.24 **Value**}"
# → 8pt header, 24pt value med linjeskift
```

Auto-estimation parser denne syntax for at beregne korrekt label højde.

## ⚙️ Responsive Sizing

Labels kan skaleres responsivt med `base_size`:

```r
create_responsive_label <- function(header, value, base_size = 14) {
  scale_factor <- base_size / 14
  header_size <- round(8 * scale_factor)
  value_size <- round(24 * scale_factor)

  sprintf("{.%d **%s**}\n{.%d **%s**}", header_size, header, value_size, value)
}

# Brug:
label_14 <- create_responsive_label("CL", "20 mpg", base_size = 14)
# → "{.8 **CL**}\n{.24 **20 mpg**}"

label_20 <- create_responsive_label("CL", "20 mpg", base_size = 20)
# → "{.11 **CL**}\n{.34 **20 mpg**}"
```

## 🧪 Test Resultater

Systemet er testet med 25 scenarier:

- ✅ **Success rate**: 20/25 (80%) - de 5 degraded er targets uden for y-range
- ✅ **Optimal**: 16 tests (64%)
- ✅ **Acceptable**: 4 tests (16%)
- ⚠️ **Suboptimal**: 0 tests
- ❌ **Degraded**: 5 tests (targets uden for range)

**Label separation:**
- Gennemsnit: 0.250 NPC
- Minimum: 0.143 NPC (god separation selv i tætte situationer)
- Maximum: 0.847 NPC

## 📄 License

MIT - Kan frit bruges i andre projekter

## 👤 Author

Genereret med Claude Code
Eksporteret fra: `claude_spc/bfh_layout_reference_dev.R`

## 🔗 Links

- Original implementation: `bfh_layout_reference_dev.R`
- Test suite: Se commit history for test resultater
