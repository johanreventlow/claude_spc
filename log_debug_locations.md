# Log Debug Calls Documentation

Dette dokument indeholder en oversigt over `log_debug` kald i codebasen efter cleanup.

## Status efter cleanup

**Før cleanup**: 259+ kald fordelt på 20+ filer
**Efter cleanup**: ~130 kald fordelt på 25+ filer
**Reduktion**: ~50% færre debug logs

## Cleanup gennemført

### Fjernede elementer:
- **Dekorative separatorer**: Fjernet `===`, `---` og lignende
- **Status emojis**: Fjernet ✅, ⭐, 🔧, 🧹, 📊, 🔄 fra log beskeder
- **Redundante startup logs**: Konsolideret multiple logs for samme operation
- **Verbose parameter logging**: Simplificeret SPC plot generation debug output
- **Unødvendige bekræftelser**: Fjernet "completed successfully" og lignende

### Bevarede elementer:
- **Struktureret logging**: `log_debug_kv()` og `log_debug_block()` bevaret for vigtige events
- **Error context**: Debug logs der giver kontekst ved fejl
- **Core business logic**: Debug logs for kritiske SPC beregninger
- **Performance monitoring**: Vigtige metrics og health status
- **State management**: Logs der hjælper med at spore tilstandsændringer

## Primære komponenter med debug logging

Følgende komponenter bruger stadig debug logging til kritiske operationer:

- **APP_SERVER**: Grundlæggende server lifecycle events
- **SPC_CALC_DEBUG**: Vigtige SPC beregningskontrol
- **EVENT_SYSTEM**: Centraliseret event koordinering
- **AUTO_DETECT**: Kolonne detektering og validering
- **SESSION_LIFECYCLE**: Session management og cleanup
- **PERFORMANCE_MONITOR**: System health og performance

## Anbefalinger

For fremtidig debug logging:
1. **Brug struktureret logging**: `log_debug_kv()` fremfor rå `log_debug()`
2. **Undgå dekorative elementer**: Ingen emojis eller separatorer
3. **Fokus på værdiskabende information**: Kun logs der hjælper med fejlfinding
4. **Konsolider relaterede logs**: Samle multiple operations i færre beskeder
5. **Brug passende log levels**: Debug for udvikling, info/warn for produktion

## Implementation notes

Debug logging kan nemt genaktiveres ved behov ved at:
1. Tilføje flere `log_debug()` kald til specifikke funktioner
2. Implementere feature flags for debug niveauer
3. Bruge miljøvariabler til at kontrollere debug verbosity

Denne cleanup bevarer den strukturerede logging arkitektur mens den reducerer støj i log output betydeligt.