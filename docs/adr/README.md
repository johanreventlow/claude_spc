# Architecture Decision Records (ADRs)

Dette directory indeholder Architecture Decision Records (ADRs) for SPCify projektet.

## Hvad er en ADR?

En ADR dokumenterer en signifikant arkitektonisk beslutning taget i projektet, inklusiv:
- **Kontekst**: Hvilken situation/problem førte til beslutningen?
- **Beslutning**: Hvad blev besluttet?
- **Konsekvenser**: Hvilke fordele, ulemper og trade-offs følger af beslutningen?
- **Rationale**: Hvorfor er denne løsning bedre end alternativer?

## Hvorfor ADRs?

ADRs hjælper med at:
- **Dokumentere "hvorfor"**: Ikke bare "hvad" men "hvorfor" beslutninger blev truffet
- **Onboarding**: Nye udviklere kan forstå projektets arkitektoniske evolution
- **Undgå regression**: Forhindrer at "løste" problemer genintroduceres
- **Knowledge preservation**: Bevarer institutional knowledge når teammedlemmer skifter

## ADR Naming Convention

```
ADR-XXX-beskrivende-titel.md
```

Hvor:
- `XXX` er et 3-cifret sekventielt nummer (001, 002, 003...)
- `beskrivende-titel` er en kort, hyphen-separated beskrivelse
- `.md` er Markdown format

**Eksempler:**
- `ADR-001-ui-sync-throttle-250ms.md`
- `ADR-002-shared-data-signatures.md`
- `ADR-003-qic-cache-invalidation-strategy.md`

## ADR Template

Brug følgende template til nye ADRs:

```markdown
# ADR-XXX: [Navn på beslutning]

## Status
Proposed / Accepted / Deprecated / Superseded by ADR-YYY

## Kontekst
Beskriv baggrunden. Hvilket problem løses? Hvilke constraints eksisterer?
Hvad var situationen før beslutningen?

## Beslutning
Forklar den arkitektoniske beslutning. Hvad blev valgt? Hvordan fungerer det?

## Konsekvenser

### Fordele
- Hvad opnås med denne beslutning?
- Hvilke problemer løses?

### Ulemper
- Hvilke trade-offs accepteres?
- Hvilke begrænsninger introduceres?

### Mitigations
- Hvordan mitigeres ulemper?
- Hvilke fallback-strategier findes?

## Relaterede Beslutninger
- Link til andre ADRs der påvirkes eller påvirker denne
- Link til relevante dokumenter (SPRINT plans, REMEDIATION plans, etc.)

## Implementation Notes
Eventuelle tekniske detaljer, kode-eksempler, eller implementation guidance.

## Referencer
- Links til eksterne kilder
- Dokumentation
- Research papers
- Best practice guides

## Dato
[ÅÅÅÅ-MM-DD]
```

## Hvordan Bruges ADRs?

### Hvornår skal du lave en ADR?

Lav en ADR når du:
- Træffer en beslutning der påvirker systemarkitektur
- Vælger mellem flere tekniske alternativer med trade-offs
- Implementerer et non-obvious pattern eller workaround
- Afviser en "obvious" løsning af gode grunde
- Ændrer en eksisterende arkitektonisk beslutning

**Eksempler fra SPCify:**
- Hvorfor throttle UI sync på 250ms i stedet for 800ms? → ADR-001
- Hvorfor centralisere data signatures? → Ville være ADR-002
- Hvorfor context-aware cache invalidation? → Ville være ADR-003

### Hvornår skal du IKKE lave en ADR?

Lav IKKE en ADR for:
- Bug fixes uden arkitektoniske implikationer
- Simple refactorings
- Kode cleanup
- Trivielle implementationsdetaljer
- Åbenlyse best practices

### ADR Lifecycle

```
Proposed → Accepted → (Deprecated) → (Superseded)
    ↑                        ↓
    └────── Rejected ────────┘
```

**Status meanings:**
- **Proposed**: Under overvejelse, ikke implementeret endnu
- **Accepted**: Implementeret og aktiv
- **Deprecated**: Ikke længere anbefalet, men stadig i kodebasen
- **Superseded**: Erstattet af en nyere ADR
- **Rejected**: Foreslået men ikke implementeret

### Opdatering af ADRs

ADRs er **immutable** efter accept:
- Lav IKKE om i eksisterende ADRs
- Hvis beslutning ændres: Lav ny ADR der supersedes den gamle
- Opdater `Status` i gammel ADR til `Superseded by ADR-XXX`

**Eksempel:**
```markdown
# ADR-001: Original beslutning

## Status
~~Accepted~~ **Superseded by ADR-042** - 2025-12-15

[... original content forbliver uændret ...]
```

## Eksisterende ADRs

| ADR | Titel | Status | Dato |
|-----|-------|--------|------|
| [ADR-001](./ADR-001-ui-sync-throttle-250ms.md) | UI Sync Throttle 250ms | Accepted | 2025-10-10 |

## Søgning i ADRs

For at finde ADRs relateret til et specifikt emne:

```bash
# Søg i alle ADRs
grep -r "throttle" docs/adr/

# Find ADRs om performance
grep -r "performance" docs/adr/

# Find ADRs der supersedes andre
grep -r "Superseded" docs/adr/
```

## Integration med Development Workflow

ADRs integrerer med development workflow:

1. **Under planning**: Identificér arkitektoniske beslutninger i SPRINT plans
2. **Under implementation**: Dokumentér non-obvious beslutninger i ADRs
3. **Under code review**: Referér til relevante ADRs for kontekst
4. **Under onboarding**: Læs ADRs for at forstå "hvorfor"

**Git commits bør referere til ADRs:**
```
feat(performance): Implementer shared data signatures (ADR-002)

Centraliseret signature system reducerer redundant hashing.
Se ADR-002 for rationale og performance gains.
```

## Yderligere Læsning

- [Michael Nygard's ADR documentation](https://cognitect.com/blog/2011/11/15/documenting-architecture-decisions)
- [ADR GitHub organization](https://adr.github.io/)
- [Architecture Decision Records: When to Use Them](https://www.thoughtworks.com/en-us/insights/blog/architecture/architecture-decision-records-when-to-use-them)

---

**Maintained by**: SPCify Development Team
**Last updated**: 2025-10-10
