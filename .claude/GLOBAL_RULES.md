# Global Claude Code Rules

This file documents that Claude reads global Claude Code settings from `~/.claude/` in addition to project-specific rules in `CLAUDE.md`.

## What I Read

Claude automatically reads from:
1. **Global settings** in `~/.claude/` (system-wide rules, commit policies, git workflows)
2. **Project settings** in `/CLAUDE.md` (project-specific rules and architecture)

## Combined Approach

When working on this project, I apply:
- **Global rules first** (from `~/.claude/`)
- **Project rules override/extend** (from `CLAUDE.md`)

This ensures consistency across all projects while allowing project-specific customization.

## What's Covered by Global Rules

- Commit message formats and attribution policies
- Git branch operations and safety checks
- General development workflow
- Claude Code best practices

## What's Project-Specific (in CLAUDE.md)

- SPC app architecture
- Shiny patterns and state management
- Testing strategy
- Configuration management
- BFHcharts integration
- Cross-repository coordination

---

**Updated:** 2025-10-21
