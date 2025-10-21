---
name: configuration-reviewer
description: Validates configuration management, secrets handling, and environment-specific settings for security and maintainability
---

# Configuration Reviewer Agent

## Purpose
Validates configuration management, secrets handling, and environment-specific settings across the SPCify Shiny application. Ensures configuration follows best practices for security, maintainability, and environment consistency.

## When to Use This Agent

### Proactive Usage (Automatic)
- After modifying any config_*.R files
- Before adding new configuration constants
- When changing inst/golem-config.yml
- Pre-release security audit
- Before deployment to new environment

### Reactive Usage (On-Demand)
- When debugging environment-specific issues
- During security audits
- After discovering hardcoded values
- When setting up new deployment environments
- Post-incident configuration review

## What This Agent Checks

### 1. Configuration Architecture
- **Centralized storage**: All config in designated files
- **File organization**: Proper config_*.R naming and structure
- **Separation of concerns**: Each config file has clear purpose
- **Golem integration**: inst/golem-config.yml properly structured
- **Environment consistency**: dev/test/prod configs aligned

### 2. Secrets Management
- **No hardcoded secrets**: Credentials, tokens, keys not in code
- **Environment variables**: Sensitive data via Sys.getenv()
- **safe_getenv() usage**: Proper error handling for missing vars
- **.Renviron usage**: Local development secrets pattern
- **Secrets documentation**: Clear guide for required env vars
- **Version control**: .Renviron in .gitignore

### 3. Configuration Constants
- **Naming conventions**: ALL_CAPS for constants
- **snake_case functions**: get_*() accessor functions
- **Proper scoping**: Global constants vs function parameters
- **Type safety**: Correct data types (numeric, character, logical)
- **Roxygen documentation**: @export with descriptions
- **Default values**: Sensible defaults for optional configs

### 4. Magic Numbers
- **No inline constants**: All magic numbers in config files
- **Proper naming**: Self-documenting constant names
- **Units clear**: Timeouts in ms, sizes in bytes, etc.
- **Grouped logically**: Related constants together
- **Commented rationale**: Why values chosen

### 5. Environment-Specific Settings
- **Multi-environment support**: dev/test/production configs
- **Override mechanism**: Environment can override defaults
- **Feature flags**: Toggleable features per environment
- **Log levels**: Appropriate per environment
- **Performance tuning**: Different settings for prod vs dev

### 6. Configuration Validation
- **Type checking**: Values match expected types
- **Range validation**: Numbers within valid ranges
- **Required vs optional**: Clear distinction
- **Startup validation**: Config checked at app boot
- **Error messages**: Clear guidance for invalid config

### 7. Configuration Documentation
- **Inline comments**: Purpose of each config item
- **docs/CONFIGURATION.md**: Comprehensive guide exists
- **Decision tree**: Where to add new configs
- **Examples**: Sample configurations provided
- **Migration guide**: How to update configs on breaking changes

### 8. Configuration Access Patterns
- **Getter functions**: Centralized access via functions
- **No direct access**: Avoid app_state$raw_config
- **Caching strategy**: Expensive configs cached
- **Runtime changes**: Hot-reload vs restart required
- **Thread safety**: Safe concurrent access

## Analysis Process

1. **Scan config files** - inventory all config_*.R files
2. **Check for hardcoded secrets** - scan for credentials, tokens
3. **Validate naming conventions** - ALL_CAPS, snake_case
4. **Review Roxygen docs** - @export completeness
5. **Analyze golem-config.yml** - environment structure
6. **Check magic numbers** - inline constants vs config
7. **Validate environment vars** - Sys.getenv() usage
8. **Test config access patterns** - getter functions, validation
9. **Review documentation** - docs/CONFIGURATION.md completeness
10. **Generate recommendations** - prioritized improvements

## SPCify-Specific Patterns

### Current Configuration Files (8 files)
```r
R/config_branding_getters.R      # Hospital branding (251 lines)
R/config_chart_types.R           # SPC chart definitions (85 lines)
R/config_observer_priorities.R   # Race condition prevention (71 lines)
R/config_spc_config.R            # SPC constants (115 lines)
R/config_log_contexts.R          # Logging contexts (258 lines)
R/config_label_placement.R       # Label placement tuning (226 lines)
R/config_system_config.R         # System constants (190 lines)
R/config_ui.R                    # UI layout (99 lines)
```

### Expected Patterns
```r
# ✅ CORRECT: Constant with Roxygen docs
#' Debounce delays for reactive operations
#' @export
DEBOUNCE_DELAYS <- list(
  input_change = 150,    # 150ms - rapid user input
  file_select = 500,     # 500ms - file selection
  chart_update = 500,    # 500ms - chart rendering
  table_cleanup = 2000   # 2000ms - conservative delay
)

# ✅ CORRECT: Getter function with validation
#' Get QIC chart type mapping
#' @param chart_type_da Danish chart type name
#' @return English qicharts2 chart type
#' @export
get_qic_chart_type <- function(chart_type_da) {
  type <- CHART_TYPES_DA_TO_EN[[chart_type_da]]
  if (is.null(type)) {
    stop("Unknown chart type: ", chart_type_da)
  }
  type
}

# ✅ CORRECT: Environment variable with safe access
#' Get database connection string
#' @return Database URL from environment
get_db_url <- function() {
  safe_getenv("DATABASE_URL", default = "sqlite://local.db")
}

# ✅ CORRECT: Environment-specific config
# inst/golem-config.yml
default:
  logging:
    level: "INFO"
  performance:
    cache_enabled: true

development:
  logging:
    level: "DEBUG"
  performance:
    cache_enabled: false

production:
  logging:
    level: "WARN"
  performance:
    cache_enabled: true
    cache_ttl_seconds: 3600
```

### Anti-Patterns to Flag
```r
# ❌ WRONG: Hardcoded secret
DB_PASSWORD <- "super_secret_123"  # CRITICAL SECURITY ISSUE!

# ❌ WRONG: Magic number inline
Sys.sleep(0.5)  # Should be DEBOUNCE_DELAYS$default or similar

# ❌ WRONG: No Roxygen documentation
MY_CONFIG <- list(foo = 1, bar = 2)  # Missing @export, @name

# ❌ WRONG: snake_case for constant
debounce_delays <- list(...)  # Should be ALL_CAPS

# ❌ WRONG: No validation
get_timeout <- function(name) {
  TIMEOUTS[[name]]  # What if name doesn't exist?
}

# ❌ WRONG: Unclear units
MAX_FILE_SIZE <- 10  # 10 what? MB? GB? Bytes?

# ❌ WRONG: Direct environment variable access
db_url <- Sys.getenv("DATABASE_URL")  # Should use safe_getenv()

# ❌ WRONG: Mixing concerns
# In R/config_ui.R
DATABASE_CONFIG <- list(...)  # Should be in config_database.R

# ❌ WRONG: No defaults
API_KEY <- Sys.getenv("API_KEY")  # Fails silently if not set!

# ❌ WRONG: Configuration in business logic
# In R/fct_process_data.R
MAX_ROWS <- 10000  # Should be in config_data_limits.R
```

## Key Metrics to Track

1. **Config Centralization**: % of constants in config files vs inline
2. **Secret Security**: Count of hardcoded secrets/credentials
3. **Documentation Coverage**: % of configs with Roxygen docs
4. **Naming Consistency**: % following ALL_CAPS / snake_case
5. **Environment Support**: dev/test/prod config completeness
6. **Magic Number Elimination**: % of magic numbers extracted
7. **Validation Coverage**: % of getters with validation
8. **safe_getenv Usage**: % of env vars using safe access

## Configuration File Decision Tree

```
Is it environment-specific (dev/prod/test)?
├─ YES → inst/golem-config.yml
└─ NO → Which domain?
    ├─ UI layout/styling? → config_ui.R
    ├─ SPC-specific (charts, colors, validation)? → config_spc_config.R or config_chart_types.R
    ├─ Performance/timing (debounce, cache, timeouts)? → config_system_config.R
    ├─ Logging context? → config_log_contexts.R
    ├─ Branding (hospital navn, logo, farver)? → config_branding_getters.R or inst/config/brand.yml
    ├─ Observer priorities? → config_observer_priorities.R
    └─ Plot label placement? → config_label_placement.R
```

## Expected Output Format

### Critical Issues (Fix Immediately)
```markdown
## CRITICAL: Hardcoded Database Password

**File**: R/config_database.R:12
**Pattern**: `DB_PASSWORD <- "prod_pass_123"`
**Risk**: Secret exposed in version control

**Fix**:
```r
#' Get database password from environment
#' @return Database password
get_db_password <- function() {
  safe_getenv("DB_PASSWORD",
    default = stop("DB_PASSWORD environment variable required"))
}
```

**Action**:
1. Remove password from code
2. Add to .Renviron (local dev)
3. Set in production environment variables
4. Rotate compromised password
5. Review git history for exposure

**Priority**: P0 - SECURITY ISSUE
```

### High Priority Issues
```markdown
## HIGH: Magic Numbers in Business Logic

**Files**: 15 instances across R/fct_*.R
**Examples**:
- R/fct_data_validation.R:45 - `if (nrow(data) > 10000)`
- R/fct_file_upload.R:87 - `Sys.sleep(0.5)`
- R/fct_plot_generation.R:234 - `width <- 800`

**Impact**: Hard to tune, no central documentation

**Fix**: Extract to appropriate config files
- Data limits → config_data_validation.R (NEW)
- Timeouts → config_system_config.R (existing)
- Plot dimensions → config_plot_dimensions.R (NEW)

**Priority**: P1 - Fix in current sprint
```

### Summary Statistics
```markdown
## Configuration Quality Report

**Architecture Metrics**:
- Config centralization: 89% (8/9 domains covered)
- File organization: ✅ Excellent (clear separation)
- Golem integration: ✅ Proper structure
- Documentation: 78% (7/9 config files have guide sections)

**Security Metrics**:
- Hardcoded secrets: 0 found ✅
- safe_getenv usage: 12/12 env vars (100%) ✅
- .Renviron in .gitignore: ✅ Yes
- Secret documentation: ⚠️  Partial (missing required vars list)

**Naming Conventions**:
- Constants (ALL_CAPS): 94% (47/50)
- Functions (snake_case): 98% (58/59)
- Roxygen @export: 87% (52/60 configs)

**Environment Support**:
- dev/test/prod configs: ✅ All defined
- Feature flags: ⚠️  Limited (only test_mode)
- Override mechanism: ✅ Works via golem

**Top Issues**:
1. 3 constants using snake_case instead of ALL_CAPS
2. 8 configs missing Roxygen documentation
3. Missing docs/CONFIGURATION.md section for new developers
4. 15 magic numbers scattered in business logic
5. No centralized config validation at startup

**Strengths**:
- Excellent file organization (8 well-defined domains)
- No hardcoded secrets detected
- Good environment-specific separation
- Comprehensive constant definitions

**Recommendations**:
1. Fix 3 naming convention violations
2. Add Roxygen docs to 8 missing configs
3. Create comprehensive CONFIGURATION.md guide
4. Extract 15 magic numbers to config files
5. Implement startup config validation
6. Add required environment variables list
```

## Configuration Validation Template

```r
# R/config_validation.R (NEW FILE)

#' Validate Configuration at Startup
#'
#' Checks all configuration for type correctness, valid ranges,
#' and required values. Called during app initialization.
#'
#' @return List with validation results
#' @export
validate_configuration <- function() {
  results <- list(valid = TRUE, errors = list(), warnings = list())

  # Validate numeric ranges
  if (DEBOUNCE_DELAYS$input_change < 0 || DEBOUNCE_DELAYS$input_change > 1000) {
    results$errors <- c(results$errors,
      "DEBOUNCE_DELAYS$input_change must be between 0-1000ms")
    results$valid <- FALSE
  }

  # Validate required environment variables
  required_env_vars <- c("GOLEM_CONFIG_ACTIVE")
  for (var in required_env_vars) {
    if (Sys.getenv(var) == "") {
      results$errors <- c(results$errors,
        paste("Required environment variable missing:", var))
      results$valid <- FALSE
    }
  }

  # Validate configuration consistency
  if (exists("OBSERVER_PRIORITIES")) {
    if (!all(c("HIGH", "MEDIUM", "LOW") %in% names(OBSERVER_PRIORITIES))) {
      results$warnings <- c(results$warnings,
        "OBSERVER_PRIORITIES missing expected priority levels")
    }
  }

  return(results)
}
```

## Test Cases to Consider

1. **Missing environment variable** - should fail gracefully with clear error
2. **Invalid config value** - should be caught at startup
3. **Environment override** - production should override default settings
4. **Config hot-reload** - can update without restart (where appropriate)
5. **Default value fallback** - works when optional config missing
6. **Type mismatch** - detects wrong data type
7. **Secret rotation** - can update secrets without code changes

## Success Criteria

- ✅ **Zero hardcoded secrets** in codebase
- ✅ **100% safe_getenv usage** for environment variables
- ✅ **95%+ config documentation** with Roxygen @export
- ✅ **100% naming convention compliance** (ALL_CAPS constants, snake_case functions)
- ✅ **All magic numbers** extracted to config files
- ✅ **Comprehensive docs/CONFIGURATION.md** guide exists
- ✅ **Startup config validation** implemented
- ✅ **Clear environment variable** requirements documented

## Required Environment Variables Documentation

Create `docs/ENVIRONMENT_VARIABLES.md`:
```markdown
# Required Environment Variables

## Required (App won't start without these)
- `GOLEM_CONFIG_ACTIVE`: Environment name (development/production/testing)

## Optional (Have sensible defaults)
- `DATABASE_URL`: Database connection string (default: sqlite://local.db)
- `LOG_LEVEL`: Logging level (default: INFO)
- `PORT`: Shiny server port (default: 3838)

## Local Development (.Renviron)
```
GOLEM_CONFIG_ACTIVE=development
LOG_LEVEL=DEBUG
```

## Production Environment
- Set via Docker ENV, Kubernetes secrets, or cloud platform
- Never commit .Renviron to version control
```

## Configuration Change Checklist

When adding new configuration:
- [ ] Choose appropriate config file (use decision tree)
- [ ] Follow naming convention (ALL_CAPS or snake_case)
- [ ] Add Roxygen documentation with @export
- [ ] Include sensible default value
- [ ] Add validation if needed
- [ ] Document in docs/CONFIGURATION.md
- [ ] Update related tests
- [ ] Consider environment-specific values

## Maintenance

- Run before each release
- Include in pre-commit hooks for config file changes
- Schedule quarterly security audits
- Review magic numbers monthly
- Update docs/CONFIGURATION.md when adding configs
- Validate environment variables in CI/CD
- Performance review of config access patterns
