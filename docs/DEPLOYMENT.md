# SPC App - Deployment Guide

Production deployment guide for SPC Statistical Process Control application.

## Indholdsfortegnelse

- [Prerequisites](#prerequisites)
- [Environment Setup](#environment-setup)
- [Installation](#installation)
- [Configuration](#configuration)
- [Deployment Options](#deployment-options)
- [Monitoring](#monitoring)
- [Maintenance](#maintenance)
- [Troubleshooting](#troubleshooting)

## Prerequisites

### System Requirements

**Minimum:**
- R version: ≥ 4.2.0
- RAM: 4 GB
- CPU: 2 cores
- Disk: 2 GB free space

**Recommended:**
- R version: ≥ 4.3.0
- RAM: 8 GB
- CPU: 4+ cores
- Disk: 5 GB free space

### Required Software

```bash
# R installation
# Ubuntu/Debian
sudo apt-get install r-base r-base-dev

# macOS (via Homebrew)
brew install r

# Windows
# Download from: https://cran.r-project.org/bin/windows/base/
```

### R Package Dependencies

Core dependencies (automatisk installeret):
- shiny (≥ 1.7.0)
- qicharts2 (≥ 0.7.0)
- ggplot2 (≥ 3.4.0)
- dplyr (≥ 1.1.0)
- readr (≥ 2.1.0)
- tidyr (≥ 1.3.0)

Se `DESCRIPTION` fil for fuld liste.

## Environment Setup

### 1. Clone Repository

```bash
git clone https://github.com/your-org/claude_spc.git
cd claude_spc
```

### 2. Install Dependencies

#### Option A: Using renv (Recommended)

```r
# I R konsol
install.packages("renv")
renv::restore()
```

Dette installer alle dependencies med de eksakte versioner fra `renv.lock`.

#### Option B: Manual Installation

```r
# Install core packages
install.packages(c("shiny", "qicharts2", "ggplot2", "dplyr", "readr", "tidyr"))

# Install development dependencies
install.packages(c("testthat", "devtools", "lintr", "styler"))
```

### 3. Verify Installation

```r
# Test package loading
source('global.R')

# Run unit tests
testthat::test_dir('tests/testthat')

# Check for missing dependencies
missing_pkgs <- setdiff(
  c("shiny", "qicharts2", "ggplot2", "dplyr", "readr", "tidyr"),
  rownames(installed.packages())
)

if (length(missing_pkgs) > 0) {
  stop("Missing packages: ", paste(missing_pkgs, collapse = ", "))
}
```

## Configuration

### Environment Variables

Create `.Renviron` file in project root:

```bash
# Logging
SPC_LOG_LEVEL=WARN           # INFO for dev, WARN for production
SPC_LOG_FILE=/var/log/spc/app.log

# Performance
SPC_MAX_CACHE_SIZE=50        # Max QIC cache entries
SPC_CACHE_TIMEOUT=300        # Cache timeout in seconds

# Security
SPC_ALLOWED_FILE_TYPES=csv,xlsx,xls,txt
SPC_MAX_FILE_SIZE_MB=10

# Session
SPC_SESSION_TIMEOUT=3600     # Session timeout in seconds
SPC_MAX_CONCURRENT_USERS=50
```

### Hospital Branding

Edit `inst/config/brand.yml`:

```yaml
meta:
  name: "Your Hospital Name"
  description: "Statistical Process Control værktøj"

logo:
  image: "www/hospital_logo.png"

color:
  palette:
    primary: "#003366"
    secondary: "#0066CC"
    accent: "#FF6600"
    success: "#00891a"
    warning: "#f9b928"
    danger: "#c10000"
    info: "#009ce8"
    light: "#f8f8f8"
    dark: "#202020"
```

Place logo file in `www/` directory.

Branding is automatically loaded via `config_branding_getters.R` during app initialization.

### Performance Tuning

Edit `R/config_system_config.R`:

```r
CACHE_CONFIG <- list(
  default_timeout_seconds = 300,        # Adjust based on data volatility
  extended_timeout_seconds = 1800,
  size_limit_entries = 50               # Increase for high-traffic
)

PERFORMANCE_CONFIG <- list(
  max_data_points = 500,                # Limit chart complexity
  debounce_delay_ms = 800,              # UI responsiveness
  auto_detect_timeout_ms = 5000         # Column detection timeout
)
```

## Deployment Options

### Option 1: Shiny Server (Recommended)

#### Install Shiny Server

```bash
# Ubuntu/Debian
sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.20.1002-amd64.deb
sudo gdebi shiny-server-1.5.20.1002-amd64.deb

# Verify installation
sudo systemctl status shiny-server
```

#### Configure Shiny Server

Edit `/etc/shiny-server/shiny-server.conf`:

```conf
# SPC App Configuration
server {
  listen 3838;

  # Define app location
  location /spc {
    site_dir /srv/shiny-server/spc;
    log_dir /var/log/shiny-server;

    # Directory index disabled for security
    directory_index off;

    # App timeout and resource limits
    app_init_timeout 60;
    app_idle_timeout 3600;

    # Performance settings
    simple_scheduler 10;      # Max concurrent processes
  }
}

# Admin interface (optional)
admin 4151 {
  required_user shiny-admin;
}
```

#### Deploy App

```bash
# Copy app to Shiny Server directory
sudo mkdir -p /srv/shiny-server/spc
sudo cp -r /path/to/claude_spc/* /srv/shiny-server/spc/

# Set correct permissions
sudo chown -R shiny:shiny /srv/shiny-server/spc

# Restart Shiny Server
sudo systemctl restart shiny-server
```

Access app at: `http://your-server:3838/spc`

### Option 2: RStudio Connect

#### Deploy via rsconnect Package

```r
# Install rsconnect
install.packages("rsconnect")

# Configure Connect server
rsconnect::setAccountInfo(
  name = "your-connect-server",
  token = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

# Deploy app
rsconnect::deployApp(
  appDir = ".",
  appName = "spc-app",
  appTitle = "SPC Statistical Process Control",
  forceUpdate = TRUE
)
```

#### Connect Configuration

Set environment variables in Connect dashboard:
- Log level: WARN
- Max workers: 4-8 (based on usage)
- Idle timeout: 60 minutes
- Max processes: 3 per user

### Option 3: ShinyProxy (Docker)

#### Create Dockerfile

```dockerfile
FROM rocker/shiny:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'qicharts2', 'ggplot2', 'dplyr', 'readr', 'tidyr'), repos='https://cran.rstudio.com/')"

# Copy app files
COPY . /srv/shiny-server/spc
WORKDIR /srv/shiny-server/spc

# Expose port
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/spc', host='0.0.0.0', port=3838)"]
```

#### Build and Run

```bash
# Build Docker image
docker build -t spc-app:latest .

# Run container
docker run -d \
  --name spc-app \
  -p 3838:3838 \
  -v /path/to/logs:/var/log/spc \
  -e SPC_LOG_LEVEL=WARN \
  spc-app:latest
```

### Option 4: Standalone (Development)

```r
# Run directly from R
source('global.R')

# Or via command line
Rscript -e "source('global.R')"
```

Access at: `http://localhost:3838`

## Security

### Authentication

#### Shiny Server Pro (Built-in Auth)

Edit `/etc/shiny-server/shiny-server.conf`:

```conf
location /spc {
  site_dir /srv/shiny-server/spc;

  # Require authentication
  required_user *;
  auth_type pam;
}
```

#### Reverse Proxy (nginx)

```nginx
server {
  listen 80;
  server_name spc.your-hospital.com;

  # Redirect to HTTPS
  return 301 https://$server_name$request_uri;
}

server {
  listen 443 ssl;
  server_name spc.your-hospital.com;

  # SSL certificates
  ssl_certificate /etc/ssl/certs/spc.crt;
  ssl_certificate_key /etc/ssl/private/spc.key;

  # Basic auth (optional)
  auth_basic "SPC App";
  auth_basic_user_file /etc/nginx/.htpasswd;

  location / {
    proxy_pass http://localhost:3838;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;

    # Timeouts for long-running computations
    proxy_read_timeout 300s;
    proxy_connect_timeout 75s;
  }
}
```

### File Upload Security

Configured in `R/utils_input_sanitization.R`:

- Max file size: 10 MB (configurable)
- Allowed file types: CSV, Excel, TSV
- Automatic virus scanning (hvis configureret)
- Path traversal protection

### Data Privacy

**Recommendations:**
- Enable HTTPS in production
- Use firewall to restrict access
- Implement authentication
- Regular security audits
- Encrypt data at rest (if storing uploads)

## Monitoring

### Health Checks

Create `/srv/shiny-server/spc/healthcheck.R`:

```r
# Simple health check endpoint
healthcheck <- function(req) {
  list(
    status = "healthy",
    version = "1.0.0",
    timestamp = Sys.time()
  )
}
```

### Logging

#### Application Logs

Logs location:
- Shiny Server: `/var/log/shiny-server/`
- App-specific: Configured via `SPC_LOG_FILE` env variable

#### Log Rotation

Create `/etc/logrotate.d/spc-app`:

```bash
/var/log/spc/*.log {
  daily
  rotate 30
  compress
  delaycompress
  missingok
  notifempty
  create 0640 shiny shiny
  sharedscripts
  postrotate
    systemctl reload shiny-server > /dev/null 2>&1 || true
  endscript
}
```

### Performance Monitoring

#### Metrics to Track

- **Response time** - P50, P95, P99 latency
- **Memory usage** - per session and total
- **Cache hit rate** - QIC cache effectiveness
- **Active sessions** - concurrent users
- **Error rate** - application errors per hour

#### Monitoring Tools

**Option A: Prometheus + Grafana**

Install `shinyloadtest` package for metrics export:

```r
install.packages("shinyloadtest")
```

**Option B: Application Insights**

```r
# Log custom metrics
log_metric <- function(name, value) {
  cat(sprintf("METRIC: %s = %f\n", name, value))
}
```

## Maintenance

### Backup Strategy

#### Application Code

```bash
# Git repository backup
git bundle create spc-app-backup.bundle --all

# Store off-site
rsync -avz spc-app-backup.bundle backup-server:/backups/
```

#### User Data (if applicable)

```bash
# Backup user sessions and uploads
tar -czf spc-data-$(date +%Y%m%d).tar.gz /var/lib/shiny-server/spc/

# Retention: 30 days
find /backups/ -name "spc-data-*.tar.gz" -mtime +30 -delete
```

### Updates

#### Application Updates

```bash
# On feature branch
git checkout sprint5/integration-testing
git pull origin sprint5/integration-testing

# Test updates
R -e "source('global.R'); testthat::test_dir('tests/testthat')"

# Deploy to production
sudo systemctl stop shiny-server
sudo cp -r /path/to/claude_spc/* /srv/shiny-server/spc/
sudo systemctl start shiny-server
```

#### R Package Updates

```bash
# Update packages (test in staging first!)
R -e "update.packages(ask = FALSE)"

# Verify compatibility
R -e "source('global.R'); testthat::test_dir('tests/testthat')"
```

### Cache Management

#### Clear QIC Cache

```r
# Connect to running app
# Execute in R console on server
app_state$cache$qic$clear()
```

#### Clear Startup Cache

```bash
# Remove cached artifacts
rm -rf /tmp/spc_startup_cache/
# Or via R
R -e "source('R/utils_startup_cache.R'); unlink(STARTUP_CACHE_CONFIG$cache_dir, recursive = TRUE)"
```

## Troubleshooting

### Common Issues

#### App won't start

**Error:** "Failed to start Shiny app"

**Solutions:**
1. Check logs: `tail -f /var/log/shiny-server/spc-*.log`
2. Verify R packages: `R -e "source('global.R')"`
3. Check permissions: `ls -la /srv/shiny-server/spc/`
4. Test standalone: `R -e "shiny::runApp('/srv/shiny-server/spc')"`

#### Slow performance

**Symptoms:** Long load times, timeouts

**Solutions:**
1. Increase cache size in `config_system_config.R`
2. Reduce max data points (PERFORMANCE_CONFIG)
3. Add more Shiny Server workers
4. Monitor memory usage: `free -h`
5. Check QIC cache hit rate via logs

#### Memory leaks

**Symptoms:** Memory usage grows over time

**Solutions:**
1. Restart Shiny Server regularly: `sudo systemctl restart shiny-server`
2. Set app_idle_timeout in shiny-server.conf
3. Enable session cleanup in `R/utils_memory_management.R`
4. Monitor with: `ps aux | grep shiny`

#### Authentication issues

**Error:** "Access denied"

**Solutions:**
1. Verify user credentials
2. Check nginx .htpasswd file
3. Test auth bypass: curl with credentials
4. Review nginx error logs

### Diagnostic Commands

```bash
# Check Shiny Server status
sudo systemctl status shiny-server

# View real-time logs
sudo tail -f /var/log/shiny-server/*.log

# Check R package versions
R -e "sessionInfo()"

# Monitor resource usage
htop
# or
docker stats spc-app

# Test app connectivity
curl http://localhost:3838/spc

# Check disk space
df -h
```

### Emergency Procedures

#### Rollback to Previous Version

```bash
# Stop app
sudo systemctl stop shiny-server

# Restore from Git
cd /srv/shiny-server/spc
git checkout <previous-commit-hash>

# Restart
sudo systemctl start shiny-server
```

#### Clear All Sessions

```bash
# Force kill all R sessions
sudo pkill -9 -f "R.*shiny"

# Restart server
sudo systemctl restart shiny-server
```

## Performance Benchmarks

### Expected Performance

**Small datasets (<100 points):**
- Load time: <1 second
- Plot generation: <200 ms
- Cache hit: <5 ms

**Medium datasets (100-300 points):**
- Load time: 1-3 seconds
- Plot generation: 200-500 ms
- Cache hit: <10 ms

**Large datasets (300-500 points):**
- Load time: 3-5 seconds
- Plot generation: 500-1000 ms
- Cache hit: <20 ms

### Load Testing

```r
# Install shinyloadtest
install.packages("shinyloadtest")

# Record session
shinyloadtest::record_session("http://localhost:3838/spc")

# Run load test (50 concurrent users)
shinycannon recording.log http://localhost:3838/spc \
  --workers 50 \
  --loaded-duration-minutes 5 \
  --output-dir loadtest-results
```

## Support

### Contact

- **Technical issues:** IT Support
- **Bug reports:** GitHub Issues
- **Feature requests:** Product team

### Resources

- [Shiny Server Admin Guide](https://docs.rstudio.com/shiny-server/)
- [Performance Tuning](https://shiny.rstudio.com/articles/performance.html)
- [Deployment Best Practices](https://engineering-shiny.org/deploy.html)

## Version

This deployment guide is for SPC App version 1.0 (Sprint 5 complete).

Last updated: January 2025
