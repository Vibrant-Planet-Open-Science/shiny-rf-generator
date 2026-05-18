# RF Generator v2 — Container image for AWS ECS Fargate (behind ALB)
# Repo: Vibrant-Planet-Open-Science/shiny-rf-generator
#
# Build:  docker build -t rf-generator:dev .
# Run:    docker run --rm -p 8080:8080 rf-generator:dev
# Visit:  http://localhost:8080
#
# ----------------------------------------------------------------------------
# Base image
# ----------------------------------------------------------------------------
# rocker/geospatial: R + tidyverse + sf/terra/stars + GDAL/PROJ/GEOS system
# libraries pre-installed. If your renv.lock doesn't actually use spatial
# packages, drop to rocker/r-ver:<version> for a leaner image.
# See: https://rocker-project.org/images/versioned/rocker.html
#
# VERIFY: match this tag to your local R version (R.version.string).
FROM rocker/geospatial:4.5.3

# ----------------------------------------------------------------------------
# System dependencies (add as needed)
# ----------------------------------------------------------------------------
# rocker/geospatial already includes GDAL/PROJ/GEOS/UDUNITS plus curl,
# openssl, xml2, fontconfig dev libs. V8 is the only renv.lock dependency
# not pre-installed; libv8-dev is a virtual package that resolves to
# libnode-dev on Ubuntu jammy (the base image's distro).
RUN apt-get update && apt-get install -y --no-install-recommends \
      libv8-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# ----------------------------------------------------------------------------
# Renv: restore R packages from lockfile
# ----------------------------------------------------------------------------
# Copy renv files BEFORE app code so Docker's layer cache reuses the
# expensive package-install step whenever only app code changes.
WORKDIR /srv/shiny-app

# Pull Linux binaries from Posit Public Package Manager instead of compiling
# from CRAN source. Typical impact: ~20 min build → ~2 min build.
# https://packagemanager.posit.co/client/#/repos/cran/setup
ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/jammy/latest

RUN R -e "install.packages('renv', repos = c(CRAN = '${RENV_CONFIG_REPOS_OVERRIDE}'))"

COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

RUN R -e "renv::restore(prompt = FALSE)"

# ----------------------------------------------------------------------------
# Application code
# ----------------------------------------------------------------------------
# .dockerignore controls what comes in here. Keep renv/library/ excluded —
# we just rebuilt it inside the container.
COPY . .

# ----------------------------------------------------------------------------
# Runtime config
# ----------------------------------------------------------------------------
# Container listens on 8080; the ALB target group and ECS task definition
# both point at this port. Overrides Shiny's default of 3838.
ENV SHINY_PORT=8080
ENV SHINY_HOST=0.0.0.0

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=3 \
  CMD curl -fsS http://localhost:8080/ || exit 1

# ----------------------------------------------------------------------------
# Entrypoint
# ----------------------------------------------------------------------------
# app.R lives in R/ — matches your `shiny::runApp("R", launch.browser = FALSE)`
# invocation. host=0.0.0.0 is required so traffic from outside the container
# can reach the Shiny server (default 127.0.0.1 would only accept local).
CMD ["R", "-e", "shiny::runApp('R', host = '0.0.0.0', port = 8080, launch.browser = FALSE)"]
