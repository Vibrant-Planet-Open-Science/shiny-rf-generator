# =============================================================================
# RF Generator v2 — Shiny App
# =============================================================================
#
# 8-screen wizard for building wildlife habitat Response Functions (RFs).
# Users define an AOI, filter stands, pick ecosystem components, set weights,
# and download RF outputs as CSV/markdown.
#
# Architecture:
#   - Single renderUI screen router (output$main_screen) switches between
#     8 render_*() functions based on state$screen
#   - Each render_*() builds one full-page card with its own nav buttons
#   - state (reactiveValues) holds all wizard state; screens read via isolate()
#     to avoid re-rendering the entire page on every input change
#
# For styling/aesthetics:
#   - All CSS lives in the `app_css` string below — edit colors, fonts, spacing there
#   - CSS classes: .card, .hdr, .crumb, .details-wrap, .section-label,
#     .rf-choice, .navbar, .status-ok, .subtitle
#   - Plotly chart styling: search for plotly::layout() calls
#   - DataTables styling: search for DT::renderDataTable options
#   - Color palette constants: forest green #1e3a28, sienna #8b4513,
#     parchment #faf5ed, muted text #7a8a75, warm border #e0d4b8
#
# Data flow:
#   AOI (upload or map) → freq_table (StandIDs + counts)
#   → filter by ForestType/StructureClass → load StandLevel from S3
#   → select ECs → set weights/effects → compute_de_rf() → plots + downloads
#
# =============================================================================

source(here::here("R", "functions.R"))
source(here::here("R", "ec_labels.R"))
source(here::here("R", "ec_tooltips.R"))
source(here::here("R", "charts.R"))

library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(dplyr)
library(aws.s3)
library(purrr)
library(tibble)
library(tidyr)
library(plotly)
library(DT)


# Register the project-root www/ folder as a Shiny static-asset path.
# This lets the browser fetch www/style.css, www/<image>, etc. — without
# moving www/ to live inside R/. here::here() anchors to the project root.
shiny::addResourcePath("www", here::here("www"))


# =============================================================================
# Constants & helpers
# =============================================================================

# Screen order — the wizard progresses through these left to right.
# "species" is conditionally skipped when rf_type == "stand".
SCREENS <- c("aoi", "filters", "review", "rftype", "ecs", "weights", "download")

# Labels shown in the breadcrumb. "species" is omitted from breadcrumb
# because it's conditional — the breadcrumb always shows 7 steps.
STEP_LABELS <- c(aoi="AOI", filters="Filters", review="Review", rftype="RF Type",
                 ecs="ECs", weights="Weights", download="Download")
BREADCRUMB_ORDER <- c("aoi", "filters", "review", "rftype", "ecs", "weights", "download")

# Full-name tooltips for the breadcrumb (shown on hover via bslib::tooltip).
# STEP_LABELS gives the abbreviated badge text; this gives the full descriptor.
CRUMB_TOOLTIPS <- c(
  aoi      = "Area of interest",
  filters  = "Filters",
  review   = "Review & Confirm",
  rftype   = "Response Function Type",
  ecs      = "Ecosystem Components",
  weights  = "Weights & effects",
  download = "Review & Download"
)


# Maximum relative year shown in plots and the on-screen table.
# Downloads still include the full simulation horizon.
MAX_DISPLAY_YEAR <- 20

# Wizard section grouping (v0.2 breadcrumb). Three sections name the shape
# of the work: DEFINE (where + what), BUILD (the curve), EXPORT (the result).
# Defined as data so the breadcrumb renderer can transform it into markup —
# moving a screen between sections is a one-line edit here, no renderer change.
WIZARD_SECTIONS <- list(
  list(name = "Define", screens = c("aoi", "filters", "review")),
  list(name = "Build",  screens = c("rftype", "ecs", "weights")),
  list(name = "Export", screens = c("download"))
)


# Null-coalescing operator: returns `b` if `a` is NULL, empty, or ""
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

# Pre-compute the Shiny input IDs for EC checkbox groups.
# These match the IDs generated in render_ecs() → render_group().
# Used by selected_ecs() reactive to read only these inputs (not all inputs).
EC_GROUP_IDS <- unique(ec_labels$subcategory) |>
  tolower() |>
  (\(x) gsub("[^a-z]", "_", x))() |>
  (\(x) paste0("ecgrp_", x))()


# =============================================================================
# Startup data — loaded once when the app starts (small files only)
# =============================================================================

# RESOLVE Ecoregions 2017, filtered to western US and simplified.
# Used by the "Select on map" tab on the AOI screen.
# Built by scripts/prep_ecoregions.R — see that file to regenerate.
ecoregions_sf <- sf::st_read(here::here("www", "ecoregions_western.geojson"), quiet = TRUE)

# Western US counties shapefile — used to intersect ecoregion selections
# with county boundaries, then look up per-county TreeMap stand IDs from S3.
counties_sf <- sf::st_read(here::here("data", "tl_2024_western_counties.gpkg"), quiet = TRUE)

# California county GEOIDs — used for variant detection.
# If ≥50% of AOI counties are in CA, variant = "CA"; otherwise "CR".
ca_county_geoids <- readRDS(here::here("data", "ca_counties.rds"))$GEOID

# -----------------------------------------------------------------------------
# Load bundled lookup table
# -----------------------------------------------------------------------------
# Was: all_vars_codes <- s3_read_rds(S3_ALL_VARS)
# Bundled locally to eliminate startup S3 dependency. The file is small
# (~2 KB) and stable; refresh by re-running the aws s3 cp command in
# data/README.md if Kat's upstream pipeline regenerates it.
# Source: s3://vp-open-science/biodiversity/habitat-suitability/response_function_data/conus/v0.0.0/rshiny-spatial-data/all_vars_codes.rds
all_vars_path <- here::here("data", "all_vars_codes.rds")

if (!file.exists(all_vars_path)) {
  stop(sprintf(
    "Bundled file missing: %s\n  Working dir: %s\n  This file should ship with the app. Check the deployment manifest.",
    all_vars_path, getwd()
  ))
}

all_vars_codes <- readRDS(all_vars_path)

forest_types <- all_vars_codes |>
  dplyr::filter(variable == "ForTyp") |>
  dplyr::mutate(readable_values = trimws(readable_values)) |>
  dplyr::arrange(readable_values) |>
  dplyr::pull(readable_values)

structure_classes <- all_vars_codes |>
  dplyr::filter(variable == "Structure_Class") |>
  dplyr::arrange(value) |>
  dplyr::pull(readable_values) |>
  trimws()

# Lookups: readable name → numeric code (used to render the code badge in
# the filter pickers on page 02). Built from the same all_vars_codes table.
ft_code_lookup <- all_vars_codes |>
  dplyr::filter(variable == "ForTyp") |>
  dplyr::mutate(readable_values = trimws(readable_values)) |>
  dplyr::select(name = readable_values, code = value)

sc_code_lookup <- all_vars_codes |>
  dplyr::filter(variable == "Structure_Class") |>
  dplyr::mutate(readable_values = trimws(readable_values)) |>
  dplyr::select(name = readable_values, code = value)

# Helpers — vectorized; fall back to "" when no match.
ft_code_for <- function(name) {
  m <- match(name, ft_code_lookup$name)
  ifelse(is.na(m), "", as.character(ft_code_lookup$code[m]))
}
sc_code_for <- function(name) {
  m <- match(name, sc_code_lookup$name)
  ifelse(is.na(m), "", as.character(sc_code_lookup$code[m]))
}

# Stand metadata lookup — maps StandID+Variant → Forest_Type, Structure_Class, etc.
# Passed to get_tm_ids() which joins it with TreeMap IDs from the AOI.
stand_lookup <- readRDS(here::here("data", "unique_stand_data.rds"))
stand_lookup <- stand_lookup[complete.cases(stand_lookup), ]

# Species crosswalk — code → common + scientific name.
# Built from FVS_SpeciesCrosswalk.xls via scripts/build_species_crosswalk.R.
# Used to render the species picker on page 04 with friendly labels.
species_labels <- readr::read_csv(
  here::here("data", "species_crosswalk.csv"),
  show_col_types = FALSE
)

# Lookup helpers — return the friendly label (or fall back to the code
# itself when no crosswalk row exists for a given species). Both vectorized.
species_common <- function(code) {
  m <- match(code, species_labels$code)
  ifelse(is.na(m), code, species_labels$common_name[m])
}
species_scientific <- function(code) {
  m <- match(code, species_labels$code)
  out <- species_labels$scientific_name[m]
  ifelse(is.na(m) | is.na(out) | !nzchar(out), "", out)
}


# =============================================================================
# All styling lives in www/style.css — see :root tokens and global rules..
# =============================================================================



# =============================================================================
# UI — persistent header + screen router + details drawer
# =============================================================================
# The page is a simple fluidPage (not page_fillable) so content scrolls naturally.
# The header and details drawer are always visible; the main_screen uiOutput
# swaps between wizard screens.

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  tags$head(
    # Google Fonts: Poppins (display + body) + JetBrains Mono (structural)
    # preconnect hints let the browser open the TLS connection in parallel with
    # parsing the rest of <head>, shaving ~100ms off first paint.
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(
      rel = "stylesheet",
      href = paste0(
        "https://fonts.googleapis.com/css2?",
        "family=Poppins:ital,wght@0,300;0,400;0,500;0,600;0,700;0,800;1,300;1,400&",
        "family=JetBrains+Mono:wght@400;500;600&display=swap"
      )
    ),
    # Design system tokens, components, and global typography.
    tags$link(rel = "stylesheet", href = "www/style.css"),
    
    # Custom JS handlers
    tags$script(HTML("
      // Scroll to top when navigating between screens.
      Shiny.addCustomMessageHandler('scroll_top', function(m) { window.scrollTo(0, 0); });

      // Sidebar collapse toggle.
      document.addEventListener('click', function(e) {
        if (e.target.matches('.sidebar-toggle')) {
          var s = e.target.closest('.sidebar');
          var collapsed = s.classList.toggle('is-collapsed');
          e.target.textContent = collapsed ? 'expand +' : 'collapse \u2212';
        }
      });

      // ── Generic chip-pill remover/clearer — works for any checkboxGroupInput.
      // Used by the filter pickers on page 02. Iterates instead of using a
      // CSS attribute selector because filter values can contain quotes,
      // slashes, spaces (e.g., 'Eastern white pine / Eastern hemlock').
      function removeChipPick(inputId, value) {
        var inputs = document.querySelectorAll('#' + inputId + ' input[type=checkbox]');
        for (var i = 0; i < inputs.length; i++) {
          if (inputs[i].value === value && inputs[i].checked) {
            inputs[i].click();
            return;
          }
        }
      }
      function clearAllChipPicks(inputId) {
        document.querySelectorAll('#' + inputId + ' input[type=checkbox]:checked')
                .forEach(function(cb) { cb.click(); });
      }

       // ── Species picker — client-side search filter.
      // Hides .checkbox rows whose text doesn't match. All checkboxes stay
      // in the DOM so input$species_picks selections persist across filtering.
      function filterSpeciesList(term) {
        var t = (term || '').toLowerCase().trim();
        document.querySelectorAll('#species_picks .checkbox').forEach(function(row) {
          var text = row.textContent.toLowerCase();
          row.style.display = (t === '' || text.indexOf(t) !== -1) ? '' : 'none';
        });
      }

      // Pill X click — find the matching checkbox, click it.
      // .click() fires the native change event, Shiny picks up the new state.
      function removeSpeciesPick(code) {
        var cb = document.querySelector('#species_picks input[value=\"' + code + '\"]');
        if (cb && cb.checked) cb.click();
      }

      // Clear all — click every checked checkbox in the picker.
      function clearAllSpeciesPicks() {
        document.querySelectorAll('#species_picks input[type=checkbox]:checked').forEach(function(cb) {
          cb.click();
        });
      }

      // ── EC picker — client-side search filter (mirrors filterSpeciesList).
      // Hides .checkbox rows whose text doesn't match. Also collapses whole
      // .species-picker sections when none of their children match. All
      // checkboxes stay in the DOM so EC selections persist across filtering.
      function filterEcList(term) {
        var t = (term || '').toLowerCase().trim();
        var container = document.getElementById('ecs_sections');
        if (!container) return;
        container.querySelectorAll('.species-picker').forEach(function(section) {
          var anyVisible = false;
          section.querySelectorAll('.checkbox').forEach(function(row) {
            var match = (t === '' || row.textContent.toLowerCase().indexOf(t) !== -1);
            row.style.display = match ? '' : 'none';
            if (match) anyVisible = true;
          });
          section.style.display = anyVisible ? '' : 'none';
        });
      }

      // Re-apply the filter whenever Shiny re-renders #ecs_sections (every
      // checkbox click rebuilds it because the X / Y count in each header
      // updates). Hook on shiny:value so we attach after the EC screen is
      // actually present in the DOM.
      $(document).on('shiny:value', function(e) {
        if (e.name !== 'ecs_sections_inner') return;
        // Defer one tick so the new DOM is in place before we re-filter.
        setTimeout(function() {
          var input = document.getElementById('ec_search');
          if (input) filterEcList(input.value);
        }, 0);
      });

      // ── Page 06 — EC weight row helpers (segmented controls).
      // Effect toggle: clicked button gets .is-selected; row gets the matching
      // .fx-positive / .fx-negative / .fx-range class so CSS can swap the right
      // column between the importance picker and the min/max inputs.
      function selectEcEffect(safeId, effect) {
        var row = document.getElementById('ecrow_' + safeId);
        if (!row) return;
        row.querySelectorAll('.ec-effect-btn').forEach(function(btn) {
          btn.classList.toggle('is-selected', btn.dataset.effect === effect);
        });
        row.classList.remove('fx-positive', 'fx-negative', 'fx-range');
        if (effect === 'Positive')      row.classList.add('fx-positive');
        else if (effect === 'Negative') row.classList.add('fx-negative');
        else if (effect === 'Range')    row.classList.add('fx-range');
        Shiny.setInputValue('fx_' + safeId, effect, {priority: 'event'});
      }

      // Importance picker: 1-5 segmented buttons.
      function selectEcImportance(safeId, value) {
        var row = document.getElementById('ecrow_' + safeId);
        if (!row) return;
        row.querySelectorAll('.ec-importance-btn').forEach(function(btn) {
          btn.classList.toggle('is-selected',
            parseInt(btn.dataset.value, 10) === value);
        });
        Shiny.setInputValue('wt_' + safeId, value, {priority: 'event'});
      }

      // Min/max number inputs — push to Shiny on every keystroke. Empty input
      // becomes null so the backend NA-coercion kicks in.
      function pushEcRange(safeId, kind, raw) {
        var v = (raw === '' || raw == null) ? null : parseFloat(raw);
        if (v != null && !isFinite(v)) v = null;
        Shiny.setInputValue('rng_' + kind + '_' + safeId, v, {priority: 'event'});
      }

      // ── Page 07 — chart view toggle (TIME SERIES / BAR · T10).
      // Selected button gets .is-selected; sets input$chart_view which the
      // server-side renderUI reads to swap between ggiraph time-series and bar.
      function selectChartView(view) {
        document.querySelectorAll('.chart-toggle-btn').forEach(function(btn) {
          btn.classList.toggle('is-selected', btn.dataset.view === view);
        });
        Shiny.setInputValue('chart_view', view, {priority: 'event'});
      }
    "))


  ),
  div(class = "app-container",

      # ── Top bar (v0.1) ────────────────────────────────────────────────────
      # Forest-deep persistent header with brand mark, breadcrumb, and action
      # buttons. The HVRA name + sci name inputs that previously lived here
      # have been moved to the .hvra-temp strip below; they migrate into the
      # page-01 sidebar in a later phase. Action buttons are visual scaffold
      # only (no handlers wired yet).
      tags$header(class = "topbar",
        div(class = "topbar-left",
          div(class = "topbar-brand",
            tags$img(class = "topbar-mark",
                    src = "www/VPDC_Avatar_Color_2024.png",
                    alt = "Vibrant Planet"),
            tags$span(class = "topbar-brand-name", "RF Generator"),
          ),
          tags$nav(class = "crumbs", uiOutput("breadcrumb", inline = TRUE))
        ),
        div(class = "topbar-actions",
          tags$button(class = "topbar-btn", "Save draft"),
          tags$button(class = "topbar-btn", "View library")
        )
      ),

      # ── Species banner ────────────────────────────────────────────────────
      # Pure context display below the top bar. Empty until HVRA name typed.
      # Always rendered (in ui), so it persists across all wizard screens.
      uiOutput("species_banner"),


      # ── Page grid: main column + sidebar ──────────────────────────────────
      # Two-column layout; sidebar is sticky on the right at 22rem.
      # Both children are always rendered. The sidebar holds the RF metadata
      # inputs (HVRA name, sci name, description, authors, etc.) — these
      # persist across all 8 wizard screens because Shiny input IDs are global.
      div(class = "page",

        # MAIN COLUMN — the wizard screen router (unchanged behavior)
        uiOutput("main_screen"),

        # SIDEBAR — RF details panel
        # Naming cluster (HVRA + sci) sits above a dashed separator, then
        # provenance fields preserve Kat's original order (description →
        # authors → assumptions → workshop notes → references). The legacy
        # ecoregion_appl input is intentionally dropped — the AOI selection
        # itself encodes ecoregion applicability now. build_factsheet()
        # gracefully shows "—" for the missing field; clean up later.
        tags$aside(class = "sidebar",

          div(class = "sidebar-header",
            div(class = "sidebar-title", "RF ", tags$em("details")),
            # Collapse toggle is visual scaffold only for phase 4a — wiring
            # comes when we add per-screen conditional visibility later.
            tags$button(class = "sidebar-toggle", "collapse \u2212")
          ),

          # — Naming cluster —
          div(class = "sidebar-naming",
            div(class = "sidebar-eyebrow", "Name this RF"),
            div(class = "field",
              tags$label(class = "field-label field-label-required",
                         "HVRA common name"),
              textInput("hvra_name", label = NULL,
                        placeholder = "e.g. Whitebark pine"),
              div(class = "field-help",
                  "Required. Use the common name biologists in your region ",
                  "would recognize. Populates the species banner above.")
            ),
            div(class = "field", style = "margin-bottom: 0;",
              tags$label(class = "field-label field-label-optional",
                         "Scientific name"),
              textInput("hvra_sci", label = NULL,
                        placeholder = "Pinus albicaulis")
            )
          ),

          # — Provenance & assumptions —
          div(class = "sidebar-eyebrow", "Provenance & assumptions"),
          div(class = "field",
            tags$label(class = "field-label field-label-optional", "Description"),
            textAreaInput("desc", label = NULL, rows = 3,
              placeholder = "Brief description of what this RF represents \u2014 habitat focus, target audience, anything that helps a future user understand it.")
          ),
          div(class = "field",
            tags$label(class = "field-label field-label-optional", "Authors"),
            textAreaInput("authors_text", label = NULL, rows = 3,
                          placeholder = "One per line: Name, Affiliation"),
            div(class = "field-help", "e.g. K. Duffy, Vibrant Planet")
          ),
          div(class = "field",
            tags$label(class = "field-label field-label-optional", "Assumptions"),
            textAreaInput("assumptions", label = NULL, rows = 3,
              placeholder = "What conditions or limits should a future user know about?")
          ),
          div(class = "field",
            tags$label(class = "field-label field-label-optional", "Workshop notes"),
            textAreaInput("workshop_notes", label = NULL, rows = 3,
              placeholder = "Date, attendees, key decisions, points of disagreement\u2026"),
            div(class = "field-help",
                "Captures the workshop context this RF emerged from.")
          ),
          div(class = "field",
            tags$label(class = "field-label field-label-optional", "References"),
            textAreaInput("refs", label = NULL, rows = 3,
              placeholder = "Citations, DOIs, links to source studies\u2026")
          )
        )
      )
    )
  )


# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # ── Wizard state ──────────────────────────────────────────────────────────
  # All wizard data lives here. Screens read these values (often via isolate)
  # and the btn_next/btn_back handlers update them.
  state <- reactiveValues(
    screen           = "aoi",       # Current screen name (key into SCREENS)
    aoi_method       = NULL,        # "file" or "map"
    selected_regions = character(),  # Ecoregion names selected on map
    aoi_stands       = NULL,        # Total stand count from AOI
    freq_table       = NULL,        # Data frame: StandID, count, Variant, Forest_Type, etc.
    variant          = NULL,        # "CA" or "CR" — determined from AOI county overlap
    rf_type          = NULL,        # "stand" or "species"
    selected_species = character(),  # Species codes selected by user
    selected_ecs     = character(),  # EC column names selected by user
    ec_weights       = list(),      # Named list: ec_column → integer 1-5
    ec_effects       = list(),      # Named list: ec_column → "Positive"/"Negative"/"Range"
    ec_ranges        = list(),      # Named list: ec_column → c(min, max) or c(NA, NA)
    species_list     = NULL,        # Character vector of species in AOI (loaded from S3)
    filtered_data    = NULL,        # Filtered StandLevel data frame (loaded from S3)
    rf_results       = NULL         # Output of compute_weighted_de_rf(): list(per_ec, combined)
    # Filter selections live in input$ft_picks / input$sc_picks (canonical),
    # not in state. Empty selection = no filter active.
  )

  # ── Selected ECs reactive ─────────────────────────────────────────────────
  # Reads ONLY the ecgrp_* checkbox inputs (not every input in the app).
  # This prevents the EC picker screen from flickering when checkboxes change.
  # See EC_GROUP_IDS above for the specific input IDs.
  selected_ecs <- reactive({
    ecs <- unlist(lapply(EC_GROUP_IDS, function(id) input[[id]]))
    unique(ecs)
  })

  # ── Navigation helpers ────────────────────────────────────────────────────
  # nav_to() changes the active screen and scrolls to top.
  nav_to <- function(s) {
    state$screen <- s
    session$sendCustomMessage("scroll_top", list())
  }

  # ── Forward navigation (Next / Compute RFs / Looks good) ─────────────────
  # This handler does double duty: it advances the screen AND triggers
  # data loading at key transition points (review→rftype, rftype→species).
  observeEvent(input$btn_next, {
    s <- state$screen

    # -- Leaving review screen: load StandLevel data from S3 -----------------
    # This is the big load (~750MB). Only runs once; cached in state$filtered_data.
    if (s == "review" && is.null(state$filtered_data)) {
      req(state$variant, state$freq_table)
      tryCatch({
        withProgress(message = "Loading stand data (this may take a minute)...", {
          stand_raw <- load_stand_data(state$variant)
          # Apply page-02 filters via the filtered_stands() reactive.
          # Reads input$ft_picks / input$sc_picks (canonical state for the
          # checkbox-picker UI). Empty selection = no filter on that dim.
          ft <- filtered_stands() %||% state$freq_table
          state$filtered_data <- get_filtered_stand_data(stand_raw, ft)
        })
      }, error = function(e) {
        showNotification(paste("Failed to load stand data:", e$message),
                         type = "error", duration = 10)
      })
    }

    # -- Leaving weights screen: collect settings and compute RFs ------------
    if (s == "weights") {
      picks <- isolate(selected_ecs())
      wts <- list(); fxs <- list(); rngs <- list()
      for (col in picks) {
        safe <- gsub("[^a-zA-Z0-9]", "_", col)
        wts[[col]]  <- as.integer(input[[paste0("wt_", safe)]] %||% 3L)
        fxs[[col]]  <- input[[paste0("fx_", safe)]] %||% "Positive"
        rng_min <- input[[paste0("rng_min_", safe)]]
        rng_max <- input[[paste0("rng_max_", safe)]]
        rngs[[col]] <- c(
          if (is.null(rng_min) || is.na(rng_min)) NA_real_ else rng_min,
          if (is.null(rng_max) || is.na(rng_max)) NA_real_ else rng_max
        )
      }
      state$ec_weights <- wts
      state$ec_effects <- fxs
      state$ec_ranges  <- rngs

      # Compute fire (DE) RFs using calendar-year alignment.
      # See compute_de_rf() in functions.R and docs/timing_assumptions.md.
      tryCatch({
        withProgress(message = "Computing response functions...", {
          ec_conf <- data.frame(
            Column = picks,
            Weight = vapply(picks, function(c) as.integer(wts[[c]] %||% 3L), integer(1)),
            Effect = vapply(picks, function(c) fxs[[c]] %||% "Positive", character(1)),
            Min    = vapply(picks, function(c) rngs[[c]][1] %||% NA_real_, numeric(1)),
            Max    = vapply(picks, function(c) rngs[[c]][2] %||% NA_real_, numeric(1)),
            stringsAsFactors = FALSE
          )
          de_raw <- compute_de_rf(state$filtered_data, picks, fire_base_year = 2035)
          state$rf_results <- compute_weighted_de_rf(de_raw, ec_conf)
        })
      }, error = function(e) {
        showNotification(paste("RF computation failed:", e$message),
                         type = "error", duration = 10)
      })
    }

    # Advance to next screen
    idx <- which(SCREENS == s)
    if (length(idx) && idx < length(SCREENS)) nav_to(SCREENS[idx + 1])
  })

  # ── Back navigation ───────────────────────────────────────────────────────
  observeEvent(input$btn_back, {
    s <- state$screen
    idx <- which(SCREENS == s)
    if (length(idx) && idx > 1) nav_to(SCREENS[idx - 1])
  })


  # ── Breadcrumb click navigation (back-only) ──────────────────────────
  # Done crumbs in the breadcrumb fire input$crumb_click with the target
  # screen key. We only honor it for screens earlier than the current one
  # — forward jumps are rejected because state from intermediate screens
  # may not be populated yet.
  observeEvent(input$crumb_click, {
    target  <- input$crumb_click
    if (is.null(target) || !nzchar(target)) return()
    cur_idx <- which(BREADCRUMB_ORDER == state$screen)
    tgt_idx <- which(BREADCRUMB_ORDER == target)
    if (length(tgt_idx) && length(cur_idx) && tgt_idx < cur_idx) {
      nav_to(target)
    }
  }, ignoreInit = TRUE)



  # ── Reset (start over) ───────────────────────────────────────────────────
  observeEvent(input$btn_reset, {
    state$screen           <- "aoi"
    state$aoi_method       <- NULL
    state$selected_regions <- character()
    state$aoi_stands       <- NULL
    state$freq_table       <- NULL
    state$variant          <- NULL
    state$rf_type          <- NULL
    state$selected_species <- character()
    state$species_list     <- NULL
    state$filtered_data    <- NULL
    state$selected_ecs     <- character()
    state$ec_weights       <- list()
    state$ec_effects       <- list()
    state$ec_ranges        <- list()
    state$rf_results       <- NULL
    updateTextInput(session, "hvra_name", value = "")
    updateTextInput(session, "hvra_sci",  value = "")
    # Clear picker inputs so a fresh AOI starts with no selections.
    updateCheckboxGroupInput(session, "species_picks", selected = character())
    updateTextInput(session, "species_search", value = "")
    updateCheckboxGroupInput(session, "ft_picks", selected = character())
    updateCheckboxGroupInput(session, "sc_picks", selected = character())
  })

  # RF type card click handler (stand vs species).
  # Stand-level has no follow-up choices on this screen, so picking it
  # auto-advances to ECs — the explicit Continue tap was unintuitive.
  # Species still waits: the user has to pick ≥1 tree species below and
  # click Continue, since the species pane appears after the click.
  observeEvent(input$pick_rftype, {
    state$rf_type <- input$pick_rftype
    if (isTRUE(input$pick_rftype == "stand")) {
      nav_to("ecs")
    }
  })

  # ── Species data load (triggers when user picks "species" rf_type) ──────
  # Loads StdStk from S3 on first species pick; cached in state$species_list
  # for the rest of the session. Replaces the load-on-Next behavior from the
  # pre-merge architecture, since the species picker now lives on this screen.
  observeEvent(input$pick_rftype, {
    if (!isTRUE(input$pick_rftype == "species")) return()
    if (!is.null(state$species_list))           return()
    req(state$variant, state$freq_table)
    tryCatch({
      withProgress(message = "Loading species data...", {
        stdstk <- load_stdstk_data(state$variant)
        aoi_stand_ids <- as.character(state$freq_table$StandID)
        stdstk_filtered <- stdstk |>
          dplyr::filter(as.character(StandID) %in% aoi_stand_ids)
        qualifying <- stdstk_filtered |>
          dplyr::filter(Year == 2035, LiveTpa > 5, !Species %in% c("All", ""))
        state$species_list <- sort(unique(qualifying$Species))
      })
    }, error = function(e) {
      showNotification(paste("Failed to load species:", e$message),
                       type = "error", duration = 10)
    })
  }, ignoreInit = TRUE)

  # ── Species pane (rftype screen, conditional content) ─────────────────
  # Reveals when state$rf_type == "species". Three sub-states:
  #   - list NULL     → load is in flight (or not yet triggered)
  #   - list empty    → no qualifying species in this AOI
  #   - list populated → search + selected-pills + checkbox grid
  #
  # Source of truth for selections is input$species_picks (the checkbox
  # group). Pills render from it; pill X and Clear All dispatch JS that
  # toggles the corresponding checkbox, which fires Shiny's normal change
  # event back into input$species_picks. No state mirror needed.
  output$species_pane <- renderUI({
    if (!isTRUE(state$rf_type == "species")) return(NULL)
    sp <- state$species_list
    if (is.null(sp)) {
      return(div(class = "species-pane-loading", "Loading species from S3..."))
    }
    if (length(sp) == 0) {
      return(div(class = "species-pane-empty", "No species found in your AOI."))
    }

    div(class = "species-picker",
      # Header + subtitle
      h3("Select target species"),
      p(class = "subtitle",
        "Showing only species documented in your AOI. ",
        "Search by species code."
      ),

      # Search input — wrapped in a class so we can style separately from
      # the global Shiny textInput. onkeyup fires the JS row-filter.
      div(class = "species-search",
        tags$input(
          type = "text",
          id = "species_search",
          class = "form-control",
          placeholder = "Search species \u2014 PSME, ABCO, ...",
          onkeyup = "filterSpeciesList(this.value);"
        )
      ),

      # Selected pills row (server-rendered from input$species_picks)
      uiOutput("species_pills"),

      # Section header: total available
      div(class = "species-section-header",
        div(class = "section-label", "Available species"),
        tags$span(class = "species-section-count", length(sp))
      ),

      # Checkbox grid — never filtered server-side. JS hides non-matching
      # rows by reading textContent (which now includes common + scientific
      # names + code, so search-by-any-field works for free).
      div(class = "species-grid",
        checkboxGroupInput("species_picks", NULL,
          choiceNames = lapply(sp, function(code) {
            common <- species_common(code)
            sci    <- species_scientific(code)
            tags$span(class = "species-row",
              tags$span(class = "species-row-name", common),
              if (nzchar(sci)) tags$em(class = "species-row-sci", sci),
              tags$span(class = "species-row-code", code)
            )
          }),
          choiceValues = sp,
          selected = isolate(input$species_picks)
        )
      )
    )
  })

  # ── Selected-species pills row (server-rendered from input$species_picks)
  # Each pill: code text + X button. X click dispatches JS that finds the
  # matching checkbox in #species_picks and clicks it (which fires Shiny's
  # change event and removes the code from input$species_picks).
  output$species_pills <- renderUI({
    picks <- input$species_picks %||% character()
    if (length(picks) == 0) return(NULL)

    div(class = "species-pills",
      lapply(picks, function(code) {
        tags$span(class = "chip chip-orange chip-removable",
          tags$span(class = "chip-label", species_common(code)),
          tags$span(class = "chip-code", code),
          tags$button(class = "chip-remove",
            type = "button",
            onclick = sprintf("removeSpeciesPick('%s');", code),
            "\u00d7"
          )
        )
      }),
      tags$button(class = "species-clear-all",
        type = "button",
        onclick = "clearAllSpeciesPicks();",
        "Clear all"
      )
    )
  })



  # ── RF Type page footer (reactive) ────────────────────────────────────
  # Four states per the page 04 brief:
  #   - nothing picked            → "Pick a response function type"  (Continue disabled)
  #   - stand picked              → "Stand-level RF · Continue to ECs"
  #   - species + ≥1 species pick → "Species RF · {summary} · Continue to ECs"
  #   - species, no picks         → "Pick at least one tree species above" (disabled)
  output$rftype_page_footer <- renderUI({
    rt    <- state$rf_type
    picks <- input$species_picks %||% character()

    species_summary <- function(p) {
      if (length(p) == 1) p
      else paste(length(p), "species selected")
    }

    if (is.null(rt)) {
      status <- "Pick a response function type"
      disabled <- TRUE
    } else if (rt == "stand") {
      status <- "Stand-level RF \u00b7 Continue to ECs"
      disabled <- FALSE
    } else if (rt == "species" && length(picks) == 0) {
      status <- "Pick at least one tree species above"
      disabled <- TRUE
    } else {
      status <- paste0("Species RF \u00b7 ", species_summary(picks),
                       " \u00b7 Continue to ECs")
      disabled <- FALSE
    }

    back_btn <- tags$button(
      id = "btn_back", type = "button",
      class = "btn btn-ghost action-button",
      tags$span(class = "action-label", "\u2190 Back")
    )

    next_btn <- tags$button(
      id = "btn_next", type = "button",
      class = "btn btn-continue action-button",
      disabled = if (disabled) "disabled" else NULL,
      tags$span(class = "action-label", "Continue \u2192")
    )

    div(class = "page-footer",
      div(class = "page-footer-status",
        div(class = "page-footer-status-mark"),
        tags$span(status)
      ),
      div(class = "page-footer-actions", back_btn, next_btn)
    )
  })



  # ── Breadcrumb (v0.2 — sectioned) ───────────────────────────────────────
  # Renders three sections (Define / Build / Export) each containing a run
  # of step crumbs, with vertical dividers between sections. State classes
  # on individual crumbs unchanged from v0.1 (is-done / is-current /
  # is-future). The section containing the current screen gets is-current
  # on its label. WIZARD_SECTIONS (top of file) is the source data.
  output$breadcrumb <- renderUI({
    cur <- state$screen
    cur_idx <- which(BREADCRUMB_ORDER == cur)
    if (length(cur_idx) == 0) cur_idx <- 1

    # Which section contains the current screen?
    current_section_idx <- which(sapply(WIZARD_SECTIONS,
                                        function(s) cur %in% s$screens))

    # Helper: build one section's labeled run of crumbs + interleaved seps.
    build_section <- function(section, is_current) {
      section_class <- paste("crumbs-section",
                             if (is_current) "is-current" else "")

      section_steps <- list()
      for (j in seq_along(section$screens)) {
        key   <- section$screens[j]
        i     <- which(BREADCRUMB_ORDER == key)
        label <- STEP_LABELS[[key]]
        cls   <- if (i < cur_idx)       "crumb is-done"
                 else if (i == cur_idx) "crumb is-current"
                 else                   "crumb is-future"
        mark  <- if (i < cur_idx) NULL else as.character(i)

        # Insert separator before all crumbs except the first in the section
        if (j > 1) {
          section_steps <- c(section_steps,
                             list(tags$span("\u203a", class = "crumb-sep")))
        }
        crumb_el <- tags$span(class = cls,
          # Done crumbs get a JS hook that fires input$crumb_click. Future and
          # current crumbs render the same markup but the cursor: not-allowed
          # CSS rule + cls check on the server prevents navigation.
          onclick = if (i < cur_idx)
            sprintf("Shiny.setInputValue('crumb_click', '%s', {priority:'event'});", key)
          else NULL,
          tags$span(class = "crumb-mark", mark),
          tags$span(class = "crumb-label", label)
        )

        # Wrap with a tooltip showing the full step name.
        crumb_el <- bslib::tooltip(crumb_el, CRUMB_TOOLTIPS[[key]], placement = "bottom")

        section_steps <- c(section_steps, list(crumb_el))

      }

      div(class = section_class,
        div(class = "crumbs-section-label", section$name),
        div(class = "crumbs-section-steps", section_steps)
      )
    }

    # Build all sections with vertical dividers between them.
    parts <- list()
    for (s_idx in seq_along(WIZARD_SECTIONS)) {
      if (s_idx > 1) {
        parts <- c(parts, list(div(class = "crumbs-divider")))
      }
      parts <- c(parts, list(
        build_section(WIZARD_SECTIONS[[s_idx]], s_idx == current_section_idx)
      ))
    }
    tagList(parts)
  })


    # ── Species banner ──────────────────────────────────────────────────────
  # Two-state UI: empty (prompt) vs populated (HVRA name + meta).
  # The empty/populated decision is driven by whether input$hvra_name has
  # any non-whitespace content. Reads input$hvra_sci + state$* for meta.
  # Note: input IDs are global in Shiny — the HVRA inputs currently live
  # in the .hvra-temp strip below this banner; when phase 4 moves them
  # into the sidebar, this renderUI doesn't change.
  output$species_banner <- renderUI({
    nm  <- trimws(input$hvra_name %||% "")
    sci <- trimws(input$hvra_sci  %||% "")

    # ─ Empty state ────────────────────────────────────────────────
    if (!nzchar(nm)) {
      return(
        div(class = "species-banner",
          div(class = "species-banner-empty",
            tags$span(class = "species-banner-eyebrow", "Authoring"),
            tags$span(class = "species-banner-prompt",
              tags$em("name your RF in the details panel")
            )
          ),
          div(class = "species-banner-meta-pending",
            tags$span("AOI pending"),
            tags$span("variant pending")
          )
        )
      )
    }

    # ─ Populated state ────────────────────────────────────────────
    regs    <- state$selected_regions %||% character()
    variant <- state$variant
    stands  <- state$aoi_stands

    aoi_str <- if (length(regs) > 0) {
      paste0(length(regs), " ecoregion", if (length(regs) > 1) "s" else "")
    } else if (!is.null(stands)) {
      "uploaded boundary"
    } else {
      NULL
    }

    # Helper to build one meta cell only when the value exists
    meta_cell <- function(key, val) {
      div(class = "species-banner-meta-item",
        tags$span(class = "species-banner-meta-key", key),
        tags$span(class = "species-banner-meta-val", val)
      )
    }
    meta_items <- list()
    if (!is.null(aoi_str))  meta_items <- c(meta_items, list(meta_cell("AOI", aoi_str)))
    if (!is.null(variant))  meta_items <- c(meta_items, list(meta_cell("Variant", variant)))
    if (!is.null(stands))   meta_items <- c(meta_items, list(meta_cell("Stands", format(stands, big.mark = ","))))

    div(class = "species-banner",
      div(class = "species-banner-name",
        tags$span(class = "species-banner-name-common", nm),
        if (nzchar(sci)) tags$span(class = "species-banner-name-sci", sci)
      ),
      div(class = "species-banner-meta", meta_items)
    )
  })

  # =========================================================================
  # Screen renderers
  # =========================================================================
  # Each render_*() function returns a complete card div for one wizard screen.
  # They are called from output$main_screen via switch(state$screen, ...).
  # They use isolate() when reading reactive values to prevent the screen
  # router from re-rendering on every input change.

  # ── Screen 1: AOI ───────────────────────────────────────────────────────
  # Two tabs: upload a .gpkg/.tif file, or click ecoregions on a leaflet map.
  # Both paths produce a freq_table (StandID → count) stored in state.
  render_aoi <- function() {
    div(class = "main-col",

      # ── Page header ──────────────────────────────────────────────────────
      # Mixed-weight Poppins title, ghosted "01" numeral, mono overline +
      # step counter, orange under-rule (via ::after pseudo-element in CSS).
      div(class = "page-header",
        div(class = "page-header-numeral", "01"),
               div(class = "page-header-overline",
          tags$span(class = "page-header-step", "Define"),
          tags$span("\u00b7 Area of Interest")
        ),
        h1(tags$strong("Define"), " ", tags$em("where this RF applies.")),
        p("Select one or more ecoregions on the map below, or upload a custom ",
          "boundary file. Your area of interest tells us which forest stands ",
          "to model and which FVS variant to use.")
      ),

      # ── Teaching block ───────────────────────────────────────────────────
      # Native <details open> element gives free open/close behavior; no JS.
      # `open = NA` produces the bare HTML `open` attribute (default-open).
      # The "Don't show again" button is visual scaffold for now —
      # cookie/localStorage persistence is a future polish pass.
      tags$details(class = "teaching-block",
        tags$summary(class = "teaching-summary",
          tags$span(class = "teaching-eyebrow", "First time here?"),
          tags$span(class = "teaching-headline",
            "A response function describes how habitat shifts when fire or ",
            "treatment changes the landscape."
          ),
          tags$span(class = "teaching-chevron")
        ),
        div(class = "teaching-content",
          div(class = "teaching-body",
            "You're about to build one. Across the next seven steps, you'll ",
            "define where this RF applies, narrow to the forest types and ",
            "structure classes that matter for your species, choose the ",
            tags$em("ecosystem components"),
            " that drive its habitat, and weight them by importance and ",
            "direction. We'll compute the curve and let you compare scenarios."
          ),
          div(class = "teaching-body",
            "You don't have to predict an exact future \u2014 you're encoding ",
            "what science and your expert judgment say about how habitat ",
            "behaves under stress, so a planner can compare wildfire ",
            "intensities and treatments side-by-side."
          ),
          div(class = "teaching-footer",
            tags$a(class = "teaching-deepen", href = "#", "Read the methods paper"),
            tags$button(class = "teaching-skip", "Don't show again \u2192")
          )
        )
      ),

      # ── Map card ─────────────────────────────────────────────────────────
      # Tab strip at top swaps Map (default) / Upload file. The underlying
      # tabsetPanel keeps its server behavior; CSS in style.css restyles its
      # Bootstrap nav-tabs markup. Map canvas is positioned: relative so the
      # selection callout overlay can absolute-position inside it.
      div(class = "map-card",
        tabsetPanel(id = "aoi_tabs", type = "tabs",
          tabPanel("Map",
            div(class = "map-card-meta",
              div(class = "map-card-overline",
                  "RESOLVE 2017 ecoregions \u00b7"),
              div(class = "map-card-help",
                  "click to select \u00b7 click again to remove")
            ),
            div(class = "map-canvas",
              leafletOutput("ecoregion_map", height = "100%"),
              uiOutput("selected_regions_text")  # absolute-positioned overlay
            ),
            uiOutput("map_card_footer")
          ),
          tabPanel("Upload file",
            div(class = "upload-zone",
              div(class = "upload-zone-prompt",
                tags$strong("Drop your boundary file here"),
                " or click below to browse."),
              div(class = "upload-zone-formats", ".gpkg \u00b7 .tif \u00b7 .zip (zipped shapefile) \u00b7 .shp + companions"),
              fileInput("aoi_file", "Area of interest",
                      accept = c(".gpkg", ".tif", ".zip", ".shp", ".shx", ".dbf", ".prj"),
                      multiple = TRUE,
                      width = "100%",
                      buttonLabel = "Choose file",
                      placeholder = "No file selected"),
              helpText("Upload a .gpkg, .tif, zipped shapefile (.zip), or .shp with companion .shx, .dbf, .prj files."),
              uiOutput("aoi_file_status")
            )
          )
        )
      ),

      # ── Page footer ──────────────────────────────────────────────────────
      # Status text + Continue button rendered server-side so they react
      # to HVRA name + AOI state. See output$aoi_page_footer.
      uiOutput("aoi_page_footer")

    )
  }   



  # ── Screen 2: Filters ──────────────────────────────────────────────────
  # Checkboxes for forest type and structure class. Choices are populated
  # from the AOI's freq_table (only types/classes present in the user's stands).
  # Leaving checkboxes blank means "include all".
  render_filters <- function() {
    div(class = "main-col",

      # ── Page header ──────────────────────────────────────────────────────
      div(class = "page-header",
        div(class = "page-header-numeral", "02"),
        div(class = "page-header-overline",
          tags$span(class = "page-header-step", "Define"),
          tags$span("\u00b7 Filters")
        ),
        h1(tags$strong("Narrow"), " ", tags$em("to where this RF applies.")),
        uiOutput("filters_intro_copy")  # reactive — reads HVRA name + AOI count
      ),

      # ── Teaching block (default closed on this page) ─────────────────────
      tags$details(class = "teaching-block",
        tags$summary(class = "teaching-summary",
          tags$span(class = "teaching-eyebrow", "Why filter?"),
          tags$span(class = "teaching-headline",
            "Pre-filtering improves RF specificity by removing stands where the species wouldn't occur regardless of fire effects."
          ),
          tags$span(class = "teaching-chevron")
        ),
        div(class = "teaching-content",
          div(class = "teaching-body",
            "Forest type and structure class are coarse but powerful filters. ",
            "If your HVRA depends on dense ", tags$em("mixed-conifer"),
            " stands and there are ", tags$em("non-forest"),
            " or ", tags$em("woodland"),
            " stands in your AOI, including them would dilute the response signal across irrelevant terrain."
          ),
          div(class = "teaching-body",
            "Structure classes use the O'Hara 1996 typology: ",
            tags$em("seedling/sapling"), ", ", tags$em("pole"), ", ",
            tags$em("medium"), ", ", tags$em("large"), ", ",
            tags$em("multi-storied"), ". Pick the classes where your HVRA actually lives."
          ),
          div(class = "teaching-footer",
            tags$a(class = "teaching-deepen", href = "#", "See O'Hara's typology")
          )
        )
      ),

      # ── Forest type section ──────────────────────────────────────────────
      div(uiOutput("filters_ft_section")),

      # ── Structure class section ──────────────────────────────────────────
      div(uiOutput("filters_sc_section")),

      # ── Live count readout (or empty-state recovery) ─────────────────────
      uiOutput("filters_count_readout"),

      # ── Page footer ──────────────────────────────────────────────────────
      uiOutput("filters_page_footer")
    )
  }


  # ── Screen 3: Review ───────────────────────────────────────────────────
  # Summary table showing AOI, variant, filters, and matching stand count.
  # "Looks good" button triggers the big S3 data load (see btn_next handler).
  render_review <- function() {
    div(class = "main-col",

      # ── Page header ──────────────────────────────────────────────────────
      div(class = "page-header",
        div(class = "page-header-numeral", "03"),
        div(class = "page-header-overline",
          tags$span(class = "page-header-step", "Define"),
          tags$span("\u00b7 Review")
        ),
        h1(tags$strong("Confirm"), " ", tags$em("your setup.")),
        p("One last look before we load the stand data. The fetch takes ",
          "about 1 minute or more for an ecoregion-scale AOI. You can come back ",
          "to any prior step using the breadcrumb above.")
      ),

      # ── Summary card (rendered server-side from state$*) ─────────────────
      uiOutput("review_summary_card"),

      # ── Low-stand warning (kept from legacy as a contextual safety net) ──
      uiOutput("low_stand_warning"),

      # ── Info callout: load-time heads-up ─────────────────────────────────
      # Copy uses placeholder figures (~15s, ~50 MB) until we measure the
      # actual fetch in production. Adjust based on real timing.
      div(class = "callout callout-info",
        div(class = "callout-mark", "i"),
        div(class = "callout-text",
          tags$strong("About 1 minute."),
          " Once you continue, we fetch ~50 MB of FVS output data and ",
          "species stock tables from Open Science S3. You'll see a ",
          "progress indicator with what's happening at each step."
        )
      ),

      # ── Page footer ──────────────────────────────────────────────────────
      uiOutput("review_page_footer")
    )
  }

  # ── Screen 4: RF Type ──────────────────────────────────────────────────
  # Editorial choice between stand-level and species-specific RF. Uses the
  # design system page chrome (header / teaching / footer). The .rf-choice
  # cards keep their v0.1 selection styling (3px forest-deep border) for
  # now; corner radio/check indicator deferred to the CSS migration pass.
  render_rftype <- function() {
    selected <- state$rf_type
    cls_stand   <- paste("rf-choice", if (identical(selected, "stand"))   "selected" else "")
    cls_species <- paste("rf-choice", if (identical(selected, "species")) "selected" else "")

    div(class = "main-col",

      # ── Page header ──────────────────────────────────────────────────────
      div(class = "page-header",
        div(class = "page-header-numeral", "04"),
        div(class = "page-header-overline",
          tags$span(class = "page-header-step", "Build"),
          tags$span("\u00b7 Response function type")
        ),
        h1(tags$strong("Choose"), " ", tags$em("what your RF is about.")),
        p("A response function can describe how a forest stand changes overall, ",
          "or how one or more tree species fare within it. The choice depends on ",
          "what your HVRA actually responds to.")
      ),

      # ── Teaching block (default open on first visit) ─────────────────────
      tags$details(class = "teaching-block",
        tags$summary(class = "teaching-summary",
          tags$span(class = "teaching-eyebrow", "Stand-level vs species-specific \u2014 which fits?"),
          tags$span(class = "teaching-headline",
            "Stand-level reads structure across the forest. ",
            "Species-specific zooms into one or more target species."
          ),
          tags$span(class = "teaching-chevron")
        ),
        div(class = "teaching-content",
          div(class = "teaching-body",
            tags$strong("Stand-level RFs"), " use whole-stand metrics. ",
            "Use them when your species responds to ", tags$em("structural conditions"),
            " \u2014 canopy density, snag count, surface fuel load \u2014 ",
            "regardless of which tree species are present."
          ),
          div(class = "teaching-body",
            tags$strong("Species-specific RFs"), " zoom in on one or more tree species' presence. ",
            "Use them when your HVRA depends on specific trees \u2014 ",
            "whitebark pine for Clark's nutcracker, or oak ",
            tags$em("or"), " maple ", tags$em("or"), " madrone ",
            "for an acorn-or-mast-dependent cavity nester."
          ),
          div(class = "teaching-body",
            "Most workshops produce stand-level RFs. ",
            "Species-specific is the right choice maybe a quarter of the time."
          ),
          div(class = "teaching-footer",
            tags$a(class = "teaching-deepen", href = "#", "See methods paper \u00b7 \u00a73.2")
          )
        )
      ),

      # ── Two choice cards ─────────────────────────────────────────────────
      fluidRow(
        column(6, div(class = cls_stand,
          onclick = "Shiny.setInputValue('pick_rftype', 'stand', {priority: 'event'});",
          div(class = "section-label", "Stand-level"),
          h2(tags$strong("Whole stand"), " ", tags$em("metrics.")),
          p("Canopy cover, structure, fuels, snag density. The forest as a system."),
          div(class = "chip-grid",
            tags$span(class = "chip", "Canopy cover"),
            tags$span(class = "chip", "QMD"),
            tags$span(class = "chip", "Basal area"),
            tags$span(class = "chip", "Snag density")
          )
        )),
        column(6, div(class = cls_species,
          onclick = "Shiny.setInputValue('pick_rftype', 'species', {priority: 'event'});",
          div(class = "section-label", "Species-specific"),
          h2(tags$strong("One or more"), " ", tags$em("tree species.")),
          p("Live trees per acre or basal area for the species your HVRA depends on."),
          div(class = "chip-grid",
            tags$span(class = "chip", "Whitebark pine"),
            tags$span(class = "chip", "Oak"),
            tags$span(class = "chip", "Douglas-fir"),
            tags$span(class = "chip", "+ more")
          )
        ))
      ),

      # ── Species picker pane (conditional on rf_type == "species") ────────
      # Currently the legacy flat checkbox list — pending Kat's reference data
      # before we build the grouped/badged/searchable picker from the mockup.
      uiOutput("species_pane"),

      # ── Page footer ──────────────────────────────────────────────────────
      uiOutput("rftype_page_footer")
    )
  }




  # ── Screen 6: EC picker ────────────────────────────────────────────────
  # Checkboxes for ecosystem components, grouped by subcategory.
  # Default ECs (show_default=TRUE in ec_labels) shown first; advanced ECs
  # hidden behind a toggle. EC definitions live in R/ec_labels.R.
  render_ecs <- function() {
    div(class = "main-col",

      # ── Page header ──────────────────────────────────────────────────────
      div(class = "page-header",
        div(class = "page-header-numeral", "05"),
        div(class = "page-header-overline",
          tags$span(class = "page-header-step", "Build"),
          tags$span("· Ecosystem components")
        ),
        h1(tags$strong("Pick"), " ", tags$em("ecosystem components.")),
        p("Select the ECs that matter for this RF. ",
          "You'll set importance and effect direction next.")
      ),

      # ── Teaching block (default open on first visit) ─────────────────────
      tags$details(class = "teaching-block",
        tags$summary(class = "teaching-summary",
          tags$span(class = "teaching-eyebrow", "What's an EC?"),
          tags$span(class = "teaching-headline",
            "An ecosystem component is a measurable stand attribute that drives your HVRA's habitat."
          ),
          tags$span(class = "teaching-chevron")
        ),
        div(class = "teaching-content",
          div(class = "teaching-body",
            "Pick the ones your HVRA actually responds to — fewer, ",
            "well-chosen ECs make a sharper response curve than a long ",
            "list of weakly-related metrics."
          ),
          div(class = "teaching-body",
            "Categories below are organized by signal type: ",
            tags$span(class = "chip", "canopy"), ", ",
            tags$span(class = "chip", "fuels"), ", ",
            tags$span(class = "chip", "stand metrics"), ", ",
            tags$span(class = "chip", "understory"), ", ",
            "and ", tags$span(class = "chip", "wildlife habitat"), "."
          ),
          div(class = "teaching-footer",
            tags$a(class = "teaching-deepen", href = "#",
                   "Read the methods paper →")
          )
        )
      ),

      # ── Search field — mirrors the species picker's .species-search markup
      # (raw <input> wrapped in a styled div, inline onkeyup hook).
      div(class = "species-search",
        tags$input(
          type = "text",
          id = "ec_search",
          class = "form-control",
          placeholder = "Search ECs — canopy, snags, fuels, ...",
          onkeyup = "filterEcList(this.value);"
        )
      ),

      # ── Section list (reactive on input$show_advanced + checkbox state) ──
      # Outer div has a stable id so the inline JS can find it after each
      # uiOutput re-render and re-apply the #ec_search filter.
      div(id = "ecs_sections", uiOutput("ecs_sections_inner")),

      tags$hr(),
      checkboxInput("show_advanced", "Show advanced ECs", value = FALSE),

      # ── Page footer ──────────────────────────────────────────────────────
      uiOutput("ecs_page_footer")
    )
  }

  # ── Screen 7: Weights & effects ────────────────────────────────────────
  # Editorial chrome (header + teaching + reactive footer) plus one .ec-row
  # per picked EC. Each row hosts the subcategory badge, range/median stats
  # (visible only in Range mode), a segmented POS/NEG/RANGE effect toggle,
  # and a 1-5 importance picker (or min/max inputs when Range is on).
  # See render_ec_weight_row() for the row markup. Input IDs use a "safe"
  # version of the column name (non-alphanumeric replaced with _).
  render_weights <- function() {
    picks <- isolate(selected_ecs())

    body <- if (length(picks) == 0) {
      div(class = "ec-empty",
        "No ECs picked yet. Go back to ", tags$strong("Ecosystem components"),
        " and pick at least one.")
    } else {
      do.call(tagList, lapply(picks, render_ec_weight_row))
    }

    div(class = "main-col",

      # ── Page header ──────────────────────────────────────────────────────
      div(class = "page-header",
        div(class = "page-header-numeral", "06"),
        div(class = "page-header-overline",
          tags$span(class = "page-header-step", "Build"),
          tags$span("· Weights & effects")
        ),
        h1(tags$strong("Weights"), " ", tags$em("& effects.")),
        p("Set importance (",
          tags$strong("1 = low, 5 = critical"), ", ties allowed) and ",
          "effect direction for each EC.")
      ),

      # ── Teaching block (default open on first visit) ─────────────────────
      tags$details(class = "teaching-block",
        tags$summary(class = "teaching-summary",
          tags$span(class = "teaching-eyebrow", "How weighting works"),
          tags$span(class = "teaching-headline",
            "Each EC gets an importance score and a direction. ",
            "We combine them into a single weighted curve."
          ),
          tags$span(class = "teaching-chevron")
        ),
        div(class = "teaching-content",
          div(class = "teaching-body",
            tags$strong("Importance (1–5)"), " sets how much each EC pulls the ",
            "final RF. Two ECs at ", tags$strong("5"), " contribute equally; ",
            "one at ", tags$strong("5"), " and one at ", tags$strong("2"),
            " means the first carries roughly 2.5× the weight."
          ),
          div(class = "teaching-body",
            tags$strong("Effect"), " says which direction is \"good\" for the HVRA. ",
            tags$span(class = "chip", "Positive"), " means more is better, ",
            tags$span(class = "chip", "Negative"), " means less is better, and ",
            tags$span(class = "chip", "Range"),
            " means intermediate values are best — too little and too much ",
            "are both bad (e.g. shrub cover)."
          ),
          div(class = "teaching-footer",
            tags$a(class = "teaching-deepen", href = "#",
                   "Read the methods paper →")
          )
        )
      ),

      # ── EC rows (or empty state) ─────────────────────────────────────────
      div(id = "ec_weight_rows", body),

      # ── Page footer ──────────────────────────────────────────────────────
      uiOutput("weights_page_footer")
    )
  }

  # Render one EC row on the Weights & Effects screen.
  # Markup follows the .ec-row 3-column grid in style.css:
  #   meta (label + badge + range stats) | effect toggle | importance OR range
  # The right-column swap (importance picker / min/max inputs) is CSS-driven:
  # both are in the DOM; .fx-range on the row decides which is visible.
  #
  # Input IDs preserved from the v0.1 implementation so the btn_next handler
  # (search for s == "weights") reads the same names: wt_<safe>, fx_<safe>,
  # rng_min_<safe>, rng_max_<safe>. State persists across back-nav because
  # Shiny inputs are global; isolate() reads them here to set initial visuals.
  render_ec_weight_row <- function(col) {
    info <- ec_labels[ec_labels$column == col, ]
    lbl  <- if (nrow(info) > 0) info$label[1] else col
    cat  <- if (nrow(info) > 0) info$subcategory[1] else ""
    unit <- if (nrow(info) > 0) info$unit[1] else ""
    safe <- gsub("[^a-zA-Z0-9]", "_", col)

    stats <- ec_range_stats(state$filtered_data, col)

    # Saved state (back-nav) — NULL on first visit. Default the effect to
    # "Positive" so each row arrives with a sensible pick (matches the
    # majority case in workshops); user can flip to NEG or RANGE freely.
    saved_fx  <- isolate(input[[paste0("fx_", safe)]])
    saved_wt  <- isolate(input[[paste0("wt_", safe)]]) %||% 3L
    saved_min <- isolate(input[[paste0("rng_min_", safe)]])
    saved_max <- isolate(input[[paste0("rng_max_", safe)]])

    needs_default_fx <- is.null(saved_fx)
    if (needs_default_fx) saved_fx <- "Positive"

    fx_class <- switch(saved_fx,
                       "Positive" = "fx-positive",
                       "Negative" = "fx-negative",
                       "Range"    = "fx-range",
                       "")

    fmt_stat <- function(x) if (is.na(x)) "—" else format(round(x, 1), big.mark = ",")
    unit_suffix <- if (nzchar(unit)) paste0(" ", unit) else ""

    div(class = paste("ec-row", fx_class), id = paste0("ecrow_", safe),

      # Push the "Positive" default to Shiny on first render of this row so
      # input$fx_<safe> is non-null (the footer "X of Y ECs set" check reads
      # it). Skipped on back-nav when the user already has a saved value.
      if (needs_default_fx) tags$script(HTML(sprintf(
        "Shiny.setInputValue('fx_%s', 'Positive', {priority: 'event'});", safe
      ))),

      # ── Meta row: title + subcategory badge inline ───────────────────────
      div(class = "ec-row-meta",
        div(class = "ec-row-title", lbl),
        tags$span(class = "ec-row-badge", toupper(cat))
      ),

      # ── Effect toggle (POS / NEG / RANGE) ───────────────────────────────
      div(class = "ec-effect-toggle",
        tags$span(class = "ec-effect-label", "Effect"),
        lapply(c("Positive", "Negative", "Range"), function(eff) {
          icon  <- switch(eff, "Positive" = "↗", "Negative" = "↘", "Range" = "∧")
          label <- switch(eff, "Positive" = "POS", "Negative" = "NEG", "Range" = "RANGE")
          cls  <- paste("ec-effect-btn",
                        if (identical(saved_fx, eff)) "is-selected" else "")
          tags$button(type = "button", class = cls,
            `data-effect` = eff,
            onclick = sprintf("selectEcEffect('%s', '%s');", safe, eff),
            tags$span(class = "ec-effect-icon", icon),
            tags$span(label)
          )
        })
      ),

      # ── Right column A: importance picker (visible unless fx-range) ─────
      div(class = "ec-importance-picker",
        tags$span(class = "ec-importance-label", "Importance"),
        div(class = "ec-importance-buttons",
          tags$span(class = "ec-importance-anchor", "low"),
          lapply(1:5, function(v) {
            cls <- paste("ec-importance-btn",
                         if (saved_wt == v) "is-selected" else "")
            tags$button(type = "button", class = cls,
              `data-value` = v,
              onclick = sprintf("selectEcImportance('%s', %d);", safe, v),
              v)
          }),
          tags$span(class = "ec-importance-anchor", "crit")
        )
      ),

      # ── Right column B: min/max inputs (visible only when fx-range) ─────
      # Leading "Range" row label (shared 7rem width with Effect / Importance)
      # so the inputs start at the same x as the buttons in those rows.
      div(class = "ec-range-inputs",
        tags$span(class = "ec-range-row-label", "Range"),
        tags$span(class = "ec-range-label", "Min"),
        tags$input(type = "number",
          class = "ec-range-input",
          id    = paste0("rng_min_", safe),
          value = if (!is.null(saved_min) && !is.na(saved_min)) saved_min else NULL,
          oninput = sprintf("pushEcRange('%s', 'min', this.value);", safe)
        ),
        tags$span(class = "ec-range-label", "Max"),
        tags$input(type = "number",
          class = "ec-range-input",
          id    = paste0("rng_max_", safe),
          value = if (!is.null(saved_max) && !is.na(saved_max)) saved_max else NULL,
          oninput = sprintf("pushEcRange('%s', 'max', this.value);", safe)
        ),
        if (nzchar(unit)) tags$span(class = "ec-range-unit", unit)
      ),

      # ── Stats line: range + median, only visible in fx-range mode. ───────
      # Sits below the min/max inputs as context for what bounds to set.
      div(class = "ec-row-stats",
        tags$span(class = "stat-num", fmt_stat(stats$min)), unit_suffix,
        tags$span(class = "stat-sep", "—"),
        tags$span(class = "stat-num", fmt_stat(stats$max)), unit_suffix,
        tags$span(class = "stat-sep", "·"),
        "med ",
        tags$span(class = "stat-num", fmt_stat(stats$median)), unit_suffix
      )
    )
  }



  # ── Screen 8: Review & download ────────────────────────────────────────
  # Editorial chrome (header + chart card + table card + collapsible details
  # for EC config and Fact sheet). Charts use ggiraph for pixel-match with
  # the design mockup; table is custom HTML for full styling control.
  # Download row is the existing v0.1 button strip — slated for its own
  # design pass. EC config and Fact sheet sections retain the existing DT
  # / uiOutput renderers, just wrapped in <details> cards.
  render_download <- function() {
    nm       <- input$hvra_name %||% "Unnamed HVRA"
    sci      <- input$hvra_sci  %||% ""
    name_str <- if (nzchar(sci)) paste0(nm, " (", sci, ")") else nm

    div(class = "main-col",

      # ── Page header ──────────────────────────────────────────────────────
      div(class = "page-header",
        div(class = "page-header-numeral", "07"),
        div(class = "page-header-overline",
          tags$span(class = "page-header-step", "Export"),
          tags$span("· Download")
        ),
        h1(tags$strong("Your RF"), " ", tags$em("is ready.")),
        p("The weighted response function for ", tags$strong(name_str),
          " across six wildfire intensity classes. ",
          "Toggle to a year-10 snapshot, scan the table, download the data.")
      ),

      # ── Chart card (toggle + ggiraph chart) ──────────────────────────────
      div(class = "output-card",
        div(class = "output-card-header",
          div(class = "output-card-header-text",
            tags$span(class = "output-card-overline", "WEIGHTED RF"),
            tags$span(class = "output-card-title",
              tags$strong("Response"), " ", tags$em("by wildfire intensity"))
          ),
          # Chart-view toggle (TIME SERIES / BAR) — wired by selectChartView JS.
          # Year picker for the bar view lives inside rf_chart_swap so it
          # appears only when the user toggles to bar.
          div(class = "chart-toggle",
            tags$button(type = "button",
              class = "chart-toggle-btn is-selected",
              `data-view` = "timeseries",
              onclick = "selectChartView('timeseries');",
              "Time series"),
            tags$button(type = "button",
              class = "chart-toggle-btn",
              `data-view` = "bar",
              onclick = "selectChartView('bar');",
              "Bar")
          )
        ),
        div(class = "output-card-body",
          div(class = "rf-chart-container",
            uiOutput("rf_chart_swap")
          )
        )
      ),

      # ── Table card (RF values at every t-step) ───────────────────────────
      div(class = "output-card",
        div(class = "output-card-header",
          div(class = "output-card-header-text",
            tags$span(class = "output-card-overline", "TABULAR OUTPUT"),
            tags$span(class = "output-card-title",
              tags$strong("RF values"), " ", tags$em("at every t-step"))
          )
        ),
        div(class = "output-card-body",
          uiOutput("rf_table_card")
        )
      ),

      # ── Collapsible: per-EC RF breakdown ─────────────────────────────────
      # The wide table from v0.1: one row per MgmtID x rel.time, with one
      # column per picked EC plus the Combined column. Year filter dropdown
      # narrows to a single relative-year if needed. Reactives behind this
      # (rf_table_wide, filtered_rf_tbl, output$rf_preview_table) are
      # unchanged from the v0.1 implementation.
      tags$details(class = "details-card",
        tags$summary(class = "details-card-summary",
          tags$span("Per-EC RF breakdown"),
          tags$span(class = "details-card-chevron")
        ),
        div(class = "details-card-body",
          div(style = "margin-bottom: 0.75rem;",
            selectInput("rf_year_filter", label = "View year:",
                        choices = NULL, selected = "All",
                        width = "150px")
          ),
          DT::dataTableOutput("rf_preview_table")
        )
      ),

      # ── Collapsible: EC configuration ────────────────────────────────────
      tags$details(class = "details-card",
        tags$summary(class = "details-card-summary",
          tags$span("EC configuration"),
          tags$span(class = "details-card-chevron")
        ),
        div(class = "details-card-body",
          DT::dataTableOutput("ec_config_table")
        )
      ),

      # ── Collapsible: Fact sheet ──────────────────────────────────────────
      tags$details(class = "details-card",
        tags$summary(class = "details-card-summary",
          tags$span("Fact sheet preview"),
          tags$span(class = "details-card-chevron")
        ),
        div(class = "details-card-body",
          uiOutput("factsheet_preview")
        )
      ),
      # -- Download row (v0.1 placeholder -- slated for its own design pass) --
      div(class = "download-row-legacy",
          style = "margin-top: 1.25rem; display: flex; gap: 0.5rem; flex-wrap: wrap;",
        downloadButton("dl_rf",        "RF outputs (CSV)",   class = "btn btn-outline-secondary"),
        downloadButton("dl_ec",        "EC config (CSV)",    class = "btn btn-outline-secondary"),
        downloadButton("dl_factsheet", "Fact sheet (md)",    class = "btn btn-outline-secondary"),
        downloadButton("dl_all",       "Download all (zip)", class = "btn btn-primary")
      ),

      # -- Page footer (Back / Start a new RF) --
      # Matches the .page-footer pattern from pages 02-06: status row + actions
      # row. The "Start a new RF" button keeps id="btn_reset" so the existing
      # observer at line ~615 fires (clears all state and returns to AOI).
      div(class = "page-footer",
        div(class = "page-footer-status",
          div(class = "page-footer-status-mark"),
          tags$span("RF complete \u00b7 download below or start a fresh wizard")
        ),
        div(class = "page-footer-actions",
          tags$button(id = "btn_back", type = "button",
            class = "btn btn-ghost action-button",
            tags$span(class = "action-label", "\u2190 Back")
          ),
          tags$button(id = "btn_reset", type = "button",
            class = "btn btn-ghost action-button",
            tags$span(class = "action-label", "Start a new RF")
          )
        )
      )
    )
  }

  # ── Screen router ─────────────────────────────────────────────────────
  # This is the central UI switch. When state$screen changes (via nav_to),
  # the matching render_*() function is called and its output replaces
  # the previous screen content.
  output$main_screen <- renderUI({
    switch(state$screen,
           "aoi"      = render_aoi(),
           "filters"  = render_filters(),
           "review"   = render_review(),
           "rftype"   = render_rftype(),
           "ecs"      = render_ecs(),
           "weights"  = render_weights(),
           "download" = render_download(),
           div("Unknown screen:", state$screen)
    )
  })


  # =========================================================================
  # Screen 1 server logic: AOI (map + upload)
  # =========================================================================

  # ── Ecoregion map ─────────────────────────────────────────────────────
  # Renders RESOLVE ecoregion polygons colored by their native COLOR column.
  # Click a polygon to toggle selection (fills dark green when selected).
  output$ecoregion_map <- renderLeaflet({
    leaflet(ecoregions_sf) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -115, lat = 42, zoom = 5) |>
      addPolygons(
        group = "eco",
        fillColor = ~COLOR, fillOpacity = 0.45,
        color = "#1e3a28", weight = 1,
        label = ~ECO_NAME,
        layerId = ~ECO_NAME
      )
  })

  # ── Ecoregion click handler ───────────────────────────────────────────
  # Toggles the clicked ecoregion in state$selected_regions, redraws the map
  # with highlighting, then runs the county intersection → tmid pipeline
  # to build freq_table (same shape as file upload produces).
  observeEvent(input$ecoregion_map_shape_click, {
    clicked <- input$ecoregion_map_shape_click$id
    if (is.null(clicked) || clicked == "") return()
    cur <- state$selected_regions
    if (clicked %in% cur) {
      state$selected_regions <- setdiff(cur, clicked)
    } else {
      state$selected_regions <- c(cur, clicked)
    }

    # Redraw polygons — selected ones get dark green fill + thicker border
    sel <- state$selected_regions
    eco <- ecoregions_sf |> dplyr::mutate(.sel = ECO_NAME %in% sel)
    leafletProxy("ecoregion_map") |>
      clearGroup("eco") |>
      addPolygons(
        data = eco, group = "eco",
        fillColor   = ~ifelse(.sel, "#1e3a28", COLOR),
        fillOpacity = ~ifelse(.sel, 0.6, 0.45),
        color       = ~ifelse(.sel, "#1e3a28", "#1e3a28"),
        weight      = ~ifelse(.sel, 3, 1),
        label       = ~ECO_NAME,
        layerId     = ~ECO_NAME
      )

    # Build freq_table from selected ecoregions via county intersection
    if (length(sel) > 0) {
      tryCatch({
        withProgress(message = "Finding stands in selected ecoregions...", {
          # Dissolve selected ecoregion polygons into one shape
          eco_sel <- ecoregions_sf |>
            dplyr::filter(ECO_NAME %in% sel) |>
            sf::st_union() |> sf::st_sf() |>
            sf::st_transform(sf::st_crs(counties_sf))

          # Intersect with counties to get GEOIDs
          aoi_counties <- suppressWarnings(sf::st_intersection(counties_sf, eco_sel))
          geoids <- unique(aoi_counties$GEOID)

          # Determine variant: CA if ≥50% of counties are in California
          variant_pct <- sum(geoids %in% ca_county_geoids) / length(geoids)
          variant <- ifelse(variant_pct >= 0.5, "CA", "CR")

          # Fetch per-county TreeMap ID files from S3
          bucket_contents <- s3_list_bucket(S3_TMIDS_PREFIX)
          files <- sapply(bucket_contents, function(x) x$Key)
          # Extract GEOID from filenames (pattern: CountyName_GEOID_tmids_only.rds)
          file_geoids <- sapply(files, function(f) {
            parts <- strsplit(trimws(basename(f)), "_")[[1]]
            if (length(parts) >= 2) parts[2] else NA_character_
          })
          matched_files <- files[file_geoids %in% geoids]

          # Read and combine TreeMap IDs across all matched county files
          tm_out <- NULL
          for (f in matched_files) {
            chunk <- s3_read_rds(f)
            colnames(chunk) <- c("lat", "lon", "tm_id")
            chunk <- chunk |>
              dplyr::filter(!is.na(tm_id)) |>
              dplyr::select(tm_id) |>
              table()
            chunk <- data.frame(StandID = names(chunk), count = as.numeric(chunk))
            if (is.null(tm_out)) {
              tm_out <- chunk
            } else {
              tm_out <- chunk |>
                dplyr::rename(count2 = count) |>
                dplyr::full_join(tm_out, by = "StandID") |>
                dplyr::mutate(
                  count  = dplyr::if_else(is.na(count), 0, count),
                  count2 = dplyr::if_else(is.na(count2), 0, count2),
                  count  = count + count2
                ) |>
                dplyr::select(-count2)
            }
          }

          # Join with stand metadata to get Forest_Type, Structure_Class, etc.
          if (!is.null(tm_out) && nrow(tm_out) > 0) {
            tm_out <- tm_out |>
              dplyr::mutate(Variant = variant, StandID = as.numeric(StandID)) |>
              dplyr::left_join(stand_lookup, by = c("StandID", "Variant"))
            state$freq_table <- tm_out
            state$aoi_stands <- nrow(tm_out)
            state$variant    <- variant
            state$aoi_method <- "map"
          }
        })
      }, error = function(e) {
        showNotification(paste("Ecoregion processing failed:", e$message),
                         type = "error", duration = 10)
      })
    } else {
      # No ecoregions selected — clear AOI state
      state$freq_table <- NULL
      state$aoi_stands <- NULL
      state$variant    <- NULL
      state$aoi_method <- NULL
    }
  })

  # Clear AOI selection — resets state and redraws the map's polygons in
  # their default (unselected) styling. Mirrors the redraw block in the
  # ecoregion_map_shape_click handler; in a future cleanup pass we should
  # extract the redraw into a helper used by both click and clear.
  observeEvent(input$btn_aoi_clear, {
    state$selected_regions <- character()
    state$freq_table       <- NULL
    state$aoi_stands       <- NULL
    state$variant          <- NULL
    state$aoi_method       <- NULL

    leafletProxy("ecoregion_map") |>
      clearGroup("eco") |>
      addPolygons(
        data = ecoregions_sf, group = "eco",
        fillColor = ~COLOR, fillOpacity = 0.45,
        color = "#1e3a28", weight = 1,
        label = ~ECO_NAME,
        layerId = ~ECO_NAME
      )
  })

  # ── AOI file reader ─────────────────────────────────────────────────
  # Handles .zip (zipped shapefile) and .shp + companion files.
  # Returns an sf object, or NULL if the file is .gpkg/.tif (legacy path).
  read_aoi <- function(file_df) {
    exts <- tolower(tools::file_ext(file_df$name))

    # Zipped shapefile
    if ("zip" %in% exts) {
      out_dir <- tempfile("aoi_")
      dir.create(out_dir)
      utils::unzip(file_df$datapath[exts == "zip"][1], exdir = out_dir)
      shp <- list.files(out_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
      if (length(shp) == 0) stop("No .shp file found inside the uploaded zip.")
      return(sf::st_read(shp[1], quiet = TRUE))
    }

    # Raw .shp + companion files
    if ("shp" %in% exts) {
      required <- c("shp", "shx", "dbf")
      missing  <- setdiff(required, exts)
      if (length(missing) > 0) {
        stop(paste("Missing companion files:", paste0(".", missing, collapse = ", ")))
      }
      if (!"prj" %in% exts) warning("No .prj uploaded \u2014 CRS will be undefined until reprojected.")
      stage <- tempfile("aoi_")
      dir.create(stage)
      file.copy(file_df$datapath, file.path(stage, file_df$name))
      shp <- file.path(stage, file_df$name[exts == "shp"][1])
      return(sf::st_read(shp, quiet = TRUE))
    }

    # GeoPackage or TIFF — signal to use legacy path
    if (any(exts %in% c("gpkg", "tif"))) return(NULL)

    stop("Unsupported file type. Upload .gpkg, .tif, .zip, or .shp + companion files.")
  }


  # Map selection callout — absolute-positioned overlay on the map (lives
  # inside .map-canvas in render_aoi). Nothing rendered in empty state so
  # the overlay disappears entirely.
  output$selected_regions_text <- renderUI({
    regs <- state$selected_regions
    if (length(regs) == 0) return(NULL)

    eyebrow <- if (length(regs) == 1) "Selected \u00b7 1 ecoregion"
               else paste0("Selected \u00b7 ", length(regs), " ecoregions")
    name_str <- if (length(regs) == 1) regs[1]
                else paste0(length(regs), " ecoregions")

    div(class = "map-selection",
      div(class = "map-selection-eyebrow", eyebrow),
      div(class = "map-selection-name", name_str),
      if (!is.null(state$aoi_stands) && !is.null(state$variant)) {
        div(class = "map-selection-meta",
          tags$strong(format(state$aoi_stands, big.mark = ",")),
          " stands \u00b7 variant ",
          tags$strong(state$variant)
        )
      }
    )
  })

  # Map card in-card footer — count + Clear button below the map canvas.
  # The Clear button is visual scaffold here; wiring an observeEvent for
  # input$btn_aoi_clear is deferred to a follow-up (or phase 4c if we want
  # to bundle it with the Continue button reactive).
  output$map_card_footer <- renderUI({
    n <- length(state$selected_regions %||% character())
    div(class = "map-card-footer",
      div(class = "map-card-status",
        tags$span(class = "map-card-status-num", n),
        tags$span(class = "map-card-status-label",
                  if (n == 1) "ecoregion selected" else "ecoregions selected")
      ),
      if (n > 0) {
        actionButton("btn_aoi_clear", "Clear selection",
                     class = "map-card-clear")
      }
    )
  })

  # Page footer — status copy + Continue button, both reactive.
  # Continue is enabled iff HVRA name has non-whitespace content AND
  # an AOI has been set (state$variant is non-NULL — set by both the
  # ecoregion click handler and the file upload handler when an AOI
  # successfully resolves to stands). Status copy specifies exactly
  # what's blocking so the user knows what to do next.
  output$aoi_page_footer <- renderUI({
    has_name <- nzchar(trimws(input$hvra_name %||% ""))
    has_aoi  <- !is.null(state$variant)

    status_text <- if (!has_name && !has_aoi) "add HVRA name & ecoregion to continue"
                   else if (!has_name)         "add HVRA name to continue"
                   else if (!has_aoi)          "select an AOI to continue"
                   else                        "ready to continue"

    # Build the button as raw tags$button so we control the disabled
    # attribute precisely. The `action-button` class is what Shiny's
    # client-side JS binding looks for to wire clicks to input$btn_next;
    # everything else is styling. The disabled attribute is omitted (NULL)
    # when enabled, set to "disabled" otherwise — the browser blocks click
    # events on disabled buttons natively, so Shiny never sees them.
    btn <- tags$button(
      id = "btn_next",
      type = "button",
      class = "btn btn-continue action-button",
      disabled = if (has_name && has_aoi) NULL else "disabled",
      tags$span(class = "action-label", "Continue to filters \u2192")
    )

    div(class = "page-footer",
      div(class = "page-footer-status",
        div(class = "page-footer-status-mark"),
        tags$span(status_text)
      ),
      div(class = "page-footer-actions", btn)
    )
  })




  # ── File upload handler ───────────────────────────────────────────────
  # Handles .gpkg, .tif (via get_tm_ids), and .zip/.shp (via read_aoi).
  observeEvent(input$aoi_file, {
    req(input$aoi_file)
    tryCatch({
      withProgress(message = "Processing uploaded AOI...", {
        file_df <- input$aoi_file
        exts <- tolower(tools::file_ext(file_df$name))

        if (any(exts %in% c("zip", "shp"))) {
          # Shapefile path: read AOI geometry, write to temp .gpkg, then run pipeline
          aoi_sf <- read_aoi(file_df)
          tmp_gpkg <- tempfile(fileext = ".gpkg")
          sf::st_write(aoi_sf, tmp_gpkg, quiet = TRUE)
          tm_ids <- get_tm_ids(
            aoi_path   = tmp_gpkg,
            filetype   = "gpkg",
            unique_ids = stand_lookup
          )
        } else {
          # Legacy .gpkg / .tif path
          tm_ids <- get_tm_ids(
            aoi_path   = file_df$datapath[1],
            filetype   = tools::file_ext(file_df$name[1]),
            unique_ids = stand_lookup
          )
        }

        state$freq_table <- tm_ids
        state$aoi_stands <- nrow(tm_ids)
        state$variant    <- unique(tm_ids$Variant)[1]
        state$aoi_method <- "file"
      })
    }, error = function(e) {
      showNotification(paste("AOI upload failed:", e$message),
                       type = "error", duration = 10)
    })
  })

  # Success message after file upload
  output$aoi_file_status <- renderUI({
    if (!is.null(state$aoi_stands) && identical(state$aoi_method, "file")) {
      div(class = "status-ok",
          "\u2713 File loaded \u2014 ", tags$strong(state$aoi_stands), " stands matched")
    }
  })


  # =========================================================================
  # Screen 3 server logic: Review
  # =========================================================================

  # Count of stands after page-02 chip filters. Just nrow of filtered_stands().
  # Kept as a separate reactive because the review screen summary table reads it.
  filtered_stand_count <- reactive({
    nrow(filtered_stands() %||% data.frame())
  })


    # ─────────────────────────────────────────────────────────────────────────
  # Page 02 (Filters) — chip toggles, state init, filtered-stands reactive
  # ─────────────────────────────────────────────────────────────────────────

  # When the AOI changes, clear any filter picks. The picker UI rebuilds
  # against the new AOI's available types/classes; carrying stale picks
  # forward would leave phantom selections that aren't rendered as rows.
  observeEvent(state$freq_table, {
    updateCheckboxGroupInput(session, "ft_picks", selected = character())
    updateCheckboxGroupInput(session, "sc_picks", selected = character())
  }, ignoreNULL = FALSE)

  # Empty-state recovery button (clears both filters back to default = empty).
  observeEvent(input$btn_filters_reset, {
    updateCheckboxGroupInput(session, "ft_picks", selected = character())
    updateCheckboxGroupInput(session, "sc_picks", selected = character())
  })

    # Total available counts (denominator for "7 of 23 selected" readouts).
  available_forest_types <- reactive({
    ft <- state$freq_table
    if (is.null(ft) || !"Forest_Type" %in% names(ft)) return(character())
    sort(unique(trimws(ft$Forest_Type)))
  })
  available_structure_classes <- reactive({
    ft <- state$freq_table
    if (is.null(ft) || !"Structure_Class_Description" %in% names(ft)) return(character())
    sort(unique(trimws(ft$Structure_Class_Description)))
  })

  # Reactive intro copy for the page header — substitutes HVRA name and stand count.
  output$filters_intro_copy <- renderUI({
    nm <- trimws(input$hvra_name %||% "")
    species_label <- if (nzchar(nm)) nm else "your species"
    total_stands <- nrow(state$freq_table %||% data.frame())
    p("Pick the forest types and structure classes where ",
      tags$strong(species_label),
      " is found. Leave blank to include all ",
      tags$strong(format(total_stands, big.mark = ",")),
      " stands in your AOI.")
  })

  # Render one filter section (forest type or structure class).
  # Mirrors the species picker on page 04: section header with X-of-Y count,
  # subtitle, removable selected pills, two-column checkbox grid with name +
  # numeric code badge per row.
  #
  # `picker_id` is the Shiny input id (e.g., "ft_picks"). `code_for` is a
  # vectorized lookup function (ft_code_for / sc_code_for).
  render_filter_section <- function(label, subtitle, picker_id, available, code_for) {
    selected <- input[[picker_id]] %||% character()

    choice_names <- lapply(available, function(name) {
      code <- code_for(name)
      tags$span(class = "species-row",
        tags$span(class = "species-row-text",
          tags$span(class = "species-row-name", name)
        ),
        if (nzchar(code)) tags$span(class = "species-row-code", code)
      )
    })

    div(class = "species-picker",
      div(class = "species-picker-header",
        h3(label),
        tags$span(class = "species-picker-count",
          sprintf("%d of %d selected", length(selected), length(available))
        )
      ),
      p(class = "subtitle", subtitle),

      uiOutput(paste0(picker_id, "_pills")),

      div(class = "species-grid",
        checkboxGroupInput(picker_id, NULL,
          choiceNames  = choice_names,
          choiceValues = available,
          selected     = isolate(selected)
        )
      )
    )
  }

  # Render the selected-pills row for a filter picker. Returns NULL when
  # nothing is picked so the strip just isn't there.
  render_filter_pills <- function(picker_id, code_for) {
    picks <- input[[picker_id]] %||% character()
    if (length(picks) == 0) return(NULL)

    div(class = "species-pills",
      lapply(picks, function(name) {
        code <- code_for(name)
        # JSON-encode the value so quotes/slashes/spaces in names don't
        # break the inline JS string (e.g., "Eastern white pine / Eastern hemlock").
        value_js <- jsonlite::toJSON(name, auto_unbox = TRUE)
        tags$span(class = "chip chip-orange chip-removable",
          tags$span(class = "chip-label", name),
          if (nzchar(code)) tags$span(class = "chip-code", code),
          tags$button(class = "chip-remove",
            type = "button",
            onclick = sprintf("removeChipPick('%s', %s);", picker_id, value_js),
            "×"
          )
        )
      }),
      tags$button(class = "species-clear-all",
        type = "button",
        onclick = sprintf("clearAllChipPicks('%s');", picker_id),
        "Clear all"
      )
    )
  }


  # Render one EC subcategory section (Canopy, Fuels, etc.). Mirrors the
  # filter-section pattern — section header with X / Y count, two-column
  # checkbox grid — but no removable-pills row (ECs are simple checkbox
  # picks, not named tags the user reorders). The grp_id derivation here
  # must match EC_GROUP_IDS so selected_ecs() picks them up.
  render_ec_section <- function(group_name, df) {
    if (nrow(df) == 0) return(NULL)
    grp_id   <- paste0("ecgrp_", tolower(gsub("[^a-z]", "_", tolower(group_name))))
    selected <- input[[grp_id]] %||% character()
    n_sel    <- sum(df$column %in% selected)

    unit_part <- ifelse(nzchar(df$unit), paste0(" (", df$unit, ")"), "")
    choice_names <- lapply(seq_len(nrow(df)), function(i) {
      tags$span(class = "species-row",
        tags$span(class = "species-row-text",
          tags$span(class = "species-row-name", df$label[i]),
          tags$span(class = "species-row-unit", unit_part[i])
        )
      )
    })

    div(class = "species-picker",
      div(class = "species-picker-header",
        h3(group_name),
        tags$span(class = "species-picker-count",
                  sprintf("%d / %d", n_sel, nrow(df)))
      ),
      div(class = "species-grid",
        checkboxGroupInput(grp_id, NULL,
          choiceNames  = choice_names,
          choiceValues = df$column,
          selected     = isolate(selected)
        )
      )
    )
  }


  # Filtered stands reactive — single source of truth for the post-filter
  # subset of the AOI. Used by the count readout on page 02, the review
  # screen on page 03, and the data-load handler on btn_next.
  #
  # Filter semantics: each input$*_picks vector lists the items the user
  # picked to INCLUDE. Empty selection = no filter on that dimension =
  # all stands pass through. (Restored from the pre-chip-UI behavior.)
  filtered_stands <- reactive({
    ft <- state$freq_table
    if (is.null(ft)) return(NULL)

    sel_ft <- input$ft_picks %||% character()
    sel_sc <- input$sc_picks %||% character()

    if (length(sel_ft) > 0 && "Forest_Type" %in% names(ft)) {
      ft <- ft |> dplyr::filter(trimws(Forest_Type) %in% sel_ft)
    }
    if (length(sel_sc) > 0 && "Structure_Class_Description" %in% names(ft)) {
      ft <- ft |> dplyr::filter(trimws(Structure_Class_Description) %in% sel_sc)
    }
    ft
  })


  output$filters_ft_section <- renderUI({
    render_filter_section(
      label     = "Forest type",
      subtitle  = "Stands matching any selected forest type are included. Pick none to include all.",
      picker_id = "ft_picks",
      available = available_forest_types(),
      code_for  = ft_code_for
    )
  })

  output$filters_sc_section <- renderUI({
    render_filter_section(
      label     = "Structure class",
      subtitle  = "Stands matching any selected structure class are included. Pick none to include all.",
      picker_id = "sc_picks",
      available = available_structure_classes(),
      code_for  = sc_code_for
    )
  })

  output$ft_picks_pills <- renderUI({ render_filter_pills("ft_picks", ft_code_for) })
  output$sc_picks_pills <- renderUI({ render_filter_pills("sc_picks", sc_code_for) })

  # Count readout — stat block (count > 0) or empty-state recovery (count == 0).
  output$filters_count_readout <- renderUI({
    n_filtered <- nrow(filtered_stands() %||% data.frame())
    n_total    <- nrow(state$freq_table %||% data.frame())

    if (n_filtered == 0 && n_total > 0) {
      div(class = "empty-state",
        div(class = "empty-state-headline", "Your filters returned 0 stands."),
        div(class = "empty-state-body",
          "Most likely your forest type and structure class selections don't overlap. ",
          "Try widening one of them, or reset to all-selected and start over."
        ),
        actionButton("btn_filters_reset", "\u2190 Reset filters",
                     class = "btn btn-ghost")
      )
    } else {
      div(class = "type-stat",
        div(class = "type-stat-num", format(n_filtered, big.mark = ",")),
        div(class = "type-stat-label",
          tags$strong("stands match your filters"),
          tags$br(),
          tags$em(paste0("down from ", format(n_total, big.mark = ","), " in your AOI"))
        )
      )
    }
  })

  # Page footer — status copy + Continue. With the new include-only filter
  # semantics, "skipping filters" is the same as not picking anything, so
  # the explicit Skip-filters link is gone.
  output$filters_page_footer <- renderUI({
    n_filtered  <- nrow(filtered_stands() %||% data.frame())
    n_total     <- nrow(state$freq_table %||% data.frame())
    any_picked  <- length(input$ft_picks %||% character()) +
                   length(input$sc_picks %||% character()) > 0

    status_text <- if (n_filtered == 0 && any_picked)
                     "0 stands match \u00b7 adjust filters"
                   else if (!any_picked)
                     paste0("All ", format(n_total, big.mark=","),
                            " stands included \u00b7 no filters set")
                   else
                     paste0(format(n_filtered, big.mark=","), " of ",
                            format(n_total, big.mark=","), " stands match")

    back_btn <- tags$button(
      id = "btn_back", type = "button",
      class = "btn btn-ghost action-button",
      tags$span(class = "action-label", "\u2190 Back")
    )

    next_btn <- tags$button(
      id = "btn_next", type = "button",
      class = "btn btn-continue action-button",
      disabled = if (n_filtered > 0) NULL else "disabled",
      tags$span(class = "action-label", "Continue \u2192")
    )

    div(class = "page-footer",
      div(class = "page-footer-status",
        div(class = "page-footer-status-mark"),
        tags$span(status_text)
      ),
      div(class = "page-footer-actions", back_btn, next_btn)
    )
  })


  # ─────────────────────────────────────────────────────────────────────────
  # Page 05 (ECs) — section list + reactive page footer
  # ─────────────────────────────────────────────────────────────────────────

  # Section list reactor — re-renders when input$show_advanced toggles or any
  # checkbox changes (the count in each section header recomputes). The JS
  # MutationObserver in www handlers re-applies #ec_search filtering after
  # each rerender so the search field doesn't get clobbered.
  output$ecs_sections_inner <- renderUI({
    show_adv <- isTRUE(input$show_advanced)
    rows <- if (show_adv) ec_labels else dplyr::filter(ec_labels, show_default)

    # Preserve source-file ordering of subcategories (Canopy → Stand metrics
    # → Understory → Fuels → Wildlife habitat → Carbon → Growth) instead of
    # split()'s alphabetical default.
    groups <- split(rows, factor(rows$subcategory,
                                 levels = unique(ec_labels$subcategory)))
    div(
      lapply(names(groups), function(g) render_ec_section(g, groups[[g]]))
    )
  })

  # Reactive footer — disables Continue when 0 ECs picked. Mirrors the
  # 02 / 04 pattern.
  output$ecs_page_footer <- renderUI({
    picks <- selected_ecs()
    n     <- length(picks)

    status_text <- if (n == 0)
                     "Pick at least one EC to continue"
                   else
                     sprintf("%d EC%s selected · ready to continue",
                             n, if (n == 1) "" else "s")

    back_btn <- tags$button(
      id = "btn_back", type = "button",
      class = "btn btn-ghost action-button",
      tags$span(class = "action-label", "← Back")
    )

    next_btn <- tags$button(
      id = "btn_next", type = "button",
      class = "btn btn-continue action-button",
      disabled = if (n > 0) NULL else "disabled",
      tags$span(class = "action-label", "Continue →")
    )

    div(class = "page-footer",
      div(class = "page-footer-status",
        div(class = "page-footer-status-mark"),
        tags$span(status_text)
      ),
      div(class = "page-footer-actions", back_btn, next_btn)
    )
  })


  # ─────────────────────────────────────────────────────────────────────────
  # Page 06 (Weights & Effects) — reactive footer
  # ─────────────────────────────────────────────────────────────────────────

  # Footer status reflects how many of the picked ECs have an effect set
  # (POS, NEG, or RANGE). Compute is gated until every row has a pick —
  # the backend can fall back to defaults but it's better UX to make the
  # user actively choose before kicking off the compute.
  output$weights_page_footer <- renderUI({
    picks   <- selected_ecs()
    n_total <- length(picks)

    if (n_total == 0) {
      status   <- "No ECs picked — go back to Ecosystem components"
      disabled <- TRUE
    } else {
      n_set <- sum(vapply(picks, function(col) {
        safe <- gsub("[^a-zA-Z0-9]", "_", col)
        !is.null(input[[paste0("fx_", safe)]])
      }, logical(1)))

      if (n_set == n_total) {
        status   <- "All ECs set · ready to compute"
        disabled <- FALSE
      } else {
        status   <- sprintf("%d of %d ECs set · pick an effect for the rest",
                            n_set, n_total)
        disabled <- TRUE
      }
    }

    back_btn <- tags$button(
      id = "btn_back", type = "button",
      class = "btn btn-ghost action-button",
      tags$span(class = "action-label", "← Back")
    )
    next_btn <- tags$button(
      id = "btn_next", type = "button",
      class = "btn btn-continue action-button",
      disabled = if (disabled) "disabled" else NULL,
      tags$span(class = "action-label", "Compute RFs →")
    )

    div(class = "page-footer",
      div(class = "page-footer-status",
        div(class = "page-footer-status-mark"),
        tags$span(status)),
      div(class = "page-footer-actions", back_btn, next_btn)
    )
  })


  # ─────────────────────────────────────────────────────────────────────────
  # Page 04 (Review) — summary card + edit-link observers + page footer
  # ─────────────────────────────────────────────────────────────────────────

  # Summary card: 4 labeled rows pulling everything from state$*.
  # Edit links jump back to the relevant prior screen via nav_to().
  output$review_summary_card <- renderUI({
    aoi_desc <- if (length(state$selected_regions) > 0)
      paste(state$selected_regions, collapse = ", ")
    else if (!is.null(state$aoi_stands)) "Uploaded boundary"
    else "Not set"

    aoi_count <- nrow(state$freq_table %||% data.frame())

    # Filters row: read-only chips, or condensed "all" text when nothing
    # is picked (which under the new include-only semantics = no filter).
    ft_sel <- input$ft_picks %||% character()
    sc_sel <- input$sc_picks %||% character()
    ft_empty <- length(ft_sel) == 0
    sc_empty <- length(sc_sel) == 0

    filters_value <- if (ft_empty && sc_empty) {
      tags$span(class = "summary-row-value",
                "All forest types \u00b7 all structure classes")
    } else {
      tagList(
        if (!ft_empty) div(class = "chip-grid", style = "margin-bottom: 0.5rem;",
          lapply(ft_sel, function(v) tags$span(class = "chip chip-orange", v))
        ),
        if (!sc_empty) div(class = "chip-grid",
          lapply(sc_sel, function(v) tags$span(class = "chip chip-orange", v))
        )
      )
    }

    # Build one row helper
    row <- function(label, value, edit_id = NULL) {
      div(class = "summary-row",
        div(class = "summary-row-label", label),
        div(class = "summary-row-value", value),
        if (!is.null(edit_id))
          actionLink(edit_id, "Edit \u2197", class = "summary-row-edit")
        else
          div()  # empty third column to preserve grid
      )
    }

    div(class = "summary-card",
      div(class = "summary-card-header",
        div(class = "summary-card-mark"),
        div(class = "summary-card-overline", "Setup summary")
      ),
      row("AOI",
        tagList(
          tags$strong(aoi_desc),
          div(class = "summary-row-value-meta",
              format(aoi_count, big.mark = ","), " stands at AOI scale")
        ),
        edit_id = "btn_review_edit_aoi"
      ),
      row("Filters", filters_value, edit_id = "btn_review_edit_filters"),
      row("Variant",
        tagList(
          tags$strong(state$variant %||% "\u2014"),
          div(class = "summary-row-value-meta",
              "Determined by the AOI overlap with FVS variant boundaries.")
        )
      ),
      row("Stands",
        div(class = "type-stat",
          div(class = "type-stat-num",
              format(filtered_stand_count(), big.mark = ",")),
          div(class = "type-stat-label",
              tags$strong("matching all filters"))
        )
      )
    )
  })

  # Edit-link observers: jump back to the relevant prior screen.
  observeEvent(input$btn_review_edit_aoi,     { nav_to("aoi") })
  observeEvent(input$btn_review_edit_filters, { nav_to("filters") })

  # Low-stand warning — kept as a safety net for sub-50 stand counts. Lives
  # below the summary card as a yellow callout. Not in the page brief but
  # protective UX worth preserving.
  output$low_stand_warning <- renderUI({
    n <- filtered_stand_count()
    if (n > 0 && n < 50) {
      div(class = "callout callout-warning",
        div(class = "callout-mark", "!"),
        div(class = "callout-text",
          tags$strong("Only ", format(n, big.mark = ","), " stands match."),
          " Results may not be statistically robust. Consider broadening ",
          "your filters before continuing."
        )
      )
    }
  })

  # Page footer — status copy + Load data button. Copy reflects the headline
  # number on this page so the user sees what they're about to load.
  output$review_page_footer <- renderUI({
    n <- filtered_stand_count()
    variant <- state$variant %||% "?"
    status_text <- paste0(format(n, big.mark = ","), " stands \u00b7 ",
                          variant, " variant \u00b7 ready to load")

    back_btn <- tags$button(
      id = "btn_back", type = "button",
      class = "btn btn-ghost action-button",
      tags$span(class = "action-label", "\u2190 Back")
    )

    next_btn <- tags$button(
      id = "btn_next", type = "button",
      class = "btn btn-continue action-button",
      tags$span(class = "action-label", "Load data \u2192")
    )

    div(class = "page-footer",
      div(class = "page-footer-status",
        div(class = "page-footer-status-mark"),
        tags$span(status_text)
      ),
      div(class = "page-footer-actions", back_btn, next_btn)
    )
  })

  # =========================================================================
  # Screen 7 server logic: Weights preview (DT table, shown below real UI)
  # =========================================================================
  output$weights_preview <- DT::renderDataTable({
    picks <- selected_ecs()
    if (length(picks) == 0) return(data.frame(Message = "No ECs selected"))
    ec_labels |>
      filter(column %in% picks) |>
      mutate(Importance = 3, Effect = "Positive") |>
      select(EC = label, Category = subcategory, Unit = unit, Importance, Effect)
  }, options = list(dom = 't', pageLength = 50), rownames = FALSE)


  # =========================================================================
  # Screen 8 server logic: RF plots, tables, downloads
  # =========================================================================

  # ── Plot data reactive ─────────────────────────────────────────────────
  # Filters rf_results to 0 <= rel.time <= MAX_DISPLAY_YEAR and rounds values.
  # Both the bar plot, time series, and on-screen table read from this.
  # Download handlers use state$rf_results directly (full horizon).
  rf_plot_data <- reactive({
    res <- state$rf_results
    if (is.null(res)) return(NULL)
    list(
      combined = res$combined |>
        dplyr::filter(rel.time >= 0, rel.time <= MAX_DISPLAY_YEAR) |>
        dplyr::mutate(median_combined_rf = round(median_combined_rf, 2)),
      per_ec = res$per_ec |>
        dplyr::filter(rel.time >= 0, rel.time <= MAX_DISPLAY_YEAR) |>
        dplyr::mutate(median_rf = round(median_rf, 2))
    )
  })


  # ── Page 07 (Review & download) — new ggiraph chart outputs ──────────
  # Chart-view toggle (TIME SERIES / BAR · T10) is wired client-side via
  # selectChartView; here we read input$chart_view and swap which
  # girafeOutput is rendered. Both renderers read from rf_plot_data().

  output$rf_chart_swap <- renderUI({
    view <- input$chart_view %||% "timeseries"
    # height = "auto" lets the container match the SVG's natural aspect ratio
    # (set by width_svg / height_svg in render_rf_*), so the chart fills its
    # parent without empty bands top/bottom.
    if (view == "bar") {
      # Build the year selector from the available rel.time values (post-
      # disturbance only — year 0 is the pre-disturbance baseline = all 0s,
      # not interesting). Preserve the previously selected year across
      # toggle re-renders via isolate().
      d <- rf_plot_data()
      yrs <- if (!is.null(d)) {
        v <- sort(unique(d$combined$rel.time))
        v[v > 0]
      } else {
        c(1, 5, 10, 15, 20)
      }
      prev_year <- isolate(input$bar_year)
      sel_year  <- if (!is.null(prev_year) && as.numeric(prev_year) %in% yrs)
                     prev_year
                   else if (10 %in% yrs) 10
                   else yrs[1]

      tagList(
        div(class = "chart-bar-controls",
          tags$label(class = "chart-bar-label", "Year:"),
          selectInput("bar_year", NULL,
            choices  = yrs,
            selected = sel_year,
            width    = "90px")
        ),
        ggiraph::girafeOutput("rf_chart_bar", height = "auto")
      )
    } else {
      ggiraph::girafeOutput("rf_chart_ts",  height = "auto")
    }
  })

  output$rf_chart_ts <- ggiraph::renderGirafe({
    d <- rf_plot_data()
    if (is.null(d)) return(NULL)
    render_rf_timeseries(d$combined)
  })

  # Bar snapshot — driven by input$bar_year from the dropdown rendered in
  # rf_chart_swap. Defaults to year 10 when the input is unset (first render
  # or before the selectInput initializes); falls back to the latest available
  # post-disturbance year if year 10 isn't in the data.
  output$rf_chart_bar <- ggiraph::renderGirafe({
    d <- rf_plot_data()
    if (is.null(d)) return(NULL)
    yrs    <- sort(unique(d$combined$rel.time))
    target <- as.numeric(input$bar_year %||% NA_real_)
    if (is.na(target) || !(target %in% yrs)) {
      target <- if (10 %in% yrs) 10 else max(yrs[yrs > 0], na.rm = TRUE)
    }
    render_rf_bar(d$combined, year = target)
  })

  # Custom HTML table (vs DT) for full design-system control: mono headers,
  # negatives in red, italic baseline row. See render_rf_table_html() in
  # R/charts.R.
  output$rf_table_card <- renderUI({
    d <- rf_plot_data()
    if (is.null(d)) {
      return(div(class = "rf-table-empty", "No RF results yet."))
    }
    render_rf_table_html(d$combined)
  })

  # Year dropdown for the bar plot (excludes year 0, shows only post-fire years)
  output$rf_year_selector <- renderUI({
    d <- rf_plot_data()
    if (is.null(d)) return(NULL)
    times <- sort(unique(d$combined$rel.time))
    times <- times[times > 0]
    selectInput("rf_year", "Relative year", choices = times,
                selected = times[1], width = "150px")
  })

  # ── Bar plot (plotly): combined weighted RF at one time step ───────────
  output$rf_bar_plot <- plotly::renderPlotly({
    d <- rf_plot_data()
    if (is.null(d)) return(plotly::plotly_empty())
    yr <- as.numeric(input$rf_year %||% 1)
    comb <- d$combined |> dplyr::filter(rel.time == yr)
    if (nrow(comb) == 0) return(plotly::plotly_empty())
    comb <- comb |> dplyr::mutate(
      bar_color = ifelse(median_combined_rf >= 0, "#1e3a28", "#7a2020")
    )
    plotly::plot_ly(comb, x = ~MgmtID, y = ~median_combined_rf,
                    type = "bar", marker = list(color = ~bar_color),
                    hovertemplate = "%{x}: %{y}<extra></extra>") |>
      plotly::layout(
        title = paste0("Combined weighted RF \u2014 year ", yr),
        xaxis = list(title = "", categoryorder = "trace"),
        yaxis = list(title = "Weighted effect on HVRA", zeroline = TRUE),
        plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff"
      )
  })

  # ── Time series plot (plotly): all treatments over time ───────────────
  # FIC1-6 are colored light red → dark red. Treatment MgmtIDs (when added
  # in a future sprint) will use greens/browns/blues — never red.
  output$rf_ts_plot <- plotly::renderPlotly({
    d <- rf_plot_data()
    if (is.null(d)) return(plotly::plotly_empty())
    comb <- d$combined |> dplyr::arrange(MgmtID, rel.time)
    mgmts <- unique(comb$MgmtID)

    # Build color map: FIC = red gradient, treatments = non-red palette
    fic_ids <- grep("^FIC", mgmts, value = TRUE) |> sort()
    fic_reds <- colorRampPalette(c("#ffb3b3", "#8b0000"))(max(length(fic_ids), 1))
    trt_ids <- setdiff(mgmts, fic_ids)
    trt_cols <- c("#1e3a28", "#2d6a4f", "#8b4513", "#4a6fa5", "#6b4c9a", "#2a7f62")
    mgmt_colors <- setNames(
      c(setNames(fic_reds, fic_ids),
        setNames(trt_cols[seq_along(trt_ids)], trt_ids)),
      c(fic_ids, trt_ids)
    )

    p <- plotly::plot_ly()
    for (mgmt in mgmts) {
      dd <- comb |> dplyr::filter(MgmtID == mgmt)
      p <- p |> plotly::add_trace(
        data = dd, x = ~rel.time, y = ~median_combined_rf,
        type = "scatter", mode = "lines+markers", name = mgmt,
        line = list(color = mgmt_colors[[mgmt]]),
        marker = list(color = mgmt_colors[[mgmt]]),
        hovertemplate = paste0(mgmt, " yr %{x}: %{y}<extra></extra>")
      )
    }
    p |> plotly::layout(
      title = "Combined weighted RF over time",
      xaxis = list(title = "Relative year"),
      yaxis = list(title = "Weighted effect on HVRA", zeroline = TRUE),
      plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
      hovermode = "x unified"
    )
  })

  # ── Full results table ─────────────────────────────────────────────────
  # Wide format: rows = MgmtID x rel.time, columns = per-EC RF + Combined.
  # Capped at MAX_DISPLAY_YEAR via rf_plot_data(); downloads keep all years.

  # Build the wide table from rf_plot_data (already capped at MAX_DISPLAY_YEAR)
  #
  # ── COLUMN RENAME — pending Kat sign-off ────────────────────────────────
  # The raw column headers from the FVS pipeline (e.g. "Stratum_1_Crown_Cover",
  # "QMD", "Surface_Herb", "MgmtID", "rel.time") are domain-precise but hard
  # to read for non-FVS-fluent reviewers. We swap them to the friendly labels
  # from R/ec_labels.R for the on-screen Per-EC RF breakdown table; the
  # downloaded CSV is unaffected (it reads from state$rf_results directly,
  # NOT from this reactive).
  #
  # To revert (e.g. if Kat prefers the raw FVS names for cross-referencing
  # with her source notebooks): delete the `dplyr::rename(...)` block below
  # and the `friendly_ec_names` block above it. Also revert the column-name
  # references in output$rf_preview_table (num_cols uses "Scenario" and
  # "Year" as exclusions; flip back to "MgmtID" and "rel.time").
  # ────────────────────────────────────────────────────────────────────────
  rf_table_wide <- reactive({
    d <- rf_plot_data()
    if (is.null(d)) return(NULL)
    wide <- d$per_ec |>
      tidyr::pivot_wider(names_from = EC, values_from = median_rf)
    joined <- wide |>
      dplyr::left_join(
        d$combined |>
          dplyr::mutate(Combined = median_combined_rf) |>
          dplyr::select(MgmtID, rel.time, Combined),
        by = c("MgmtID", "rel.time")
      ) |>
      dplyr::arrange(MgmtID, rel.time)

    # Swap raw FVS column names -> friendly EC labels via ec_labels lookup.
    # Defensive: only renames columns that exist in ec_labels$column; any
    # unrecognised EC keeps its raw header so we never drop data.
    friendly_ec_names <- setNames(ec_labels$label, ec_labels$column)
    ec_cols_present   <- intersect(names(joined), names(friendly_ec_names))
    names(joined)[match(ec_cols_present, names(joined))] <-
      friendly_ec_names[ec_cols_present]

    # Rename the metadata columns too for consistency with the primary RF
    # table card (which uses "Scenario" as the row identifier).
    joined |>
      dplyr::rename(Scenario = MgmtID, Year = rel.time)
  })

  # Populate the year dropdown from available display years.
  # Reads tbl$Year (renamed from rel.time in rf_table_wide). If the rename
  # is reverted per Kat's feedback, flip back to tbl$rel.time.
  observe({
    tbl <- rf_table_wide()
    if (is.null(tbl)) return()
    yrs <- sort(unique(tbl$Year))
    updateSelectInput(session, "rf_year_filter",
                      choices  = c("All", yrs),
                      selected = "All")
  })

  # Apply year filter to the table. Same Year/rel.time dependency note
  # applies as the observe block above.
  filtered_rf_tbl <- reactive({
    tbl <- rf_table_wide()
    if (is.null(tbl)) return(data.frame(Message = "No RF results yet"))
    yr <- input$rf_year_filter
    if (is.null(yr) || yr == "All") return(tbl)
    tbl |> dplyr::filter(Year == as.numeric(yr))
  })

  # Per-EC RF breakdown — class drops the "stripe" so the alternating-row
  # tint goes away (matches the design-system look). Negatives in the EC and
  # Combined value columns get the danger color via styleInterval (skips
  # Scenario and Year which aren't RF values; renamed from MgmtID/rel.time
  # in rf_table_wide() pending Kat sign-off).
  output$rf_preview_table <- DT::renderDataTable({
    tbl <- filtered_rf_tbl()
    if (is.null(tbl) || ncol(tbl) < 2) {
      return(DT::datatable(data.frame(Message = "No RF results yet"),
                           options = list(dom = "t"), rownames = FALSE,
                           class = "compact hover row-border"))
    }
    # Excluded names match the renames in rf_table_wide() (Scenario/Year).
    # If you revert that rename, flip these back to "MgmtID" and "rel.time".
    num_cols <- setdiff(names(tbl), c("Scenario", "Year"))
    dt <- DT::datatable(
      tbl,
      class    = "compact hover row-border",
      options  = list(dom = "tip", searching = FALSE, paging = TRUE,
                      pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    )
    if (length(num_cols) > 0) {
      dt <- DT::formatStyle(
        dt,
        columns = num_cols,
        color   = DT::styleInterval(c(-0.0001), c("#A8341E", "#14180F"))
      )
    }
    dt
  })

  # ── EC config table (download screen) ──────────────────────────────────
  # Same "compact hover row-border" class as the per-EC breakdown so both
  # DT tables share the design-system look.
  output$ec_config_table <- DT::renderDataTable({
    picks <- selected_ecs()
    if (length(picks) == 0) return(data.frame(Message = "No ECs selected"))
    wts <- state$ec_weights; fxs <- state$ec_effects; rngs <- state$ec_ranges
    df <- ec_labels |>
      filter(column %in% picks) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        Importance = as.integer(wts[[column]] %||% 3L),
        Effect     = fxs[[column]] %||% "Positive",
        Range      = if (!is.null(rngs[[column]]) && !all(is.na(rngs[[column]])))
                       paste(rngs[[column]], collapse = " \u2013 ") else "\u2014"
      ) |>
      dplyr::ungroup() |>
      select(EC = label, Category = subcategory, Importance, Effect, Range)
    DT::datatable(
      df,
      class    = "compact hover row-border",
      options  = list(dom = "t", pageLength = 50, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # ── Fact sheet preview ─────────────────────────────────────────────────
  output$factsheet_preview <- renderUI({
    nm <- input$hvra_name %||% "Unnamed HVRA"
    HTML(paste0(
      "<h3>", nm, "</h3>",
      "<p><strong>Ecoregion:</strong> ",
      if (length(state$selected_regions) > 0) paste(state$selected_regions, collapse = ", ") else "\u2014",
      "</p>",
      "<p><strong>Description:</strong> ", input$desc %||% "\u2014", "</p>",
      "<p><strong>Assumptions:</strong> ", input$assumptions %||% "\u2014", "</p>",
      "<p><strong>References:</strong> ", input$refs %||% "\u2014", "</p>"
    ))
  })


  # =========================================================================
  # Download handlers
  # =========================================================================
  # Each build_*() helper assembles a data frame or string; the downloadHandler
  # writes it to the temp file Shiny provides.

  # RF outputs CSV: per-EC median RF + combined score, wide format
  build_rf_csv <- function() {
    res <- state$rf_results
    if (is.null(res)) return(data.frame(Message = "No results"))
    wide <- res$per_ec |> tidyr::pivot_wider(names_from = EC, values_from = median_rf)
    wide |> dplyr::left_join(
      res$combined |> dplyr::select(MgmtID, rel.time, Combined_RF = median_combined_rf),
      by = c("MgmtID", "rel.time")
    )
  }

  # EC config CSV: one row per selected EC with user's weight/effect/range settings
  build_ec_csv <- function() {
    picks <- isolate(selected_ecs())
    if (length(picks) == 0) return(data.frame())
    wts <- state$ec_weights; fxs <- state$ec_effects; rngs <- state$ec_ranges
    data.frame(
      Column     = picks,
      Label      = vapply(picks, function(c) {
        r <- ec_labels$label[ec_labels$column == c]; if (length(r)) r[1] else c
      }, character(1)),
      Importance = vapply(picks, function(c) as.integer(wts[[c]] %||% 3L), integer(1)),
      Effect     = vapply(picks, function(c) fxs[[c]] %||% "Positive", character(1)),
      Min        = vapply(picks, function(c) if (!is.null(rngs[[c]])) rngs[[c]][1] else NA_real_, numeric(1)),
      Max        = vapply(picks, function(c) if (!is.null(rngs[[c]])) rngs[[c]][2] else NA_real_, numeric(1)),
      stringsAsFactors = FALSE
    )
  }

  # Fact sheet markdown: HVRA metadata + EC table
  build_factsheet <- function() {
    nm   <- input$hvra_name %||% "Unnamed HVRA"
    sci  <- input$hvra_sci %||% ""
    desc <- input$desc %||% ""
    eco  <- input$ecoregion_appl %||% ""
    auth <- input$authors_text %||% ""
    assu <- input$assumptions %||% ""
    refs <- input$refs %||% ""
    notes <- input$workshop_notes %||% ""
    ec_tbl <- build_ec_csv()
    ec_md <- if (nrow(ec_tbl) > 0) {
      paste0("| ", ec_tbl$Label, " | ", ec_tbl$Importance, " | ", ec_tbl$Effect, " |",
             collapse = "\n")
    } else "No ECs selected"
    paste(
      paste0("# ", nm),
      if (nzchar(sci)) paste0("*", sci, "*") else "",
      "", "## Description", desc,
      "", "## Ecoregion applicability", eco,
      "", "## Authors", auth,
      "", "## Assumptions", assu,
      "", "## EC Configuration",
      "| EC | Importance | Effect |", "| --- | --- | --- |", ec_md,
      "", "## References", refs,
      if (nzchar(notes)) paste("", "## Workshop Notes", notes) else "",
      sep = "\n"
    )
  }

  output$dl_rf <- downloadHandler(
    filename = function() paste0("rf_outputs_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(build_rf_csv(), file, row.names = FALSE)
  )
  output$dl_ec <- downloadHandler(
    filename = function() paste0("ec_config_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(build_ec_csv(), file, row.names = FALSE)
  )
  output$dl_factsheet <- downloadHandler(
    filename = function() paste0("factsheet_", Sys.Date(), ".md"),
    content  = function(file) writeLines(build_factsheet(), file)
  )
  output$dl_all <- downloadHandler(
    filename = function() paste0("rf_package_", Sys.Date(), ".zip"),
    content  = function(file) {
      tmpdir <- tempdir()
      rf_f <- file.path(tmpdir, "rf_outputs.csv")
      ec_f <- file.path(tmpdir, "ec_config.csv")
      fs_f <- file.path(tmpdir, "factsheet.md")
      write.csv(build_rf_csv(), rf_f, row.names = FALSE)
      write.csv(build_ec_csv(), ec_f, row.names = FALSE)
      writeLines(build_factsheet(), fs_f)
      zip(file, c(rf_f, ec_f, fs_f), flags = "-j")
    }
  )
}

shinyApp(ui, server)
