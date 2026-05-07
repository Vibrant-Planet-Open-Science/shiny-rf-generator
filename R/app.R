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

# Forest type and structure class lookup — loaded from S3 (~3KB).
# Used as global fallback for filter checkboxes; the actual choices are
# narrowed to AOI-specific values when freq_table is available.
all_vars_codes <- s3_read_rds(S3_ALL_VARS)

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

      // Chip toggle — pushes {group, value} to a single shared Shiny input.
      // priority:'event' ensures consecutive clicks on the same chip both fire
      // (Shiny otherwise dedupes identical values).
      document.addEventListener('click', function(e) {
        if (e.target.matches('.chip-toggle')) {
          Shiny.setInputValue('toggle_chip', {
            group: e.target.dataset.chipGroup,
            value: e.target.dataset.chipValue
          }, {priority: 'event'});
        }
      });

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
          # Apply chip-driven filters via the filtered_stands() reactive.
          # No more input$ft_filter / input$sc_filter — page 02 writes to
          # state$selected_forest_types / state$selected_structure_classes
          # which filtered_stands() reads from.
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
    # New: clear the species picker state so a fresh AOI starts clean.
    updateCheckboxGroupInput(session, "species_picks", selected = character())
    updateTextInput(session, "species_search", value = "")
  })

  # RF type card click handler (stand vs species)
  observeEvent(input$pick_rftype, { state$rf_type <- input$pick_rftype })

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
      tags$details(class = "teaching-block", open = NA,
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
                  "RESOLVE 2017 ecoregions \u00b7 click to select"),
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
      tags$details(class = "teaching-block", open = NA,
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
          h2(tags$strong("Whole-stand"), " ", tags$em("metrics.")),
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
    # Helper: renders one subcategory group of EC checkboxes
    render_group <- function(df, group_name) {
      if (nrow(df) == 0) return(NULL)
      # Input ID matches pattern used by EC_GROUP_IDS and selected_ecs()
      grp_id <- paste0("ecgrp_", tolower(gsub("[^a-z]", "_", tolower(group_name))))
      unit_part <- if ("unit" %in% names(df)) {
        ifelse(nzchar(df$unit), paste0(" (", df$unit, ")"), "")
      } else rep("", nrow(df))
      div(style = "margin-bottom:18px;",
          div(class = "section-label", group_name),
          checkboxGroupInput(
            grp_id, NULL,
            choiceNames  = paste0(df$label, unit_part),
            choiceValues = df$column
          )
      )
    }

    default_ecs  <- ec_labels |> filter(show_default == TRUE)
    advanced_ecs <- ec_labels |> filter(show_default == FALSE)
    default_groups  <- split(default_ecs,  default_ecs$subcategory)
    advanced_groups <- split(advanced_ecs, advanced_ecs$subcategory)

    div(class = "card",
        h2("Pick ecosystem components"),
        p("Select the ECs that matter for this RF. You'll set importance and effect type next.",
          class = "subtitle"),
        textInput("ec_search", NULL, placeholder = "\U0001F50D Search ECs (visual only for now)...",
                  width = "100%"),
        lapply(names(default_groups), function(g) render_group(default_groups[[g]], g)),
        tags$hr(),
        checkboxInput("show_advanced", "Show advanced ECs", value = FALSE),
        # conditionalPanel uses client-side JS — no server round-trip
        conditionalPanel(
          condition = "input.show_advanced == true",
          lapply(names(advanced_groups), function(g) render_group(advanced_groups[[g]], g))
        ),
        div(class = "navbar",
            actionButton("btn_back", "\u2190 Back", class = "btn btn-outline-secondary btn-lg"),
            actionButton("btn_next", "Next \u2192", class = "btn btn-primary btn-lg")
        )
    )
  }

  # ── Screen 7: Weights & effects ────────────────────────────────────────
  # One row per selected EC with:
  #   - EC label + subcategory badge
  #   - Importance dropdown (1-5, default 3)
  #   - Effect type radio: Positive / Negative / Range
  #   - Conditional min/max inputs when Range is selected
  # Input IDs use a "safe" version of the column name (non-alphanumeric → _).
  render_weights <- function() {
    picks <- isolate(selected_ecs())
    if (length(picks) == 0) {
      body <- div(style = "color:#7a8a75;font-style:italic;padding:20px 0;",
                  "No ECs selected. Go back and pick at least one.")
    } else {
      ec_rows <- lapply(picks, function(col) {
        info <- ec_labels[ec_labels$column == col, ]
        lbl  <- if (nrow(info) > 0) info$label[1] else col
        cat  <- if (nrow(info) > 0) info$subcategory[1] else ""
        # Safe ID: replace non-alphanumeric chars with _ (for Shiny input IDs)
        safe <- gsub("[^a-zA-Z0-9]", "_", col)
        div(style = "border:1px solid #e0d4b8;border-radius:8px;padding:14px 18px;margin-bottom:10px;background:#fff;",
            fluidRow(
              column(4,
                     bslib::tooltip(
                       trigger = tags$span(
                         tags$strong(lbl),
                         tags$i(class = "bi bi-info-circle ms-1", style = "opacity: 0.6;")
                       ),
                       ec_tooltip(col)
                     ),
                     # Subcategory badge
                     tags$span(style = "display:inline-block;background:#e8dcc8;color:#4a3c2a;font-size:11px;padding:2px 8px;border-radius:10px;margin-left:8px;", cat)
              ),
              column(2,
                     div(class = "section-label", "Importance"),
                     selectInput(paste0("wt_", safe), NULL, choices = 1:5, selected = 3, width = "80px")
              ),
              column(3,
                     div(class = "section-label", "Effect"),
                     radioButtons(paste0("fx_", safe), NULL,
                                  choices = c("Positive", "Negative", "Range"),
                                  selected = "Positive", inline = TRUE)
              ),
              column(3,
                     # Range min/max inputs — only visible when "Range" is selected
                     conditionalPanel(
                       condition = sprintf("input.fx_%s == 'Range'", safe),
                       div(class = "section-label", "Range"),
                       fluidRow(
                         column(6, numericInput(paste0("rng_min_", safe), "Min", value = NA, width = "100%")),
                         column(6, numericInput(paste0("rng_max_", safe), "Max", value = NA, width = "100%"))
                       )
                     )
              )
            )
        )
      })
      body <- do.call(tagList, ec_rows)
    }
    div(class = "card",
        h2("Weights & effects"),
        p("Set importance (1 = low, 5 = critical, ties allowed) and effect direction for each EC.",
          class = "subtitle"),
        body,
        div(class = "navbar",
            actionButton("btn_back", "\u2190 Back",           class = "btn btn-outline-secondary btn-lg"),
            actionButton("btn_next", "Compute RFs \u2192",    class = "btn btn-primary btn-lg")
        )
    )
  }

  # ── Screen 8: Review & download ────────────────────────────────────────
  # Three tabs: RF outputs (bar + timeseries plots + table), EC config, fact sheet.
  # Four download buttons: RF CSV, EC config CSV, fact sheet MD, zip of all three.
  render_download <- function() {
    nm      <- input$hvra_name %||% "Unnamed HVRA"
    sci     <- input$hvra_sci %||% ""
    name_str <- if (nzchar(sci)) paste0(nm, " (", sci, ")") else nm
    div(class = "card",
        h2("Review & download"),
        p(name_str, class = "subtitle"),
        tabsetPanel(type = "tabs",
                    # -- RF outputs tab: plotly bar + timeseries, then data table --
                    tabPanel("RF outputs",
                             div(style = "padding:20px 0;",
                                 tabsetPanel(id = "rf_plot_tabs", type = "pills",
                                   tabPanel("Bar plot",
                                     div(style = "padding:12px 0;",
                                         fluidRow(
                                           column(4, uiOutput("rf_year_selector")),
                                           column(8)
                                         ),
                                         plotly::plotlyOutput("rf_bar_plot", height = "350px")
                                     )
                                   ),
                                   tabPanel("Time series",
                                     div(style = "padding:12px 0;",
                                         plotly::plotlyOutput("rf_ts_plot", height = "400px")
                                     )
                                   )
                                 ),
                                 tags$hr(),
                                 tags$details(class = "details-wrap",
                                   tags$summary("RF table"),
                                   div(style = "padding-top:12px;",
                                       selectInput("rf_year_filter", label = "View year:",
                                                   choices = NULL, selected = "All",
                                                   width = "150px"),
                                       DT::dataTableOutput("rf_preview_table")
                                   )
                                 )
                             )
                    ),
                    # -- EC configuration tab --
                    tabPanel("EC configuration",
                             div(style = "padding:20px 0;", DT::dataTableOutput("ec_config_table"))
                    ),
                    # -- Fact sheet preview tab --
                    tabPanel("Fact sheet",
                             div(style = "padding:20px 0;", uiOutput("factsheet_preview"))
                    )
        ),
        # Download buttons row
        div(style = "margin-top:20px;display:flex;gap:10px;flex-wrap:wrap;",
            downloadButton("dl_rf",        "RF outputs (CSV)",     class = "btn btn-outline-secondary"),
            downloadButton("dl_ec",        "EC config (CSV)",      class = "btn btn-outline-secondary"),
            downloadButton("dl_factsheet", "Fact sheet (md)",      class = "btn btn-outline-secondary"),
            downloadButton("dl_all",       "Download all (zip)",   class = "btn btn-primary")
        ),
        div(class = "navbar",
            actionButton("btn_back",  "\u2190 Back",          class = "btn btn-outline-secondary btn-lg"),
            actionButton("btn_reset", "Start a new RF", class = "btn btn-outline-danger btn-lg")
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

  # Initialize filter state from AOI freq_table whenever it changes.
  # On AOI selection: all forest types and structure classes start selected
  # (the "skip filters" default). On AOI clear: state resets to NULL.
  # NOTE: changing the AOI wipes any filter selections the user made — this
  # is a known interim behavior; smarter merging is a future polish.
  observeEvent(state$freq_table, {
    ft <- state$freq_table
    if (is.null(ft)) {
      state$selected_forest_types     <- NULL
      state$selected_structure_classes <- NULL
      return()
    }
    if ("Forest_Type" %in% names(ft)) {
      state$selected_forest_types <- sort(unique(trimws(ft$Forest_Type)))
    }
    if ("Structure_Class_Description" %in% names(ft)) {
      state$selected_structure_classes <-
        sort(unique(trimws(ft$Structure_Class_Description)))
    }
  }, ignoreNULL = FALSE)

  # Chip toggle handler — single input shared by all chips on the page.
  # JS pushes {group: "ft"|"sc", value: <chip value>} via Shiny.setInputValue
  # with priority:"event" so consecutive clicks on the same chip both fire.
  observeEvent(input$toggle_chip, {
    grp <- input$toggle_chip$group
    val <- input$toggle_chip$value
    if (grp == "ft") {
      cur <- state$selected_forest_types %||% character()
      state$selected_forest_types <-
        if (val %in% cur) setdiff(cur, val) else c(cur, val)
    } else if (grp == "sc") {
      cur <- state$selected_structure_classes %||% character()
      state$selected_structure_classes <-
        if (val %in% cur) setdiff(cur, val) else c(cur, val)
    }
  })

  # Reset filters back to all-selected (used by both the empty-state recovery
  # button and the Skip filters affordance in the page footer).
  observeEvent(input$btn_filters_reset, {
    ft <- state$freq_table
    if (is.null(ft)) return()
    if ("Forest_Type" %in% names(ft))
      state$selected_forest_types <- sort(unique(trimws(ft$Forest_Type)))
    if ("Structure_Class_Description" %in% names(ft))
      state$selected_structure_classes <-
        sort(unique(trimws(ft$Structure_Class_Description)))
  })

  # Skip filters → reset, then advance to review screen.
  observeEvent(input$btn_filters_skip, {
    ft <- state$freq_table
    if (is.null(ft)) return()
    if ("Forest_Type" %in% names(ft))
      state$selected_forest_types <- sort(unique(trimws(ft$Forest_Type)))
    if ("Structure_Class_Description" %in% names(ft))
      state$selected_structure_classes <-
        sort(unique(trimws(ft$Structure_Class_Description)))
    nav_to("review")
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
      " is found. The defaults \u2014 leaving everything selected \u2014 ",
      "include all ",
      tags$strong(format(total_stands, big.mark = ",")),
      " stands in your AOI.")
  })

  # Helper: render one chip section (label + count + chip grid).
  # Used by both the forest type and structure class outputs below.
  render_chip_section <- function(label, group_key, available, selected) {
    chips <- lapply(available, function(v) {
      cls <- paste("chip chip-toggle", if (v %in% selected) "chip-orange" else "")
      tags$button(
        class = cls,
        `data-chip-group` = group_key,
        `data-chip-value` = v,
        v
      )
    })
    div(
      div(class = "section-controls",
        div(class = "section-controls-label", label),
        div(class = "section-controls-count",
          tags$strong(length(selected)), " of ", tags$strong(length(available)),
          " selected"
        )
      ),
      div(class = "chip-grid", chips)
    )
  }


  # Filtered stands reactive — single source of truth for the post-filter
  # subset of the AOI. Used by the count readout on page 02, the review
  # screen on page 03, and the data-load handler on btn_next.
  #
  # Filter is "active" when the user's selection is NOT the full available
  # set. Three cases:
  #   - all chips selected:    no filter, NAs preserved, count = full
  #   - partial selection:     filter applied, NAs dropped, count = subset
  #   - zero chips selected:   filter applied to empty set → 0 stands
  #                            (triggers the empty-state recovery box)
  filtered_stands <- reactive({
    ft <- state$freq_table
    if (is.null(ft)) return(NULL)

    sel_ft <- state$selected_forest_types     %||% character()
    sel_sc <- state$selected_structure_classes %||% character()
    avail_ft <- available_forest_types()
    avail_sc <- available_structure_classes()

    if ("Forest_Type" %in% names(ft) && length(sel_ft) < length(avail_ft)) {
      ft <- ft |> dplyr::filter(trimws(Forest_Type) %in% sel_ft)
    }
    if ("Structure_Class_Description" %in% names(ft) && length(sel_sc) < length(avail_sc)) {
      ft <- ft |> dplyr::filter(trimws(Structure_Class_Description) %in% sel_sc)
    }
    ft
  })


  output$filters_ft_section <- renderUI({
    render_chip_section(
      label     = "Forest type",
      group_key = "ft",
      available = available_forest_types(),
      selected  = state$selected_forest_types %||% character()
    )
  })

  output$filters_sc_section <- renderUI({
    render_chip_section(
      label     = "Structure class",
      group_key = "sc",
      available = available_structure_classes(),
      selected  = state$selected_structure_classes %||% character()
    )
  })

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

  # Page footer — status copy + Continue + Skip filters affordance.
  output$filters_page_footer <- renderUI({
    n_filtered <- nrow(filtered_stands() %||% data.frame())
    n_total    <- nrow(state$freq_table %||% data.frame())
    all_selected <- (n_filtered == n_total) && n_total > 0

    status_text <- if (n_filtered == 0)         paste0("0 stands match \u00b7 adjust filters")
                   else if (all_selected)       paste0("All ", format(n_total, big.mark=","), " stands included")
                   else                         paste0(format(n_filtered, big.mark=","), " stands ready")

    btn <- tags$button(
      id = "btn_next", type = "button",
      class = "btn btn-continue action-button",
      disabled = if (n_filtered > 0) NULL else "disabled",
      tags$span(class = "action-label", "Continue to review \u2192")
    )

    div(class = "page-footer",
      div(class = "page-footer-status",
        div(class = "page-footer-status-mark"),
        tags$span(status_text)
      ),
      div(class = "page-footer-actions",
        actionButton("btn_filters_skip", "Skip filters \u2192",
                     class = "skip-link"),
        btn
      )
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

    # Filters row: read-only chips (or condensed "All ..." text when full set)
    ft_sel   <- state$selected_forest_types     %||% character()
    sc_sel   <- state$selected_structure_classes %||% character()
    ft_avail <- available_forest_types()
    sc_avail <- available_structure_classes()
    ft_full  <- length(ft_sel) == length(ft_avail) && length(ft_avail) > 0
    sc_full  <- length(sc_sel) == length(sc_avail) && length(sc_avail) > 0

    filters_value <- if (ft_full && sc_full) {
      tags$span(class = "summary-row-value",
                "All forest types \u00b7 all structure classes")
    } else {
      tagList(
        if (!ft_full) div(class = "chip-grid", style = "margin-bottom: 0.5rem;",
          lapply(ft_sel, function(v) tags$span(class = "chip chip-orange", v))
        ),
        if (!sc_full) div(class = "chip-grid",
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

    btn <- tags$button(
      id = "btn_next", type = "button",
      class = "btn btn-continue action-button",
      tags$span(class = "action-label", "Load data \u2192")
    )

    div(class = "page-footer",
      div(class = "page-footer-status",
        div(class = "page-footer-status-mark"),
        tags$span(status_text)
      ),
      div(class = "page-footer-actions", btn)
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
  rf_table_wide <- reactive({
    d <- rf_plot_data()
    if (is.null(d)) return(NULL)
    wide <- d$per_ec |>
      tidyr::pivot_wider(names_from = EC, values_from = median_rf)
    wide |>
      dplyr::left_join(
        d$combined |>
          dplyr::mutate(Combined = median_combined_rf) |>
          dplyr::select(MgmtID, rel.time, Combined),
        by = c("MgmtID", "rel.time")
      ) |>
      dplyr::arrange(MgmtID, rel.time)
  })

  # Populate the year dropdown from available display years
  observe({
    tbl <- rf_table_wide()
    if (is.null(tbl)) return()
    yrs <- sort(unique(tbl$rel.time))
    updateSelectInput(session, "rf_year_filter",
                      choices  = c("All", yrs),
                      selected = "All")
  })

  # Apply year filter to the table
  filtered_rf_tbl <- reactive({
    tbl <- rf_table_wide()
    if (is.null(tbl)) return(data.frame(Message = "No RF results yet"))
    yr <- input$rf_year_filter
    if (is.null(yr) || yr == "All") return(tbl)
    tbl |> dplyr::filter(rel.time == as.numeric(yr))
  })

  output$rf_preview_table <- DT::renderDataTable(
    filtered_rf_tbl(),
    options = list(dom = "tip", searching = FALSE, paging = TRUE,
                   pageLength = 25, scrollX = TRUE),
    rownames = FALSE
  )

  # ── EC config table (download screen) ──────────────────────────────────
  output$ec_config_table <- DT::renderDataTable({
    picks <- selected_ecs()
    if (length(picks) == 0) return(data.frame(Message = "No ECs selected"))
    wts <- state$ec_weights; fxs <- state$ec_effects; rngs <- state$ec_ranges
    ec_labels |>
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
  }, options = list(dom = 't', pageLength = 50), rownames = FALSE)

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
