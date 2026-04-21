source(here::here("R", "functions.R"))
source(here::here("R", "ec_labels.R"))

library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(dplyr)
library(aws.s3)
library(purrr)

library(shiny)
library(leaflet)
library(dplyr)
library(tibble)
library(tidyr)
library(DT)


# ─────────────────────────────────────────────────────────────────────────────
# Constants & helpers
# ─────────────────────────────────────────────────────────────────────────────

SCREENS <- c("aoi", "filters", "review", "rftype", "species", "ecs", "weights", "download")
STEP_LABELS <- c(aoi="AOI", filters="Filters", review="Review", rftype="RF Type",
                 ecs="ECs", weights="Weights", download="Download")
BREADCRUMB_ORDER <- c("aoi", "filters", "review", "rftype", "ecs", "weights", "download")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

# Pre-compute EC checkbox group IDs (matches IDs generated in render_ecs)
EC_GROUP_IDS <- unique(ec_labels$subcategory) |>
  tolower() |>
  (\(x) gsub("[^a-z]", "_", x))() |>
  (\(x) paste0("ecgrp_", x))()

# Ecoregion polygons for map selection (~1.8MB, RESOLVE 2017 simplified)
ecoregions_sf <- sf::st_read(here::here("www", "ecoregions_western.geojson"), quiet = TRUE)
# Western counties shapefile for ecoregion → GEOID intersection
counties_sf <- sf::st_read(here::here("data", "tl_2024_western_counties.gpkg"), quiet = TRUE)
# CA counties for variant detection
ca_county_geoids <- readRDS(here::here("data", "ca_counties.rds"))$GEOID

# Load real filter lookups from S3 (small file, ~3KB)
all_vars_codes <- aws.s3::s3readRDS(bucket = S3_BUCKET, object = S3_ALL_VARS)

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

# Stand metadata lookup (used by get_tm_ids for AOI → StandID join)
stand_lookup <- readRDS(here::here("data", "unique_stand_data.rds"))
stand_lookup <- stand_lookup[complete.cases(stand_lookup), ]
mock_species           <- c("Pinus ponderosa (PP)","Pseudotsuga menziesii (DF)",
                            "Pinus albicaulis (WB)","Populus tremuloides (AS)")

# ─────────────────────────────────────────────────────────────────────────────
# Inline styles
# ─────────────────────────────────────────────────────────────────────────────

app_css <- "
  body { background: #faf5ed; font-family: 'Helvetica Neue', Arial, sans-serif; color: #1e3a28; }
  .app-container { max-width: 1000px; margin: 0 auto; padding: 20px; }
  .hdr { background: #1e3a28; color: #faf5ed; padding: 16px 20px; border-radius: 6px; margin-bottom: 12px; }
  .hdr input[type='text'] { background: #fff; border: 1px solid #4a5c4e; border-radius: 4px; padding: 8px; }
  .crumb { font-family: 'Courier New', monospace; font-size: 12px; text-align: right; padding-top: 8px; }
  .crumb .done { color: #c9b88a; }
  .crumb .active { color: #faf5ed; font-weight: 600; }
  .crumb .todo { color: #7a8a75; }
  .details-wrap { background: #fff; border: 1px solid #e0d4b8; border-radius: 6px;
                  padding: 10px 14px; margin-bottom: 16px; }
  .details-wrap summary { cursor: pointer; font-weight: 500; color: #4a3c2a; }
  .card { background: #fff; border: 1px solid #e0d4b8; border-radius: 10px;
          padding: 28px; box-shadow: 0 1px 3px rgba(0,0,0,0.06); margin-bottom: 20px; }
  .card h2 { font-family: Georgia, serif; color: #1e3a28; margin-top: 0; margin-bottom: 4px; font-size: 26px; }
  .card .subtitle { color: #7a8a75; margin-bottom: 20px; font-size: 14px; }
  .section-label { font-family: 'Courier New', monospace; font-size: 11px;
                   text-transform: uppercase; color: #8b4513; letter-spacing: 0.04em;
                   margin-top: 16px; margin-bottom: 8px; }
  .rf-choice { border: 1px solid #e0d4b8; border-radius: 8px; padding: 24px;
               cursor: pointer; background: #fff; min-height: 160px; transition: all 0.15s; }
  .rf-choice:hover { border-color: #1e3a28; }
  .rf-choice.selected { border: 3px solid #1e3a28; padding: 22px; }
  .rf-choice h3 { font-family: Georgia, serif; color: #1e3a28; margin-top: 0; }
  .rf-choice p { color: #4a3c2a; margin-bottom: 0; font-size: 14px; }
  .navbar { display: flex; justify-content: space-between; margin-top: 30px; }
  .btn-primary { background: #1e3a28 !important; border-color: #1e3a28 !important; color: white !important; }
  .btn-primary:hover { background: #2d5a3d !important; border-color: #2d5a3d !important; }
  .btn-outline-secondary { color: #4a3c2a !important; border-color: #c9b88a !important; background: transparent !important; }
  .status-ok { color: #1e3a28; margin-top: 10px; font-weight: 500; }
"

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('scroll_top', function(m) { window.scrollTo(0, 0); });
    "))
  ),
  div(class = "app-container",
      
      # Persistent header
      div(class = "hdr",
          fluidRow(
            column(5,
                   tags$input(id = "hvra_name", type = "text",
                              placeholder = "HVRA common name (required)",
                              style = "width:100%;margin-bottom:6px;",
                              onchange = "Shiny.setInputValue('hvra_name', this.value);"),
                   tags$input(id = "hvra_sci", type = "text",
                              placeholder = "Scientific name (optional)",
                              style = "width:100%;",
                              onchange = "Shiny.setInputValue('hvra_sci', this.value);")
            ),
            column(7, div(class = "crumb", uiOutput("breadcrumb", inline = TRUE)))
          )
      ),
      
      # Details drawer
      tags$details(class = "details-wrap",
                   tags$summary("Details (description, authors, assumptions…)"),
                   div(style = "padding-top:12px;",
                       textAreaInput("desc", "HVRA description", rows = 2, width = "100%"),
                       textAreaInput("ecoregion_appl", "Ecoregion applicability", rows = 2, width = "100%"),
                       textAreaInput("authors_text", "Authors (one per line: name, affiliation)", rows = 3, width = "100%"),
                       textAreaInput("assumptions", "Assumptions", rows = 3, width = "100%",
                                     placeholder = "What assumptions were made?"),
                       textAreaInput("workshop_notes", "Workshop notes", rows = 2, width = "100%"),
                       textAreaInput("refs", "References", rows = 2, width = "100%")
                   )
      ),
      
      uiOutput("main_screen")
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# Server
# ─────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  state <- reactiveValues(
    screen           = "aoi",
    aoi_method       = NULL,
    selected_regions = character(),
    aoi_stands       = NULL,
    freq_table       = NULL,
    variant          = NULL,
    rf_type          = NULL,
    selected_species = character(),
    selected_ecs     = character(),
    ec_weights       = list(),
    ec_effects       = list(),
    ec_ranges        = list(),
    species_list     = NULL,
    filtered_data    = NULL,
    rf_results       = NULL
  )
  
  # Reactive that only depends on the specific ecgrp_* checkbox inputs,

  # NOT on every input in the app (fixes flicker on EC picker screen).
  selected_ecs <- reactive({
    ecs <- unlist(lapply(EC_GROUP_IDS, function(id) input[[id]]))
    unique(ecs)
  })
  
  # Navigation ----------------------------------------------------------------
  nav_to <- function(s) {
    state$screen <- s
    session$sendCustomMessage("scroll_top", list())
  }
  
  observeEvent(input$btn_next, {
    s <- state$screen

    # Load filtered StandLevel data when leaving review screen
    if (s == "review" && is.null(state$filtered_data)) {
      req(state$variant, state$freq_table)
      tryCatch({
        withProgress(message = "Loading stand data (this may take a minute)...", {
          stand_raw <- load_stand_data(state$variant)
          # Apply filter selections to freq_table
          ft <- state$freq_table
          if (length(input$ft_filter) > 0 && "Forest_Type" %in% names(ft))
            ft <- ft |> dplyr::filter(trimws(Forest_Type) %in% input$ft_filter)
          if (length(input$sc_filter) > 0 && "Structure_Class_Description" %in% names(ft))
            ft <- ft |> dplyr::filter(trimws(Structure_Class_Description) %in% input$sc_filter)
          state$filtered_data <- get_filtered_stand_data(stand_raw, ft)
        })
      }, error = function(e) {
        showNotification(paste("Failed to load stand data:", e$message),
                         type = "error", duration = 10)
      })
    }

    if (s == "rftype" && isTRUE(state$rf_type == "stand")) { nav_to("ecs"); return() }

    # Load species list when entering species screen (first time only)
    if (s == "rftype" && isTRUE(state$rf_type == "species") && is.null(state$species_list)) {
      tryCatch({
        withProgress(message = "Loading species data (this may take a minute)...", {
          stdstk <- load_stdstk_data(state$variant)
          aoi_stand_ids <- as.character(state$freq_table$StandID)
          stdstk_filtered <- stdstk |>
            dplyr::filter(as.character(StandID) %in% aoi_stand_ids)
          species <- sort(unique(stdstk_filtered$Species))
          species <- species[!species %in% c("All", "")]
          state$species_list <- species
        })
      }, error = function(e) {
        showNotification(paste("Failed to load species:", e$message),
                         type = "error", duration = 10)
      })
    }

    # Collect weights/effects/ranges when leaving weights screen
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

      # Compute RFs
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
          treatment_ids <- mgmt_labels$mgmt_id[mgmt_labels$type != "baseline"]
          state$rf_results <- compute_combined_rf(
            state$filtered_data, ec_conf, treatment_ids
          )
        })
      }, error = function(e) {
        showNotification(paste("RF computation failed:", e$message),
                         type = "error", duration = 10)
      })
    }

    idx <- which(SCREENS == s)
    if (length(idx) && idx < length(SCREENS)) nav_to(SCREENS[idx + 1])
  })
  
  observeEvent(input$btn_back, {
    s <- state$screen
    if (s == "ecs" && isTRUE(state$rf_type == "stand")) { nav_to("rftype"); return() }
    idx <- which(SCREENS == s)
    if (length(idx) && idx > 1) nav_to(SCREENS[idx - 1])
  })
  
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
  })
  
  observeEvent(input$pick_rftype, { state$rf_type <- input$pick_rftype })
  
  # Breadcrumb ----------------------------------------------------------------
  output$breadcrumb <- renderUI({
    cur <- state$screen
    cur_idx <- which(BREADCRUMB_ORDER == cur)
    if (length(cur_idx) == 0) cur_idx <- 1
    parts <- lapply(seq_along(BREADCRUMB_ORDER), function(i) {
      key <- BREADCRUMB_ORDER[i]
      cls <- if (i < cur_idx) "done" else if (i == cur_idx) "active" else "todo"
      tags$span(class = cls, STEP_LABELS[[key]])
    })
    seps <- rep(list(tags$span(" › ", class = "todo")), length(parts) - 1)
    tagList(c(rbind(parts[-length(parts)], seps), list(parts[[length(parts)]])))
  })
  
  # ── SCREEN RENDERERS ─────────────────────────────────────────────────────
  # Defined inside server so they can see `state` and helpers via lexical scope.
  
  render_aoi <- function() {
    div(class = "card",
        h2("Define your area of interest"),
        p("Upload a boundary file or select ecoregions on the map.", class = "subtitle"),
        tabsetPanel(id = "aoi_tabs", type = "tabs",
                    tabPanel("Upload boundary file",
                             div(style = "padding:20px 0;",
                                 fileInput("aoi_file", "Choose .gpkg or .tif file",
                                           accept = c(".gpkg", ".tif"), width = "100%"),
                                 uiOutput("aoi_file_status")
                             )
                    ),
                    tabPanel("Select on map",
                             div(style = "padding:12px 0;",
                                 leafletOutput("ecoregion_map", height = "400px"),
                                 div(style = "margin-top:12px;color:#4a3c2a;font-size:13px;",
                                     uiOutput("selected_regions_text"))
                             )
                    )
        ),
        div(class = "navbar",
            div(),
            actionButton("btn_next", "Next →", class = "btn btn-primary btn-lg")
        )
    )
  }
  
  render_filters <- function() {
    ft <- isolate(state$freq_table)
    if (!is.null(ft) && "Forest_Type" %in% names(ft)) {
      ft_choices <- sort(unique(trimws(ft$Forest_Type)))
      ft_choices <- ft_choices[!is.na(ft_choices) & nzchar(ft_choices)]
    } else {
      ft_choices <- forest_types
    }
    if (!is.null(ft) && "Structure_Class_Description" %in% names(ft)) {
      sc_choices <- sort(unique(trimws(ft$Structure_Class_Description)))
      sc_choices <- sc_choices[!is.na(sc_choices) & nzchar(sc_choices)]
    } else {
      sc_choices <- structure_classes
    }
    div(class = "card",
        h2("Filter stands"),
        p("Narrow to relevant forest types and structure classes. Leave blank to include all.",
          class = "subtitle"),
        fluidRow(
          column(6,
                 div(class = "section-label", "Forest type"),
                 checkboxGroupInput("ft_filter", NULL, choices = ft_choices)
          ),
          column(6,
                 div(class = "section-label", "Structure class"),
                 checkboxGroupInput("sc_filter", NULL, choices = sc_choices)
          )
        ),
        div(class = "navbar",
            actionButton("btn_back", "← Back",  class = "btn btn-outline-secondary btn-lg"),
            actionButton("btn_next", "Next →",  class = "btn btn-primary btn-lg")
        )
    )
  }
  
  render_review <- function() {
    div(class = "card",
        h2("Review"),
        p("Confirm what data will be used before loading.", class = "subtitle"),
        tableOutput("review_summary"),
        uiOutput("low_stand_warning"),
        div(class = "navbar",
            actionButton("btn_back", "← Back", class = "btn btn-outline-secondary btn-lg"),
            actionButton("btn_next", "Looks good — load data →", class = "btn btn-primary btn-lg")
        )
    )
  }
  
  render_rftype <- function() {
    selected <- state$rf_type
    cls_stand   <- paste("rf-choice", if (identical(selected, "stand"))   "selected" else "")
    cls_species <- paste("rf-choice", if (identical(selected, "species")) "selected" else "")
    div(class = "card",
        h2("What is this RF for?"),
        p("Choose the type of response function you're building.", class = "subtitle"),
        fluidRow(
          column(6, div(class = cls_stand,
                        onclick = "Shiny.setInputValue('pick_rftype', 'stand', {priority: 'event'});",
                        h3("Stand / habitat characteristics"),
                        p("Canopy cover, tree diameter, basal area, shrub density — describing forest structure.")
          )),
          column(6, div(class = cls_species,
                        onclick = "Shiny.setInputValue('pick_rftype', 'species', {priority: 'event'});",
                        h3("Individual tree species"),
                        p("Stand metrics plus species-specific live TPA and live BA for one or more target species.")
          ))
        ),
        div(class = "navbar",
            actionButton("btn_back", "← Back", class = "btn btn-outline-secondary btn-lg"),
            actionButton("btn_next", "Next →", class = "btn btn-primary btn-lg")
        )
    )
  }
  
  render_species <- function() {
    sp <- isolate(state$species_list) %||% mock_species
    div(class = "card",
        h2("Select target species"),
        p("Choose one or more species present in your AOI. Species-specific metrics will be added to the EC list.",
          class = "subtitle"),
        if (length(sp) == 0) {
          p("No species found in your AOI.", style = "color:#7a8a75;font-style:italic;")
        } else {
          checkboxGroupInput("species_picks", NULL, choices = sp)
        },
        div(class = "navbar",
            actionButton("btn_back", "← Back", class = "btn btn-outline-secondary btn-lg"),
            actionButton("btn_next", "Next →", class = "btn btn-primary btn-lg")
        )
    )
  }
  
  render_ecs <- function() {
    render_group <- function(df, group_name) {
      if (nrow(df) == 0) return(NULL)
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
        textInput("ec_search", NULL, placeholder = "🔍 Search ECs (visual only for now)...",
                  width = "100%"),
        lapply(names(default_groups), function(g) render_group(default_groups[[g]], g)),
        tags$hr(),
        checkboxInput("show_advanced", "Show advanced ECs", value = FALSE),
        conditionalPanel(
          condition = "input.show_advanced == true",
          lapply(names(advanced_groups), function(g) render_group(advanced_groups[[g]], g))
        ),
        div(class = "navbar",
            actionButton("btn_back", "← Back", class = "btn btn-outline-secondary btn-lg"),
            actionButton("btn_next", "Next →", class = "btn btn-primary btn-lg")
        )
    )
  }
  
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
        safe <- gsub("[^a-zA-Z0-9]", "_", col)
        div(style = "border:1px solid #e0d4b8;border-radius:8px;padding:14px 18px;margin-bottom:10px;background:#fff;",
            fluidRow(
              column(4,
                     tags$strong(lbl),
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
            actionButton("btn_back", "← Back",           class = "btn btn-outline-secondary btn-lg"),
            actionButton("btn_next", "Compute RFs →",    class = "btn btn-primary btn-lg")
        )
    )
  }
  
  render_download <- function() {
    nm      <- input$hvra_name %||% "Unnamed HVRA"
    sci     <- input$hvra_sci %||% ""
    name_str <- if (nzchar(sci)) paste0(nm, " (", sci, ")") else nm
    div(class = "card",
        h2("Review & download"),
        p(name_str, class = "subtitle"),
        tabsetPanel(type = "tabs",
                    tabPanel("RF outputs",
                             div(style = "padding:20px 0;",
                                 p("Weighted RFs across all MgmtIDs and timepoints.", style = "color:#7a8a75;"),
                                 plotOutput("rf_preview_plot", height = "300px"),
                                 br(),
                                 DT::dataTableOutput("rf_preview_table")
                             )
                    ),
                    tabPanel("EC configuration",
                             div(style = "padding:20px 0;", DT::dataTableOutput("ec_config_table"))
                    ),
                    tabPanel("Fact sheet",
                             div(style = "padding:20px 0;", uiOutput("factsheet_preview"))
                    )
        ),
        div(style = "margin-top:20px;display:flex;gap:10px;flex-wrap:wrap;",
            downloadButton("dl_rf",        "RF outputs (CSV)",     class = "btn btn-outline-secondary"),
            downloadButton("dl_ec",        "EC config (CSV)",      class = "btn btn-outline-secondary"),
            downloadButton("dl_factsheet", "Fact sheet (md)",      class = "btn btn-outline-secondary"),
            downloadButton("dl_all",       "Download all (zip)",   class = "btn btn-primary")
        ),
        div(class = "navbar",
            actionButton("btn_back",  "← Back",          class = "btn btn-outline-secondary btn-lg"),
            actionButton("btn_reset", "Start a new RF", class = "btn btn-outline-danger btn-lg")
        )
    )
  }
  
  # ── Screen router ─────────────────────────────────────────────────────────
  output$main_screen <- renderUI({
    switch(state$screen,
           "aoi"      = render_aoi(),
           "filters"  = render_filters(),
           "review"   = render_review(),
           "rftype"   = render_rftype(),
           "species"  = render_species(),
           "ecs"      = render_ecs(),
           "weights"  = render_weights(),
           "download" = render_download(),
           div("Unknown screen:", state$screen)
    )
  })
  
  # ── Map + AOI ────────────────────────────────────────────────────────────
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

  observeEvent(input$ecoregion_map_shape_click, {
    clicked <- input$ecoregion_map_shape_click$id
    if (is.null(clicked) || clicked == "") return()
    cur <- state$selected_regions
    if (clicked %in% cur) {
      state$selected_regions <- setdiff(cur, clicked)
    } else {
      state$selected_regions <- c(cur, clicked)
    }

    # Redraw polygons with selection highlight
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

    # Build freq_table from selected ecoregions
    if (length(sel) > 0) {
      tryCatch({
        withProgress(message = "Finding stands in selected ecoregions...", {
          eco_sel <- ecoregions_sf |>
            dplyr::filter(ECO_NAME %in% sel) |>
            sf::st_union() |> sf::st_sf() |>
            sf::st_transform(sf::st_crs(counties_sf))

          aoi_counties <- suppressWarnings(sf::st_intersection(counties_sf, eco_sel))
          geoids <- unique(aoi_counties$GEOID)

          variant_pct <- sum(geoids %in% ca_county_geoids) / length(geoids)
          variant <- ifelse(variant_pct >= 0.5, "CA", "CR")

          bucket_contents <- aws.s3::get_bucket(bucket = S3_BUCKET, prefix = S3_TMIDS_PREFIX)
          files <- sapply(bucket_contents, function(x) x$Key)
          file_geoids <- sapply(files, function(f) {
            parts <- strsplit(trimws(basename(f)), "_")[[1]]
            if (length(parts) >= 2) parts[2] else NA_character_
          })
          matched_files <- files[file_geoids %in% geoids]

          tm_out <- NULL
          for (f in matched_files) {
            chunk <- aws.s3::s3readRDS(bucket = S3_BUCKET, object = f)
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
      state$freq_table <- NULL
      state$aoi_stands <- NULL
      state$variant    <- NULL
      state$aoi_method <- NULL
    }
  })

  output$selected_regions_text <- renderUI({
    regs <- state$selected_regions
    if (length(regs) == 0) {
      tags$em("No regions selected yet. Click an ecoregion to select.", style = "color:#7a8a75;")
    } else {
      tagList(
        tags$span(tags$strong(length(regs), "selected:"), " ",
                  paste(regs, collapse = ", ")),
        if (!is.null(state$aoi_stands)) {
          div(class = "status-ok",
              paste0(format(state$aoi_stands, big.mark = ","),
                     " stands — variant: ", state$variant))
        }
      )
    }
  })
  
  observeEvent(input$aoi_file, {
    req(input$aoi_file)
    tryCatch({
      withProgress(message = "Processing uploaded AOI...", {
        tm_ids <- get_tm_ids(
          aoi_path   = input$aoi_file$datapath,
          filetype   = tools::file_ext(input$aoi_file$name),
          unique_ids = stand_lookup
        )
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
  
  output$aoi_file_status <- renderUI({
    if (!is.null(state$aoi_stands) && identical(state$aoi_method, "file")) {
      div(class = "status-ok",
          "✓ File loaded — ", tags$strong(state$aoi_stands), " stands matched")
    }
  })
  
  # ── Review summary ────────────────────────────────────────────────────────
  filtered_stand_count <- reactive({
    ft <- state$freq_table
    if (is.null(ft)) return(0L)
    # Apply forest type filter if any selected (trim to match)
    if (length(input$ft_filter) > 0 && "Forest_Type" %in% names(ft)) {
      ft <- ft |> dplyr::filter(trimws(Forest_Type) %in% input$ft_filter)
    }
    # Apply structure class filter if any selected (trim to match)
    if (length(input$sc_filter) > 0 && "Structure_Class_Description" %in% names(ft)) {
      ft <- ft |> dplyr::filter(trimws(Structure_Class_Description) %in% input$sc_filter)
    }
    nrow(ft)
  })

  output$review_summary <- renderTable({
    aoi_desc <- if (length(state$selected_regions) > 0)
      paste(state$selected_regions, collapse = ", ")
    else if (!is.null(state$aoi_stands)) "Uploaded file"
    else "Not set"
    data.frame(
      Item = c("AOI", "FVS variant", "Forest type filter",
               "Structure class filter", "Matching stands (after filters)"),
      Value = c(
        aoi_desc,
        state$variant %||% "—",
        if (length(input$ft_filter) > 0) paste(input$ft_filter, collapse = ", ") else "All",
        if (length(input$sc_filter) > 0) paste(input$sc_filter, collapse = ", ") else "All",
        format(filtered_stand_count(), big.mark = ",")
      )
    )
  }, striped = TRUE, width = "100%")

  output$low_stand_warning <- renderUI({
    n <- filtered_stand_count()
    if (n > 0 && n < 50) {
      div(style = "color:#7a2020;font-weight:500;margin:12px 0;",
          paste0("⚠ Only ", n, " stands match your filters. ",
                 "Results may not be statistically robust. Consider broadening filters."))
    }
  })
  
  # ── Weights preview ───────────────────────────────────────────────────────
  output$weights_preview <- DT::renderDataTable({
    picks <- selected_ecs()
    if (length(picks) == 0) return(data.frame(Message = "No ECs selected"))
    ec_labels |>
      filter(column %in% picks) |>
      mutate(Importance = 3, Effect = "Positive") |>
      select(EC = label, Category = subcategory, Unit = unit, Importance, Effect)
  }, options = list(dom = 't', pageLength = 50), rownames = FALSE)
  
  # ── Download previews ─────────────────────────────────────────────────────
  output$rf_preview_plot <- renderPlot({
    res <- state$rf_results
    if (is.null(res)) {
      plot.new(); text(0.5, 0.5, "No RF results yet", col = "#7a8a75"); return()
    }
    comb <- res$combined
    # Show combined RF at rel.time == 0 (or nearest) per MgmtID
    t0 <- comb |> dplyr::filter(rel.time == min(abs(rel.time)))
    if (nrow(t0) == 0) t0 <- comb |> dplyr::slice_head(n = 1, by = MgmtID)
    vals <- t0$median_combined_rf
    names(vals) <- t0$MgmtID
    op <- par(mar = c(7, 5, 3, 2))
    barplot(vals,
            col = ifelse(vals >= 0, "#1e3a28", "#7a2020"),
            border = NA, las = 2,
            main = "Combined weighted RF by treatment",
            ylab = "Weighted effect on HVRA")
    abline(h = 0, col = "#1e3a28", lwd = 1)
    par(op)
  })

  output$rf_preview_table <- DT::renderDataTable({
    res <- state$rf_results
    if (is.null(res)) return(data.frame(Message = "No RF results yet"))
    # Pivot per-EC results wide: rows = MgmtID × rel.time, columns = ECs
    wide <- res$per_ec |>
      tidyr::pivot_wider(names_from = EC, values_from = median_rf)
    # Join combined score
    wide |>
      dplyr::left_join(
        res$combined |> dplyr::select(MgmtID, rel.time, Combined = median_combined_rf),
        by = c("MgmtID", "rel.time")
      )
  }, options = list(dom = 't', pageLength = 50, scrollX = TRUE), rownames = FALSE)
  
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
                       paste(rngs[[column]], collapse = " – ") else "—"
      ) |>
      dplyr::ungroup() |>
      select(EC = label, Category = subcategory, Importance, Effect, Range)
  }, options = list(dom = 't', pageLength = 50), rownames = FALSE)
  
  output$factsheet_preview <- renderUI({
    nm <- input$hvra_name %||% "Unnamed HVRA"
    HTML(paste0(
      "<h3>", nm, "</h3>",
      "<p><strong>Ecoregion:</strong> ",
      if (length(state$selected_regions) > 0) paste(state$selected_regions, collapse = ", ") else "—",
      "</p>",
      "<p><strong>Description:</strong> ", input$desc %||% "—", "</p>",
      "<p><strong>Assumptions:</strong> ", input$assumptions %||% "—", "</p>",
      "<p><strong>References:</strong> ", input$refs %||% "—", "</p>"
    ))
  })
  
  # ── Download handlers ──────────────────────────────────────────────────
  build_rf_csv <- function() {
    res <- state$rf_results
    if (is.null(res)) return(data.frame(Message = "No results"))
    wide <- res$per_ec |> tidyr::pivot_wider(names_from = EC, values_from = median_rf)
    wide |> dplyr::left_join(
      res$combined |> dplyr::select(MgmtID, rel.time, Combined_RF = median_combined_rf),
      by = c("MgmtID", "rel.time")
    )
  }

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
