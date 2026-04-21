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

mock_ecoregions <- data.frame(
  name    = c("Southern Rockies", "Cascades", "Klamath Mountains",
              "Sierra Nevada", "Blue Mountains", "Wasatch & Uinta"),
  biome   = c("Temperate conifer","Temperate conifer","Temperate conifer",
              "Temperate conifer","Temperate conifer","Montane"),
  variant = c("CR","CA","CA","CA","CA","CR"),
  lat     = c(39.5, 47.5, 41.5, 38.5, 45.0, 40.5),
  lng     = c(-106.5, -121.5, -123.0, -119.5, -118.5, -111.0)
)

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
    rf_type          = NULL,
    selected_species = character(),
    selected_ecs     = character(),
    ec_weights       = list(),
    ec_effects       = list()
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
    if (s == "rftype" && isTRUE(state$rf_type == "stand")) { nav_to("ecs"); return() }
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
    state$rf_type          <- NULL
    state$selected_species <- character()
    state$selected_ecs     <- character()
    state$ec_weights       <- list()
    state$ec_effects       <- list()
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
    div(class = "card",
        h2("Filter stands"),
        p("Narrow to relevant forest types and structure classes. Leave blank to include all.",
          class = "subtitle"),
        fluidRow(
          column(6,
                 div(class = "section-label", "Forest type"),
                 checkboxGroupInput("ft_filter", NULL, choices = forest_types)
          ),
          column(6,
                 div(class = "section-label", "Structure class"),
                 checkboxGroupInput("sc_filter", NULL, choices = structure_classes)
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
    div(class = "card",
        h2("Select target species"),
        p("Choose one or more species present in your AOI. Species-specific metrics will be added to the EC list.",
          class = "subtitle"),
        checkboxGroupInput("species_picks", NULL,
                           choices = mock_species, selected = mock_species),
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
    body <- if (length(picks) == 0) {
      div(style = "color:#7a8a75;font-style:italic;padding:20px 0;",
          "No ECs selected. Go back and pick at least one.")
    } else {
      tagList(
        DT::dataTableOutput("weights_preview"),
        div(style = "margin-top:16px;color:#7a8a75;font-size:13px;",
            "Full weights UI (1–5 stepper, effect type pills, range inputs) — wire up next.")
      )
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
    leaflet(mock_ecoregions) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -115, lat = 42, zoom = 4) |>
      addCircleMarkers(
        ~lng, ~lat, layerId = ~name, label = ~name,
        radius = 10, fillColor = "#2D6A4F", color = "#1B4D36",
        weight = 1, fillOpacity = 0.7
      )
  })
  
  observeEvent(input$ecoregion_map_marker_click, {
    clicked <- input$ecoregion_map_marker_click$id
    if (is.null(clicked)) return()
    cur <- state$selected_regions
    state$selected_regions <- if (clicked %in% cur) setdiff(cur, clicked) else c(cur, clicked)
  })
  
  output$selected_regions_text <- renderUI({
    regs <- state$selected_regions
    if (length(regs) == 0) {
      tags$em("No regions selected yet. Click a marker to select.", style = "color:#7a8a75;")
    } else {
      tags$span(tags$strong(length(regs), "selected:"), " ",
                paste(regs, collapse = ", "))
    }
  })
  
  observeEvent(input$aoi_file, {
    state$aoi_method <- "file"
    state$aoi_stands <- 1234
  })
  
  output$aoi_file_status <- renderUI({
    if (!is.null(state$aoi_stands) && identical(state$aoi_method, "file")) {
      div(class = "status-ok",
          "✓ File loaded — ", tags$strong(state$aoi_stands), " stands matched")
    }
  })
  
  # ── Review summary ────────────────────────────────────────────────────────
  output$review_summary <- renderTable({
    aoi_desc <- if (length(state$selected_regions) > 0)
      paste(state$selected_regions, collapse = ", ")
    else if (!is.null(state$aoi_stands)) "Uploaded file"
    else "Not set"
    data.frame(
      Item = c("AOI", "FVS variant", "Forest type filter",
               "Structure class filter", "Matching stands"),
      Value = c(
        aoi_desc,
        "CR (placeholder)",
        if (length(input$ft_filter) > 0) paste(input$ft_filter, collapse = ", ") else "All",
        if (length(input$sc_filter) > 0) paste(input$sc_filter, collapse = ", ") else "All",
        as.character(state$aoi_stands %||% "—")
      )
    )
  }, striped = TRUE, width = "100%")
  
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
    dist <- paste0("FIC", 1:6)
    vals <- c(-0.18, -0.14, -0.09, -0.03, 0.05, 0.11)
    op <- par(mar = c(5, 5, 3, 2))
    barplot(vals, names.arg = dist,
            col = ifelse(vals >= 0, "#1e3a28", "#7a2020"),
            border = NA, ylim = c(-0.3, 0.3), las = 1,
            main = "Placeholder RF preview",
            ylab = "Weighted effect in HVRA value")
    abline(h = 0, col = "#1e3a28", lwd = 1)
    par(op)
  })
  
  output$rf_preview_table <- DT::renderDataTable({
    set.seed(42)
    data.frame(
      MgmtID = c("BASE","FIC1","FIC2","FIC3","FIC4","FIC5","FIC6","MRCC","MTTH","RMGP"),
      t0  = round(runif(10, -0.3, 0.1), 3),
      t5  = round(runif(10, -0.2, 0.2), 3),
      t10 = round(runif(10, -0.1, 0.3), 3),
      t20 = round(runif(10, -0.1, 0.4), 3)
    )
  }, options = list(dom = 't', pageLength = 20), rownames = FALSE)
  
  output$ec_config_table <- DT::renderDataTable({
    picks <- selected_ecs()
    if (length(picks) == 0) return(data.frame(Message = "No ECs selected"))
    ec_labels |>
      filter(column %in% picks) |>
      select(EC = label, Category = subcategory, Unit = unit)
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
  
  # Download handlers (placeholders)
  output$dl_rf <- downloadHandler(
    filename = function() "rf_outputs.csv",
    content  = function(file) write.csv(data.frame(placeholder = "RF outputs go here"), file, row.names = FALSE)
  )
  output$dl_ec <- downloadHandler(
    filename = function() "ec_config.csv",
    content  = function(file) write.csv(data.frame(placeholder = "EC config goes here"), file, row.names = FALSE)
  )
  output$dl_factsheet <- downloadHandler(
    filename = function() "factsheet.md",
    content  = function(file) writeLines("# Placeholder fact sheet", file)
  )
  output$dl_all <- downloadHandler(
    filename = function() "rf_package.zip",
    content  = function(file) file.create(file)
  )
}

shinyApp(ui, server)
