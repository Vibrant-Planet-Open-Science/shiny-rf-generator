source(paste0(here::here(), "/R/functions.R"))

library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(plotly)
library(colorspace)
library(kableExtra)
library(bslib)

# Load GeoPackage once
gpkg_data <- sf::st_read(here::here('data', 'CR-CA-SugarpineVariants.gpkg'), quiet = TRUE) %>%
  sf::st_transform(crs = 4326)

# Load dynamic options for Forest Type and Structure Class
all_vars_codes <- readRDS(here::here('data', 'unique_stand_data.rds'))%>%
  filter(complete.cases(.))

forest_type_options <- all_vars_codes %>%
  arrange(Forest_Type)%>%
  select(Forest_Type) %>%
  distinct()
structure_class_options <- all_vars_codes %>%
  arrange(Structure_Class)%>%
  select("Structure_Class_Description") %>%
  distinct()

# UI
ui <- fluidPage(
  titlePanel("RF-Generator 2.0"),
  sidebarLayout(
    sidebarPanel(
      h3("Step 1: Select Region"),
      leafletOutput("map", height = "300px"),
      fileInput("aoi_file", "Optional: Upload AOI Geopackage", accept = c(".gpkg")),
      
      h3("Step 2: Select Data Type"),
      radioButtons("response_type", "What level of response function do you want to generate?",
                   choices = c("Stand-level Characteristics", "Individual Tree Species"),
                   selected = "Stand-level Characteristics"),
      
      h3("Step 3: Select Filtering Criteria"),
      selectInput("forest_type", "Forest Type", choices = c("All", forest_type_options), multiple = TRUE),
      selectInput("structure_class", "Structure Class", choices = c("All", structure_class_options), multiple = TRUE),
      
      h3("Step 4: Select Ecosystem Components"),
      uiOutput("ecosystem_components_ui"),
      
      h3("Step 5: Assign Importance Weights"),
      uiOutput("weights_ui"),
      
      actionButton("generate", "Generate Response Function")
    ),
    mainPanel(
        h3("Metadata"),
        textOutput("variant_info"), # Selected variant
        textOutput("selected_forest_type"), # Selected forest type
        textOutput("selected_structure_class"), # Selected structure class
        textOutput("selected_response_type"), # Selected response type
        textOutput("selected_ecosystem_components"), # Selected ecosystem components
        h3("Results"),
        tableOutput("filtered_data"),
        plotOutput("response_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  values <- reactiveValues(
    map_variant = NULL,
    aoi_data = NULL,
    filtered_data = NULL
  )
  
  # Reactive for normalized weights
  normalized_weights <- reactive({
    req(input$ecosystem_components)
    user_weights <- sapply(input$ecosystem_components, function(component) {
      as.numeric(input[[paste0(component, "_weight")]])
    })
    # Ensure weights are numeric and calculate simplex weights
    if (length(user_weights) > 0) {
      user_weights / sum(user_weights, na.rm = TRUE)
    } else {
      NULL
    }
  })
  
  # Step 1: Clickable Map
  output$map <- renderLeaflet({
    color_palette <- colorFactor(palette = "Set1", domain = gpkg_data$FVSVariant)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = gpkg_data,
        group = "Variants",
        fillColor = ~color_palette(FVSVariant),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~FVSVarName,
        layerId = ~FVSLocCode
      ) %>%
      addLayersControl(
        overlayGroups = c("Variants", "AOI"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Add AOI to the map when uploaded
  observeEvent(input$aoi_file, {
    req(input$aoi_file)
    values$aoi_data <- st_read(input$aoi_file$datapath)
    
    leafletProxy("map") %>%
      addPolygons(
        data = values$aoi_data,
        group = "AOI",
        color = "blue",
        weight = 2,
        opacity = 1,
        fillOpacity = 0.3,
        label = ~paste0("AOI: ", input$aoi_file$name)
      )
  })
  
  # Reactive for clicked polygon
  selected_polygon <- reactiveVal(NULL)
  
  observeEvent(input$map_shape_click, {
    selected_polygon(input$map_shape_click$id)
  })
  
  output$variant_info <- renderText({
    req(selected_polygon())
    selected <- gpkg_data[gpkg_data$FVSLocCode == selected_polygon(), ]
    paste("Selected Variant:", selected$FVSVarName)
  })
  
  # Metadata outputs for user selections
  output$selected_forest_type <- renderText({
    req(input$forest_type)
    paste("Selected Forest Type(s):", paste(input$forest_type, collapse = ", "))
  })
  
  output$selected_structure_class <- renderText({
    req(input$structure_class)
    paste("Selected Structure Class(es):", paste(input$structure_class, collapse = ", "))
  })
  
  output$selected_response_type <- renderText({
    req(input$response_type)
    paste("Selected Response Type:", input$response_type)
  })
  
  output$selected_ecosystem_components <- renderText({
    req(input$ecosystem_components)
    if (length(input$ecosystem_components) == 0) {
      "No ecosystem components selected."
    } else {
      # Retrieve normalized weights
      simplex_weights <- normalized_weights()
      
      # Format output with simplex weights
      components_with_weights <- mapply(function(component, weight) {
        paste(component, "(Simplex Weight:", round(weight, 2), ")")
      }, input$ecosystem_components, simplex_weights)
      
      # Combine components into a single output string
      paste("Selected Ecosystem Component(s):", paste(components_with_weights, collapse = ", "))
    }
  })
  
  # Step 4: Ecosystem Components
  variable_descriptions <- read.csv(here::here('data', 'variable_descriptions.csv'))%>%
    filter(row_number() >= which(Variable == "Tpa"))
  
  output$ecosystem_components_ui <- renderUI({
    req(variable_descriptions)
    variable_choices <- setNames(
      variable_descriptions$Variable, 
      paste(variable_descriptions$Variable, "-", variable_descriptions$Description)
    )
    selectInput(
      "ecosystem_components", 
      "Select up to 5 variables", 
      choices = variable_choices, 
      multiple = TRUE, 
      selectize = TRUE
    )
  })
  
  # Step 4: Min/Max Threshold and Weight Dropdowns
  output$weights_ui <- renderUI({
    req(input$ecosystem_components)
    components <- input$ecosystem_components
    
    sliders <- lapply(components, function(component) {
      fluidRow(
        column(8, sliderInput(
          inputId = paste0(component, "_range"),
          label = paste(component, "Range"),
          min = 0, max = 100, value = c(20, 80)
        )),
        column(4, selectInput(
          inputId = paste0(component, "_weight"),
          label = paste(component, "Weight (1 = Least Important, 5 = Most Important)"),
          choices = c(1, 2, 3, 4, 5),
          selected = 3
        ))
      )
    })
    do.call(tagList, sliders)
  })
  
  # Step 5: Generate Response Function
  observeEvent(input$generate, {
    req(values$aoi_data, input$ecosystem_components)
    
    forest_filter <- if ("All" %in% input$forest_type) TRUE else values$aoi_data$forest_type %in% input$forest_type
    structure_filter <- if ("All" %in% input$structure_class) TRUE else values$aoi_data$structure_class %in% input$structure_class
    
    filtered <- values$aoi_data %>%
      filter(forest_filter & structure_filter)
    
    components <- input$ecosystem_components
    filtered <- filtered %>%
      mutate(across(all_of(components), ~ case_when(
        . >= input[[paste0(., "_range")]][1] & . <= input[[paste0(., "_range")]][2] ~ . * input[[paste0(., "_weight")]],
        TRUE ~ NA_real_
      )))
    
    values$filtered_data <- filtered
  })
  
  output$filtered_data <- renderTable({
    req(values$filtered_data)
    values$filtered_data
  })
  
  output$response_plot <- renderPlot({
    req(values$filtered_data)
    components <- input$ecosystem_components
    plot_data <- values$filtered_data %>% select(all_of(components))
    
    boxplot(plot_data, main = "Response Function by Ecosystem Components")
  })
}

# Run App
shinyApp(ui, server)