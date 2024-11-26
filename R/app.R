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
all_vars_codes <- read.csv(here::here('data', 'all_vars_codes.csv'))
forest_type_options <- all_vars_codes %>%
  filter(variable == "ForTyp") %>%
  pull(readable_values)
structure_class_options <- all_vars_codes %>%
  filter(variable == "Structure_Class") %>%
  pull(readable_values)

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
      h3("Results"),
      textOutput("variant_info"), # Moved to mainPanel
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
    # Read AOI data
    values$aoi_data <- st_read(input$aoi_file$datapath)
    
    # Add the AOI to the map
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
  
  # Step 4: Ecosystem Components
  output$ecosystem_components_ui <- renderUI({
    components <- c("Tpa", "BA", "QMD", "Stratum_1_Crown_Cover", 
                    "Stratum_1_Nom_Ht", "Surface_Herb", "Surface_Shrub", "Standing_Dead")
    checkboxGroupInput("ecosystem_components", "Select up to 5 variables", 
                       choices = components, inline = TRUE)
  })
  
  # Step 4: Min/Max Threshold and Weight Sliders
  output$weights_ui <- renderUI({
    req(input$ecosystem_components)
    components <- input$ecosystem_components
    
    sliders <- lapply(components, function(component) {
      fluidRow(
        column(8, sliderInput(
          inputId = paste0(component, "_range"),
          label = paste(component, "Range"),
          min = 0, max = 100, value = c(20, 80) # Adjust min, max, and default range as needed
        )),
        column(4, sliderInput(
          inputId = paste0(component, "_weight"),
          label = paste(component, "Weight"),
          min = 1, max = 5, value = 3
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
