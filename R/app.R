
source(paste0(here::here(),"/R/functions.R"))

library(plotly)
library(colorspace)
library(kableExtra)
library(dplyr)
library(shiny)
library(bslib)



# UI
ui <- fluidPage(
  titlePanel("Forest Type and Structure Filtering"),
  uiOutput("main_page")
)


# Server
server <- function(input, output, session) { 
  
  values <- reactiveValues(
    sf_data = NULL,
    freq_table = NULL,
    raster_data = NULL
  )
  # Reactive value to track the current page
  #vars=readRDS()
  all_fvs_variables <-
    aws.s3::s3readRDS(
      bucket = 'vp-open-science',
      object = 'rf-generator-data/rshiny-spatial-data/all_vars_codes.rds')
  current_page <- reactiveVal("home")
  
  # Debug: Monitor page transitions
  observe({
    print(current_page()) # Debug: Verify page transitions
  })
  
  # Reactive value to store user inputs dynamically
  user_inputs <- reactiveVal(
    data.frame(
      Variable = character(), 
      Value = character()
      )
    )
  
  # Render dynamic UI
  output$main_page <- renderUI({
    if (current_page() == "home") {
      fluidPage(
        h3("Step 1: Select Filters"),
        fileInput("geopackage", "Upload Geopackage", accept = c(".gpkg",'tif')),
        actionButton("go_to_page2", "Process")
      )
    } else if (current_page() == "page2") {
      fluidPage(
        h3("Step 1: Select Filters"),
        selectInput("selected_filters", "What filters would you like to apply? (max = 3)",
                    choices = c("Structure Class (Seral Stage)", "Forest Type"),
                    multiple = TRUE),
        actionButton("go_to_page3", "Go to Page 3")
      )
    } else if (current_page() == "page3") {
      fluidPage(
        h3("Step 2: Select Forest Structure Metric"),
        uiOutput("filter_inputs"),
        actionButton("go_to_page4", "Apply Filters")
      )
    } else if (current_page() == "page4") {
      filtered_data <- filter_data()
      fluidPage(
        h3("Review your current selection"),
        tableOutput("filtered_table"),
        actionButton("go_to_page5", "choose variables")
      )
    } else if (current_page() == "page5") {
      fluidPage(
        h3("Choose variables for your response function (up to 5)"),
        uiOutput("render_filters"),
        actionButton("go_to_page6", "choose weights")
      )
    } else if (current_page() == "page6") {
      fluidPage(
        h3("Choose your weights"),
        uiOutput("gather_weights"),
        actionButton("go_to_page7", "download")
      )
  }
    })
  # Create UI elements for filter inputs
  
  
  
  # Navigate to Page 2
  observeEvent(input$go_to_page2, {
 
    filetype = stringr::str_sub(input$geopackage$datapath, stringr::str_length(input$geopackage$datapath) - 2,  stringr::str_length(input$geopackage$datapath))
    unique_stand_info <-
      aws.s3::s3readRDS(
        bucket = 'vp-open-science',
        object = 'rf-generator-data/rshiny-spatial-data/unique_stands_western.rds'
 #     readRDS('/Users/eyackulic/Desktop/unique_stands_western.rds'
              ) |>
      mutate(StandID = as.character(StandID))
    
    
    values$freq_table <- 
      get_tm_ids(
        aoi_path = input$geopackage$datapath,
        filetype = filetype,
        unique_ids = unique_stand_info
      )

    values$freq_table
    
    current_page("page2")
  })
  
  observeEvent(input$go_to_page3, {
    current_page("page3")
  })
  
  # Capture user inputs and navigate to Page 3
  observeEvent(input$go_to_page4, {
    # Update user_inputs dataframe
    inputs <- user_inputs()
    if (!is.null(input$forest_type)) {
      inputs <- rbind(inputs, data.frame(Variable = "Forest Type", Value = input$forest_type))
    }
    if (!is.null(input$structure_class)) {
      inputs <- rbind(inputs, data.frame(Variable = "Structure Class", Value = input$structure_class))
    }
    user_inputs(inputs)
    current_page("page4")
  })
  observeEvent(input$go_to_page5, {
  #variables?
    current_page('page5')    
  })
  observeEvent(input$go_to_page6, {

    current_page('page6')    
  })
  # Navigate back to home
  observeEvent(input$go_to_home, {
    user_inputs(data.frame(Variable = character(), Value = character())) # Reset inputs
    current_page("home")
  })
  
  
  
  
  
  
  
  
  ##Page 3#
  output$filter_inputs <- renderUI({
    selected <- input$selected_filters
    
    df <- values$freq_table

  #  all_vars <- '/Users/eyackulic/Desktop/all_vars_codes.rds' |> 
    #readRDS()
    all_fvs_variables <-
    aws.s3::s3readRDS(
      bucket = 'vp-open-science',
      object = 'rf-generator-data/rshiny-spatial-data/all_vars_codes.rds')
      
   
    
    all_fvs_variables <-
      all_fvs_variables |>
      dplyr::mutate(
        val = dplyr::if_else(variable %in% 'ForTyp'& value %in% unique(df$ForTyp), 1, val),
        val = dplyr::if_else(variable %in% 'Structure_Class'& value %in% unique(df$Structure_Class), 1, val)
      ) |>
      filter(val > 0) |>
      select(-val)
    
    ui_elements <- list()
    if ("Forest Type" %in% selected) {
      choice = all_fvs_variables |> dplyr::filter(variable %in% 'ForTyp')
      ui_elements <- c(ui_elements, selectInput(
        "forest_type", "Select Forest Type", choices = choice$readable_values, multiple = T
      ))
    }
    if ("Structure Class (Seral Stage)" %in% selected) {
      choice = all_fvs_variables |> dplyr::filter(variable %in% 'Structure_Class')
      ui_elements <- c(ui_elements, selectInput(
        "structure_class", "Select Structure Class", choices = choice$readable_values, multiple = T
      ))
    }
    do.call(tagList, ui_elements)
  })
  
  ## Page 4
  # Reactive function to filter data based on user inputs
  filter_data <- reactive({
    inputs <- user_inputs()
    
    # Load all_fvs_variables reactively
    vars <- all_fvs_variables
    
    validate(need(!is.null(vars), "all_fvs_variables is not loaded."))
    validate(need(is.data.frame(vars), "all_fvs_variables must be a data frame."))
    
    filtered_forests <- data.frame(
      value = character(),
      variable = character(),
      readable_variable = character(),
      readable_values = character(),
      stringsAsFactors = FALSE
    )
    filtered_structure <- filtered_forests
    
    if ("Forest Type" %in% inputs$Variable) {
      selected_forests <- inputs$Value[inputs$Variable == "Forest Type"]
      filtered_forests <- 
        vars |>
        dplyr::filter(variable == "ForTyp") |>
        dplyr::filter(readable_values %in% selected_forests)
    }
    
    if ("Structure Class" %in% inputs$Variable) {
      selected_class <- inputs$Value[inputs$Variable == "Structure Class"]
      filtered_structure <- 
        vars |>
        dplyr::filter(variable == "Structure_Class") |>
        dplyr::filter(readable_values %in% selected_class)
    }
    
    result <- dplyr::bind_rows(filtered_forests, filtered_structure) |>
      dplyr::filter(!is.na(readable_values))
    
    return(result)
  })
  
  ## Page 4
  # Output filtered data
  output$filtered_table <- renderTable({
    filter_data()
  })
  
  

  
  ##PAGE 5 ###


  filter_variables  <- reactive({
    rdf <- values$freq_table |> distinct()
    filters <- filter_data()
    withProgress(message = 'Gathering filters', value = 0, {
      
      if ("Forest Type" %in% filters$readable_variable) {
        rdf <- 
          rdf |> 
          dplyr::filter(ForTyp %in% filters[filters$readable_variable %in% 'Forest Type',]$value) 
      }
    if ("Structure Class" %in% filters$readable_variable) {
      rdf <-
        rdf |> 
        dplyr::filter(Structure_Class %in% filters[filters$readable_variable %in% 'Structure Class',]$value) 
      }
    
      incProgress(1/10, detail = paste("Adding Stand Data"))
       #error is occurring here because rdf structure doesnt match actual full dataframe anymore

    #need an if statement here that choses variant path based on ids variant code
    if(unique(rdf$Variant) %in% 'CA'){
    #stand_level_path <- '/Users/eyackulic/Downloads/CA-FIC-StandLevel_2024-09-25.rds'
    stand_level_path <-  'rf-generator-data/raw-data/CA-FIC-StandLevel_2024-09-25.rds'
    #stk_path <- '/Users/eyackulic/Downloads/CA-FIC-StdStk_2024-09-25.rds'
    stk_path <- 'rf-generator-data/raw-data/CA-FIC-StdStk_2024-09-25.rds'
    }else{
      #stand_level_path <- '/Users/eyackulic/Downloads/CA-FIC-StandLevel_2024-09-25.rds'
      stand_level_path <- 'rf-generator-data/raw-data/CR-TRT-StandFilter_2024-10-10.rds' 
      #stk_path <- '/Users/eyackulic/Downloads/CA-FIC-StdStk_2024-09-25.rds'
      stk_path <- 'rf-generator-data/raw-data/CR-TRT-StdStk_2024-10-10.rds'
    }
    
    stand_data <- 
      stand_level_path |>
      aws.s3::s3readRDS(
        bucket = 'vp-open-science'
      ) |>
      #readRDS() |>
      get_filtered_stand_data(rdf) |> 
      dplyr::mutate(
        rel.time = Year - 2030
      ) #need to add this for multiple variants
    incProgress(6/10, detail = paste("Adding Species Data"))
    
    all_variables <- 
      stk_path |>
      aws.s3::s3readRDS(
        bucket = 'vp-open-science'
      ) |>
      #readRDS() |> 
      dplyr::filter(!Species %in% 'All') |>
      get_filtered_stdstk(stand_data_frame = stand_data) |>
      stand_stk_wide(filtered_stands = stand_data) |>
      cleanDF()
    incProgress(10/10, detail = paste("Finished!"))
    })
     all_variables 
    
    })
    
  
    output$render_filters <- renderUI({   
    
      all_variables <-filter_variables()
      print(all_variables)
      all_variable_names <- 
      all_variables |> 
      get_potential_variable_names()
      print(all_variable_names)
      

    #remove instead by number of non-NA observations
    ui_elements <- list()
    ui_elements <- c(ui_elements, selectInput(
      "rf_variables", "Select Variables", choices = all_variable_names, multiple = T
    ))
    
    do.call(tagList, ui_elements)
  })
  
  ##PAGE 6 ###
  #STILL NEED TO LINK UP DATA TO SLIDERS 
    
  
  # filtered_data <- reactive({
  #   data <- filter_variables()
  #   subset(data, disturbance.group == input$dist)
  # })

  
  # Print filtered data for debugging
  # observe({
  #   print(filtered_data())
  # })
  
#   # Define a reactive expression to calculate weighted sum for each group
#   weighted_sum_data <- reactive({
#     # Filter the data based on input$dist and input$t
#     filtered <- filtered_data() %>%
#       filter(Year == input$t)
#     # Calculate the weighted sum for each variable based on input weights
#     weighted_sum <- filtered %>%
#       mutate(
#         weighted_Stratum_1_Crown_Cover = input$cc_weight * Stratum_1_Crown_Cover,
#         weighted_QMD = input$qmd_weight * QMD,
#         weighted_Forest_Down_Dead_Wood = input$sng_weight * Forest_Down_Dead_Wood,
#         weighted_Surface_Shrub = input$sh_weight * Surface_Shrub,
#         weighted_Surface_Herb = input$hb_weight * Surface_Herb,
#         weighted_soil_i = input$soil_weight * soil.i
#         # Add other variables and their weights as needed
#       ) %>%
#       # Calculate the total weighted sum
#       mutate(
#         weighted.RF = ((weighted_Stratum_1_Crown_Cover +
#                           weighted_QMD +
#                           weighted_Forest_Down_Dead_Wood +
#                           weighted_Surface_Shrub +
#                           weighted_Surface_Herb +
#                           weighted_soil_i)*(1-input$mit_weight))
#         # Add other variables here if needed
#       ) %>%
#       # Select only the necessary columns for output
#       select(disturbance.group, treatment.name, Year, weighted.RF, REBA.Code)
#     
#     return(weighted_sum)
#   })
#   
#   # Render table for weighted data
#   output$weighted_table <- renderTable({
#     weighted_sum_data()
#   })
#   
#   
#   weighted_sum.t <- reactive({
#     # Filter the data based on input$dist and input$t
#     filtered <- filtered_data()
#     # Calculate the weighted sum for each variable based on input weights
#     weighted.t <- filtered %>%
#       mutate(
#         weighted_Stratum_1_Crown_Cover = input$cc_weight * Stratum_1_Crown_Cover,
#         weighted_QMD = input$qmd_weight * QMD,
#         weighted_Forest_Down_Dead_Wood = input$sng_weight * Forest_Down_Dead_Wood,
#         weighted_Surface_Shrub = input$sh_weight * Surface_Shrub,
#         weighted_Surface_Herb = input$hb_weight * Surface_Herb,
#         weighted_soil_i = input$soil_weight * soil.i
#         # Add other variables and their weights as needed
#       ) %>%
#       # Calculate the total weighted sum
#       mutate(
#         weighted.RF = ((weighted_Stratum_1_Crown_Cover +
#                           weighted_QMD +
#                           weighted_Forest_Down_Dead_Wood +
#                           weighted_Surface_Shrub +
#                           weighted_Surface_Herb +
#                           weighted_soil_i)*(1-input$mit_weight))
#         # Add other variables here if needed
#       ) %>%
#       # Select only the necessary columns for output
#       select(disturbance.group, treatment.name, Year, weighted.RF, REBA.Code, color.pallate, color)
#     
#     return(weighted.t)
#   })
#   
#   # Print filtered data for debugging
#   # observe({
#   #   print('weighted RF timeseries')
#   #   print(weighted_sum.t())
#   # })
#   # Output the filtered data as a plot (timeseries plot)
#   output$plot <- renderPlotly({
#     weighted_data <- weighted_sum.t() # Store the reactive value
#     plot_ly(data = weighted_data, x = ~Year, y = ~weighted.RF, type = 'scatter', mode = 'lines+markers', 
#             color = ~treatment.name, colors = ~color, text = ~treatment.name) %>%
#       layout(title = paste("Response Functions for", unique(weighted_data$disturbance.group)),
#              xaxis = list(title = "Time Since Disturbance"),
#              yaxis = list(title = "Weighted Effect in SARA value", range = c(-1.1, 1.1)),
#              margin = list(l = 75, r = 75, t = 100, b = 100)) # Adjust margins as needed
#   })
#   
#   
#   # Download response functions as CSV
#   output$download_csv <- downloadHandler(
#     filename = function() {
#       paste("response_functions_", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#       write.csv(weighted_sum_data(), file)
#     }
#   )
#   
#   # Download weights as CSV
#   output$download_weights <- downloadHandler(
#     filename = function() {
#       paste("weights_", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#       # Create a dataframe of weights
#       weights <- data.frame(
#         Canopy_Cover = input$cc_weight,
#         Tree_Diameter = input$qmd_weight,
#         Snag_Downed_Wood_Abundance = input$sng_weight,
#         Shrub_Density = input$sh_weight,
#         Herbaceous_Density = input$hb_weight,
#         Soil_Integrity = input$soil_weight,
#         Mitigation_Effectiveness = input$mit_weight
#         # Add other weights as needed
#       )
#       write.csv(weights, file)
#     }
#   )
#   
#   
#   
#   output$gather_weights <- renderUI({ 
#   
#       layout_sidebar(
#         # Sidebar panel for inputs ----
#         sidebar = sidebar(
#           width = 550, # Adjust the width of the sidebar here
#           
#           
#           # Input: Select the random distribution type ----
#           
#           h5('Strategic Area, Resource, or Asset (SARA) Information:'),
#           textInput("SARA", "Species Common Name"),
#           # Read the uploaded table
#           dist_table <- read.csv(here::here("data", "dist-table.csv")),
#           
#           # Extract unique values from disturbance_grp
#           disturbance_options <- unique(dist_table$disturbance_grp),
#           
#           # Generate radio buttons dynamically
#           radioButtons("dist", "Response Function Type:",
#                        choices = setNames(disturbance_options, disturbance_options)),
#                        h5('Ecosystem Components')#,
# #                        p(HTML('Ecosystem Component (EC) effects represent how an increase or decrease in each ecosystem component
# #         
# #         affects the species’ currently-mapped habitat suitability (the SARA) relative to current conditions. <br> <br> Ecosystem Component (EC) effects can range from -1  to +1 where, for example: <br> <br>
# #           
# #           -1: (100% reduction EC) |  0: (no change in EC) | +1 (100% increase in EC) <br> <br> <i> For example, a canopy cover effect of +1 indicates that a 100% increase in canopy cover within the currently mapped habitat would benefit this species.</i>')),
# #           
# #                        # the current species SARA would be strongly positive for the SARA’s value to the species.</i>"
# #                        # Sliders for variable weighting
# #                        sliderInput("cc_weight", HTML("Canopy Cover: <br> <i>Increase or decrease in percent canopy cover</i>"), min = -1, max = 1, value = 0, step = 0.25),
# #                        sliderInput("qmd_weight", HTML("Tree Diameter: <br> <i>Increase or decrease in average tree diameter</i>"), min = -1, max = 1, value = 0, step = 0.25),
# #                        sliderInput("sng_weight", HTML("Snag & Downed Wood Abundance: <br> <i>Increase or decrease in count of hard and soft snags</i>"), min = -1, max = 1, value = 0, step = 0.25),
# #                        sliderInput("sh_weight", HTML("Shrub Density: <br> <i> Increase or decrease in shrubs and saplings</i>"), min = -1, max = 1, value = 0, step = 0.25),
# #                        sliderInput("hb_weight", HTML("Herbaceous Density: <br> <i>Increase or decrease in herbaceous cover</i>"), min = -1, max = 1, value = 0, step = 0.25),
# #                        sliderInput("soil_weight", HTML("Soil Integrity: <br> <i>Increase or decrease in bare soil and erosion</i>"), min = -1, max = 1, value = 0, step = 0.25),
# #                        #sliderInput("h2o_weight", HTML("Water Quality: <br> <i>Response to sediment delivery in surface water</i>"), min = -1, max = 1, value = 0, step = 0.25),
# #                        sliderInput("mit_weight", HTML("Mitigation Effectiveness: <br> <i>adjustment  of impacts 0 (no mitigation) to 1 (full mitigation) of effects</i>"), min = 0, max = 1, value = 0, step = .10),
# #                        
# #                        # br() element to introduce extra vertical spacing ----
# #                        br(),
# #                        # Input: Slider for the number of observations to generate ----
# #                        sliderInput("t",
# #                                    "Time Since Disturbance:",
# #                                    value = 1,
# #                                    min = 1,
# #                                    max = 10),
# #                        br(),
# #                        
# #                        downloadButton("download_csv", "Download Response Functions"),
# #                        downloadButton("download_weights", "Download Weights")
# #           ),
# #           
# #           # Main panel for displaying outputs ----
# #           # Output: A tabset that combines three panels ----
# #           
# #           navset_card_underline(
# #             title = "Response Functions",
# #             # Panel with table ----
# #             nav_panel("RF Table", tableOutput("weighted_table")),
# #             
# #             # Panel with plot ----
# #             nav_panel("RF Timeseries Plot", plotlyOutput("plot")),
# #             
# #             # Panel with summary ----
# #             nav_panel("RF Assumptions", textAreaInput("RF Assumptions", "Workshop assumptions for this set of response functions:", width = '100%', height = '100%'))
# #             
# #             
# #           )
# #         )
# #       
# #   }
# #   )
# #   
 }
 shinyApp(ui, server)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# # output$map <- renderLeaflet({
# #   req(values$sf_data)
# #   
# #   leaflet() %>%
# #     addProviderTiles("OpenStreetMap") %>%
# #     addPolygons(data = sf::st_transform(values$sf_data, 4326), color = "blue", weight = 1)
# # })
# # 
# # # Render the frequency table
# # # output$freq_table <- renderTable({
# # #   req(values$freq_table)
# # #   values$freq_table[1:10,]
# # # })
# # 
# # # Render the frequency plot
# # output$freq_plot <- renderPlot({
# #   req(values$freq_table)
# #   
# #   ggplot(values$freq_table, aes(x = value, y = count)) +
# #     geom_bar(stat = "identity", fill = "steelblue") +
# #     theme_minimal() +
# #     labs(title = "Frequency of Raster Values",
# #          x = "Raster Value",
# #          y = "Frequency")
# # })
