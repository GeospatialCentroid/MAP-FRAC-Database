# MAP-FRAC Shiny Web Application #
##################################

# set up
library(shiny)
library(shinyWidgets)
library(bslib)
library(sf)
library(plotly)
library(leaflet)
library(DT)
library(tidyverse)


# read in data
load("data/app_data.RData")

sample_app <- st_jitter(sample_app, factor = 0.005)
genome_app <- st_jitter(genome_app, factor = 0.005)

basins <- read_sf("data/ShalePlays_US_EIA_Dec2021.shp") %>% 
  # fix typo
  mutate(Lithology = if_else(Lithology == "MIxed Shale & Chalk", "Mixed Shale & Chalk", Lithology))

# create summarized sample file for map
sample_sum <- sample_app %>% 
  distinct(well_ID, .keep_all = TRUE)

# Define UI -----------------
ui <- fluidPage(
  class = "container-all",
  theme = bslib::bs_theme(
    version = "5",
    bootswatch = "darkly",
    bg = "#101010",
    fg = "#FFF",
    primary = "#7198ab",
    secondary = "#F27F0c",
    #success = "#a5c90f",
    base_font = font_google("Inria Sans")
  ),
  includeCSS("www/style.css"),
  
  navbarPage(
    "MAP-FRAC Database",
    
    # ## use shinydashboard ##
    # header = tagList(useShinydashboard()),
    
    tabPanel(
      "Sample Explorer",
      h4("Placeholder for subheader/description of project"),
      
      fluidRow(
        column(12,
               card(full_screen = TRUE,
                    layout_sidebar(
                      open = TRUE,
                      sidebar = sidebar(
                        position = "right",
                        checkboxGroupButtons("select_lith", "Filter by Lithology:",
                                    choices = unique(sample_app$Lithology),
                                    selected = unique(sample_app$Lithology),
                                    individual = TRUE
                                    ),
                        selectizeInput("zoom_basin", "Zoom to Basin:",
                        choices = unique(sample$basin),
                        options = list(
                          placeholder = 'Please select an option below',
                          onInitialize = I('function() { this.setValue(""); }')
                        ))),
                        leafletOutput("sample_map")
                      )
                    )
                   ),
               card(height = 350,
                   card_header(HTML(paste("Click a point on the map to view time series", em("(if available)")))),
                   plotlyOutput("timeseries")))
      
    ),
    tabPanel(
      "Genome Explorer",
      h4("Placeholder for subheader"),
                      card(full_screen = TRUE,
                           layout_sidebar(
                             open = TRUE, 
                             sidebar = sidebar(
                               position = "left",
                               accordion(open = FALSE,
                                 accordion_panel("Filter by Taxonomy:",
                                   selectizeGroupUI(id = "taxonomy_filter",
                                                                  inline = FALSE,
                                                                  params = list(
                                                                    domain = list(inputId = "domain", title = "Domain:"),
                                                                    phylum = list(inputId = "phylum", title = "Phylum:"),
                                                                    class = list(inputId = "class", title = "Class:"),
                                                                    order = list(inputId = "order", title = "Order:"),
                                                                    family = list(inputId = "family", title = "Family:"),
                                                                    genus = list(inputId = "genus", title = "Genus:"),
                                                                    species = list(inputId = "species", title = "Species:")
                                                                  )))
                                 )
                              
                               ),
                             leafletOutput("genome_map")
                           )
      ),
      card(DT::dataTableOutput("genome_table"))
      
    )
    
  )
)

# Define server -----------------
server <- function(input, output) {

  ## reactive sample data -----------
  basin_filtered <- reactive({
    basins %>% 
      filter(Lithology %in% input$select_lith)
  })
  
  ## sample plotly (test) ----
  output$timeseries <- plotly::renderPlotly({
    plot_ly(height = 250)
  })
  

  ## sample map ----
  output$sample_map <- leaflet::renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolygons(
        data = basin_filtered(),
        stroke = FALSE,
        fillOpacity = 0.5,
        fillColor = "#91B187"
      ) %>%
      addCircleMarkers(
        data = sample_sum,
        radius = ~ sqrt(n_samples) * 5,
        stroke = FALSE,
        fillOpacity = 0.5,
        fillColor = "#F7AD19",
        popup = paste(
          "Well:",
          sample_sum$well_ID,
          "<br>",
          paste("Basin:", sample_sum$Basin),
          "<br>",
          paste("Number of Samples:", sample_sum$n_samples),
          "<br>",
          paste("Max days since frack:", sample_sum$max_days_since_frack)
        )
      )
  })

  
  ### zoom to basin ------
  observeEvent(input$zoom_basin, {
    
    if (input$zoom_basin == "") {
      leafletProxy("sample_map")
    } else {
    
    zoom <- reactive({
      sample %>%
        filter(basin == input$zoom_basin) %>%
        st_bbox() %>%
        st_as_sfc(crs = st_crs(sample)) %>%
        st_centroid() %>%
        st_coordinates()

    })

    leafletProxy('sample_map') %>%
      setView(lng = zoom()[1], lat = zoom()[2], zoom = 7)
    }


  })
  
  
  ## select taxonomy
  taxa_mod <- callModule(
    module = selectizeGroupServer,
    id = "taxonomy_filter",
    inline = FALSE,
    data = st_drop_geometry(genome),
    vars = c("domain", "phylum", "class", "order", "family", "genus", "species")
  )
  output$genome_table <- DT::renderDataTable(taxa_mod())
  
  
  ## genome map -----
  output$genome_map <- leaflet::renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolygons(data = basins, stroke = FALSE, fillOpacity = 0.7, fillColor = "#91B187") %>%
      addCircleMarkers(data = genome,
                       radius = 6,
                       stroke = FALSE,
                       fillOpacity = 0.5,
                       fillColor = "#F7AD19")
  })
  


}

# Run the app ---------------------------
shinyApp(ui = ui, server = server)