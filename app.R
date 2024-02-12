# MAP-FRAC Shiny Web Application #
##################################

# set up
library(shiny)
library(sf)
library(plotly)
library(leaflet)
library(bslib)

# read in data
sample <- read_sf("data/sample_app.shp") %>% 
  st_jitter(factor = 0.009)
genome <- read_sf("data/genome_app.shp") %>% 
  st_jitter(factor = 0.009)

basins <- read_sf("data/ShalePlays_US_EIA_Dec2021.shp")

# Define UI -----------------
ui <- fluidPage(
  class = "container-all",
  theme = bslib::bs_theme(bootswatch = "darkly"),
  includeCSS("www/style.css"),
  navbarPage(
    "MAP-FRAC Database",
    
    
    tabPanel(
      "Sample Explorer",
      h4("Placeholder for subheader/description of project"),
      
      fluidRow(
        column(
          5,
          "Placehoder for user input selections?",
          plotlyOutput("timeseries")
        ),
        column(7,
               leafletOutput("sample_map"))
      ),
      fluidRow()
    ),
    tabPanel(
      "Genome Explorer",
      h4("Placeholder for subheader"),
      fluidRow(sidebarLayout(
        sidebarPanel("select inputs go here"),
        mainPanel(leafletOutput("genome_map"))
      ))
    )
    
  )
)

# Define server -----------------
server <- function(input, output) {

  #sample plotly (test)
  output$timeseries <- plotly::renderPlotly({
    plot_ly()
  })

  #sample map
  output$sample_map <- leaflet::renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolygons(data = basins) %>%
      addCircleMarkers(data = sample,
                       radius = 6,
                       color = "yellow")
  })

}

# Run the app ---------------------------
shinyApp(ui = ui, server = server)