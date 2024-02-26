# MAP-FRAC Shiny Web Application #
##################################

# set up
library(shiny)
library(shinyWidgets)
library(bslib)
library(sf)
library(plotly)
library(leaflet)
library(leaflegend)
library(DT)
library(tidyverse)


# read in data
load("data/app_data.RData")

# jitter well locations for sample map
sample_app <- st_jitter(sample_app, factor = 0.005)


# create summarized sample file for map (temporary)
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
                                    choices = unique(basin_sample$Lithology),
                                    selected = unique(basin_sample$Lithology),
                                    individual = TRUE
                                    ),
                        selectizeInput("zoom_basin", "Zoom to Basin:",
                        choices = unique(sample_app$basin),
                        options = list(
                          placeholder = 'Please select an option below',
                          onInitialize = I('function() { this.setValue(""); }')
                        ))),
                        leafletOutput("sample_map")
                      )
                    )
                   ),
               card(#height = 420,
                   #card_header(HTML(paste("Click a point on the map to view time series", em("(if available)")))),
                 card_header(em("De-select basin names from the legend on the right to zoom to certain wells")),
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
    basin_sample %>% 
      filter(Lithology %in% input$select_lith)
  })
  
  ## sample plotly (test) ----
  output$timeseries <- plotly::renderPlotly({
    sample_app %>% 
      plotly::plot_ly(height = 500, 
                      colors = "Dark2") %>% 
      add_trace(x = sample_app$days_since_frack,
                y = sample_app$well_ID,
                split = ~sample_app$well_ID,
                color = ~sample_app$Basin,
                name = ~sample_app$Basin,
                legendgroup = ~sample_app$Basin,
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE) %>% 
      layout(showlegend = TRUE,
             xaxis = list(title = "Days Since Frack")) %>% 
      # hacky way to get one trace per group in legend
      style(showlegend = FALSE, traces = c(1:4, 6:10, 12:14, 16, 18, 20:21, 23:24))
 
  })
  

  ## sample map ----
  output$sample_map <- leaflet::renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      addMapPane("Basins", zIndex = 410) %>%
      addMapPane("Wells", zIndex = 420) %>%
      addPolygons(
        data = basin_sample,
        group = "Basins",
        stroke = FALSE,
        fillOpacity = 0.5,
        fillColor = "#91B187",
        options = pathOptions(pane = "Basins")
        
      ) %>%
      addCircleMarkers(
        data = sample_sum,
        group = "Wells",
        radius = ~ sqrt(n_samples)*3,
        stroke = FALSE,
        fillOpacity = 0.5,
        fillColor = "#F7AD19",
        options = pathOptions(pane = "Wells"),
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
      ) %>% 
      addLegendSize(
        values = sample_sum$n_samples,
        color = '#F7AD19',
        fillColor = '#F7AD19',
       # opacity = 0.5,
        title = "Number of Samples",
        shape = "circle",
        breaks = 5,
        baseSize = 10,
        orientation = "horizontal",
        position = "bottomleft") %>% 
      addLayersControl(position = "bottomleft",
                       overlayGroups = c("Basins", "Wells"),
                       options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  ## filter basin ----
  observeEvent(input$select_lith, {
    leafletProxy('sample_map') %>%
      clearGroup("Basins") %>%
      addPolygons(
        data = basin_filtered(),
        group = "Basins",
        stroke = FALSE,
        fillOpacity = 0.5,
        fillColor = "#91B187",
        popup = paste(
          "Basin:",
          basin_filtered()$Basin,
          "<br>",
          paste("Lithology:", basin_filtered()$Lithology),
          "<br>",
          paste("Shale Play:", basin_filtered()$Shale_play)
        )
      )
  })
  
  
  ### zoom to basin ------
  observeEvent(input$zoom_basin, {
    
    if (input$zoom_basin == "") {
      leafletProxy("sample_map")
    } else {
    
    zoom <- reactive({
      sample_sum %>%
        filter(basin == input$zoom_basin) %>%
        st_bbox() %>%
        st_as_sfc(crs = st_crs(sample_app)) %>%
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
    data = st_drop_geometry(genome_app),
    vars = c("domain", "phylum", "class", "order", "family", "genus", "species")
  )
  output$genome_table <- DT::renderDataTable(taxa_mod())
  
  
  
  
  ## genome map -----
  
  
  # color palette 
  # If you want to set your own colors manually:
  pal <- colorNumeric(
    palette = c('#2cb2ba', '#94b674', '#fbb92d'),
    domain = basin_genome$n_MAG_samples
  )
  
  
  output$genome_map <- leaflet::renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolygons(
        data = basin_genome,
        stroke = FALSE,
        fillOpacity = 1,
        fillColor = ~pal(n_MAG_samples),
        popup = paste(
          "Basin:",
          basin_genome$Basin,
          "<br>",
          paste("Number of MAG samples:", basin_genome$n_MAG_samples)
        )
      ) %>% 
      addLegend("bottomright", data = basin_genome, values = ~n_MAG_samples,
                pal = pal, title = "Number of <br/> MAG samples")
  })
  


}

# Run the app ---------------------------
shinyApp(ui = ui, server = server)