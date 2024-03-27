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
load("data/app_data_Update.RData")

# create color palettes
pal_basin <- colorFactor(
  palette = c("#72266C", "#0A81A7", rep("grey", 5), "#F15F26", rep("grey", 3),
              "#D58D4E", rep("grey", 3), "#47B862", rep("grey", 4), "#FDBB3D",
               "#269380", rep("grey", 8),  "#B796C6", "grey"),
  domain = sediment_basin$NAME
)

pal_play <- colorFactor(
  palette = c("#72266C", "#0A81A7", rep("grey", 5), "#F15F26", rep("grey", 3),
              "#D58D4E", "grey", "#47B862", rep("grey", 5), "#FDBB3D", "grey",
              "#269380", rep("grey", 9),  "#B796C6", "grey"),
  domain = play_basin$Basin
)





# jitter well locations for sample map
# sample_app <- st_jitter(sample_app, factor = 0.005) %>% 
#   st_transform(crs = 4326)

# for now, create string of basin names that does not include international ones
basin_names <- sample_app %>% 
  filter(!shale_basin %in% c("Bowland Shale", "Sichuan", "Western Canadian")) %>% 
  arrange(shale_basin) %>% 
  pull(shale_basin) %>% 
  unique()

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
    #base_font = font_google("Inria Sans")
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
                        selectizeInput("zoom_basin", "Zoom to Basin:",
                        choices = basin_names,
                        options = list(
                          placeholder = 'Please select an option below',
                          onInitialize = I('function() { this.setValue(""); }')
                        )),
                        checkboxGroupButtons("time_series", "Filter by Time Series:",
                                    choiceNames = c("Early (0-100 days)", "Mid (100-365 days", "Late (>365 Days)", "No Time Series"),
                                    choiceValues = c("early", "mid", "late", "none"),
                                    selected = c("early", "mid", "late", "none"),
                                    individual = TRUE
                                    )),
                        leafletOutput("sample_map")
                      )
                    )
                   ),
               card(height = 750,
                   #card_header(HTML(paste("Click a point on the map to view time series", em("(if available)")))),
                 #card_header(em("De-select basin names from the legend on the right to zoom to certain wells")),
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
  sample_filtered <- reactive({
    sample_app %>%
      filter(timeseries_stage %in% input$time_series) %>%
      # count # samples per well
      group_by(well_id) %>%
      mutate(n_samples = n()) %>%
      distinct(well_id, .keep_all = TRUE) %>%
      ungroup()
    
  })
  ## jitter points for 2 zoom levels
  
  sample_jitter1 <- reactive({
    # if(nrow(sample_filtered()>0)) {
    st_jitter(sample_filtered(), factor = 0.015) %>%
      st_transform(crs = 4326)
    # }
  })
  
  sample_jitter2 <- reactive({
    st_jitter(sample_filtered(), factor = 0.005) %>%
      st_transform(crs = 4326)
  })
  
  
  
  ## time series plotly ----
    output$timeseries <- plotly::renderPlotly({
      fig_1 <- sample_app %>%
        arrange(shale_basin) %>%
        plotly::plot_ly(height = 700) %>%
        add_trace(
          x = sample_app$days_since_frack,
          y = sample_app$well_id,
          split = ~ sample_app$well_id,
          color = ~ factor(sample_app$shale_basin),
          colors = c(
            "#72266C",
            "#0A81A7",
            "#94305A",
            '#F15F26',
            "#D58D4E",
            "#47B862",
            "#FDBB3D",
            "#269380",
            "#60BFD9",
            "#D51D5C",
            "#B796C6"
          ),
          name = ~ sample_app$shale_basin,
          legendgroup = ~ sample_app$shale_basin,
          type = 'scatter',
          mode = 'lines+markers',
          connectgaps = TRUE
        ) %>%
        layout(showlegend = TRUE,
               xaxis = list(title = "Days Since Frack")) %>%
        # hacky way to get one trace per group in legend
        style(showlegend = FALSE,
              traces = c(2:5, 8:12, 13:15, 17, 19, 21:22, 24, 26, 28))
      
      fig_2 <- sample_app %>%
        arrange(shale_basin) %>%
        plotly::plot_ly(height = 700, showlegend = F) %>%
        add_trace(
          x = sample_app$days_since_frack,
          y = sample_app$well_id,
          split = ~ sample_app$well_id,
          color = ~ factor(sample_app$shale_basin),
          colors = c(
            "#72266C",
            "#0A81A7",
            "#94305A",
            '#F15F26',
            "#D58D4E",
            "#47B862",
            "#FDBB3D",
            "#269380",
            "#60BFD9",
            "#D51D5C",
            "#B796C6"
          ),
          name = ~ sample_app$shale_basin,
          #legendgroup = ~ sample_app$shale_basin,
          type = 'scatter',
          mode = 'lines+markers',
          connectgaps = TRUE
        ) #%>%
        # layout(showlegend = TRUE,
        #        xaxis = list(title = "Days Since Frack")) %>%
        # style(showlegend = FALSE,
        #       traces = c(2:5, 8:12, 13:15, 17, 19, 21:22, 24, 26, 28))

      subplot(fig_1,
              fig_2,
              nrows = 1,
              shareY = TRUE,
              shareX = TRUE,
              margin = 0.025,
              widths = c(0.8, 0.2)) %>%
        layout(xaxis = list(range = c(0, 2500), title = "Days Since Frack"),
               xaxis2 = list(range = c(4500, 5000)))
      
    })
  

  ## sample map ----
  output$sample_map <- leaflet::renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      addMapPane("Basins", zIndex = 410) %>%
      addMapPane("Plays", zIndex = 420) %>% 
      addMapPane("Wells", zIndex = 430) %>%
      addPolygons(
        data = sediment_basin,
        group = "Basins",
        stroke = FALSE,
        fillOpacity = 0.4,
        fillColor = ~pal_basin(NAME),
        #fillColor = "#91B187",
        options = pathOptions(pane = "Basins"),
        popup = paste(
          "Basin:",
          sediment_basin$NAME)
        
      ) %>%
      addPolygons(
        data = filter(play_basin, Shale_play %in% sample_app$shale_play),
        group = "Plays",
        stroke = TRUE,
        weight = 0.5,
        color = "lightgrey",
        fillOpacity = 0.75,
        fillColor = ~pal_play(Basin),
        options = pathOptions(pane = "Plays"),
        popup = paste(
          "Play:",
          play_basin[play_basin$Shale_play %in% sample_app$shale_play,]$Shale_play)
      ) %>% 
      addScaleBar(position = "bottomright") %>% 
      addLayersControl(position = "bottomleft",
                       overlayGroups = c("Basins", "Plays"),
                       options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      # add plays not sampled
      addPolygons(
        data = filter(play_basin, !Shale_play %in% sample_app$shale_play),
        group = "Plays",
        stroke = FALSE,
        fillOpacity = 0.65,
        fillColor = "grey",
        options = pathOptions(pane = "Basins"),
        popup = paste(
          "Play:",
          play_basin[!play_basin$Shale_play %in% sample_app$shale_play,]$Shale_play)
      ) %>% 
      addScaleBar(position = "bottomright") %>% 
      addLayersControl(position = "bottomleft",
                       overlayGroups = c("Basins", "Plays"),
                       options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # proxy for filtering points
  observeEvent(sample_filtered(), {
    
    if(nrow(sample_filtered()) == 0) {
      leafletProxy('sample_map') %>% 
        clearMarkers() %>% 
        clearControls()
    } else {
    
    leafletProxy('sample_map') %>% 
      clearMarkers() %>% 
        clearControls() %>% 
      addCircleMarkers(
        data = sample_jitter1(),
        group = "Wells",
        radius = ~ sqrt(n_samples)*3,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillOpacity = 0.75,
        fillColor = "#f7f257",
        #fillColor = "#F7AD19",
        options = pathOptions(pane = "Wells"),
        popup = paste(
          "Well:",
          sample_filtered()$well_id,
          "<br>",
          paste("Basin:", sample_filtered()$shale_basin),
          "<br>",
          paste("Play:", sample_filtered()$shale_play),
          "<br>",
          paste("Number of Samples:", sample_filtered()$n_samples),
          "<br>",
          paste("Range of days since frack:", paste0(sample_filtered()$min_days_since_frack, "-", sample_filtered()$max_days_since_frack))
        )
      ) %>% 
      addCircleMarkers(
        data = sample_jitter2(),
        group = "jitter2",
        radius = ~ sqrt(n_samples)*3,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillOpacity = 0.75,
        fillColor = "#f7f257",
        options = pathOptions(pane = "Wells"),
        popup = paste(
          "Well:",
          sample_filtered()$well_id,
          "<br>",
          paste("Basin:", sample_filtered()$shale_basin),
          "<br>",
          paste("Play:", sample_filtered()$shale_play),
          "<br>",
          paste("Number of Samples:", sample_filtered()$n_samples),
          "<br>",
          paste("Range of days since frack:", paste0(sample_filtered()$min_days_since_frack, "-", sample_filtered()$max_days_since_frack))
        )
      ) %>% 
      groupOptions("Wells", zoomLevels = 1:6) %>% 
      groupOptions("jitter2", zoomLevels = 7:20) %>% 
        addLegendSize(
          values = sample_filtered()$n_samples,
          color = '#f7f257',
          fillColor = '#f7f257',
         # opacity = 0.5,
          title = HTML("Number of</br> Well Samples"),
          shape = "circle",
          breaks = 5,
          baseSize = 10,
          orientation = "vertical",
          position = "bottomright")
    }
    
  })
  
  ## filter basin ----
  # observeEvent(input$select_lith, {
  #   leafletProxy('sample_map') %>%
  #     clearGroup("Basins") %>%
  #     addPolygons(
  #       data = basin_filtered(),
  #       group = "Basins",
  #       stroke = FALSE,
  #       fillOpacity = 0.5,
  #       fillColor = "#91B187",
  #       popup = paste(
  #         "Basin:",
  #         basin_filtered()$Basin,
  #         "<br>",
  #         paste("Lithology:", basin_filtered()$Lithology),
  #         "<br>",
  #         paste("Shale Play:", basin_filtered()$Shale_play)
  #       )
  #     )
  # })
  
  
  ### zoom to basin ------
  observeEvent(input$zoom_basin, {

    if (input$zoom_basin == "") {
      leafletProxy("sample_map")
    } else {

    zoom <- reactive({
      sediment_basin %>%
        filter(NAME == input$zoom_basin) %>%
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