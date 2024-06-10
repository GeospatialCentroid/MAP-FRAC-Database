# MAP-FRAC Shiny Web Application #
##################################

# set up ----
library(shiny)
library(shinyWidgets)
library(bslib)
library(sf)
library(plotly)
library(leaflet)
library(leaflegend)
library(DT)
library(tidyverse)
library(data.table)


# read in data
#load("data/app_data.RData")
load("data/app_data_Update.RData")

# create color palettes (this is a hacky way based on alphabetical order of basin names)
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

pal_salinity <- colorNumeric(palette = "Reds", domain = sample_app$salinity_conductivity_m_s_cm)



# jitter well locations for sample map
# sample_app <- st_jitter(sample_app, factor = 0.005) %>% 
#   st_transform(crs = 4326)

# for now, create string of basin names that does not include international ones
basin_names <- sample_app %>% 
  filter(!shale_basin %in% c("Bowland Shale", "Sichuan", "Western Canadian")) %>% 
  arrange(shale_basin) %>% 
  pull(shale_basin) %>% 
  unique()

# fix for NA placement in legend
css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

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
    id = "nav",
    "MAP-FRAC Database",
    
    ## Sample Explorer ----
    tabPanel(
      "Sample Explorer",
      h4(
        strong(
          "The Microorganisms Affecting Production in FRACturing systems (MAP-FRAC)
         database"
        ),
        "is a metagenomic catalog of microbial functional potential across
         geologically-distinct shale basins."
      ),
      p(
        style = "color: #a3a2a2",
        "Explore the",
        strong("178"),
        "produced fluid samples from",
        strong("32"),
        "fracking wells across",
        strong("8"),
        "shale basins underlying the United States."
      ),
      card(
        full_screen = TRUE,
        height = 600,
        layout_sidebar(
          open = TRUE,
          sidebar = sidebar(
            width = 375,
            position = "right",
            selectizeInput(
              "zoom_basin",
              "Zoom to Basin:",
              choices = basin_names,
              options = list(
                placeholder = 'Please select an option below',
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            radioGroupButtons(
              "point_type",
              "View points By:",
              choices = c("Wells", "Samples"),
              selected = "Wells"
            ),
            conditionalPanel(
              "input.point_type == 'Wells'",
              radioGroupButtons(
                "time_series",
                "Wells with timeseries sampling:",
                choices = c("Yes", 'No', "Show All Samples"),
                selected = "Show All Samples",
                status = "primary"
              ),
              uiOutput("time_selection"),
              uiOutput("salinity_selection"),
              conditionalPanel(
                "input.time_series == 'Yes'",
                checkboxGroupInput(
                  "time_category",
                  "Time Series Stage:",
                  choiceNames = c("Early (0-100 days)", "Mid (100-365 days", "Late (>365 Days)"),
                  choiceValues = c("early", "mid", "late"),
                  selected = c("early", "mid", "late")
                )
              ),
              checkboxGroupInput(
                "salinity_class",
                "Salinity Classification:",
                choiceNames = c("Brine", "High", "Moderate", "Not Measured"),
                choiceValues = c("brine", "high", "moderate", "N/A"),
                selected = c("brine", "high", "moderate", "N/A")
              )
            ),
            conditionalPanel(
              "input.point_type == 'Samples'",
              selectizeInput(
                "sample_var_size",
                "Size Points By:",
                choices = c(
                  "Salinity" = "salinity_conductivity_m_s_cm",
                  "Percent Sulfide Producers" = "perc_sulfide_producers",
                  "Percent Acetate Producers" = "perc_acetate_producers",
                  "Percent Methanogens" = "perc_methanogens"
                ),
                options = list(
                  placeholder = 'Please select an option below',
                  onInitialize = I('function() { this.setValue(""); }')
                )
              ),
              p("Filter Samples:"),
              sliderInput(
                "salinity_range",
                "Salinity (Conductivity in mS/cm)",
                min = min(sample_app$salinity_conductivity_m_s_cm, na.rm = TRUE),
                max = round(max(
                  sample_app$salinity_conductivity_m_s_cm, na.rm = TRUE
                )),
                value = c(
                  min(sample_app$salinity_conductivity_m_s_cm, na.rm = TRUE),
                  round(max(
                    sample_app$salinity_conductivity_m_s_cm, na.rm = TRUE
                  ))
                )
              ),
              sliderInput(
                "sulfide_range",
                "Percent Sulfide Producers",
                min = min(sample_app$perc_sulfide_producers, na.rm = TRUE),
                max = round(max(
                  sample_app$perc_sulfide_producers, na.rm = TRUE
                )),
                value = c(
                  min(sample_app$perc_sulfide_producers, na.rm = TRUE),
                  round(max(
                    sample_app$perc_sulfide_producers, na.rm = TRUE
                  ))
                )
              ),
              sliderInput(
                "acetate_range",
                "Percent Acetate Producers",
                min = min(sample_app$perc_acetate_producers, na.rm = TRUE),
                max = round(max(
                  sample_app$perc_acetate_producers, na.rm = TRUE
                )),
                value = c(
                  min(sample_app$perc_acetate_producers, na.rm = TRUE),
                  round(max(
                    sample_app$perc_acetate_producers, na.rm = TRUE
                  ))
                )
              ),
              sliderInput(
                "methanogen_range",
                "Percent Methanogens",
                min = min(sample_app$perc_methanogens, na.rm = TRUE),
                max = round(max(sample_app$perc_methanogens, na.rm = TRUE)),
                value = c(min(sample_app$perc_methanogens, na.rm = TRUE), round(
                  max(sample_app$perc_methanogens, na.rm = TRUE)
                ))
              )
            )
          ),
          leafletOutput("sample_map", height = "100%")
        )
      ), 
      card(height = 750,
           #card_header(HTML(paste("Click a point on the map to view time series", em("(if available)")))),
           #card_header(em("De-select basin names from the legend on the right to zoom to certain wells")),
           plotlyOutput("timeseries")),
      card(height = 600, DT::dataTableOutput("sample_table"))
    ),
    
    ## Genome Explorer -----
    tabPanel(
      "Genome Explorer",
      h4("Placeholder for subheader"),
      navset_card_tab(
        title = "",
        id = "nav_genome",
        full_screen = TRUE,
        height = 600,
        # layout_sidebar(
        #   open = TRUE,
        sidebar = sidebar(
          position = "left",
          conditionalPanel(
            "input.nav_genome == 'MAG Relative Abundance'",
            selectizeInput(
              "max_abun_group",
              "Choose taxonomic level to summarize relative abundances by:",
              choices = c(
                "Domain" = "domain",
                "Phylum" = "phylum",
                "Class" = "class",
                "Order" = "order",
                "Familiy" = "family",
                "Genus" = "genus",
                "Species" = "species"
              ),
              selected = "genus"
            ),
            accordion(
              open = FALSE,
              accordion_panel(
                "Filter by Taxonomy:",
                selectizeGroupUI(
                  id = "taxonomy_filter",
                  inline = FALSE,
                  params = list(
                    domain = list(inputId = "domain", title = "Domain:"),
                    phylum = list(inputId = "phylum", title = "Phylum:"),
                    class = list(inputId = "class", title = "Class:"),
                    order = list(inputId = "order", title = "Order:"),
                    family = list(inputId = "family", title = "Family:"),
                    genus = list(inputId = "genus", title = "Genus:"),
                    species = list(inputId = "species", title = "Species:")
                  )
                )
              )
            ),
            em(
              "Disclaimer: Values shown on the map represent the maximum relative abundance value
                               for each taxa (taxonomic level to summarize by selected by user abover) averaged within
                               each basin/play."
            )
          ),
          conditionalPanel(
            "input.nav_genome == 'MAG Cores'",
            radioGroupButtons(
              "core_cutoff",
              "Core MAGS defined as:",
              choiceNames = c(">50% of samples per basin", ">70% of samples per basin"),
              choiceValues = c(0.5, 0.7),
              status = "primary"
            )
          )
        ), 
        nav_panel("MAG Relative Abundance",
                  leafletOutput("genome_map", height = "100%")),
        nav_panel("MAG Cores",
                  leafletOutput("cores_map"))

          
          ),
      card(height = 600, DT::dataTableOutput("genome_table"))
      
    )
  )
)

# Define server -----------------
server <- function(input, output, session) {
  

  ## reactive sample data -----------
  
   sample_filtered <- reactive({
     if (input$point_type == "Wells") {
       req(input$time_series)
       if (input$time_series == "Show All Samples") {
         sample_app %>%
           filter(salinity_classification %in% input$salinity_class) %>% 
           # count # samples per well
           group_by(well_id) %>%
           mutate(n_samples = n()) %>%
           distinct(well_id, .keep_all = TRUE) %>%
           ungroup()
       } else if (input$time_series == "Yes") {
         sample_app %>%
           filter(timeseries_stage %in% input$time_category,
                  salinity_classification %in% input$salinity_class) %>%
           # count # samples per well
           group_by(well_id) %>%
           mutate(n_samples = n()) %>%
           distinct(well_id, .keep_all = TRUE) %>%
           ungroup()
       } else {
         sample_app %>%
           filter(timeseries_stage == "none",
                  salinity_classification %in% input$salinity_class) %>%
           # count # samples per well
           group_by(well_id) %>%
           mutate(n_samples = n()) %>%
           distinct(well_id, .keep_all = TRUE) %>%
           ungroup()
       }
       
     } else {
       sample_app %>% 
         filter(salinity_conductivity_m_s_cm %inrange% input$salinity_range,
                perc_sulfide_producers %inrange% input$sulfide_range,
                perc_acetate_producers %inrange% input$acetate_range,
                perc_methanogens %inrange% input$methanogen_range)
     }
    
   })
    
  # observeEvent(input$salinity_range, {
  #   print(input$salinity_range[1])
  #   print(input$salinity_range[2])
  # })
   
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
        marker = list(size = 10),
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
      layout(
        showlegend = TRUE,
        legend = list(title = list(text = 'Shale Basin'), orientation = 'h'),
        xaxis = list(title = "Days Since Frack", gridcolor = "gray"),
        yaxis = list(title = "Fracking Well", gridcolor = "gray"),
        plot_bgcolor = 'transparent',
        paper_bgcolor = 'transparent',
        font = list(color = 'white')
      ) %>%
      # hacky way to get one trace per group in legend
      style(
        showlegend = FALSE,
        traces = c(2:5, 8:12, 13:15, 17, 19, 21:22, 24, 26, 28)
      )
    
    fig_2 <- sample_app %>%
      arrange(shale_basin) %>%
      plotly::plot_ly(height = 700, showlegend = F) %>%
      add_trace(
        x = sample_app$days_since_frack,
        y = sample_app$well_id,
        split = ~ sample_app$well_id,
        color = ~ factor(sample_app$shale_basin),
        marker = list(size = 10),
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
    
    subplot(
      fig_1,
      fig_2,
      nrows = 1,
      shareY = TRUE,
      shareX = TRUE,
      margin = 0.025,
      widths = c(0.8, 0.2)
    ) %>%
      layout(
        xaxis = list(range = c(0, 2500), title = "Days Since Frack"),
        xaxis2 = list(range = c(4500, 5000), gridcolor = "gray")
      )
    
  })
  
  
  ## sample map ----
  output$sample_map <- leaflet::renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      addMapPane("Basins", zIndex = 410) %>%
      addMapPane("Plays", zIndex = 420) %>%
      addMapPane("Inputs", zIndex = 430) %>%
      addMapPane("Wells", zIndex = 440) %>%
      addPolygons(
        data = sediment_basin,
        group = "Basins",
        stroke = FALSE,
        fillOpacity = 0.65,
        fillColor = ~ pal_basin(NAME),
        #fillColor = "#91B187",
        options = pathOptions(pane = "Basins"),
        popup = paste("Basin:",
                      sediment_basin$NAME)

      ) %>%
      addPolygons(
        data = filter(play_basin, Shale_play %in% sample_app$shale_play),
        group = "Plays",
        stroke = TRUE,
        weight = 0.5,
        color = "lightgrey",
        fillOpacity = 0.75,
        fillColor = ~ pal_play(Basin),
        options = pathOptions(pane = "Plays"),
        popup = paste("Play:",
                      play_basin[play_basin$Shale_play %in% sample_app$shale_play, ]$Shale_play)
      ) %>%
      # add plays not sampled
      addPolygons(
        data = filter(play_basin,!Shale_play %in% sample_app$shale_play),
        group = "Plays",
        stroke = FALSE,
        fillOpacity = 0.65,
        fillColor = "grey",
        options = pathOptions(pane = "Basins"),
        popup = paste("Play:",
                      play_basin[!play_basin$Shale_play %in% sample_app$shale_play, ]$Shale_play)
      ) %>%
      # add input samples
      addMarkers(
        data = st_jitter(sample_inputs, 0.05),
        group = "Inputs",
        options = pathOptions(pane = "Inputs"),
        icon = ~ makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-shape-png-25.png",
                 iconWidth = 18,
                 iconHeight = 18),
        popup = paste(
          paste("Sample Type:", sample_inputs$sample_type),
          "<br>",
          paste("Input Type:",
                paste0(sample_inputs$sample_subtype)),
          "<br>",
          "Well:",
          sample_inputs$well_id,
          "<br>",
          paste("Basin:", sample_inputs$shale_basin),
          "<br>",
          paste("Play:", sample_inputs$shale_play))
      ) %>%
      addScaleBar(position = "bottomright") %>%
      addLayersControl(
        position = "bottomleft",
        overlayGroups = c("Basins", "Plays", "Inputs"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  # 
  ### proxy for filtering points --------
  
   observe({
     input$nav
     #observeEvent(sample_filtered(), {
     if (nrow(sample_filtered()) == 0 | is.null(sample_filtered())) {
       leafletProxy('sample_map') %>%
         clearGroup(c("Wells", "jitter2")) %>%
         clearControls()
     } else {
       leafletProxy('sample_map') %>%
         clearGroup(c("Wells", "jitter2")) %>%
         clearControls() %>%
         addCircleMarkers(
           data = sample_jitter1(),
           group = "Wells",
           radius = if (input$point_type == "Wells") {
             ~ sqrt(n_samples) * 3
           } else {
             if (input$sample_var_size == "") {
               6
             } else {
               ~ sqrt(sample_jitter1()[[input$sample_var_size]])
             }
           },
       #radius = ~ sqrt(n_samples) * 3,
           stroke = TRUE,
           weight = 1,
           color =  "black",
           fillOpacity = 0.85,
           fillColor = "white",
           #fillColor = "#F7AD19",
           options = pathOptions(pane = "Wells"),
           popup =  if (input$point_type == "Wells") {
             paste(
               "Well:",
               sample_filtered()$well_id,
               "<br>",
               paste("Basin:", sample_filtered()$shale_basin),
               "<br>",
               paste("Play:", sample_filtered()$shale_play),
               "<br>",
               paste("Number of Samples:", sample_filtered()$n_samples),
               "<br>",
               paste(
                 "Range of days since frack:",
                 paste0(
                   sample_filtered()$min_days_since_frack,
                   "-",
                   sample_filtered()$max_days_since_frack
                 )
               ),
               '<img src=', paste0(sample_filtered()$well_id, "_area_plot.png"),
               ' width="300"',
               '>'
             )
           } else {
             paste(
               "Well:",
               sample_filtered()$well_id,
               "<br>",
               paste("Sample:", sample_filtered()$sample_id),
               "<br>",
               paste(
                 "Time Series Stage:",
                 sample_filtered()$timeseries_stage
               ),
               "<br>",
               paste(
                 "Salinity Classification:",
                 sample_filtered()$salinity_classification
               ),
               "<br>",
               paste(
                 "Salinity Conductivity (mS/cm):",
                 round(sample_filtered()$salinity_conductivity_m_s_cm)
               ),
               "<br>",
               paste(
                 "Percent Sulfide Producers:",
                 round(sample_filtered()$perc_sulfide_producers)
               ),
               "<br>",
               paste(
                 "Percent Acetate Producers:",
                 round(sample_filtered()$perc_acetate_producers)
               ),
               "<br>",
               paste(
                 "Percent Methanogens:",
                 round(sample_filtered()$perc_methanogens)
               )
               # '<img src=', paste0(sample_filtered()$well_id, "_area_plot.png"),
               # ' width="300"',
               # '>'
             )
           }
         ) %>%
         addCircleMarkers(
           data = sample_jitter2(),
           group = "jitter2",
           radius = if (input$point_type == "Wells") {
             ~ sqrt(n_samples) * 3
           } else {
             if (input$sample_var_size == "") {
               6
             } else {
               ~ sqrt(sample_jitter1()[[input$sample_var_size]])
             }
           },
           #radius = ~ sqrt(n_samples) * 3,
           stroke = TRUE,
           weight = 1,
           color = "black",
           fillOpacity = 0.85,
           fillColor = "white",
           options = pathOptions(pane = "Wells"),
           popup =  if (input$point_type == "Wells") {
             paste(
               "Well:",
               sample_filtered()$well_id,
               "<br>",
               paste("Basin:", sample_filtered()$shale_basin),
               "<br>",
               paste("Play:", sample_filtered()$shale_play),
               "<br>",
               paste("Number of Samples:", sample_filtered()$n_samples),
               "<br>",
               paste(
                 "Range of days since frack:",
                 paste0(
                   sample_filtered()$min_days_since_frack,
                   "-",
                   sample_filtered()$max_days_since_frack
                 )
               )
             )
           } else {
             paste(
               "Well:",
               sample_filtered()$well_id,
               "<br>",
               paste("Sample:", sample_filtered()$sample_id),
               "<br>",
               paste(
                 "Time Series Stage:",
                 sample_filtered()$timeseries_stage
               ),
               "<br>",
               paste(
                 "Salinity Classification:",
                 sample_filtered()$salinity_classification
               ),
               "<br>",
               paste(
                 "Salinity Conductivity (mS/cm):",
                 round(sample_filtered()$salinity_conductivity_m_s_cm)
               ),
               "<br>",
               paste(
                 "Percent Sulfide Producers:",
                 round(sample_filtered()$perc_sulfide_producers)
               ),
               "<br>",
               paste(
                 "Percent Acetate Producers:",
                 round(sample_filtered()$perc_acetate_producers)
               ),
               "<br>",
               paste(
                 "Percent Methanogens:",
                 round(sample_filtered()$perc_methanogens)
               )
             )
           }
         ) %>%
         groupOptions("Wells", zoomLevels = 1:6) %>%
         groupOptions("jitter2", zoomLevels = 7:20) %>%
         addLegendSize(
           values = if (input$point_type == "Wells") {
             sample_filtered()$n_samples
           } else {
             sample_filtered()[[input$sample_var_size]]
           },
           color = 'white',
           fillColor = 'white',
           # opacity = 0.5,
           title = if (input$point_type == "Wells") {
             HTML("Number of</br> Well Samples")
           } else {
             HTML("Selected Size Variable:")
           },
           shape = "circle",
           breaks = 4,
           baseSize = 8,
           orientation = "vertical",
           position = "bottomright"
         )
     }
     
   })
   
  
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
        setView(lng = zoom()[1],
                lat = zoom()[2],
                zoom = 7)
    }


  })
   
   
   ## sample table ------
   output$sample_table <- DT::renderDataTable(st_drop_geometry(sample_app),
                                              options = list(paging = FALSE))
   

  # genome filter ----------------
  taxa_mod <- callModule(
    module = selectizeGroupServer,
    id = "taxonomy_filter",
    inline = FALSE,
    # for now filter out international basins
    data = filter(genome_app, !Basin %in% c("Sichuan", "Western Canadian", "Bowland Shale")),
    vars = c(
      "domain",
      "phylum",
      "class",
      "order",
      "family",
      "genus",
      "species"
    )
  )
  
  ## genome map -----
   
   ### MAG Relative Abundance ------
  
  # reactive polygon layer based on filtered taxa
  basin_genome <- reactive({
    taxa_mod() %>% 
      group_by(Basin, !!sym(input$max_abun_group)) %>% 
      summarise(max_rel_abundance = max(rel_abundance)) %>% 
      group_by(Basin) %>% 
      summarise(avg_rel_abundance = mean(max_rel_abundance)) %>% 
      inner_join(sediment_basin, by = c("Basin" = "NAME")) %>% 
      st_as_sf()
  })
  
  
  play_genome <- reactive({
    taxa_mod() %>% 
      group_by(Basin, Play, !!sym(input$max_abun_group)) %>% 
      summarise(max_rel_abundance = max(rel_abundance)) %>% 
      group_by(Basin, Play) %>% 
      summarise(avg_rel_abundance = mean(max_rel_abundance)) %>% 
    inner_join(play_basin, by = c("Basin", "Play" = "Shale_play")) %>%
    st_as_sf()
  })
  
  # color palette
  pal_genome_basin <- reactive({
    colorNumeric(
      palette = c('#2cb2ba', '#94b674', '#fbb92d'),
      domain = basin_genome()$avg_rel_abundance
    )
  })
  
  # temp fix for removing weird placement of NA in legend - remove it entirely
  pal_noNA <- reactive({
    colorNumeric(
      palette = c('#2cb2ba', '#94b674', '#fbb92d'),
      domain = basin_genome()$avg_rel_abundance,
      na.color = NA
    )
  })
  
  pal_genome_play <- reactive({
    colorNumeric(
      palette = c('#2cb2ba', '#94b674', '#fbb92d'),
      domain = play_genome()$avg_rel_abundance
    )
  })
  
  # pull basin bounds for default view
  bbox <- st_bbox(sediment_basin) %>% as.vector()
  
  output$genome_map <- leaflet::renderLeaflet({
    leaflet() %>%
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
      addProviderTiles("OpenStreetMap") %>%
      addMapPane("Basins", zIndex = 410) %>%
      addMapPane("Plays", zIndex = 420) %>%
      addScaleBar(position = "bottomright") %>%
      addLayersControl(
        position = "bottomleft",
        overlayGroups = c("Basins", "Plays"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    
    input$nav
    
    if (nrow(basin_genome()) == 0 | is.null(basin_genome())) {
      leafletProxy('genome_map') %>%
        clearGroup(c("Basins", "Plays")) %>% 
        clearControls()
    } else {
      leafletProxy('genome_map') %>%
        clearGroup(c("Basins", "Plays")) %>%
        clearControls() %>%
        addPolygons(
          group = "Basins",
          data = basin_genome(),
          stroke = FALSE,
          fillOpacity = 0.65,
          fillColor = ~ pal_genome_basin()(avg_rel_abundance),
          popup = paste(
            "Basin:",
            basin_genome()$Basin,
            "<br>",
            "Average MAG Relative Abundance:",
            round(basin_genome()$avg_rel_abundance, 2)
          )
        ) %>%
        addPolygons(
          group = "Plays",
          data = play_genome(),
          stroke = FALSE,
          fillOpacity = 0.85,
          fillColor = ~ pal_genome_play()(avg_rel_abundance),
          popup = paste(
            "Basin:",
            play_genome()$Basin,
            "<br>",
            "Play:",
            play_genome()$Play,
            "<br>",
            paste(
              "Average MAG Relative Abundance:",
              round(play_genome()$avg_rel_abundance, 2)
            )
          )
        ) %>%
        addLegend(
          "bottomright",
          data = basin_genome(),
          values = ~ avg_rel_abundance,
          pal = pal_noNA(),
          #pal = pal_genome_basin(),
          title = "Average MAG <br/> Relative Abundance"
        ) 
        
    }
    
  })
  
  ### MAG Cores ------
  
  ## genome table ------
  output$genome_table <- DT::renderDataTable(taxa_mod(),
                                             options = list(paging = FALSE))
  
  
}

# Run the app ---------------------------
shinyApp(ui = ui, server = server)