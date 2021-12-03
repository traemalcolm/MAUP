#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse) #data wrangling
library(uuid)
library(vroom) #reading and importing data
library(sf) #spatial data
# library(tigris) #geojoin
library(leaflet) #interactive maps
library(htmlwidgets) #interactive map labels 
library(leaflet.extras)
library(shiny)
library(googlesheets4)


shpfiles <- c(
  'censusblocks/censusblocks_fire_Clip.shp', # census blocks
  'censusblocks/grid_fire', # grid
  'censusblocks/neighborhoods_fire_Clip', # neighborhoods
  'censusblocks/wards_fire_Clip', # wards
  'censusblocks/zipcodes_fire_Clip' # zipcodes
)

bins <- c(
  c(0, 6, 14, 31, 72, 145), # census blocks
)



selected <- sample(0:length(shpfiles), 1)


#read in shapefile 
censusblocks <- st_read("censusblocks/censusblocks_fire_Clip.shp")

### MAKE INTERACTIVE MAP
labels <-sprintf(
  "<strong>%s</strong><br/>%s fire incidents in September 2021",
  censusblocks$NAMELSAD20, censusblocks$COUNT) %>%
  lapply(htmltools::HTML)

#color palette 
bin = c(0, 6, 14, 31, 72, 145)
pal <- colorBin(palette = "OrRd", bins = bin, domain = censusblocks$COUNT)

blocks_interactive <- censusblocks %>%
  st_transform(crs = st_crs("+init=epsg:4326")) %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(label = labels, 
              stroke = FALSE, 
              smoothFactor = .5, 
              opacity = 1, 
              fillOpacity = 0.7, 
              fillColor = ~ pal(COUNT), 
              highlightOptions = highlightOptions(weight = 5,
                                                  fillOpacity = 1, 
                                                  color = "white", 
                                                  opacity = 1, 
                                                  bringToFront = TRUE)) %>%
  addDrawToolbar(
    targetGroup = "draw",
    polylineOptions = FALSE,
    polygonOptions = FALSE,
    circleOptions = FALSE,
    rectangleOptions = FALSE,
    circleMarkerOptions = FALSE,
    markerOptions = drawMarkerOptions(makeAwesomeIcon(icon = "fire", library="glyphicon", markerColor = "red")), #markerIcon = fireIcon),
    editOptions = editToolbarOptions(
      selectedPathOptions = selectedPathOptions()
    )
  ) %>% 
  addLegend("bottomright", 
            pal = pal, 
            values = ~ COUNT, 
            title = "fire incidents", 
            opacity = 0.7)


# Define UI for application 
ui <- fluidPage(
  mainPanel(
    leafletOutput("blocks", width = "100%", height = 800)
  )
)

# load google sheet
# TODO: load API creds automatically
gsOut <- gs4_get('https://docs.google.com/spreadsheets/d/11DX7BY-xk_CvkJ7HG3MlOpQYkGCf1B3lBIQzxzJ1jUM/edit?usp=sharing')

# generate UUID
uid <- UUIDgenerate()

# Define server logic required to draw a map
server <- function(input, output) {
  
  # render map
  output$blocks <- renderLeaflet(blocks_interactive)
  
  # observe new marker events
  observeEvent(input$blocks_draw_new_feature,{
    print('new marker')
    features <- input$blocks_draw_new_feature
    
    # push row to google sheet
    newRow <- data.frame(
      uid,
      features$properties$`_leaflet_id`, 
      features$geometry$coordinates[[1]], 
      features$geometry$coordinates[[2]],
      Sys.time(),
      FALSE
      )
    names(newRow) <- c('uuid', 'leaflet_id', 'long', 'lat', 'timestamp', 'is_deleted')
    sheet_append(gsOut, data = newRow) # push to sheet
    
    # log
    print('new marker')
    print(newRow)
    print('====================')
  }
  )
  
  # observe edit marker events 
  observeEvent(input$blocks_draw_edited_features,{
    features <- input$blocks_draw_edited_features
    print('edit marker event')
    
    # push row to google sheet
    # TODO: edit all markers, not just the first one
    newRow <- data.frame(
      uid,
      features$features[[1]]$properties$`_leaflet_id`,
      features$features[[1]]$geometry$coordinates[[1]], 
      features$features[[1]]$geometry$coordinates[[1]],
      Sys.time(), 
      FALSE
    )
    names(newRow) <- c('uuid', 'leaflet_id', 'long', 'lat', 'timestamp', 'is_deleted')
    sheet_append(gsOut, data = newRow) # push to sheet
    
    # log
    print('edited marker')
    print(newRow)
    print('====================')
  }
  )
  
  # observe delete marker events 
  observeEvent(input$blocks_draw_deleted_features,{
    features <- input$blocks_draw_deleted_features
    
    # TODO: delete all markers, not just the first one in the list
    # push row to google sheet
    newRow <- data.frame(
      uid,
      features$features[[1]]$properties$`_leaflet_id`,
      features$features[[1]]$geometry$coordinates[[1]],
      features$features[[1]]$geometry$coordinates[[2]],
      Sys.time(),
      TRUE
    )
    names(newRow) <- c('uuid', 'leaflet_id', 'long', 'lat', 'timestamp', 'is_deleted')
    sheet_append(gsOut, data = newRow) # push to sheet

    # log
    print('deleted marker')
    print(newRow)
    print('====================')
  }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
