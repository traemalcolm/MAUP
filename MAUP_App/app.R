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
library(leaflet) #interactive maps
library(htmlwidgets) #interactive map labels 
library(leaflet.extras)
library(shiny)
library(googlesheets4)
library(uuid) #generate uuid  

#################### setup #################### 

# load google sheet
# TODO: load API creds automatically
gsOut <- gs4_get('https://docs.google.com/spreadsheets/d/11DX7BY-xk_CvkJ7HG3MlOpQYkGCf1B3lBIQzxzJ1jUM/edit?usp=sharing')

# generate UUID for current session
uid <- UUIDgenerate()
print(uid)

#################### select a random map to display #################### 


maptypes <- c('census blocks', 'census tracts', '500 sq ft grids', 'neighborhoods', 'wards', 'zipcodes')

shpfiles <- list(
  'censusblocks/censusblocks_fire_Clip.shp' # census blocks
  # 'censustracts/censustracts_fire_Clip.shp', # census tracts
  # 'grid_fire/grid_fire.shp', # grid
  # 'neighborhoods/neighborhoods_fire_Clip.shp', # neighborhoods
  # 'wards/wards_fire_Clip.shp', # wards
  # 'zipcodes/zipcodes_fire_Clip.shp' # zipcodes
)

bins <- list(
  c(0, 6, 14, 31, 72, 145) # census blocks
  # c(0, 12, 23, 39, 72, 149), # census tracts
  # c(0, 2, 4, 8, 14, 23), # grid
  # c(0, 48, 148, 264, 460, 739), # neighborhoods
  # c(0, 124, 176, 231, 348, 534), # wards
  # c(0, 21, 95, 156, 242, 402) # zipcodes
)

# select random map
selected <- sample(1:length(shpfiles), 1)
selected_map <- maptypes[selected]
selected_shp <- shpfiles[selected]
selected_bin <- bins[selected][[1]]
print(paste0('Randomly selected map of ', selected_map, ' (', selected_shp, ')'))

# read in selected shapefile 
censusblocks <- st_read(selected_shp) 

#################### draw the map #################### 


# make interactive map 
labels <-sprintf(
  "<strong>%s</strong><br/>%s fire incidents in September 2021",
  censusblocks$NAME, censusblocks$COUNT) %>%
  lapply(htmltools::HTML)

# define color palette 
bin = selected_bin
pal <- colorBin(palette = "OrRd", bins = bin, domain = censusblocks$COUNT)

# draw interactive map
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

# define UI for application 
ui <- fluidPage(
  titlePanel("Assisting Boston Fire Chief Ducky"), 
  sidebarPanel( # Sidebar with game intro 
    position = "right",
    img(src="ducky.png",width="45%"),
    h5("You’re our new mapping specialist, right? I’m so glad you’re here! 
        I’m Ducky, the chief of the Boston Fire Department. I need your help to 
        see how many fire-related incidents we had in each area last year and then we can use 
        that information to decide how to spend our budget to better serve the Boston area next year!"),
    h2("Game Rules"), 
    h5(paste0("I was given this map of ", selected_map, " to work with...what do you think? 
       Place firehouses around Boston to
       cover the most calls by dragging and dropping markers on the map.")),
    br(),
    h5("Pick one of the maps, and let's see how many fire-related calls we can cover!"),
    actionButton("results", "Click to see how you did!"), 
    ),
  mainPanel(leafletOutput("blocks", width = "100%", height = 800))
  )

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
      FALSE,
      selected_map
    )
    names(newRow) <- c('uuid', 'leaflet_id', 'long', 'lat', 'timestamp', 'is_deleted', 'map_type')
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
      features$features[[1]]$geometry$coordinates[[2]],
      Sys.time(),
      FALSE,
      selected_map
    )
    names(newRow) <- c('uuid', 'leaflet_id', 'long', 'lat', 'timestamp', 'is_deleted', 'map_type')
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
      TRUE,
      selected_map
    )
    names(newRow) <- c('uuid', 'leaflet_id', 'long', 'lat', 'timestamp', 'is_deleted', 'map_type')
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
