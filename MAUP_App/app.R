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
library(shinyalert)

#################### setup #################### 

# load google sheet
# TODO: load API creds automatically
gsOut <- gs4_get('https://docs.google.com/spreadsheets/d/11DX7BY-xk_CvkJ7HG3MlOpQYkGCf1B3lBIQzxzJ1jUM/edit?usp=sharing')

#read data from google sheet
firehouse_results <- read_sheet("https://docs.google.com/spreadsheets/d/11DX7BY-xk_CvkJ7HG3MlOpQYkGCf1B3lBIQzxzJ1jUM/edit#gid=0")

# generate UUID for current session
uid <- UUIDgenerate()
print(uid)

#################### calculate results #################### 

# filter results to create a tbl with most recent markers
results_grouped <- firehouse_results %>%
  group_by(uuid, leaflet_id) %>%
  arrange(desc(timestamp)) %>%
  slice(1:1) %>% # get most recent marker position
  filter(!is_deleted) %>% # filter out deleted markers
  ungroup()

# save the types of map that were created
result_types <- results_grouped %>% 
  select(map_type) %>% 
  # mutate(map_type = tools::toTitleCase(map_type)) %>%
  unique() %>% 
  pull()

# for each type, save a map to a list
maps <- vector(mode="list", length = length(result_types))
names(maps) <- result_types
for (result_type in result_types) {
  maps[[result_type]] <- results_grouped %>% filter(map_type == result_type) %>% select(long, lat)
}

print(maps)

#################### select a random map to display #################### 


maptypes <- c('census blocks', 'census tracts', '500 sq ft grids', 'neighborhoods', 'wards', 'zipcodes')

shpfiles <- list(
  'censusblocks/censusblocks_fire_Clip.shp', # census blocks
   'censustracts/censustracts_fire_Clip.shp', # census tracts
  'grid_fire/grid_fire.shp', # grid
  'neighborhoods/neighborhoods_fire_Clip.shp', # neighborhoods
  'wards/wards_fire_Clip.shp', # wards
   'zipcodes/zipcodes_fire_Clip.shp' # zipcodes
)

bins <- list(
  c(0, 6, 14, 31, 72, 145), # census blocks
  c(0, 12, 23, 39, 72, 149), # census tracts
  c(0, 2, 4, 8, 14, 23), # grid
   c(0, 48, 148, 264, 460, 739), # neighborhoods
   c(0, 124, 176, 231, 348, 534), # wards
   c(0, 21, 95, 156, 242, 402) # zipcodes
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
            title = "Fire Incidents", 
            opacity = 0.7)



# define UI for application 
ui <- fillPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  useShinyalert(),
  absolutePanel(class = "panel panel-default", fixed = TRUE,
                 draggable = TRUE, top = 60, right = 20, bottom = "auto",
                 width = 330, height = 590,
    style = "z-index: 5",
    fluidRow(
      column(
        width = 12, 
        offset = 0, 
       div(style =  "padding: 10px 10px ", 
              img(src="ducky.png",width="45%"),
        h5("You're our new mapping specialist, right? I'm so glad you're here!
        I'm Ducky, the chief of the Boston Fire Department. I need your help
        to decide how to spend our budget to better serve the Boston area next year!"),
        h2("Game Rules"),
        h5(paste0("I was given this map of ", selected_map, " to work with. It shows how many fire-related
    incidents we had in each", selected_map, "last year. We have enough money in our budget this year to
    to open 3 new fire stations. Your job is to use the data this map provides to pick the location of each
    fire station. Once you're finished choosing the location of each fire station take a screenshot of your map
    to use for reference and then click the 'Show Results' button below to see how the location of your fire stations compares
    to others who have played the game."), 
           br(),
    
           h5("I can't wait to see how many incidents the new fire stations cover!")),
        br(),
        actionButton("results", "Show Results!"),
      ))
    )
   
  ),
  
  leafletOutput("blocks", width = "100%", height = "100%")
  
  
    # h5("Pick one of the maps, and let's see how many fire-related calls we can cover!"),
  # textOutput("warning"), 
  
  

              
  )

  
# 
# Define server logic required to draw a map
server <- function(input, output) {
  
  # marker counter 
  rv <- reactiveValues(n = 0)
  reactive_maps <- reactiveValues()

  # render map
  
  output$blocks <- renderLeaflet(blocks_interactive)
  
  # render results
  observeEvent(input$results, {
    leafletProxy('blocks') %>%
      clearShapes()%>%
      addHeatmap(group = "heat", data = if (selected_map == 'census blocks') {
          maps$'census blocks'
        } else if (selected_map == 'census tracts') {
          maps$'census tracts'
        } else if (selected_map == '500 sq ft grids') {
          maps$'500 sq ft grids'
        } else if (selected_map == 'neighborhoods') {
          maps$'neighborhoods'
        } else if (selected_map == 'wards') {
          maps$'wards'
        } else {
          maps$'zipcodes'
        }, max=1, blur=50)
    
  })
  
  # if (selected_map == 'census blocks') {
  #   maps$'census blocks'
  # } else if (selected_map == 'census tracts') {
  #   maps$'census tracts'
  # } else {
  #   maps$'zipcodes'
  # }

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

    # marker counter 
    rv$n <- isolate(rv$n) + 1
    print(rv$n)
    if(rv$n >= 3){ 
      shinyalert("Oops!", "You've run out of fire stations.", type = "error")
      }
    
    
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
    
    rv$n <- isolate(rv$n) - 1
  }
  
)
}



# Run the application 
shinyApp(ui = ui, server = server)
