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


  'censusblocks/censusblocks_fire_Clip.shp', # census blocks
  # 'censustracts/censustracts_fire_Clip.shp', # census tracts
  'grid_fire/grid_fire.shp', # grid
  'neighborhoods/neighborhoods_fire_Clip.shp', # neighborhoods
  'wards/wards_fire_Clip.shp', # wards
  'zipcodes/zipcodes_fire_Clip.shp' # zipcodes
)

bins <- list(
  c(0, 6, 14, 31, 72, 145), # census blocks
  # c(0, 12, 23, 39, 72, 149), # census tracts
  c(0, 2, 4, 8, 14, 23), # grid
  c(0, 48, 148, 264, 460, 739), # neighborhoods
  c(0, 124, 176, 231, 348, 534), # wards
  c(0, 21, 95, 156, 242, 402) # zipcodes
)

# select random map
selected <- sample(1:length(shpfiles), 1)
selected_shp <- shpfiles[selected]
selected_bin <- bins[selected][[1]]
print(selected)
print(selected_shp)
print(selected_bin)

#read in shapefile 
censusblocks <- st_read(selected_shp) # st_read("censusblocks/censusblocks_fire_Clip.shp")


### MAKE INTERACTIVE MAP
labels <-sprintf(
  "<strong>%s</strong><br/>%s fire incidents in September 2021",
  censusblocks$NAME, censusblocks$COUNT) %>%
  lapply(htmltools::HTML)

#color palette 
bin = selected_bin # c(0, 6, 14, 31, 72, 145)
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
  # App Title
  titlePanel("Assisting Boston Fire Chief Ducky"),
  # Sidebar with game intro 
  sidebarPanel(
    position = "right",
    img(src="ducky.png",width="45%"),
    h5("You’re our new mapping specialist, right? I’m so glad you’re here! 
        I’m Ducky, the chief of the Boston Fire Department. I need your help to 
        see how many fire-related incidents we had in each area last year and then we can use 
        that information to decide how to spend our budget to better serve the Boston area next year!"),
    h2("Game Rules"), 
    h5("I was given these six maps to work with...what do you think? 
       They show fire-related calls across the City, divided up by census tracts, census blocks, 
       zip code, neighborhood, wards, and a 500 ft x 500 ft grid. Place firehouses around Boston to
       cover the most calls by dragging and dropping firehouses using the 'Draw marker' button."),
    br(),
    h5("Pick one of the maps, and let's see how many fire-related calls we can cover!"),
    actionButton("results", "Click to see how you did!"), 
    ),
  mainPanel(
    
    
    #img(src = 'ducky.png', align="left"),
    # Random map button 
    tabsetPanel(id='my_tabsetPanel',
                tabPanel('Census Blocks',
                         leafletOutput('blocks', width = "100%", height = 800)
                ),
                tabPanel('Census Tracts',
                         leafletOutput('tracts', width = "100%", height = 800)
                ),
                tabPanel('Neighborhoods',
                         leafletOutput('neighborhoods', width = "100%", height = 800)
                ),
                tabPanel('Wards',
                         leafletOutput('wards', width = "100%", height = 800)
                ),
                tabPanel('Zip Codes',
                         leafletOutput('zipcodes', width = "100%", height = 800)
                ),
                tabPanel('500 ft x 500 ft grid',
                         leafletOutput('grid', width = "100%", height = 800)
                )
                
    ),
    br(),
    fixedRow(
      column(width = 6,
             leafletOutput('blocks_results')
      ),
      column(width = 6,
             leafletOutput('grid_results')
      )
    )
    
    )
  
  )
)

# load google sheet
# TODO: load API creds automatically
gsOut <- gs4_get('https://docs.google.com/spreadsheets/d/11DX7BY-xk_CvkJ7HG3MlOpQYkGCf1B3lBIQzxzJ1jUM/edit?usp=sharing')

# generate UUID
uid <- UUIDgenerate()

# Define server logic required to draw a map
server <- function(input, output) {
  
  # render maps for tabs 
  output$blocks <- renderLeaflet({blocks_interactive})
  output$tracts <- renderLeaflet({tracts_interactive})
  output$neighborhoods <- renderLeaflet({neighborhoods_interactive})
  output$wards <- renderLeaflet({wards_interactive})
  output$zipcodes <- renderLeaflet({zip_interactive})
  output$grid <- renderLeaflet({grid_interactive})
  
  
  #render tabs 
  observe({
    input$my_tabsetPanel
    
    tab1 <- leafletProxy('blocks') 
    tab2 <- leafletProxy('tracts')
    tab3 <- leafletProxy('neighborhoods')
    tab4 <- leafletProxy('wards')  
    tab5 <- leafletProxy('zipcodes')
    tab6 <- leafletProxy('grid')
  })
      
  
  ### FIRE HOUSE RESULTS MAP
  #read data from google sheet 
  firehouse_results <- read_sheet("https://docs.google.com/spreadsheets/d/1oBRqd221N29aGoxjarPzhUXa72RlRIdh7a3huNxVoYU/edit#gid=0")
  
  # generates the uuid 
    # TODO: probably not be the right place to generate the id... 
  id <- UUIDgenerate()
  
  # filter results into seperate data tables 
    # TODO: remember to change uuid to generated id
    # TODO: timestamp range needs to be corrected 
  blocks_data <- firehouse_results %>% filter(leaflet_id == 1231) %>%
                                       filter(uuid == 'ecf86ffe-390c-469c-985c-e8c70c190d8c') %>% 
                                       filter(timestamp == (min(timestamp): max(timestamp)))
                                      # max(timestamp) finds last timestamp but what's the beginning of the range?
  
  
  
  #render maps for results 
    # TODO: can't figure out how to use the proper markers
    # TODO: maps need to link to action button so they appear with the click 
  output$blocks_results <- renderLeaflet({blocks_interactive %>%
          addMarkers(data = blocks_data,
                     icon = fire_icon
          )
      })
  output$grid_results <- renderLeaflet({grid_interactive %>%
          addMarkers(data = blocks_data,
                     icon = fire_icon
          )
      })
}
 
  # observe new marker events
  observeEvent(input$blocks_draw_new_feature,{
    print('new marker')
    features <- input$blocks_draw_new_feature

  # load google sheet
  # TODO: update to create a new google sheet each time in a directory
  # TODO: load API creds automatically
  # gsOut <- gs4_get('https://docs.google.com/spreadsheets/d/11DX7BY-xk_CvkJ7HG3MlOpQYkGCf1B3lBIQzxzJ1jUM/edit?usp=sharing')

    
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
  
  
  
  


# Run the application 
shinyApp(ui = ui, server = server)
