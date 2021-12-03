#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse) #data wrangling
library(vroom) #reading and importing data
library(sf) #spatial data
# library(tigris) #geojoin
library(leaflet) #interactive maps
library(htmlwidgets) #interactive map labels 
library(leaflet.extras)
library(shiny)
library(googlesheets4)

censusblocks <- st_read("censusblocks/censusblocks_fire_Clip.shp")

### MAKE INTERACTIVE MAP
labels <-sprintf(
  "<strong>%s</strong><br/>%s fire incidents in September 2021",
  censusblocks$NAMELSAD20, censusblocks$COUNT) %>%
  lapply(htmltools::HTML)

#color palette 
bin = c(0, 6, 14, 31, 72, 145)
pal <- colorBin(palette = "OrRd", bins = bin, domain = censusblocks$COUNT)

# create icon marker
fireIcon <- makeIcon(
  iconUrl = 'firehat.png',
  iconWidth = 10,
  iconHeight = 10,
  iconAnchorX = 100,
  iconAnchorY = 100
)

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
    markerOptions = drawMarkerOptions(markerIcon = fireIcon),
    editOptions = editToolbarOptions(
      selectedPathOptions = selectedPathOptions()
    )
  ) %>% 
  addLegend("bottomright", 
            pal = pal, 
            values = ~ COUNT, 
            title = "fire incidents", 
            opacity = 0.7)

censustracts <- st_read("censustracts/censustracts_fire_Clip.shp")

### MAKE INTERACTIVE MAP
labels <-sprintf(
  "<strong>%s</strong><br/>%s fire incidents in September 2021",
  censustracts$NAME20, censustracts$COUNT) %>%
  lapply(htmltools::HTML)

#color palette 
bin <-  c(0, 12, 23, 39, 72, 149)
pal <- colorBin(palette = "OrRd", bins = bin , domain = censustracts$COUNT)

tracts_interactive <- censustracts %>%
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
    markerOptions = drawMarkerOptions(markerIcon = fireIcon),
    editOptions = editToolbarOptions(
      selectedPathOptions = selectedPathOptions()
    )
  ) %>% 
  
  addLegend("bottomright", 
            pal = pal, 
            values = ~ COUNT, 
            title = "fire incidents", 
            opacity = 0.7)

neighborhoods <- st_read("neighborhoods/neighborhoods_fire_Clip.shp")


### MAKE INTERACTIVE MAP
labels <-sprintf(
  "<strong>%s</strong><br/>%s fire incidents in September 2021",
  neighborhoods$Name, neighborhoods$COUNT) %>%
  lapply(htmltools::HTML)

#color palette 
bin <- c(0, 48, 148, 264, 460, 739)
pal <- colorBin(palette = "OrRd", bins = bin, domain = neighborhoods$COUNT)

neighborhoods_interactive <- neighborhoods %>%
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
    markerOptions = drawMarkerOptions(markerIcon = fireIcon),
    editOptions = editToolbarOptions(
      selectedPathOptions = selectedPathOptions()
    )
  ) %>% 
  
  addLegend("bottomright", 
            pal = pal, 
            values = ~ COUNT, 
            title = "fire incidents", 
            opacity = 0.7)


#read in shapefile 
wards <- st_read("wards/wards_fire_Clip.shp")

### MAKE INTERACTIVE MAP
labels <-sprintf(
  "<strong>%s</strong><br/>%s fire incidents in September 2021",
  wards$WARD, wards$COUNT) %>%
  lapply(htmltools::HTML)

#color palette 
bin <- c(0, 124, 176, 231, 348, 534)
pal <- colorBin(palette = "OrRd", bins = bin, domain = wards$COUNT)

wards_interactive <- wards %>%
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
    markerOptions = drawMarkerOptions(markerIcon = fireIcon),
    editOptions = editToolbarOptions(
      selectedPathOptions = selectedPathOptions()
    )
  ) %>% 
  
  addLegend("bottomright", 
            pal = pal, 
            values = ~ COUNT, 
            title = "wards fire incidents", 
            opacity = 0.7)

zipcodes <- st_read("zipcodes/zipcodes_fire_Clip.shp")

### MAKE INTERACTIVE MAP
labels <-sprintf(
  "<strong>%s</strong><br/>%s fire incidents in September 2021",
  zipcodes$ZIP5, zipcodes$COUNT) %>%
  lapply(htmltools::HTML)

#color palette 
bin <- c(0, 21, 95, 156, 242, 402)
pal <- colorBin(palette = "OrRd", bins = bin, domain = zipcodes$COUNT)


zip_interactive <- zipcodes %>%
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
    markerOptions = drawMarkerOptions(markerIcon = fireIcon),
    editOptions = editToolbarOptions(
      selectedPathOptions = selectedPathOptions()
    )
  ) %>% 
  
  addLegend("bottomright", 
            pal = pal, 
            values = ~ COUNT, 
            title = "fire incidents", 
            opacity = 0.7)


#read in shapefile 
blank_grid <- st_read("blank_grid/boston_fishnet_500ft_clipped2.shp")
grid <- st_read("grid_fire/grid_fire.shp")

### MAKE INTERACTIVE MAP
labels <-sprintf(
  "<strong>%s</strong><br/>%s fire incidents in September 2021",
  grid$OBJECTID, grid$COUNT) %>%
  lapply(htmltools::HTML)

#color palette
bin <- c(0, 2, 4, 8, 14, 23)
pal <- colorBin(palette = "OrRd", bins = bin, domain = grid$COUNT)

grid_interactive <- grid %>%
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
    markerOptions = drawMarkerOptions(markerIcon = fireIcon),
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
       zip code, neighborhood, wards, and a 5x5 km grid. Place firehouses around Boston to cover the 
       most calls by dragging and dropping firehouses using the 'Draw marker' button."),
    br(),
    h5("Pick one of the maps, and let's see how many fire-related calls we can cover!")
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
                tabPanel('500 ftx500 ft grid', 
                         leafletOutput('grid', width = "100%", height = 800)   
                )
                
    )
  )
  
)


# Define server logic required to draw a map
server <- function(input, output) {
  
  # render map
  
  output$blocks <- renderLeaflet({blocks_interactive})
  output$tracts <- renderLeaflet({tracts_interactive})
  output$neighborhoods <- renderLeaflet({neighborhoods_interactive})
  output$wards <- renderLeaflet({wards_interactive})
  output$zipcodes <- renderLeaflet({zip_interactive})
  output$grid <- renderLeaflet({grid_interactive})
  
  observe({
    input$my_tabsetPanel
    
    tab1 <- leafletProxy('blocks') 
    tab2 <- leafletProxy('tracts')
    tab3 <- leafletProxy('neighborhoods')
    tab4 <- leafletProxy('wards')  
    tab5 <- leafletProxy('zipcodes')
    tab6 <- leafletProxy('grid')
      
    
  })
  
  
  # load google sheet
  # TODO: update to create a new google sheet each time in a directory
  # TODO: load API creds automatically
  gsOut <- gs4_get('https://docs.google.com/spreadsheets/d/11DX7BY-xk_CvkJ7HG3MlOpQYkGCf1B3lBIQzxzJ1jUM/edit?usp=sharing')
  
  randomVals <- eventReactive(input$nex_map, {
    
  })

  # observe marker events 
  observeEvent(input$blocks_draw_edited_features,{
    feature <- input$blocks_draw_edited_features
    
    print(feature)
    
  }
  )
  
  observeEvent(input$blocks_draw_new_feature,{
    feature <- input$blocks_draw_new_feature
    
    # print(paste0('draw_all_features: ', input$blocks_draw_all_features))
    # print(paste0('draw_edited_features: ', input$blocks_draw_edited_features))
    # print(input)
    # print(paste0('draw_deleted_features: ', input$blocks_draw_deleted_features))
    
    newRow <- data.frame(feature$geometry$coordinates[[1]], feature$geometry$coordinates[[2]])
    names(newRow) <- c('long', 'lat')
    
    # append coordinates to sheet in the format (long, lat)
    sheet_append(gsOut, data = newRow)
    
    print(paste0("long", feature$geometry$coordinates[[1]]))
    print(paste0("lat", feature$geometry$coordinates[[2]]))
  }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
