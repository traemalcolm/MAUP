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

# create icon marker
fireIcon <- makeIcon(
  iconUrl = 'https://images.vexels.com/media/users/3/149795/isolated/lists/59a3259ace3f62753d684cb15f66d989-firefighter-hat-icon.png',
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

# Define UI for application 
ui <- fluidPage(
  mainPanel(
    leafletOutput("blocks", width = "100%", height = 800)
  )

    # # Application title
    # titlePanel("Old Faithful Geyser Data"),
    # 
    # # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #        plotOutput("distPlot")
    #     )
    # )
)

# Define server logic required to draw a map
server <- function(input, output) {
  
  # render map
  output$blocks <- renderLeaflet(blocks_interactive)
  
  # load google sheet
  # TODO: update to create a new google sheet each time in a directory
  # TODO: load API creds automatically
  gsOut <- gs4_get('https://docs.google.com/spreadsheets/d/11DX7BY-xk_CvkJ7HG3MlOpQYkGCf1B3lBIQzxzJ1jUM/edit?usp=sharing')
  
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
