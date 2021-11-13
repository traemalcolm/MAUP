require(shiny)
require(sf)
require(dplyr)
require(leaflet)

# Here, we make our tracts data globally available.
tracts <- st_read('eviction_tracts.geojson') %>%
  mutate(
    majority_nonwhite = case_when(
      rent_pct_white < 50 ~ TRUE,
      TRUE ~ FALSE
    ),
    rent_pct_nonwhite = 100 - rent_pct_white,
    evic_p_hh = evic_count / tot_rental_hh
  )

# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  # Create a style that is applied to our app's body. Here, we want
  # it to occupy 100% of a browser's horizontal and vertical space.
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  # This leafletOutput creates a place for our leaflet map. Note that
  # the first argument is "map" - this means that we'll need to send
  # the renderLeaflet to a variable called output$tractmap in our server.R
  # function.
  leafletOutput("tractmap", width = "100%", height = "100%"),
  # Still within our bootstrapPage, we create an absolutePanel; this
  # is a shiny panel with absolute dimensions; i.e., these elements
  # render based on their absolute position on the page, not based on
  # their location relative to other elements on the page (our map
  # for example). We position it 10 pixels from the top, 10 pixels
  # from the right.
  absolutePanel(top = 10, right = 10,
      # We create a uiInput with the inputID "ma_town", meaning that
      # its values are accessible within server.R using input$ma_town.
      uiOutput("ma_town")
  )
)

# The server function handles the dynamic things.
server <- function(input, output, session) {
  # We want to dynamically filter data based on user input.
  # So! We create a reactive expression, which simply wraps a normal
  # expression, allowing it to change over time. When a reactive
  # value changes, other things that depend on it (for example, our
  # observe expression below) will be reevaluated.
  filteredCensus <- reactive({
    # Filter tracts on the basis of the town they're in.
    if (length(input$ma_town) > 0) {
      tracts %>% filter(ma_town == input$ma_town)  
    } else {
      tracts
    }
  })
  
  # Create a dynamic leaflet map using renderLeaflet.
  # Initially, only add provider tiles.
  # Here, you should only include those layers you don't
  # want to be reactive.
  output$tractmap <- renderLeaflet({
      leaflet() %>%
      addProviderTiles(
        providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)
      )
  })

  # The observe function includes operations that you want to be
  # dependent on user interaction. This entire block gets run
  # when you change the selection in the user interface.
  observe({
    # When the filteredCensus value changes, run this observe.
    sel_tracts <- filteredCensus()
    # get the bounding box of the selection
    bbox <- st_bbox(sel_tracts)
    # Create a color palatte using the variable here.
    pal <- colorNumeric("Greys", sel_tracts$rent_pct_white)
    # leafletProxy references an already-drawn map.
    leafletProxy("tractmap") %>%
      # Remove previously drawn shapes.
      clearShapes() %>%
      # Add a polygon layer using the selected tracts.
      addPolygons(
        data = sel_tracts,
        # No outline.
        stroke = FALSE,
        # Simplify geometries
        smoothFactor = 0.2,
        # Fully opaque.
        fillOpacity = 1,
        # Use the above color palatte to shade. 
        color = ~pal(rent_pct_white)
      )
    # When a new city is selected, fly to its boundaries (see bbox above).
    leafletProxy("tractmap") %>%
      flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
  })
  
  # Dynamically render the user interface on the basis of towns present in the
  # tract data.
  output$ma_town = renderUI({
    # Populate the uiInput with a selector which you can access with input$ma_town
    # and label it 'Town' in the UI.
    # The items in the selection should be all unique towns in MA.
    selectInput('ma_town', 'Town', sort(unique(tracts$ma_town), decreasing=FALSE))
  })
}

shinyApp(ui = ui, server = server)