library(tidyverse) #data wrangling
library(vroom) #reading and importing data
library(sf) #spatial data
library(tigris) #geojoin
library(leaflet) #interactive maps
library(htmlwidgets) #interactive map labels 

#read in csv 
grid_fire <- vroom("MAUP_App/grid_fire.csv")

#read in shapefile 
blank_grid <- st_read("MAUP_App/blank_grid/boston_fishnet_500ft_clipped2.shp")
grid <- st_read("MAUP_App/grid_fire/grid_fire.shp")




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
  
  addLegend("bottomright", 
            pal = pal, 
            values = ~ COUNT, 
            title = "fire incidents", 
            opacity = 0.7)

saveWidget(zip_interactive, "zipcodes_fire_map.html")

# save map as RDS
saveRDS(grid_interactive, "all_grid.RDS")
