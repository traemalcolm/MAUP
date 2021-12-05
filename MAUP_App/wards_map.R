library(tidyverse) #data wrangling
library(vroom) #reading and importing data
library(sf) #spatial data
library(tigris) #geojoin
library(leaflet) #interactive maps
library(htmlwidgets) #interactive map labels 


#read in shapefile 
wards <- st_read("MAUP_App/wards/wards_fire_Clip.shp")



### MAKE INTERACTIVE MAP
labels <-sprintf(
  "<strong>%s</strong><br/>%s fire incidents in September 2021",
  wards$NAME, wards$COUNT) %>%
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
  
  addLegend("bottomright", 
            pal = pal, 
            values = ~ COUNT, 
            title = "wards fire incidents", 
            opacity = 0.7)

saveWidget(wards_interactive, "wards_fire_map.html")

# save map as RDS
saveRDS(wards_interactive, "all_wards.RDS")
