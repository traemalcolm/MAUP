library(tidyverse) #data wrangling
library(vroom) #reading and importing data
library(sf) #spatial data
library(tigris) #geojoin
library(leaflet) #interactive maps
library(htmlwidgets) #interactive map labels 

#read in census tract data 
neighborhoods_fire <- vroom("MAUP_App/neighborhoods_fire.csv")
neighborhoods_fire %>% mutate_if(is.numeric, as.character) -> neighborhoods_fire #convert numeric to character

#read in shapefile 
neighborhoods <- st_read("MAUP_App/neighborhoods/neighborhoods_fire_Clip.shp")


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
  
  addLegend("bottomright", 
            pal = pal, 
            values = ~ COUNT, 
            title = "fire incidents", 
            opacity = 0.7)

saveWidget(neighborhoods_interactive, "neighborhoods_fire_map.html")

# save map as RDS
saveRDS(neighborhoods_interactive, "all_neighborhoods.RDS")
