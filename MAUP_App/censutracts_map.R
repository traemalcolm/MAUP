library(tidyverse) #data wrangling
library(vroom) #reading and importing data
library(sf) #spatial data
library(tigris) #geojoin
library(leaflet) #interactive maps
library(htmlwidgets) #interactive map labels 

#read in census tract data 
tracts_fire <- vroom("MAUP_App/censustracts_fire.csv")
tracts_fire %>% mutate_if(is.numeric, as.character) -> tracts_fire #convert numeric to character

#read in shapefile 
censustracts <- st_read("MAUP_App/censustracts/censustracts_fire_Clip.shp")

#merge fire incident data with shapefile 
all_tracts <- geo_join(censustracts, tracts_fire,
                       'GEOID20', 'GEOID20',
                       how = "inner")
#save for shiny 
saveRDS(all_tracts, "MAUP_App/all_tracts.RDS")


### MAKE INTERACTIVE MAP
labels <-sprintf(
  "<strong>%s</strong><br/>%s fire incidents in September 2021",
  all_tracts$GEOID20, all_tracts$COUNT.x) %>%
  lapply(htmltools::HTML)

#color palette 
pal <- colorBin(palette = "OrRd", 6, domain = all_tracts$COUNT.x)

map_interactive <- all_tracts %>%
  st_transform(crs = st_crs("+init=epsg:4326")) %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(label = labels, 
              stroke = FALSE, 
              smoothFactor = .5, 
              opacity = 1, 
              fillOpacity = 0.7, 
              fillColor = ~ pal(COUNT.x), 
              highlightOptions = highlightOptions(weight = 5,
                                                  fillOpacity = 1, 
                                                  color = "white", 
                                                  opacity = 1, 
                                                  bringToFront = TRUE)) %>%
              
  addLegend("bottomright", 
            pal = pal, 
            values = ~ COUNT.x, 
            title = "fire incidents", 
            opacity = 0.7)

saveWidget(map_interactive, "censustracts_fire_map.html")
