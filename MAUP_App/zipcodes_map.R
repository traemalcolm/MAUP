library(tidyverse) #data wrangling
library(vroom) #reading and importing data
library(sf) #spatial data
library(tigris) #geojoin
library(leaflet) #interactive maps
library(htmlwidgets) #interactive map labels 


#read in shapefile 
zipcodes <- st_read("MAUP_App/zipcodes/zipcodes_fire_Clip.shp")



### MAKE INTERACTIVE MAP
labels <-sprintf(
  "<strong>%s</strong><br/>%s fire incidents in September 2021",
  zipcodes$NAME, zipcodes$COUNT) %>%
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
  
  addLegend("bottomright", 
            pal = pal, 
            values = ~ COUNT, 
            title = "fire incidents", 
            opacity = 0.7)

saveWidget(zip_interactive, "zipcodes_fire_map.html")

# save map as RDS
saveRDS(zip_interactive, "all_zip.RDS")
