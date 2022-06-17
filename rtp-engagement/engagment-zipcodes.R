library(tidyverse)
library(here)

# Packages for Maps
library(sf)
library(leaflet)

wgs84 <- 4326
spn <- 2285 

psrc.zipcodes <- st_read(here('data', 'psrc_zipcodes.shp')) %>%
    st_transform(wgs84) %>%
    select(zipcode)

psrc.zipcodes.centroids <- psrc.zipcodes %>%
    st_transform(spn) %>%
    st_centroid() %>%
    st_transform(wgs84)


# Process and Clean Data and Create a Map -------------------------------------------
rtp.data <- read_csv(here('data', 'engagement_zipcodes.csv')) %>% mutate(zipcode = as.character(zipcode))

zipcode.point.response <- left_join(psrc.zipcodes.centroids, rtp.data, by=c("zipcode")) %>% drop_na()
    

i <- awesomeIcons(
    icon = 'user',
    iconColor = 'black',
    library = 'fa'
)

m <- leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("Zipcode","Responses"),
                     options = layersControlOptions(collapsed = TRUE)) %>%
    
    addEasyButton(easyButton(
        icon="fa-globe", title="Region",
        onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%
    
    addPolygons(data=psrc.zipcodes,
                fillOpacity = 0.0,
                opacity = 1.0,
                weight = 1,
                color = "#91268F",
                dashArray = "1",
                group = "Zipcode") %>%
    
    addAwesomeMarkers(data=zipcode.point.response, 
                      icon = i, 
                      clusterOptions = markerClusterOptions(),
                      group="Responses")



    
