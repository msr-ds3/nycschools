library(tidyverse)
library(dplyr)
library(leaflet)
library(httr)
library(sp)
library(rgdal)
library(readr)
library(htmlwidgets)



## load files

# load nyc census tracts file = nyc_tracts
load("/data/nycdoe/nyc_tracts.Rdata")
#marker schools
load("/data/nycdoe/clean_data/schools_lat_long.Rdata")

load("/data/nycdoe/clean_data/FINAL_mid_to_hs_feeder.Rdata")
load("/data/nycdoe/clean_data/FINAL_hs_to_mid_edit_feeder_data.Rdata")



##########################################


## school house icon

school_icon <- makeIcon(
  iconUrl = "http://www.freeiconspng.com/uploads/high-school-icon-png-8.png",
  iconWidth = 38, iconHeight = 38)


## creating middle school map
## when a middle school is chosen, the map will show which census tract 
#and which high schools students are going to


######## Function for getting middle school and year ########

get_mid_to_hs_map <- function(middle_school) {
  
  # data fed into map
  sample_ms_map_data <- mid_to_hs_edit %>% filter(mid == middle_school & year == 2013) %>% 
    distinct(school_census_tract, .keep_all = TRUE)
  
  # function for formatting output for school info
  format_school_info <- function(numStudents, location_name) {
    school_info <- paste(strsplit(location_name, "; ")[[1]], strsplit(numStudents, "; ")[[1]], sep=": ")
    paste(school_info, collapse = '\n')
  }
  
  format_school_info <- Vectorize(format_school_info)
 
  sample_ms_map_data <- sample_ms_map_data %>%
    mutate(text = format_school_info(numStudents, location_name))
  
  # marker school data
  marker_schools_search <- marker_schools %>% filter(dbn == middle_school)
  
  # merge spacial census tracts file with data
  ms_map <- merge(nyc_tracts, sample_ms_map_data, by.x=c("TRACTCE","COUNTYFP"), by.y = c("school_census_tract","hs_county"))
  
  #palette for map
  pal <- colorNumeric(palette = "Reds",
                      domain = range(ms_map@data$total_student_count, na.rm=T), na.color="#cccccc")
  
  # map
  leaflet(ms_map) %>%
    #addTiles() %>% 
    addPolygons(weight = .5, color="blue", fillColor = ~pal(total_student_count), 
                popup = ~text, 
                fillOpacity = .7, highlightOptions = highlightOptions(color = "white", weight = 3,
                                                                                                                                                                                                                                bringToFront = TRUE)) %>% 
    addLegend(pal = pal, values = ~total_student_count, opacity = 1,
              title = paste("Distribution of the </br> high schools students</br> go to")) %>%
    addMarkers(data = marker_schools_search, ~school_lon, ~school_lat, label = ~school_name, icon = school_icon) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(marker_schools_search$school_lon, marker_schools_search$school_lat, zoom = 12)
  
}

## call function for a sample school
get_mid_to_hs_map("10X368")



######## Function for getting middle school and year ########

get_hs_from_mid_map <- function(high_school) {
  # data fed into map
  sample_hs_map_data <- hs_to_mid_edit %>% filter(hs == high_school & year == 2013) %>% 
    distinct(school_census_tract, .keep_all = TRUE)
  
  # function for formatting output for school info
  format_school_info <- function(numStudents, location_name) {
    school_info <- paste(strsplit(location_name, "; ")[[1]], strsplit(numStudents, "; ")[[1]], sep=": ")
    paste(school_info, collapse = '\n')
  }
  
  format_school_info <- Vectorize(format_school_info)
  
  sample_hs_map_data <- sample_hs_map_data %>%
    mutate(text = format_school_info(numStudents, location_name))
  
  # marker school data
  marker_schools_search <- marker_schools %>% filter(dbn == high_school)
  
  # merge spacial census tracts file with data
  hs_map <- merge(nyc_tracts, sample_hs_map_data, by.x=c("TRACTCE","COUNTYFP"), 
                  by.y = c("school_census_tract","mid_county"))
  
  #palette for map
  pal <- colorNumeric(palette = "Reds",
                      domain = range(hs_map@data$total_student_count, na.rm=T), na.color="#cccccc")
  
  # map
  leaflet(hs_map) %>%
    #addTiles() %>% 
    addPolygons(weight = .5, color="blue", fillColor = ~pal(total_student_count),
                popup = ~text,
                fillOpacity = .7, highlightOptions = highlightOptions(color = "white", weight = 3,
                                                                                                                                                                                                                                bringToFront = TRUE)) %>% 
    addLegend(pal = pal, values = ~total_student_count, opacity = 1,
              title = paste("Distribution of the </br> middle schools </br> students come </br> from")) %>%
    addMarkers(data = marker_schools_search, ~school_lon, ~school_lat, label = ~school_name, icon = school_icon) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(marker_schools_search$school_lon, marker_schools_search$school_lat, zoom = 12)
  
}

# calling function
get_hs_from_mid_map("14K449")


