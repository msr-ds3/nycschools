library(httr)
library(leaflet)

#load data
load("/data/nycdoe/clean_data/students_go_to_school_join_county_join_latlong_join_name.Rdata")

#load tracts
load('/data/nycdoe/nyc_tracts.Rdata')

#############################################
#get schools zones shapefile (THIS IS High SCHOOLS!!!!)
r<- GET("https://data.cityofnewyork.us/api/geospatial/j4gs-ge7j?method=export&format=GeoJSON")
dbns <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
###############################################

#nice school icon
school_icon <- makeIcon(
  iconUrl = "http://www.freeiconspng.com/uploads/high-school-icon-png-8.png",
  iconWidth = 38, iconHeight = 38)

#function take dbn and make map
getMapFromDbn <- function(mydbn, myyear){
  sample_map_data <- student_go_to_schools_join_county_join_latLong_join_name %>% filter(dbn == mydbn & year == myyear)
  tracts_map <- merge(nyc_tracts, sample_map_data, by.x = c("TRACTCE","COUNTYFP"), by.y =c("census_tract", "county"))
  
  pal3 <- colorNumeric(palette = "Reds",
                       domain = range(tracts_map@data$numStudents, na.rm=T), na.color = "#cccccc")
  
  leaflet(tracts_map) %>%
    #addTiles() %>% 
    addPolygons(data = dbns, color = "black") %>%
    addPolygons(weight = .5, fillColor = ~pal3(numStudents), popup = ~paste("Number of Students:", numStudents), fillOpacity = .7) %>% 
    addLegend(pal = pal3, values = ~numStudents, opacity = 1, title = "number of students") %>%
    addMarkers(~lon, ~lat, icon = school_icon, label = ~`Location Name`) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(-73.98, 40.75, zoom = 13)
}


getMapFromDbn("10X445", 2010)