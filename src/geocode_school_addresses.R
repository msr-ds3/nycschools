library(ggmap)
library(tidyverse)

# specify where the final file lives
rdata_file <- '/data/nycdoe/school_info_with_lat_long.Rdata'

if (file.exists(rdata_file)) {
  # if the file exists, load it and resume
  load(rdata_file)
} else {
  # otherwise extract dbns and addresses from the open data csv
  school_info <- read_csv('/data/nycdoe/LCGMS_SchoolData_20170727.csv')
  
  lat_long <- school_info %>%
    mutate(address = paste(`Primary Address`, City, `State Code`), lon=NA, lat=NA) %>%
    select(dbn = `ATS System Code`, address, lon, lat)
}

# find entries with missing lon and lat
ndx <- which(is.na(lat_long$lon) & is.na(lat_long$lat))

print(paste(length(ndx), "dbns left to geocode"))

# geocode missing entries
lat_long[ndx, c("lon","lat")] <- geocode(lat_long$address[ndx])

# save the results
save(school_info, lat_long, file=rdata_file)
