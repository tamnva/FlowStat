
library(sf)
library(tidyverse)
library(shapefiles)

setwd("C:/Users/nguyenta/Documents/working_now/camels/de")

de_stations <-read_sf("CAMELS_DE_catchment_boundaries/gauging_stations/CAMELS_DE_gauging_stations.shp")
de_basins <- read_sf("CAMELS_DE_catchment_boundaries/catchments/CAMELS_DE_catchments.shp")
area_km2 <- st_area(de_basins)
de_stations <- st_transform(de_stations, "EPSG:4326")
de_basins <- st_transform(de_basins, "EPSG:4326")
de_basins$area_skm <- set_units(area_km2, "km2")

#colnames(de_basins) <- c("station_id", "name", "geometry")
#colnames(de_stations) <-  c("station_id", "name", "geometry")


st_write(de_stations, "de_stations.shp", append=FALSE)
st_write(de_basins, "de_basins.shp", append=FALSE)

st_write(stations, "de_stations.shp", append=FALSE)
st_write(basins, "de_basins.shp", append=FALSE)


# Read streamflow and other data
flist <- list.files("C:/Users/nguyenta/Documents/working_now/camels/de/timeseries",
                    full.names = TRUE)

library(data.table)

for (f in flist){
  print(f)
  station_id <- substr(basename(f), 31, 38)
  data <- as_tibble(read.csv(f, header = TRUE, sep = ",")) %>%
    select(date, discharge_vol_obs, precipitation_mean, temperature_mean) %>%
    rename(Q_cm_s = discharge_vol_obs,
           P_mm_day = precipitation_mean,
           T_mean_degree_celsius = temperature_mean) %>%
    mutate(gauge_id = substr(basename(f), 31,38),
           .before = 1)
  
  data$date <- as.Date(data$date, format = "%Y-%m-%d")
    
    
  if (f == flist[1]){
    fwrite(data, "temp.csv" )
  } else {
    fwrite(data, "temp.csv",append = TRUE, col.names = FALSE)
  }
    
}

de_timeseries <- fread("temp.csv")
de_timeseries <- as_tibble(de_timeseries)

saveRDS(de_timeseries, "de_timeseries.rds")
