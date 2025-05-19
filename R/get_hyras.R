
library(lubridate)
library(exactextractr)
library(data.table)
library(terra)
library(sf)

#setwd("C:/Users/nguyenta/Documents/GitHub/StreamFlowTracking")

#------------------------------------------------------------------------------#
#                Download historical hwd hyras data                            #
#------------------------------------------------------------------------------#
get_de_hist <- function(years=NA, data_dir=NA){
  
  options(timeout=3600)
  
  if (is.na(years)) years <- 2024:year(Sys.Date())
  if (is.na(data_dir)) data_dir <- tempdir()
  
  base_link <- paste0("https://opendata.dwd.de/climate_environment/",
                      "CDC/grids_germany/daily/hyras_de")
  file_name <- "_hyras_1_year_v6-0_de.nc"
  
  base_link_suffix <- c("precipitation", "air_temperature_min", 
                        "air_temperature_max", "humidity")
  file_name_prefix <- c("pr", "tasmin", "tasmax", "hurs")
  
  for (yr in years){
    for (i in 1:4){
      base_link_update <- file.path(base_link, base_link_suffix[i])
      file_name_update <- paste0(file_name_prefix[i], file_name)
      file_name_update <- gsub("year", yr, file_name_update)
      
      download.file(file.path(base_link_update, file_name_update), 
                    file.path(data_dir, file_name_update), mode="wb")
      
    }
  }
  
  output <- list()
  output[["years"]] <- years
  output[["data_dir"]] <- data_dir
  
  return(output)
}

#------------------------------------------------------------------------------#
#                         Extract hyras data                                   #
#------------------------------------------------------------------------------#
extract_de_hist <- function(years, data_dir, basins){
  
  file_name <- "_hyras_1_year_v6-0_de.nc"
  file_name_prefix <- c("pr", "tasmin", "tasmax", "hurs")
  
  output <- list() 
  
  for (yr in years){
    
    for (i in c(1:4)){
      
      file_name_update <- file.path(
        data_dir, gsub("year", yr, paste0(file_name_prefix[i], file_name)))
      
      # Get data
      data <- rast(file_name_update)
      
      if (yr == years[1]) basins <- st_transform(basins, crs(data))
      
      # Extract data
      data <- exact_extract(data, basins, fun = 'mean')
      
      data <- t(data.frame(data))
      colnames(data) <- basins$gauge_id
      
      if (yr == years[1]) {
        output[[file_name_prefix[i]]] <- data
        
      } else {
        output[[file_name_prefix[i]]] <- rbind(output[[file_name_prefix[i]]], data)
      }
    }
  }
  
  output[["date"]] <- seq.Date(as.Date(paste0(years[1], "-01-01")),
                            as.Date(paste0(years[1], "-01-01")) + 
                              nrow(output[[file_name_prefix[i]]]) - 1,
                            by = "days")
  
  
  return(output)
}



#------------------------------------------------------------------------------#
#                              Update data                                     #
#------------------------------------------------------------------------------#
update_data <- function(old_data_file, basins){
  
  message("Downloading historical data")
  historical_data <- get_de_hist(years=NA, data_dir=NA)
  
  message("Extrating historical data for basins")
  historical_data_basin_extract <- extract_de_hist(historical_data[["years"]], 
                                                   historical_data[["data_dir"]], 
                                                   basins)
  message("Reading historical data")
  old_data <- fread(old_data_file)
  old_data$time <- as.Date(substr(old_data$time,1,10))
  old_data <- old_data %>%
    filter(time < historical_data_basin_extract[["date"]][1])
  
  # Unique gauges
  gauge_id <- unique(old_data$object_id)
  
  
  message("Updating data")
  for (gauge in gauge_id){
    
    message(paste("Updating gauge:", gauge))
    col <- which(colnames(historical_data_basin_extract[["pr"]]) == gauge)
    
    new_data <- tibble(object_id = gauge,
           time = historical_data_basin_extract[["date"]],
           pr = historical_data_basin_extract[["pr"]][,col],
           tasmin = historical_data_basin_extract[["tasmin"]][,col],
           tasmax   = historical_data_basin_extract[["tasmax"]][,col],
           hurs  = historical_data_basin_extract[["hurs"]][,col],
           discharge_spec_obs = NA)
    
    
    temp <- old_data %>% 
      filter(object_id == gauge) %>%
      bind_rows(new_data) %>%
      mutate(time = paste0(time, " 00:00"),
             pr = round(pr, 2),
             tasmin  = round(tasmin, 2),
             tasmax   = round(tasmax, 2),   
             hurs  = round(hurs, 2),
             discharge_spec_obs = round(discharge_spec_obs, 2),)
    
    if (gauge == gauge_id[1]){ 
      fwrite(temp, file = old_data_file, append = FALSE, quote = FALSE, 
             row.names = FALSE, col.names = TRUE)
    } else {
      fwrite(temp, file = old_data_file, append = TRUE, quote = FALSE, 
             row.names = FALSE, col.names = FALSE)
      }
  }
}


#------------------------------------------------------------------------------#
#                              Update data                                     #
#------------------------------------------------------------------------------#
#update_data("C:/Users/nguyenta/Documents/Manuscript/google_flood_hub/data/lstm_data/time_series.csv", 
#            read_sf(file.path("data", "de_basins.shp")))
