
library(sf)
library(leaflet)
library(ggplot2)
library(shiny)
library(dplyr)
library(data.table)
library(plotly)
library(lubridate)
library(shinybusy)
library(bslib)
#library(RColorBrewer)

# Remove this
setwd("C:/Users/nguyenta/Documents/GitHub/FlowStat")

stations <- read_sf(file.path("data", "de_stations.shp")) 
basins <- read_sf(file.path("data", "de_basins.shp")) 
Q_data <- as_tibble(fread(file.path("data", "de_sim_discharge.csv")))
Q_data$date <- as.Date(Q_data$date)

"DE210310"
# Function to plot daily statistics
#------------------------------------------------------------------------------#
#                                Daily plot by years                           #
#                https://waterwatch.usgs.gov/index.php?id=ww_past              #
#------------------------------------------------------------------------------#
daily_stat <- function(input_data, gaugeid, plot_type, log_y){ 
  #input_data <- Q_data
  #gaugeid <-  "DEA11100" 
  Q_gauge_id <- input_data %>% 
    filter(gauge_id == gaugeid)
  
  current_year <- year(tail(Q_gauge_id$date, 1))
  
  if (plot_type == "Daily (by year)"){
    Q_daily_stat <- Q_gauge_id %>%
      filter(year(date) < current_year) %>%
      mutate(year = year(date),
             month = month(date),
             day = day(date)) %>%  
      group_by(month, day) %>%
      summarise(
        Q_min = min(Q_cms),
        Q_10 = quantile(Q_cms, c(0.05)),
        Q_25 = quantile(Q_cms, c(0.25)),
        Q_50 = quantile(Q_cms, c(0.5)),
        Q_75 = quantile(Q_cms, c(0.75)),
        Q_90 = quantile(Q_cms, c(0.95)),
        Q_max = max(Q_cms),
        .groups = 'drop'
      )
  } else {
    Q_daily_stat_cumsum <- Q_gauge_id %>%
      filter(year(date) < current_year) %>%
      mutate(year = year(date),
             month = month(date),
             day = day(date)) %>%  
      group_by(year) %>%
      mutate(Q_cms = cumsum(Q_cms))  %>%
      group_by(month, day) %>%
      summarise(
        Q_min = min(Q_cms),
        Q_10 = quantile(Q_cms, c(0.05)),
        Q_25 = quantile(Q_cms, c(0.25)),
        Q_50 = quantile(Q_cms, c(0.5)),
        Q_75 = quantile(Q_cms, c(0.75)),
        Q_90 = quantile(Q_cms, c(0.95)),
        Q_max = max(Q_cms),
        .groups = 'drop'
      )
  }
  

  
  date <- seq.Date(as.Date(paste0(current_year, "-01-01")),
                   as.Date(paste0(current_year, "-12-31")), by = "days")
  
  if (length(date) == 365) {
    if (plot_type == "Daily (by year)"){
      Q_daily_stat = Q_daily_stat[-c(60),]
    } else {
      Q_daily_stat_cumsum = Q_daily_stat_cumsum[-c(60),]
    }
  }
    
  
  if (plot_type == "Daily (by year)"){
    Q_daily_stat <- Q_daily_stat %>% mutate(date = date, .before = 1)
    
    plt <- ggplot(Q_daily_stat, aes(x = date)) +
      geom_line(aes(y = Q_50), color = "#009E73") +
      geom_ribbon(aes(ymin = Q_25, ymax = Q_75), fill = "#009E73", alpha = 0.2) +
      geom_ribbon(aes(ymin = Q_10, ymax = Q_90), fill = "#009E73", alpha = 0.2) +
      geom_ribbon(aes(ymin = Q_min, ymax = Q_max), fill = "#009E73", alpha = 0.2) +
      geom_line(data = Q_gauge_id %>% 
                  filter(year(date) == current_year) %>%
                  rename(Q_current_year = Q_cms),
                aes(x = date, y = Q_current_year), color = "#CC79A7") +
      labs(y = "Q (cms)", x = " ") +
      theme_bw()
    
  } else {
    Q_daily_stat_cumsum <- Q_daily_stat_cumsum %>% 
      mutate(date = date, .before = 1) 
    
    plt <- ggplot(Q_daily_stat_cumsum, aes(x = date)) +
      geom_line(aes(y = Q_50), color = "#009E73") +
      geom_ribbon(aes(ymin = Q_25, ymax = Q_75), fill = "#009E73", alpha = 0.2) +
      geom_ribbon(aes(ymin = Q_10, ymax = Q_90), fill = "#009E73", alpha = 0.2) +
      geom_ribbon(aes(ymin = Q_min, ymax = Q_max), fill = "#009E73", alpha = 0.2) +
      geom_line(data = Q_gauge_id %>% 
                  filter(year(date) == current_year) %>%
                  rename(Q_current_year = Q_cms),
                aes(x = date, y = cumsum(Q_current_year)), color = "#CC79A7") +
      labs(y = "Q (cms)", x = " ") +
      theme_bw()
  }
  
  if (log_y == 1) plt <- plt + scale_y_log10() 
  
  return(plt)
}

#------------------------------------------------------------------------------#
#                               perod statistics                               #
#------------------------------------------------------------------------------#
period_stat <- function(Q_input, period, gauge_id){
  
  #Q_input <- Q_data
  #period <- as.Date(c("2025-04-01", "2025-05-18"))
  #gauge_id <- stations$gauge_id #"DE210310"
    
  Q_input_period <- Q_input %>%
    mutate(day_of_year = yday(date),
           year = year(date)) %>%
    filter(day_of_year >= yday(yday(period[1])),
           day_of_year <= yday(period[2])) 
  
  Q_input_year <- Q_input_period %>% 
    filter(year == year(period[1])) %>% 
    group_by(gauge_id)  %>% 
    summarise(Q_cms_mean = mean(Q_cms))
  
  
  quantiles <- tibble(gauge_id = gauge_id, quantiles = NA)
  
  for (i in 1:length(gauge_id)){
    
    iloc <- which(Q_input_year$gauge_id == gauge_id[i])
    
    temp <- Q_input_period %>% 
      filter(gauge_id == gauge_id[i]) %>%
      summarise(quantiles = 100*ecdf(Q_cms)(Q_input_year$Q_cms_mean[iloc]))
    quantiles$quantiles[i] <- temp$quantiles
    
  }  
  
#  ggplot(quantiles, aes(x = "", y = quantiles)) +
#    geom_violin(fill = "skyblue", color = "black", alpha = 0.5) +
#    geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
#    labs(title = " ", y = "Number of gauges", x = "") +
#    #scale_y_continuous(
#    #  breaks = c(10, 25, 50, 75, 90),
#    #  labels = c("Much below normal (10%)", "Below normal (20%)", "Normal (50%)", 
#    #             "Above normal (75%)", "Much above normal (90%)"),
#    #  limits = c(0,100)
#    #) +
#    theme_bw()

  return(quantiles)
}

#period <- c(as.Date("2025-04-01"), as.Date("2025-04-30"))
#gauge_id <- stations$gauge_id

#period_stat_value <- period_stat(Q_data, c(as.Date("2025-04-01"), as.Date("2025-04-30")), stations$gauge_id)

#pcolor <- period_stat_value$color

#"DEA11100"
#"77.81109"
#"#023903"
#"#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7" "#999999"
#"#492050" "#90529C" "#C490CF" "#E4CAE9" "#F1F1F1" "#BCDABC" "#72B173" "#2C792D" "#023903"
#"#4A6FE3" "#788CE1" "#9DA8E2" "#C0C5E3" "#E2E2E2" "#E6BCC3" "#E495A5" "#DD6D87" "#D33F6A"
#"#841859" "#D05196" "#F398C4" "#FFD0E8" "#F6F6F6" "#C1E9C1" "#7CC57D" "#129416" "#005600"
