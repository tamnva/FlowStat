
# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

function(input, output, session) {

  # Interactive Map 

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        providers$OpenTopoMap,
        options = providerTileOptions(opacity = 1),
        group = "OpenTopoMap") %>%
      addTiles(options = providerTileOptions(opacity = 1),
               group = "OpenStreetMap") %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        options = providerTileOptions(opacity = 1),
        group = "WorldImagery") %>%
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap",
          "OpenTopoMap",
          "WorldImagery"
        ),
        overlayGroups = c("Subbasin", "Station", "Main basin"),
        options = layersControlOptions(collapsed = TRUE, 
                                       position = "bottomleft")
      )  %>%
      setView(lng = 9, lat = 50, zoom = 4)
  })

  
  observe({
    
    if(input$station_visual == "NSE"){
      prange <- c(0,1)
      pal <- colorNumeric(palette = "PiYG", 
                          domain = prange, 
                          na.color = "#ffffff")
      
      pcolor <- pal(ifelse(stations$NSE < 0, 0, stations$NSE))
      ptitle <- "NSE"
      
    } else if (input$station_visual == "Q mean period"){
    
      pal <- colorNumeric(palette = "PiYG", 
                          domain = c(0,100), 
                          na.color = "#ffffff")
      
      period <- c(as.Date("2025-04-01"), as.Date("2025-04-30"))
      gauge_id <- stations$gauge_id
      
      period_stat_value <- period_stat(Q_data, 
                                       c(as.Date("2025-04-01"), 
                                         as.Date("2025-04-30")), 
                                       stations$gauge_id)
      
      pcolor <- pal(period_stat_value$quantiles)
      ptitle <- "Percentiles Q mean period"
      prange <- c(0,100)
      
    } else {
      
    }
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addCircleMarkers(data = stations,
                 lng = st_coordinates(stations)[,1],
                 lat = st_coordinates(stations)[,2],
                 radius = 3,
                 group = "Station",
                 fillColor = pcolor,
                 fillOpacity = 0.8,
                 stroke = FALSE,
                 popup = ~ paste0(gauge_name, "; NSE = ", round(NSE,2), 
                                  "; Area (skm) = ", round(are_skm, 1)),
                 layerId = ~gauge_id
      ) %>%
      clearControls() %>%
      addLegend(position = "bottomleft", 
                pal = pal,
                values = prange,
                title = ptitle,
                opacity = 1)
    
  })

  
  # Show a popup at the given location
  showZipcodePopup <- function(gauge_id, lat, lng) {
    
    content <- as.character(tagList(
      tags$h5("Gauge ID. = ", gauge_id),
      sprintf("Basin area (square km): %s", gauge_id), tags$br()
    ))
    
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = gauge_id)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    
    event <- input$map_marker_click
    
    if (is.null(event$id)){
      return()
    }
    
    isolate({
      #showZipcodePopup(event$id, event$lat, event$lng)
      
      plt <- daily_stat(Q_data, event$id)
      
      output$input_data <- renderPlotly({
        ggplotly(
          plt[["normal"]]
        )
      })
      
      output$output_data <- renderPlotly({
        ggplotly(
          plt[["cumsum"]]
        )
      })
      
      
      leafletProxy("map") %>%
        addPolygons(data = st_geometry(subset(basins, gauge_id == event$id)),
                    group = "Subbasin",
                    stroke = TRUE,
                    weight = 2,
                    layerId = "basin_shape_id"
        )
      
    })
  })
}
