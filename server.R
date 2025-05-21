
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
      color <- c("#F1B6DA", "#B8E186",  "#4D9221", "#276419")
      pcolor <- colorBin(palette = color, bins = c(0.0, 0.5, 0.65, 0.75, 1.0))
      pcolor <- pcolor(ifelse(stations$NSE < 0, 0, stations$NSE))
      ptitle <- "NSE"
      plabels <- c("Unsatisfactory", "Satisfactory", "Good", "Very good")
      
    } else if ((input$station_visual == "Q daily mean period") &
               (input$visualize == 1)){
    
      color <- c("#D01C8B", "#F1B6DA", "#D0EBAB",  "#9CCE64", "#276419")
      pcolor <- colorBin(palette = color,bins = c(0, 10, 25, 75, 90, 100))
      
      # Replace with selected date
      period_stat_value <- period_stat(Q_data, input$date_range, stations$gauge_id)
      
      pcolor <- pcolor(period_stat_value$quantiles)
      ptitle <- "Q daily mean period"
      plabels <- c("Much below normal", "Below normal", "Normal", 
                   "Above normal", "Much above normal")
      
    } else {
      color <- c("#F1B6DA", "#B8E186",  "#4D9221", "#276419")
      pcolor <- colorBin(palette = color, bins = c(0.0, 0.5, 0.65, 0.75, 1.0))
      pcolor <- pcolor(ifelse(stations$NSE < 0, 0, stations$NSE))
      ptitle <- "NSE"
      plabels <- c("Unsatisfactory", "Satisfactory", "Good", "Very good")
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
                colors = color,
                title = ptitle,
                labels = plabels,
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
      print(event$id)
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
