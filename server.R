
# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        providers$OpenTopoMap,
        options = providerTileOptions(opacity = 0.5),
        group = "OpenTopoMap") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        options = providerTileOptions(opacity = 0.5),
        group = "WorldImagery") %>%
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap", 
          "WorldImagery",
          "OpenTopoMap"
        ),
        overlayGroups = c("Subbasin", "Station", "Main basin"),
        options = layersControlOptions(collapsed = TRUE, 
                                       position = "bottomleft")
      )  %>%
      setView(lng = 9, lat = 50, zoom = 4)
  })

  
  observe({

#    if (input$c_type == "N-NO3 (wihout point sources)"){
#      icol <- 5
#    } else {
#      icol <- 6
#    }
#    
#    irow <- which(data[["10"]]$year == input$select_year )
#    
#    radius <- c()
#    
#    for (i in data[["stations"]]$object_id){
#      radius <- c(radius, as.numeric(data[[as.character(i)]][irow, icol]))
#      
#    }
    
    #radius <- period_mean(data, NA)$percentiles
    #print(radius)
    
    leafletProxy("map") %>%
      clearShapes() %>%
#      addPolygons(data = data[["main_basin"]],
#                  group = "Main basin",
#                  stroke = TRUE,
#                  fillOpacity = 0,
#                  weight = 2) %>%
      addCircleMarkers(data = stations,
                 lng = st_coordinates(stations)[,1],
                 lat = st_coordinates(stations)[,2],
                 radius = 4,
                 group = "Station",
                 fillColor = pcolor,
                 color = pcolor,
                 fillOpacity = 0.5,
                 stroke = FALSE,
                 popup = ~gauge_name,
                 layerId = ~gauge_id
      ) 
    
    output$clicked_name <- renderPrint({
      click <- input$map_marker_click
      
      print(click)
   
    })
    
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
