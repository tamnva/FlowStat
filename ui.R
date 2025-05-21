library(leaflet)

navbarPage(
  "Near-Real-Time Streamflow Statistics", 
  id="nav",
  

  
  tabPanel(
    "Interactive map",
    
    div(
      class="outer",
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
      ),
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      add_busy_spinner(spin = "fading-circle", position = "top-left", 
                       margins = c(70, 70)),
      
      leafletOutput("map", 
                    width="100%", 
                    height="100%"),
      
      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(

        id = "controls", 
        class = "panel panel-default", 
        fixed = FALSE,
        draggable = TRUE, 
        top = 18, 
        left = "auto", 
        right = 10, 
        bottom = "auto",
        width = 400, 
        height = "auto",
        cursor = "auto",
        
        # Spatial visualization for all gauges
        checkboxInput('spatial_setting', 
                      'Spatial visualization settings (all gauges)', 
                             value = 1, width = "100%"),
        
        # Detail settings
        column(width = 12,
               conditionalPanel(
                 condition = "input.spatial_setting == 1",
                 column(width = 10,
                        selectInput("select_country", 
                                    "1. Select Country", 
                                    list("Germany",
                                         "Switzerland",
                                         "France")),
                 ),
                 
                 column(width = 2,
                        actionButton("help_country", "?", style = "margin-top: 25px;"),
                 ),
                 
                 # Select last n simulation dates
                 column(width = 10,
                        selectInput("station_visual", 
                                    "2. Select station coloring scheme by", 
                                    list("NSE",
                                         "Q daily mean period")),
                 ),
                 
                 column(width = 2,
                        actionButton("help_station", "?", style = "margin-top: 25px;"),
                 ),
                 
                 conditionalPanel(
                   condition = "input.station_visual.includes('Q daily mean period')",
                   column(width = 10,
                          dateRangeInput("date_range", 
                                         "3. Select date range",
                                         start = "2015-04-01", 
                                         end = "2015-05-18"),
                   ),
                   column(width = 10,
                          checkboxInput('visualize', 
                                        '4. Check to visualize result', 
                                        value = 0, width = "100%")
                   )
                 ),
               ),
        ),
        
        

        
        column(width = 12,
               # Temporal visualization for selected gaug
               checkboxInput('temporal_setting', 'Temporal visualization settings (single selected gauge)', 
                             value = 1, width = "100%"),
               conditionalPanel(
                 condition = "input.temporal_setting == 1",
                 plotlyOutput("input_data", height = 200),
               ),
        ),
      ),
    ),
  ),
)
