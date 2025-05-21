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
        draggable = FALSE, 
        top = 18, 
        left = "auto", 
        right = 10, 
        bottom = "auto",
        width = 400, 
        height = "auto",
        cursor = "auto",
        
        h4(" "),
        
        
        # Select countries
        

        column(width = 10,
               selectInput("select_country", 
                           "Country", 
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
                           "Station coloring by", 
                           list("NSE",
                                "Q daily mean period")),
        ),
        column(width = 2,
               actionButton("help_station", "?", style = "margin-top: 25px;"),
        ),
        
        
        dateRangeInput("date_range", 
                       "Date range",
                       start = "2015-01-01", 
                       end = as.character(Sys.Date())),
        
        plotlyOutput("input_data", height = 200),
        plotlyOutput("output_data", height = 200),
        
        verbatimTextOutput("clicked_name"),
      )
    )
  ),
)
