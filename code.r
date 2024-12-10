library(shiny)
library(leaflet)

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Interactive Map with Colourful Pins and Legend"),
  sidebarLayout(
    sidebarPanel(
      textInput("location", "Location Name:", value = "Example Location"),
      numericInput("latitude", "Latitude:", value = 53.46),
      numericInput("longitude", "Longitude:", value = -97.97),
      actionButton("add", "Add Location"),
      actionButton("clear", "Clear All"),
      hr(),
      p("Enter the location name, latitude, and longitude, then click 'Add Location'."),
      p("Click 'Clear All' to reset the map.")
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output, session) {
  # Reactive data to store locations
  locations <- reactiveVal(data.frame(
    location = character(),
    latitude = numeric(),
    longitude = numeric(),
    color = character(),
    stringsAsFactors = FALSE
  ))
  
  # Define a list of colours for the pins
  colors <- c(
    "red", "blue", "green", "purple", "orange",
    "darkred", "lightblue", "lightgreen", "pink", "cadetblue",
    "beige", "gray", "lightgray", "darkgreen", "darkblue"
  )
  
  # Add new location to the reactive data
  observeEvent(input$add, {
    new_location <- data.frame(
      location = input$location,
      latitude = input$latitude,
      longitude = input$longitude,
      color = colors[(nrow(locations()) %% length(colors)) + 1],  # Assign a colour based on number of locations
      stringsAsFactors = FALSE
    )
    locations(rbind(locations(), new_location))
  })
  
  # Clear all locations
  observeEvent(input$clear, {
    locations(data.frame(
      location = character(),
      latitude = numeric(),
      longitude = numeric(),
      color = character(),
      stringsAsFactors = FALSE
    ))
  })
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -97.97, lat = 53.46, zoom = 5)
  })
  
  # Update the map with pins and legend whenever locations change
  observe({
    data <- locations()
    map <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls()
    
    if (nrow(data) > 0) {
      # Add markers with unique colours
      for (i in seq_len(nrow(data))) {
        map <- map %>%
          addCircleMarkers(
            lng = data$longitude[i],
            lat = data$latitude[i],
            color = data$color[i],  # Use the colour for the marker
            radius = 8,  # Adjust the size of the pin
            popup = paste0(
              "<b>", data$location[i], "</b><br>",
              "Lat: ", data$latitude[i], "<br>",
              "Lon: ", data$longitude[i]
            )
          )
      }
      
      # Add a legend
      map <- map %>%
        addLegend(
          position = "bottomright",
          colors = data$color[1:min(nrow(data), length(colors))],
          labels = data$location[1:min(nrow(data), length(colors))],
          title = "Location Legend",
          opacity = 1
        )
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
