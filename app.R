
library(shiny)
library(leaflet)
library(sf)
library(tidyverse)

B1G22 <- read_csv("P5Updated22.csv") %>%
  filter(!(is.na(lon)))
  
B1G22 <- as.data.frame(B1G22)

states <- read_sf("cb_2021_us_state_500k/cb_2021_us_state_500k.shp") %>%
  st_zm() %>%
  mutate(water_km2 = (AWATER / (1000*1000)) %>% round(2))

ui <- fluidPage(
  titlePanel("Offers Map of College Football"),
  

  leafletOutput("map", width = "100vw", height = "100vh"),
  
  absolutePanel(draggable = TRUE, class = "panel panel-default", fixed = TRUE,
                left = "auto", right = 10, top = 100, bottom = "auto", width = 250, height = "auto",
                h2("School Selection"),
  selectInput(inputId = "School", label = strong("School"),
              choices = unique(B1G22$Offerer),
              selected = "Nebraska"),
  sliderInput(inputId = "Stars",
              label = strong("Stars"),
              min = 1,
              max = 5,
              step = 1,
              value = c(1,5)),
  sliderInput(inputId = "Years",
              label = strong("Years"),
              min = 2021,
              max = 2022,
              step = 1,
              value = c(2022, 2022))
    ))
  
  
  #Input Panel (Stars, Possibly select multiple or one position and slider panel for years(2017-2022))
  
  #Consider search of schools in drop-down for future use of other schools
  
  #Possibly consider overlap of two schools
    



server <- function(input, output, session){
  dataset <- reactive({
    B1G22 %>%
      filter(Offerer == input$School) %>%
      filter(Stars >= input$Stars[1] & Stars <= input$Stars[2])
  })
  
  output$map <- renderLeaflet({
    leaflet(states) %>%
      addTiles() %>%
      addPolygons() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) 
  })
  
  
 observeEvent(input$Stars, {
   if(nrow(dataset()) > 0){
     leafletProxy("map", data = dataset()) %>%
       clearMarkers() %>%
       clearMarkerClusters() %>%
       addMarkers(lat = ~lat,
                  lng = ~lon,
                  label = ~Name,
                  popup = ~paste0(
                    "Name: ", Name, "<br>",
                    "High School: ", Place, "<br>",
                    "Stars: ", Stars, "<br>",
                    "Position: ", Position, "<br>",
                    "Year: ", Year, "<br>",
                    "Commited to: ", CommittedTo, "<br>"),
                  clusterOptions = markerClusterOptions())
   }
   else{
     leafletProxy("map") %>%
       clearMarkerClusters() %>%
       clearMarkers()
     
   }
 })
 observeEvent(input$School, {
   if(nrow(dataset()) > 0){
     leafletProxy("map", data = dataset()) %>%
       clearMarkers() %>%
       clearMarkerClusters() %>%
       addMarkers(lat = ~lat,
                  lng = ~lon,
                  label = ~Name,
                  popup = ~paste0(
                    "Name: ", Name, "<br>",
                    "High School: ", Place, "<br>",
                    "Stars: ", Stars, "<br>",
                    "Position: ", Position, "<br>",
                    "Year: ", Year, "<br>",
                    "Commited to: ", CommittedTo, "<br>"),
                  clusterOptions = markerClusterOptions())
   }
   else{
     leafletProxy("map") %>%
       clearMarkerClusters() %>%
       clearMarkers()
     
   }
 })
# 
#  observe({
#     leafletProxy("map", data = dataset()) %>%
#       clearMarkers() %>%
#       clearMarkerClusters() %>%
#       addMarkers(lat = ~lat,
#                  lng = ~lon,
#                  label = ~Name,
#                  popup = ~paste0(
#                                  "Name: ", Name, "<br>",
#                                  "High School: ", Place, "<br>",
#                                  "Stars: ", Stars, "<br>",
#                                  "Position: ", Position, "<br>",
#                                  "Year: ", Year, "<br>"),
#                  clusterOptions = markerClusterOptions())
# 
#   })
    
}
shinyApp(ui, server)