##########################################################################
#####                Climate ChanGe MiGration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   KYLLIAN JAMES | 2024-03-12                   #####
#####                        Mod.: 2024-03-21                        #####
#####                     World Admin Boundaries                     #####
##########################################################################

##########################
##### Load Libraries #####
##########################

library(shiny)
library(leaflet)
library(leaflet.extras)

##############
##### UI #####
##############

ui <- fluidPage(
  
  ##### Browser Window #####
  titlePanel("Coordinates"),
  
  ##### CSS Code #####
  tags$style(".leaflet-container { cursor: crosshair !important; }"),
  
  ##### World Map #####
  leafletOutput("world_map", width = "100%", height = 450),
  #MARGIN (Top: 10px)
  tags$div(style = "margin-top: 10px;"),
  ##### CLEAR VALUE(s) ACTION BUTTON #####
  actionButton(inputId = "clear_values", label = "Clear Values"),
  ##### CLEAR ONE VALUE ACTION BUTTON #####
  actionButton(inputId = "clear_one_value", label = "Clear Value [-1]"),
  #MARGIN (Top: 10px)
  tags$div(style = "margin-top: 10px;"),
  ##### TEXT ####
  verbatimTextOutput("coordinates")
  
  )

##################
##### SERVER #####
##################

server <- function(input, output, session) {
  
  RV <- reactiveValues(LATITUDE_VALUEs = numeric(0), LONGITUDE_VALUEs = numeric(0))
  
  output$world_map <- renderLeaflet({
    
    leaflet() %>%
      addTiles(options = tileOptions(minZoom = 1, maxZoom = 18, maxNativeZoom = 18)) %>%
      # addTiles(options = tileOptions(minZoom = 8, maxZoom = 13, maxNativeZoom = 18)) %>%
      addScaleBar(position = "bottomleft") %>% 
      setView(lng = 0, lat = 0, zoom = 1) %>%
      addDrawToolbar(
        position = "topleft", 
        polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE, 
        markerOptions = FALSE, circleMarkerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(
          stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75, 
          fill = TRUE, fillOpacity = 0.25)), 
        editOptions = editToolbarOptions(edit = FALSE, remove = TRUE))
    
        })
  
  observeEvent(input$world_map_click, {
    
    click <- input$world_map_click
    
    RV$LATITUDE_VALUEs <- c(RV$LATITUDE_VALUEs, round(click$lat, 5))
    RV$LONGITUDE_VALUEs <- c(RV$LONGITUDE_VALUEs, round(click$lng, 5))
    
    })
  
  output$coordinates <- renderPrint({
    
    cat("LATITUDE_VALUEs <- c(")
    for (i in seq_along(RV$LATITUDE_VALUEs)) {
      if (i > 1) {
        if (i %% 10 == 1) {cat(", \n  ")} else {cat(", ")}}
      cat(round(RV$LATITUDE_VALUEs[i], 5))}
    cat(")")
    
    cat("\nLONGITUDE_VALUEs <- c(")
    for (i in seq_along(RV$LONGITUDE_VALUEs)) {
      if (i > 1) {
        if (i %% 10 == 1) {cat(", \n  ")} else {cat(", ")}}
      cat(round(RV$LONGITUDE_VALUEs[i], 5))}
    cat(")")
    
    })
  
  observeEvent(input$clear_values, {
    
    RV$LATITUDE_VALUEs <- numeric(0)
    RV$LONGITUDE_VALUEs <- numeric(0)
    
    })
  
  observeEvent(input$clear_one_value, {
    
    RV$LATITUDE_VALUEs <- head(RV$LATITUDE_VALUEs, -1)
    RV$LONGITUDE_VALUEs <- head(RV$LONGITUDE_VALUEs, -1)
    
    })
  
  }

#######################
##### APPLICATION #####
#######################

shinyApp(ui = ui, server = server)