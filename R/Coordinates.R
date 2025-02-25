##########################################################################
#####                Climate CHANGE MIGRATION Network                #####
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
  tags$style(".leaflet-container {cursor: crosshair !important;}"),
  
  ##### World Map #####
  leafletOutput("world_map", width = "100%", height = 510),
  #MARGIN (Top: 10px)
  tags$div(style = "margin-top: 10px;"),
  ##### Action Button => Clear Values #####
  actionButton(inputId = "clear_values", label = "Clear Values"),
  ##### Action Button => Clear One Value #####
  actionButton(inputId = "clear_one_value", label = "Clear Value [-1]"),
  #MARGIN (Top: 10px)
  tags$div(style = "margin-top: 10px;"),
  ##### Coordinates ####
  verbatimTextOutput("coordinates")
  
  )

##################
##### SERVER #####
##################

server <- function(input, output, session) {
  
  ##### Reactive Values (RV) #####
  RV <- reactiveValues(LATITUDE_VALUEs = numeric(0), LONGITUDE_VALUEs = numeric(0))
  
  ##### World Map Tiles #####
  output$world_map <- renderLeaflet({
    
    leaflet() %>%
      #Add Tile LAYER
      # addTiles(options = tileOptions(minZoom = 1, maxZoom = 18, maxNativeZoom = 18)) %>% #Min/Max Zoom Levels
      #Add Tile LAYER
      # addTiles(options = tileOptions(minZoom = 8, maxZoom = 13, maxNativeZoom = 18)) %>% #Min/Max Zoom Levels
      #Add Tile LAYER
      # addTiles(options = tileOptions(minZoom = 1, maxZoom = 10, maxNativeZoom = 18)) %>% #Min/Max Zoom Levels
      #Add Tile LAYER
      addTiles(options = tileOptions(minZoom = 8, maxZoom = 15, maxNativeZoom = 18)) %>% #Min/Max Zoom Levels
      addScaleBar(position = "bottomleft") %>% 
      #World Map View => CENTER => LONGITUDE = 0 | LATITUDE = 0 | Zoom Level = 1
      setView(lng = 0, lat = 0, zoom = 1) %>%
      addDrawToolbar(position = "topleft", #ToolBAR => POSITION
                     polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(
                       #Stroke Attributes | Fill Attributes
                       stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75, fill = TRUE, fillOpacity = 0.25)), 
                     editOptions = editToolbarOptions(edit = FALSE, remove = TRUE))
    
        })
  
  ##### World Map => Click #####
  observeEvent(input$world_map_click, { #WHEN World Map is clicked
    
    #Click => LATITUDE | LONGITUDE
    click <- input$world_map_click
    
    #Reactive Value => Store LATITUDE Values
    RV$LATITUDE_VALUEs <- c(RV$LATITUDE_VALUEs, round(click$lat, 5))
    #Reactive Value => Store LONGITUDE Values
    RV$LONGITUDE_VALUEs <- c(RV$LONGITUDE_VALUEs, round(click$lng, 5))
    
    })
  
  ##### Coordinates #####
  output$coordinates <- renderPrint({
    
    #LATITUDE Values
    cat("LATITUDE_VALUEs <- c(")
    for (i in seq_along(RV$LATITUDE_VALUEs)) {
      if (i > 1) {
        if (i %% 10 == 1) {cat(", \n  ")} else {cat(", ")}}
      cat(round(RV$LATITUDE_VALUEs[i], 5))}
    cat(")")
    
    #LONGITUDE Values
    cat("\nLONGITUDE_VALUEs <- c(")
    for (i in seq_along(RV$LONGITUDE_VALUEs)) {
      if (i > 1) {
        if (i %% 10 == 1) {cat(", \n  ")} else {cat(", ")}}
      cat(round(RV$LONGITUDE_VALUEs[i], 5))}
    cat(")")
    
    })
  
  ##### Action Button => Clear Values #####
  observeEvent(input$clear_values, { #WHEN Action Button is clicked
    
    #Reactive Value => Clear LATITUDE Values
    RV$LATITUDE_VALUEs <- numeric(0)
    #Reactive Value => Clear LONGITUDE Values
    RV$LONGITUDE_VALUEs <- numeric(0)
    
    })
  
  ##### Action Button => Clear One Value #####
  observeEvent(input$clear_one_value, { #WHEN Action Button is clicked
    
    #Reactive Value => Clear One LATITUDE Value
    RV$LATITUDE_VALUEs <- head(RV$LATITUDE_VALUEs, -1)
    #Reactive Value => Clear One LONGITUDE Value
    RV$LONGITUDE_VALUEs <- head(RV$LONGITUDE_VALUEs, -1)
    
    })
  
  }

#######################
##### APPLICATION #####
#######################

shinyApp(ui = ui, server = server)
