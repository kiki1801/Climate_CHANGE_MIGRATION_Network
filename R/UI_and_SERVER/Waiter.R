##########################################################################
#####                Climate ChanGe MiGration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   KYLLIAN JAMES | 2024-04-29                   #####
#####                        Mod.: 2024-05-01                        #####
#####                         SHINY | WAITER                         #####
##########################################################################

##########################
##### Load Libraries #####
##########################

library(shiny)
library(waiter)
library(leaflet)

##############
##### UI #####
##############

ui <- fluidPage(
  
  ##### Browser Window #####
  titlePanel("SHINY | WAITER"),
  
  ##### CSS Code #####
  tags$head(
    tags$style(
      #HTML and BODY Minimum Width (don't shrink browser window title)
      "html {min-width: 648px;}", "body {min-width: 648px;}",
      #WAITER on World Map
      ".waiter-overlay.waiter-local {position: relative !important; top: 0px!important; left: 0px!important;}",
      #Top PROGRESSION
      ".top-attendant-bar {display: none; position: absolute; top: 0; left: 0;}"
      )
    ),
  
  ##### Include Dependencies #####
  useWaiter(), useHostess(), useAttendant(), useWaitress(color = "orange"),
  
  ##### LOADING SCREEN on APPLICATION LAUNCH #####
  # waiterShowOnLoad(html = spin_fading_circles()),
  # waiterShowOnLoad(color = "white", hostess_loader("Loader", preset = "circle", text_color = "black", class = "label-center", center_page = TRUE)),
  # waiterPreloader(),
  
  ##### FluidRow n°1 #####
  fluidRow(
    
    ##### WAITER ACTION BUTTON #####
    column(width = 1, offset = 0, actionButton(inputId = "WAITER", label = "WAITER")),
    
    ##### WAITER+ ACTION BUTTON #####
    column(width = 1, offset = 0, actionButton(inputId = "WAITERx", label = "WAITER+")),
    
    ##### MESSAGE(s) on SCREEN ACTION BUTTON #####
    column(width = 1, offset = 0, actionButton(inputId = "MESSAGEonSCREEN", label = "MESSAGE(s) on SCREEN")),
    
    ##### World Map ACTION BUTTON #####
    column(width = 1, offset = 1, actionButton(inputId = "World_Map_Action_Button", label = "Draw World Map")),
    
    ##### HOSTESS ACTION BUTTON #####
    column(width = 1, offset = 0, actionButton(inputId = "HOSTESS", label = "HOSTESS")),
    
    ##### SPINNER CHOICE #####
    column(width = 2, offset = 0,
      selectInput(inputId = "SPINNER_CHOICE", label = "SPINNER CHOICE", 
        #SPINNER CHOICEs
        choices = c(
          list(
            "Circle n°1", "Circle n°2", "Circle n°3", "Circle n°4", "Circle n°5", "Circle (Fade)",
            "INNER Circles", "Dots (Chase)", "Orbiter", "WhirlY", "GooGle",
            "Cube (Fold)", "Cube Grid", "Plane (Rotate)",
            "Double Bounce", "3 Bounces", "Wave", "Ellipsis", "Facebook", "Throbber",
            "Pulse", "Ball", "Flowers", 
            "Loader n°0", "Loader n°1", "Loader n°2", "Loader n°3", "Loader n°4", "Loader n°5", "Loader n°6", 
            "Loader n°7", "Loader n°8", "Loader n°9", "Loader n°10", "Loader n°11", "Loader n°12", "Loader n°13"
            )
          ), 
          #Selected Spinner | Multiple Choice(s) | Width
        selected = "1", multiple = FALSE, width = "100%"))
    
    ),
  
  ##### World Map #####
  leafletOutput("World_Map", width = "100%", height = 475),
  #WAITER for Reactive UI Element
  # withWaiter(leafletOutput("World_Map", width = "100%", height = 475)),
  
  ##### FluidRow n°2 #####
  fluidRow(
    
    #MARGIN (Top: 10px)
    tags$div(style = "margin-top: 10px;",
      ##### ATTENDANT ACTION BUTTON #####
      column(width = 1, offset = 0, actionButton(inputId = "ATTENDANT", label = "ATTENDANT")),
    
      ##### ATTENDANT+ ACTION BUTTON #####
      column(width = 1, offset = 0, actionButton(inputId = "ATTENDANTx", label = "ATTENDANT+")),
    
      ##### MESSAGE(s)/% on ATTENDANT+ ACTION BUTTON #####
      column(width = 1, offset = 0, actionButton(inputId = "MESSAGEonPROGRESSION", label = "MESSAGE(s) on PROGRESSION")),
    
      #### ATTENDANT with WAITER ACTION BUTTON #####
      column(width = 1, offset = 1, actionButton(inputId = "ATTENDANTwithWAITER", label = "ATTENDANT with WAITER")),
    
      #### Top ATTENDANT ACTION BUTTON #####
      column(width = 1, offset = 1, actionButton(inputId = "TopATTENDANT", label = "Top ATTENDANT")),
    
      #### Modal Box ACTION BUTTON #####
      column(width = 1, offset = 0, actionButton(inputId = "ModalBox", label = "Modal Box"))
    
      )
    ),
  
  ##### ATTENDANT BAR(s) #####
  tags$div(
    #MARGIN (Top: 10px)
    style = "margin-top: 10px;",
    #ATTENDANT
    # attendantBar("PROGRESSION_1"),
    # attendantBar("PROGRESSION_1", hidden = TRUE),
    #ATTENDANT+
    attendantBar("PROGRESSION_2", max = 100, color = "success", striped = TRUE, animated =  TRUE), 
    #MESSAGE(s)/% on ATTENDANT+
    attendantBar("PROGRESSION_3", max = 100, striped = TRUE)
    ),
  attendantBar("PROGRESSION_5", height = 5, class = 'top-attendant-bar'), 
  
  ##### FluidRow n°3 #####
  fluidRow(
    
    ##### WAITRESS ACTION BUTTON #####
    column(width = 1, offset = 0, actionButton(inputId = "WAITRESS", label = "WAITRESS")),
             
    ##### OVERLAY WAITRESS ACTION BUTTON #####
    column(width = 1, offset = 0, actionButton(inputId = "OVERLAY_WAITRESS", label = "OVERLAY WAITRESS")),
    
    ##### OVERLAY (%) WAITRESS ACTION BUTTON #####
    column(width = 1, offset = 1, actionButton(inputId = "OVERLAY_Percent_WAITRESS", label = "OVERLAY (%) WAITRESS")),

    #### INFINITE OVERLAY WAITRESS ACTION BUTTON #####
    column(width = 1, offset = 1, actionButton(inputId = "INFINITE_OVERLAY_WAITRESS", label = "INFINITE OVERLAY WAITRESS")),

    #### MESSAGE on WAITRESS ACTION BUTTON #####
    column(width = 1, offset = 1, actionButton(inputId = "MESSAGEonWAITRESS", label = "MESSAGE on WAITRESS")),
    
    #### On Render WAITRESS ACTION BUTTON #####
    column(width = 1, offset = 1, actionButton(inputId = "OnRenderWAITRESS", label = "On Render WAITRESS"))
             
    ), 
  
  #MARGIN (Top: 10px)
  tags$div(style = "margin-top: 10px;",
    ##### World Map #####
    leafletOutput("World_M", width = "100%", height = 475))
  
  )

##################
##### SERVER #####
##################

server <- function(input, output, session) {
  
  #X SECONDs
  # Sys.sleep(2.5)
  
  ##### HIDE LOADING SCREEN WAITER #####
  # waiter_hide() #For waiterShowOnLoad()
  
  ##### CREATE HOSTESS #####
  # LoadHostess <- Hostess$new("Loader")
  # LoadHostess <- Hostess$new("Loader", infinite = TRUE)
  
  #PROGRESSION
  # for(i in 1:10){
  #   #X SECONDs
  #   Sys.sleep(0.5)
  #   LoadHostess$set(i * 10)}
  
  #INFINITE PROGRESSION
  # LoadHostess$start()
  # #X SECONDs
  # Sys.sleep(5)
  # LoadHostess$close()

  ##### HIDE LOADING SCREEN HOSTESS #####
  # waiter_hide() #For waiterShowOnLoad()
  
  ##### CREATE WAITER #####
  AWaiter <- Waiter$new()
  
  ##### WAITER ACTION BUTTON #####
  observeEvent(input$WAITER, { #When WAITER ACTION BUTTON is clicked
    
    #SHOW WAITER
    AWaiter$show()
    #X SECONDs
    Sys.sleep(2.5)
    #HIDE WAITER
    AWaiter$hide()
    
    })
  
  ##### SPINNER FUNCTIONs #####
  SPINNER_Functions <- list(
    "Circle n°1" = spin_1(), "Circle n°2" = spin_2(), "Circle n°3" = spin_4(), "Circle n°4" = spin_6(), "Circle n°5" = spin_circle(), "Circle (Fade)" = spin_fading_circles(),
    "INNER Circles" = spin_inner_circles(), "Dots (Chase)" = spin_chasing_dots(), "Orbiter" = spin_orbiter(), "WhirlY" = spin_whirly(), "GooGle" = spin_google(),
    "Cube (Fold)" = spin_folding_cube(), "Cube Grid" = spin_cube_grid(), "Plane (Rotate)" = spin_rotating_plane(),
    "Double Bounce" = spin_double_bounce(), "3 Bounces" = spin_three_bounce(), "Wave" = spin_wave(), "Ellipsis" = spin_ellipsis(), "Facebook" = spin_facebook(), "Throbber" = spin_throbber(),
    "Pulse" = spin_pulse(), "Ball" = spin_ball(), "Flowers" = spin_flowers(), 
    "Loader n°0" = spin_loaders(id = 1, color = "white"), "Loader n°1" = spin_loaders(id = 3, color = "white"), "Loader n°2" = spin_loaders(id = 4, color = "white"), "Loader n°3" = spin_loaders(id = 5, color = "white"), "Loader n°4" = spin_loaders(id = 6, color = "white"), "Loader n°5" = spin_loaders(id = 8, color = "white"), "Loader n°6" = spin_loaders(id = 9, color = "white"), 
    "Loader n°7" = spin_loaders(id = 10, color = "white"), "Loader n°8" = spin_loaders(id = 11, color = "white"), "Loader n°9" = spin_loaders(id = 15, color = "white"), "Loader n°10" = spin_loaders(id = 19, color = "white"), "Loader n°11" = spin_loaders(id = 31, color = "white"), "Loader n°12" = spin_loaders(id = 35, color = "white"), "Loader n°13" = spin_loaders(id = 39, color = "white"))
  
  ##### WAITER+ ACTION BUTTON #####
  observeEvent(input$WAITERx, { #When WAITER+ ACTION BUTTON is clicked
    
    #SHOW WAITER+
    waiter_show(
      html = tagList(
        SPINNER_Functions[[input$SPINNER_CHOICE]], #SPINNER
        if (!input$SPINNER_CHOICE %in% c("WhirlY", "Ball")) {h4("TEXT...")}), #Html Text
      color = "black") #BackGround Color
    #X SECONDs
    Sys.sleep(2.5)
    #HIDE WAITER+
    waiter_hide()
    
    })
  
  ##### CREATE WAITER #####
  MWaiter <- Waiter$new(html = span("Data Exploration Application Launch"))
  
  ##### MESSAGE(s) #####
  MESSAGEs <- c("Load International MiGration Stock Data", "Load GeoSpatial Data", "Load Tiles", "Draw World Administrative Boundaries")
  
  ##### MESSAGE(s) on SCREEN ACTION BUTTON #####
  observeEvent(input$MESSAGEonSCREEN, { #When MESSAGE(s) on SCREEN ACTION BUTTON is clicked
    
    #SHOW MESSAGE(s) on SCREEN
    MWaiter$show()
    #X SECONDs
    Sys.sleep(2.5)
    #DYNAMIC UPDATE(s)
    for(MESSAGE in 1:length(MESSAGEs)){
      MWaiter$update(html = MESSAGEs[MESSAGE])
      #X SECONDs
      Sys.sleep(2.5)}
    #HIDE MESSAGE(s) on SCREEN
    MWaiter$hide()
    
    })
  
  ##### CREATE WAITER #####
  World_Map_Waiter <- Waiter$new(id = "World_Map", html = span("Load Tiles"))
  
  ##### World Map ACTION BUTTON #####
  observeEvent(input$World_Map_Action_Button, { #When World Map ACTION BUTTON is clicked
    
    #SHOW World Map
    World_Map_Waiter$show()
    #World Map
    output$World_Map <- renderLeaflet({leaflet() %>% addTiles()})
    #X SECONDs
    # Sys.sleep(2.5)
    #HIDE World Map (waiter will hide waiter when element is rendered)
    # World_Map_Waiter$hide()
    
    })

  ##### World Map withWaiter() #####
  # output$World_Map <- renderLeaflet({leaflet() %>% addTiles()})
  
  ##### CREATE HOSTESS #####
  AHostess <- Hostess$new()
  
  ##### CREATE LOADER #####
  HWaiter <- Waiter$new(html = AHostess$get_loader(stroke_color = "white"))
  
  ##### HOSTESS ACTION BUTTON #####
  observeEvent(input$HOSTESS, { #When HOSTESS ACTION BUTTON is clicked
    
    #SHOW HOSTESS
    HWaiter$show()
    #LOADER
    for(i in 1:10){
      AHostess$set(i * 10)
      #X SECONDs
      Sys.sleep(1)}
    #HIDE HOSTESS
    HWaiter$hide()
    
    })
  
  ##### CREATE ATTENDANT #####
  PROGRESSION_1 <- Attendant$new("PROGRESSION_1", hide_on_max = FALSE) #MIN-MAX VALUE(s)?
  # PROGRESSION_1 <- Attendant$new(id = "PROGRESSION_1", hide_on_max = TRUE) #DONE => HIDE
  
  ##### ATTENDANT ACTION BUTTON #####
  observeEvent(input$ATTENDANT, { #When ATTENDANT ACTION BUTTON is clicked

    #PROGRESSION
    for(i in 1:10){
      PROGRESSION_1$set(i * 10)
      #X SECOND
      Sys.sleep(0.05)}

    #PROGRESSION
    # withProgressAttendant({
    #   for (i in 1:100) {
    #     incProgressAttendant(1)
        #X SECONDs
        # Sys.sleep(0.05)}}, id = "PROGRESSION_1")
    
    })
  
  ##### CREATE ATTENDANT+ #####
  PROGRESSION_2 <- Attendant$new("PROGRESSION_2")
  
  ##### ATTENDANT+ ACTION BUTTON #####
  observeEvent(input$ATTENDANTx, { #When ATTENDANT+ ACTION BUTTON is clicked
    
    #PROGRESSION
    for(i in 1:10){
      PROGRESSION_2$set(i * 10)
      #X SECONDs
      Sys.sleep(0.05)}
    
    })
  
  ##### CREATE MESSAGE(s)/% on ATTENDANT+ #####
  PROGRESSION_3 <- Attendant$new("PROGRESSION_3")
  
  ##### MESSAGE(s)/% on ATTENDANT+ ACTION BUTTON #####
  observeEvent(input$MESSAGEonPROGRESSION, { #When MESSAGE(s)/% on ATTENDANT+ ACTION BUTTON is clicked
    
    #PROGRESSION
    for(i in 1:10){
      PROGRESSION_3$set(i * 10, text = sprintf("%s%%", i * 10))
      #X SECONDs
      Sys.sleep(0.5)}
    
    })
  
  ##### CREATE ATTENDANT with WAITER #####
  AwWaiter <- Waiter$new(color = "white", html = attendantBar("PROGRESSION_4", width = 324, text = "Data Exploration Application Launch"))
  PROGRESSION_4 <- Attendant$new("PROGRESSION_4")
  
  ##### WAITER with WAITER ACTION BUTTON #####
  observeEvent(input$ATTENDANTwithWAITER, { #When ATTENDANT with WAITER ACTION BUTTON is clicked
    
    #SHOW ATTENDANT with WAITER
    AwWaiter$show()
    
    #PROGRESSION
    for(i in 1:10){
      PROGRESSION_4$set(i * 10)
      #X SECONDs
      Sys.sleep(0.05)}

    #X SECONDs
    Sys.sleep(2.5)
    
    #HIDE ATTENDANT with WAITER
    AwWaiter$hide()
    
    })
  
  ##### CREATE Top ATTENDANT #####
  PROGRESSION_5 <- Attendant$new("PROGRESSION_5", hide_on_max = TRUE)
  
  ##### Top ATTENDANT ACTION BUTTON #####
  observeEvent(input$TopATTENDANT, { #When Top ATTENDANT ACTION BUTTON is clicked
    
    #PROGRESSION
    for(i in 1:10){
      PROGRESSION_5$set(i * 10)
      #X SECOND
      Sys.sleep(0.5)}
  
    })
  
  ##### CREATE Modal Box ATTENDANT #####
  PROGRESSION_6 <- Attendant$new("PROGRESSION_6", hide_on_max = TRUE)
  
  ##### Modal Box ACTION BUTTON #####
  observeEvent(input$ModalBox, { #When Modal Box ACTION BUTTON is clicked
    
    #SHOW Modal Box
    showModal(modalDialog(size = "m", attendantBar("PROGRESSION_6", text = "Data Exploration Application Launch"), footer = NULL))
    
    #PROGRESSION
    for(i in 1:10){
      # X SECONDs
      Sys.sleep(0.5)
      PROGRESSION_6$set(i * 10)}
    
    #X SECONDs
    # Sys.sleep(0.5)
    
    #Remove Modal Box
    removeModal()
    
    })
  
  ##### CREATE WAITRESS #####
  AWaitress <- Waitress$new("#World_M")
  
  ##### WAITRESS ACTION BUTTON #####
  observeEvent(input$WAITRESS, { #When WAITRESS ACTION BUTTON is clicked
    
    #World Map
    output$World_M <- renderLeaflet({
      
      #PROGRESSION
      for(i in 1:10){
        AWaitress$inc(10) #INCREASE BY 10%
        #X SECONDs
        Sys.sleep(0.5)}
      
      #World Map
      leaflet() %>% addTiles()
      
      #HIDE WAITRESS
      AWaitress$close()
      
      })
    
    })
  
  ##### CREATE OVERLAY WAITRESS #####
  OWaitress <- Waitress$new("#World_M", theme = "overlay")
  
  ##### OVERLAY WAITRESS ACTION BUTTON #####
  observeEvent(input$OVERLAY_WAITRESS, { #When OVERLAY WAITRESS ACTION BUTTON is clicked
    
    #World Map
    output$World_M <- renderLeaflet({
      
      #PROGRESSION
      for(i in 1:10){
        OWaitress$inc(10) #INCREASE BY 10%
        #X SECONDs
        Sys.sleep(0.5)}
      
      #World Map
      leaflet() %>% addTiles()
      
      #HIDE OVERLAY WAITRESS
      OWaitress$close()
      
      })
    
    })
  
  ##### CREATE OVERLAY (%) WAITRESS #####
  OPerWaitress <- Waitress$new("#World_M", theme = "overlay-percent")
  
  ##### OVERLAY (%) WAITRESS ACTION BUTTON #####
  observeEvent(input$OVERLAY_Percent_WAITRESS, { #When OVERLAY (%) WAITRESS ACTION BUTTON is clicked
    
    #World Map
    output$World_M <- renderLeaflet({
      
      #PROGRESSION
      for(i in 1:10){
        OPerWaitress$inc(10) #INCREASE BY 10%
        #X SECONDs
        Sys.sleep(0.5)}
      
      #World Map
      leaflet() %>% addTiles()
      
      #HIDE OVERLAY (%) WAITRESS
      OPerWaitress$close()
      
      })
    
    })
  
  ##### CREATE INFINITE OVERLAY WAITRESS #####
  IOWaitress <- Waitress$new("#World_M", theme = "overlay", infinite = TRUE)
  
  ##### INFINITE OVERLAY WAITRESS ACTION BUTTON #####
  observeEvent(input$INFINITE_OVERLAY_WAITRESS, { #When INFINITE OVERLAY WAITRESS ACTION BUTTON is clicked
    
    #World Map
    output$World_M <- renderLeaflet({
      
      #SHOW INFINITE OVERLAY WAITRESS
      IOWaitress$start()
      
      #X SECONDs
      Sys.sleep(2.5)
      
      #World Map
      leaflet() %>% addTiles()
      
      #HIDE INFINITE OVERLAY WAITRESS
      IOWaitress$close()
      
      })
    
    })
  
  ##### CREATE MESSAGE on WAITRESS #####
  MWaitress <- Waitress$new("#World_M")
  
  ##### MESSAGE on WAITRESS ACTION BUTTON #####
  observeEvent(input$MESSAGEonWAITRESS, { #When MESSAGE on WAITRESS ACTION BUTTON is clicked
    
    #World Map
    output$World_M <- renderLeaflet({
      
      #SHOW MESSAGE on WAITRESS
      MWaitress$start(h3("Load Tiles..."))
      
      #PROGRESSION
      for(i in 1:10){
        MWaitress$inc(10) #INCREASE BY 10%
        #X SECONDs
        Sys.sleep(0.5)}
      
      #World Map
      leaflet() %>% addTiles()
      
      #HIDE MESSAGE on WAITRESS
      MWaitress$close()
      
      })
    
    })
  
  ##### CREATE On Render WAITRESS #####
  ORWaitress <- Waitress$new("#World_M", hide_on_render = TRUE)
  
  ##### On Render WAITRESS ACTION BUTTON #####
  observeEvent(input$OnRenderWAITRESS, { #When On Render WAITRESS ACTION BUTTON is clicked
    
    #World Map
    output$World_M <- renderLeaflet({
      
      #SHOW On Render WAITRESS
      ORWaitress$start()
      
      #PROGRESSION
      for(i in 1:10){
        ORWaitress$inc(10) #INCREASE BY 10%
        #X SECONDs
        Sys.sleep(0.5)}
      
      #World Map
      leaflet() %>% addTiles()
      
      })
    
    })
  
  }

#######################
##### APPLICATION #####
#######################

shinyApp(ui = ui, server = server)