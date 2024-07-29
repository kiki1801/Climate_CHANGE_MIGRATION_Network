##########################################################################
#####                Climate ChanGe MiGration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   KYLLIAN JAMES | 2023-11-14                   #####
#####                        Mod.: 2024-05-27                        #####
#####                               UI                               #####
##########################################################################

##########################
##### Load Libraries #####
##########################

library(shiny)
library(waiter)
library(shinyjs)

##############
##### UI #####
##############

ui <- fluidPage(
  
  ##### Browser Window #####
  titlePanel("Climate ChanGe MiGration Network"),
  
  ##### Include WAITER Dependencie(s) #####
  useWaiter(),
  
  ##### SHINY JavaScript Initialization #####
  useShinyjs(),
  
  ##### CSS Code #####
  tags$head(
    tags$style(
      #HTML and BODY Minimum Width (don't shrink browser window title)
      "html {min-width: 648px;}", "body {min-width: 648px;}",
      #WAITER => SPINNER | TEXT => COLOR
      ".orbiter-spinner .orbiter:nth-child(1) {border-bottom: 3px solid #F9F9F9 !important;}",
      ".orbiter-spinner .orbiter:nth-child(2) {border-right: 3px solid #F9F9F9 !important;}",
      ".orbiter-spinner .orbiter:nth-child(3) {border-top: 3px solid #F9F9F9 !important;}",
      ".waiter-overlay {color: #F9F9F9 !important;}",
      #CURSOR Classes => WAIT | Default
      "body.wait-cursor {cursor: wait !important;}",
      "body.default-cursor {cursor: default !important;}",
      # "body.wait-cursor, body.wait-cursor * {cursor: wait !important;}", #CSS Rule => All elements within BODY
      # "body.default-cursor, body.default-cursor * {cursor: default !important;}", #CSS Rule => All elements within BODY
      #Action Button Appearance
      ".btn-group {width: 100%;}", #Fill entire container's width (100%)
      ".btn-group .btn {width: 50%;}", #Each Button = 50% container's width
      ".btn:active {background-color: #337AB7 !important; color: #F9F9F9 !important;}", #CSS pseudo-class :active when button is clicked
      ".btn.active {background-color: #337AB7 !important; color: #F9F9F9 !important; outline: none !important;}" #CSS class .active when button with active class
      ),
    tags$script(
      #CURSOR Class To WAIT
      "$(document).ready(function(){$('body').addClass('wait-cursor');});",
      #Handle CUSTOM CURSOR Classes MESSAGE from server-side
      "Shiny.addCustomMessageHandler('CURSORwithinBODY', function(MESSAGE) {document.body.className = MESSAGE;});"
      )
    ),
  
  ##### SHOW LOADING SCREEN on APPLICATION LAUNCH (WAITER n°1) #####
  waiterShowOnLoad(
    html = tagList(
      div(spin_orbiter(), style = "margin-bottom: 10px;"), #SPINNER
      div(span("Data Exploration Application Launch"))), #Html Text
    color = "#1A1A1A"), #BackGround Color

  ##### NAVIGATION BAR #####
  navbarPage(
    #NAVIGATION BAR
    title = "Data Exploration Application",
    #Selected Tab
    selected = "MIGRATION", 
    #NaviGation Bar Position ("static-top", "fixed-top", "fixed-bottom")
    position = "static-top",
    #Collapse naviGation elements into an expandable menu on mobile devices or narrow window widths
    collapsible = TRUE,
  
    ##### MIGRATION TAB #####
    tabPanel(title = "MIGRATION",
      #Contains Multiple Tabs
      tabsetPanel(
        #Selected Tab
        selected = "RESIDENCE",
        #Tabs Look ("tabs", "pills", "hidden")
        type = "tabs",
      
        ##### RESIDENCE TAB #####
        tabPanel(title = "RESIDENCE", 
          #MARGIN (Top: 10px)
          tags$div(style = "margin-top: 10px;",
            ##### COUNTRY ACTION BUTTON #####
            actionButton(inputId = "MR_COUNTRY", label = "COUNTRY"),
            ##### REGION ACTION BUTTON #####
            actionButton(inputId = "MR_REGION", label = "REGION")
            ),
          #Conditional Panel visible when REGION ACTION BUTTON is clicked
          conditionalPanel(condition = "input.MR_REGION >= 1", 
            #MARGIN (Top: 10px)
            style = "margin-top: 10px;", 
            ##### REGION CHOICE RADIO BUTTONs #####
            div(style = "min-width: 1296px;", #RADIO BUTTONs' container minimum width (don't shrink choices)
              radioButtons(inputId = "MRR_REGION_CHOICE", label  = NULL, 
                #International MiGration Stock Data Classifications
                choices = c(
                  "Continental REGIONs", "Continental SUB-REGIONs and INTERMEDIATE REGIONs", "GEOGRAPHIC REGIONs", "Income Levels", "Development Levels (x3)"),
                #Selected Classification | Render choices inline
                selected = "Continental REGIONs", inline = TRUE
                )
              )
            )
          ),
        
        ##### BIRTH TAB #####
        tabPanel(title = "BIRTH", 
          #MARGIN (Top: 10px)
          tags$div(style = "margin-top: 10px;",
            ##### COUNTRY ACTION BUTTON #####
            actionButton(inputId = "MB_COUNTRY", label = "COUNTRY"),
            ##### REGION ACTION BUTTON #####
            actionButton(inputId = "MB_REGION", label = "REGION")
            ),
          #Conditional Panel visible when REGION ACTION BUTTON is clicked
          conditionalPanel(condition = "input.MB_REGION >= 1", 
            #MARGIN (Top: 10px)
            style = "margin-top: 10px;", 
            ##### REGION CHOICE RADIO BUTTONs #####
            div(style = "min-width: 1296px;", #RADIO BUTTONs' container minimum width (don't shrink choices)
              radioButtons(inputId = "MBR_REGION_CHOICE", label  = NULL, 
                #International MiGration Stock Data Classifications
                choices = c(
                  "Continental REGIONs", "Continental SUB-REGIONs and INTERMEDIATE REGIONs", "GEOGRAPHIC REGIONs", "Income Levels", "Development Levels (x3)"),
                #Selected Classification | Render choices inline
                selected = "Continental REGIONs", inline = TRUE
                )
              )
            )
          )
        )
      ), 
  
    ##### CLIMATE TAB #####
    tabPanel(title = "CLIMATE", 
      ##### COUNTRY ACTION BUTTON #####
      actionButton(inputId = "CLIMATE_COUNTRY", label = "COUNTRY"),
      ##### REGION ACTION BUTTON #####
      actionButton(inputId = "CLIMATE_REGION", label = "REGION"), 
      #Conditional Panel visible when REGION ACTION BUTTON is clicked
      conditionalPanel(condition = "input.CLIMATE_REGION >= 1", 
        #MARGIN (Top: 10px)
        style = "margin-top: 10px;", 
        ##### REGION CHOICE RADIO BUTTONs #####
        div(style = "min-width: 1296px;", #RADIO BUTTONs' container minimum width (don't shrink choices)
          radioButtons(inputId = "CLIMATE_R_REGION_CHOICE", label  = NULL, 
            #International MiGration Stock Data Classifications
            choices = c(
              "Continental REGIONs", "Continental SUB-REGIONs and INTERMEDIATE REGIONs", "GEOGRAPHIC REGIONs", "Income Levels", "Development Levels (x3)"),
            #Selected Classification | Render choices inline
            selected = "Continental REGIONs", inline = TRUE
            )
          )
        )
      ),
    
    ##### MIGRATION and CLIMATE TAB #####
    tabPanel(title = "MIGRATION and CLIMATE",
      #Contains Multiple Tabs
      tabsetPanel(
        #Selected Tab
        selected = "RESIDENCE",
        #Tabs Look ("tabs", "pills", "hidden")
        type = "tabs",
      
        ##### RESIDENCE TAB #####
        tabPanel(title = "RESIDENCE", 
          #MARGIN (Top: 10px)
          tags$div(style = "margin-top: 10px;",
            ##### COUNTRY ACTION BUTTON #####
            actionButton(inputId = "MCR_COUNTRY", label = "COUNTRY"),
            ##### REGION ACTION BUTTON #####
            actionButton(inputId = "MCR_REGION", label = "REGION")
            ),
          #Conditional Panel visible when REGION ACTION BUTTON is clicked
          conditionalPanel(condition = "input.MCR_REGION >= 1", 
            #MARGIN (Top: 10px)
            style = "margin-top: 10px;", 
            ##### REGION CHOICE RADIO BUTTONs #####
            div(style = "min-width: 1296px;", #RADIO BUTTONs' container minimum width (don't shrink choices)
              radioButtons(inputId = "MCRR_REGION_CHOICE", label  = NULL, 
                #International MiGration Stock Data Classifications
                choices = c(
                  "Continental REGIONs", "Continental SUB-REGIONs and INTERMEDIATE REGIONs", "GEOGRAPHIC REGIONs", "Income Levels", "Development Levels (x3)"),
                #Selected Classification | Render choices inline
                selected = "Continental REGIONs", inline = TRUE
                )
              )
            )
          ),
      
        ##### BIRTH TAB #####
        tabPanel(title = "BIRTH", 
          #MARGIN (Top: 10px)
          tags$div(style = "margin-top: 10px;",
            ##### COUNTRY ACTION BUTTON #####
            actionButton(inputId = "MCB_COUNTRY", label = "COUNTRY"),
            ##### REGION ACTION BUTTON #####
            actionButton(inputId = "MCB_REGION", label = "REGION")
            ),
          #Conditional Panel visible when REGION ACTION BUTTON is clicked
          conditionalPanel(condition = "input.MCB_REGION >= 1", 
            #MARGIN (Top: 10px)
            style = "margin-top: 10px;", 
            ##### REGION CHOICE RADIO BUTTONs #####
            div(style = "min-width: 1296px;", #RADIO BUTTONs' container minimum width (don't shrink choices)
              radioButtons(inputId = "MCBR_REGION_CHOICE", label  = NULL, 
                #International MiGration Stock Data Classifications
                choices = c(
                  "Continental REGIONs", "Continental SUB-REGIONs and INTERMEDIATE REGIONs", "GEOGRAPHIC REGIONs", "Income Levels", "Development Levels (x3)"),
                #Selected Classification | Render choices inline
                selected = "Continental REGIONs", inline = TRUE
                )
              )
            )
          )
        )
      )
    )

  )