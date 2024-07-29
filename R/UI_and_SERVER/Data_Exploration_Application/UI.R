##########################################################################
#####                Climate ChanGe MiGration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   KYLLIAN JAMES | 2023-11-14                   #####
#####                        Mod.: 2024-06-17                        #####
#####                               UI                               #####
##########################################################################

##########################
##### Load Libraries #####
##########################

library(bslib)
library(shiny)
library(waiter)
library(shinyjs)
library(leaflet) 

####################
##### THEME(s) #####
####################

#!DARK THEME
Base <- bs_theme(
  version = 5, #BootStrap Version 5.3.1
  bg = "#FCFCFC", #BackGround COLOR
  fg = "#1D1F21", #ForeGound COLOR (TEXT COLOR)
  primary = "#007BC2", #PRIMARY COLOR (BUTTON(s) | LINK(s) | INTERACTIVE ELEMENT(s))
  secondary = "#404040", #SECONDARY COLOR (ACCENT(s) | BORDER(s) | BUTTON(s))
  base_font = font_google("DM Sans", local = TRUE), #New Default Font TYPEFACE
  "font-size-base" = "0.875rem", #Font Size Base from 16px To 14px
  "line-height-base" = "1.4285", #Line HEIGHT Base from 1.5 To 1.4285
  "navbar-toggler-font-size" = "1rem", #TOGGLER Font Size from 1.09375rem To 1rem
  "navbar-toggler-border-radius" = "5px", #TOGGLER Border Radius from 3px To 5px
  "navbar-toggler-focus-width" = "0px", #Remove TOGGLER Focus (from 0.25rem to 5px)
  "nav-tabs-border-radius" = "5px", #NAVIGATION Panel Radius from 3px To 5px
  "btn-font-size" = "14px", #Action Button Font Size from 0.9375rem To 14px
  "btn-font-weight" = "400", #Action Button Font WEIGHT from 500 To 400
  "btn-border-radius" = "5px", #Action Button Border Radius from 3px To 5px
  "card-border-width" = "0px", #Remove Card BORDER
  "card-border-radius" = "16px", #Card BORDER Radius from 8px To 16px
  "card-inner-border-radius" = "calc(var(--bs-card-border-radius) / 2)", #Card Rule => INNER = BORDER Radius / 2
  "card-bg" = "#FEFEFE") #From "#FCFCFC" To "#FEFEFE"

#DARK THEME
Dark_Mode <- bs_theme(
  version = 5, #BootStrap Version 5.3.1
  bg = "#1D1F21", #BackGround COLOR
  fg = "#FCFCFC", #ForeGound COLOR (TEXT COLOR)
  primary = "#007BC2", #PRIMARY COLOR (BUTTON(s) | LINK(s) | INTERACTIVE ELEMENT(s))
  secondary = "#404040", #SECONDARY COLOR (ACCENT(s) | BORDER(s) | BUTTON(s))
  base_font = font_google("DM Sans", local = TRUE), #New Default Font TYPEFACE
  "font-size-base" = "0.875rem", #Font Size Base from 16px To 14px
  "line-height-base" = "1.4285", #Line HEIGHT Base from 1.5 To 1.4285
  "navbar-toggler-font-size" = "1rem", #TOGGLER Font Size from 1.09375rem To 1rem
  "navbar-toggler-border-radius" = "5px", #TOGGLER Border Radius from 3px to 5px
  "navbar-toggler-focus-width" = "0px", #Remove TOGGLER Focus (from 0.25rem to 5px)
  "nav-tabs-border-radius" = "5px", #NAVIGATION Panel Radius from 3px To 5px
  "btn-font-size" = "14px", #Action Button Font Size from 0.9375rem To 14px
  "btn-font-weight" = "400", #Action Button Font WEIGHT from 500 To 400
  "btn-border-radius" = "5px", #Action Button Border Radius from 3px To 5px
  "card-border-width" = "0px", #Remove Card BORDER
  "card-border-radius" = "16px", #Card BORDER Radius from 8px To 16px
  "card-inner-border-radius" = "calc(var(--bs-card-border-radius) / 2)", #Card Rule => INNER = BORDER Radius / 2
  "card-bg" = "var(--bs-secondary)") #From "#1D1F21" To "#404040"

##############
##### UI #####
##############

ui <- fluidPage(
  
  ##### Browser Window #####
  # titlePanel("Climate ChanGe MiGration Network"),
  
  ##### BSLIB+ THEME #####
  theme = Base,
  
  ##### Include WAITER Dependencie(s) #####
  useWaiter(),
  
  ##### SHINY JavaScript Initialization #####
  useShinyjs(),
  
  ##### CSS Code #####
  tags$head(
    tags$style(
      #HTML and BODY Minimum Width (don't shrink browser window title) (972px + 20px from @media)
      "html {min-width: 992px !important;}", "body {min-width: 992px !important;}",
      #BSLIB => ROW => Gap from 1rem To 10px
      ".bslib-gap-spacing {row-gap: 10px !important;}",
      #WAITER => SPINNER => COLOR
      ".orbiter-spinner .orbiter:nth-child(1) {border-bottom: 3px solid #FCFCFC !important;}",
      ".orbiter-spinner .orbiter:nth-child(2) {border-right: 3px solid #FCFCFC !important;}",
      ".orbiter-spinner .orbiter:nth-child(3) {border-top: 3px solid #FCFCFC !important;}",
      #WAITER => TEXT => COLOR
      ".waiter-overlay {color: #FCFCFC !important;}",
      #CURSOR Classes => WAIT | Default
      # "body.wait-cursor {cursor: wait !important;}",
      # "body.default-cursor {cursor: default !important;}",
      "body.wait-cursor, body.wait-cursor * {cursor: wait !important;}", #CSS Rule => All elements within BODY
      "body.default-cursor, body.default-cursor * {cursor: default !important;}", #CSS Rule => All elements within BODY
      #NAVIGATION BAR Appearance
      ".bslib-grid {white-space: nowrap !important;}", #CCMN + Dark Mode (don't shrink browser window title)
      ".bslib-grid.grid {grid-template-rows: 1fr !important; grid-auto-rows: 0 !important; row-gap: 0 !important; max-height: 32px !important; overflow: hidden !important;}", #CCMN + Dark Mode => SHRINK => ROW => ONE
      ".bslib-grid.grid.bslib-mb-spacing.html-fill-item {margin-top: 20px !important; margin-bottom: 10px !important;}", #CCMN + Dark Mode => POSITION
      "h2 {margin-bottom: 0px !important; font-size: 30px !important; font-weight: 500 !important; line-height: 1.1 !important;}", #Climate ChanGe MiGration Network Appearance
      ".g-col-md-4.bslib-grid-item.bslib-gap-spacing.html-fill-container {display: flex !important; justify-content: center !important;}", #Dark Mode Label + TOGGLE SWITCH Button => POSITION
      ".g-col-md-4.bslib-grid-item.bslib-gap-spacing.html-fill-container .shiny-input-container:not(.shiny-input-container-inline) {margin-left: auto !important;}", #Dark Mode Label + TOGGLE SWITCH Button => POSITION
      ".navbar {--bs-navbar-brand-font-size: 18px !important;}", #DEA => Font Size from 1.09375rem To 18px 
      ".navbar-brand {margin-left: -10px !important; margin-right: 10px !important;}", #DEA => POSITION
      "@media (min-width: 992px) {.navbar-nav .nav-item .nav-link.active {background-color: #B0B0B0 !important; height: calc(100% + 16px) !important; margin-top: -8px !important; display: flex !important; align-items: center !important;}}", #MIGRATION and CLIMATE => BackGround COLOR (TOGGLER !Visible) => Active
      "@media (max-width: 992px) {.navbar-nav .nav-item .nav-link.active {background-color: #B0B0B0 !important; width: calc(100% + 45px) !important; margin-left: -22.5px !important; padding-left: 22.5px !important;}}", #MIGRATION and CLIMATE => BackGround COLOR (TOGGLER Visible) => Active
      ".navbar {--bs-navbar-color: var(--bs-link-color) !important;}", #MIGRATION and CLIMATE => TEXT COLOR => !Active
      ".navbar {--bs-navbar-hover-color: var(--bs-link-hover-color) !important;}", #MIGRATION and CLIMATE => TEXT COLOR => HOVER
      ".navbar-toggler, .navbar-toggle {padding-left: 10px !important; padding-right: 10px !important;}", #TOGGLER => APPEARANCE
      ".navbar-toggler:hover, .navbar-toggle:hover {background-color: #B0B0B0 !important;}", #Add TOGGLER HOVER BackGround
      #TOGGLE SWITCH Button Appearance
      ".bslib-input-switch {display: flex !important;}", #Dark Mode Label + TOGGLE SWITCH Button => ORDER+
      ".bslib-input-switch.form-switch.form-check {margin-top: 5px !important; margin-bottom: 0px !important; padding-left: 0px !important;}", #Dark Mode Label + TOGGLE SWITCH Button => POSITION
      ".bslib-input-switch .form-check-label {order: 1 !important; margin-right: 10px !important;}", #Dark Mode Label => ORDER => n°1
      ".bslib-input-switch .form-check-input {order: 2 !important; margin-left: 0px !important;}", #TOGGLE SWITCH Button => ORDER => n°2
      ".form-switch .form-check-input {--bs-form-switch-bg: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-6 -6 12 12'%3e%3ccircle r='5' fill='rgba%2829,31,33,0.25%29'/%3e%3c/svg%3e\") !important;}", #TOGGLE SWITCH Button => !Active => Circle => SIZE
      ".form-switch .form-check-input:focus {border-color: var(--bs-border-color) !important; box-shadow: none !important;}", #TOGGLE SWITCH Button => Focus => BORDER (COLOR) | BOX-SHADOW (REMOVE)
      ".form-switch .form-check-input:checked {border-color: var(--bs-border-color) !important; --bs-form-switch-bg: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-6 -6 12 12'%3e%3ccircle r='5' fill='%23FCFCFC'/%3e%3c/svg%3e\") !important;}", #TOGGLE SWITCH Button => Active => BORDER (COLOR) | Circle (SIZE)
      ".form-switch .form-check-input:not(:checked):focus {--bs-form-switch-bg: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-6 -6 12 12'%3e%3ccircle r='5' fill='rgba%2829,31,33,0.25%29'/%3e%3c/svg%3e\") !important;}", #TOGGLE SWITCH Button => !Active | Focus => Circle => COLOR/SIZE
      #NAVIGATION Panel Appearance
      ".nav-tabs .nav-link {padding: 10px 15px !important;}",
      #Action Button Appearance
      ".btn {padding: 10px 20px !important;}", #Action Button PADDING Rule
      ".tab-pane.html-fill-container.active.show[data-value='CLIMATE'] {display: inline-block !important;}", #Action Button(s) in CLIMATE => ROW => ONE
      ".btn-default {--bs-btn-color: var(--bs-body-color) !important; --bs-btn-border-color: var(--bs-border-color) !important}", #CSS class default when button with default class
      ".btn-default {--bs-btn-hover-bg: var(--bs-primary) !important; --bs-btn-hover-color: #FCFCFC !important; --bs-btn-hover-border-color: var(--bs-light) !important;}", #CSS pseudo-class :hover when button is hover
      ".btn:active {background-color: var(--bs-link-hover-color) !important; color: #FCFCFC !important; outline: 5px auto -webkit-focus-ring-color !important; outline-offset: -2px !important;}", #CSS pseudo-class :active when button is clicked
      ".btn-default {--bs-btn-active-border-color: var(--bs-light) !important;}", #CSS pseudo-class :active and CSS class .active => BORDER => COLOR
      ".btn.active {background-color: var(--bs-primary) !important; color: #FCFCFC !important; outline: none !important;}", #CSS class .active when button with active class
      #Radio Button(s) Appearance
      ".shiny-input-container-inline .shiny-options-group .radio-inline {padding-left: 20px !important; padding-top: 2.75px !important;}", #Radio Button(s) => POSITION
      ".tab-pane:is([data-value='CLIMATE']) .shiny-input-container-inline .shiny-options-group .radio-inline {padding-top: 1px !important;}", #Radio Button(s) => POSITION in CLIMATE
      ".shiny-input-container-inline .shiny-options-group {column-gap: 10px !important; margin-left: 1.5px !important;}", #Radio Button(s) | Circle(s) => POSITION
      ".form-group.shiny-input-radiogroup.shiny-input-container.shiny-input-container-inline.shinyjs-resettable.shiny-bound-input {margin-bottom: 0px !important;}", #Radio Button(s) => MARGIN
      ".tab-pane:is([data-value='MIGRATION'], [data-value='MIGRATION and CLIMATE']) .shiny-input-container-inline .shiny-options-group .radio-inline input {margin-top: 1px !important;}", #Radio Button(s) => Circle(s)  => POSITION (!in CLIMATE)
      ".shiny-input-container .radio-inline input:not(:checked):hover {border-color: #B0B0B0 !important;}", #Radio Button(s) => !Active => HOVER => BORDER => COLOR
      ".shiny-input-container .radio-inline input:focus {box-shadow: none !important;}", #Radio Button(s) => Focus => BOX-SHADOW => REMOVE
      ".shiny-input-container .radio-inline input:checked {background-color: var(--bs-body-bg) !important;}", #Radio Button(s) => Active => BackGround => COLOR
      ".shiny-input-container .radio-inline input:checked[type = 'radio'] {--bs-form-check-bg-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-6 -6 12 12'%3e%3ccircle r='4' fill='%23007BC2'/%3e%3c/svg%3e\") !important;}", #Radio Button(s) => Active => Circle => COLOR (#007BC2)
      ".shiny-input-container .radio-inline input:checked:hover[type = 'radio'] {--bs-form-check-bg-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-6 -6 12 12'%3e%3ccircle r='4' fill='%2300629B'/%3e%3c/svg%3e\") !important;}", #Radio Button(s) => Active => HOVER => Circle => COLOR (#00629B)
      ".shiny-input-container .radio-inline input:checked:hover {border-color: #00629B !important;}", #Radio Button(s) => Active => HOVER => BORDER => COLOR (#00629B)
      #Card HEADER Appearance
      ".bslib-card .card-header {font-size: 14px !important; white-space: nowrap !important; overflow: hidden !important; text-overflow: ellipsis !important;}", #Font Size from 0.9rem To 14px | !SHRINK Headline
      #Card BODY Appearance
      ".leaflet-touch .leaflet-bar {border: 1px solid #ddd !important}", #Zoom CONTAINER => BORDER from 2px To 1px
      ".leaflet-bar {border-radius: 5px !important}", #Zoom CONTAINER => BORDER Radius from 4px To 5px
      ".leaflet-touch .leaflet-bar a:first-child {border-top-left-radius: 5px !important; border-top-right-radius: 5px !important;}", #Zoom-in CONTAINER => BORDER Radius from 2px To 5px
      # ".leaflet-touch .leaflet-control-zoom-in {font-size: 20px !important;}", #Zoom-in CONTAINER => Font Size from 22px To 20px
      # ".leaflet-control-zoom-in {text-indent: 0px !important;}", #Zoom-in CONTAINER => TEXT INDENT from 1px To 0px
      # ".leaflet-touch .leaflet-control-zoom-in {font-size: 0px !important; background-image: url(https://icons.getbootstrap.com/assets/icons/zoom-in.svg) !important; background-repeat: no-repeat !important; background-position: center !important;}", #Zoom-in CONTAINER => Replace "+"
      ".leaflet-touch .leaflet-control-zoom-in {font-size: 0px !important; background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='currentColor' class='bi bi-zoom-in' viewBox='0 0 16 16'%3e%3cpath fill-rule='evenodd' d='M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0'/%3e%3cpath d='M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z'/%3e%3cpath fill-rule='evenodd' d='M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5'/%3e%3c/svg%3e\") ; background-repeat: no-repeat !important; background-position: center !important;}", #Zoom-in CONTAINER => Replace "+"
      ".leaflet-touch .leaflet-bar a:last-child {border-bottom-left-radius: 5px !important; border-bottom-right-radius: 5px !important;}", #Zoom-out CONTAINER => BORDER Radius from 2px To 5px
      # ".leaflet-touch .leaflet-control-zoom-out {font-size: 20px !important;}", #Zoom-out CONTAINER => Font Size from 22px To 20px
      # ".leaflet-control-zoom-out {text-indent: 0px !important;}", #Zoom-out CONTAINER => TEXT INDENT from 1px To 0px
      # ".leaflet-touch .leaflet-control-zoom-out {font-size: 0px !important; background-image: url(https://icons.getbootstrap.com/assets/icons/zoom-out.svg) !important; background-repeat: no-repeat !important; background-position: center !important;}", #Zoom-out CONTAINER => Replace "-"
      ".leaflet-touch .leaflet-control-zoom-out {font-size: 0px !important; background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='currentColor' class='bi bi-zoom-out' viewBox='0 0 16 16'%3e%3cpath fill-rule='evenodd' d='M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0'/%3e%3cpath d='M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z'/%3e%3cpath fill-rule='evenodd' d='M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5'/%3e%3c/svg%3e\") ; background-repeat: no-repeat !important; background-position: center !important;}", #Zoom-out CONTAINER => Replace "-"
      ".leaflet-bar button:first-of-type {border-top-left-radius: 5px !important;border-top-right-radius: 5px !important;}", #Map View CONTAINER => BORDER Radius from 4px To 5px
      ".leaflet-bar button:last-of-type {border-bottom-left-radius: 5px !important; border-bottom-right-radius: 5px !important;}", #Map View CONTAINER => BORDER Radius from 4px To 5px
      ".leaflet-touch .leaflet-bar button {font-size: 16px !important;}", #Map View CONTAINER => ICON => SIZE
      ".leaflet-control-scale-line {border: 1px solid #777 !important; border-top: none !important; line-height: 1 !important; font-size: 10px !important;}", #Scale CONTAINER => BORDER from 2px To 1px | Font Size from 11px To 10px
      ".leaflet-control-scale-line:not(:first-child) {border-top: 1px solid #777 !important; border-bottom: none !important; margin-top: -1px !important;}", #Scale CONTAINER => BORDER from 2px To 1px | Font Size from 11px To 10px
      ".leaflet-container .leaflet-control-attribution {font-size: 10px !important;}", #Attribution CONTAINER => Font Size from 11px To 10px
      #Card FOOTER Appearance
      ".bslib-card .card-footer {font-size: 12px !important; white-space: nowrap !important; overflow: hidden !important; text-overflow: ellipsis !important;}", #Font Size from 0.9rem To 12px | !SHRINK SOURCE(s)
      ".card-footer a {text-decoration: none !important;}", #Source(s) Link => !HOVER => Remove Line
      ".card-footer a:hover {text-decoration: underline !important;}", #Source(s) Link => HOVER => Line
      #Card Full Screen ENTER (Expand) Appearance
      ".bslib-full-screen-enter.badge.rounded-pill {bottom: var(--bslib-full-screen-enter-bottom, 0) !important; box-shadow: 0px 2.5px 5px 0px rgba(0,0,0,0.15) !important; margin: 0px 3.75px 3.75px 0px !important; padding: 8.5px !important; font-size: 10px !important; opacity: 0.75 !important;}",
      "svg {vertical-align: bottom !important;}", #CENTER Expand Icon
      #Card => Expand
      "#bslib-full-screen-overlay {backdrop-filter: blur(2.5px) !important;}", #Full Screen OVERLAY => BLUR => from 2px To 2.5px
      ".bslib-full-screen-exit {top: 22px !important; font-size: 16px !important; margin-right: 16px !important;}", #Close BUTTON => POSITION
      ".bslib-full-screen-exit svg {margin-left: 5px !important;}" #Close BUTTON => MARGIN
      ),
    tags$script(
      #CURSOR Class To WAIT
      "$(document).ready(function(){$('body').addClass('wait-cursor');});",
      #Handle CUSTOM CURSOR Classes MESSAGE from server-side
      "Shiny.addCustomMessageHandler('CURSORwithinBODY', function(MESSAGE) {document.body.className = MESSAGE;});"
      )
    ),
  
  ##### Responsive 12-Column Grid #####
  layout_columns(
    ##### Browser Window #####
    titlePanel("Climate ChanGe MiGration Network"), 
    ##### Dark Mode Button (TOGGLE SWITCH) #####
    input_switch(id = "THEME", label = "Dark Mode", value = FALSE, width = "125px"),
    ##### Columns #####
    col_widths = c(8, 4)),
  
  ##### SHOW LOADING SCREEN on APPLICATION LAUNCH (WAITER n°1) #####
  waiterShowOnLoad(
    html = tagList(
      div(spin_orbiter(), style = "margin-bottom: 10px;"), #SPINNER
      div(span("Data Exploration Application Launch"))), #Html Text
    color = "#1D1F21"), #BackGround Color

  ##### NAVIGATION BAR #####
  page_navbar( #navbarPage()
    #NAVIGATION BAR HEADLINE
    title = "Data Exploration Application",
    #Selected Tab Panel
    selected = "MIGRATION",
    #NAVIGATION BAR POSITION ("static-top", "fixed-top", "fixed-bottom")
    position = "static-top",
    #!UNDERLINE Selected Tab Panel when active
    underline = FALSE,
    #Collapse NAVIGATION Elements into an expandable menu on mobile devices or narrow browser window
    collapsible = TRUE,
  
    ##### MIGRATION #####
    nav_panel(title = "MIGRATION", #tabPanel()
      #Contains Multiple Tabs
      navset_tab( #tabsetPanel()
        #Selected Tab Panel
        selected = "RESIDENCE",
      
        ##### RESIDENCE #####
        nav_panel(title = "RESIDENCE", #tabPanel()
          #MARGIN (Top: 10px)
          tags$div(style = "margin-top: 10px;",
            ##### COUNTRY ACTION BUTTON #####
            actionButton(inputId = "MR_COUNTRY", label = "COUNTRY"),
            ##### REGION ACTION BUTTON #####
            actionButton(inputId = "MR_REGION", label = "REGION")
            ),
          #Conditional Panel visible when REGION ACTION BUTTON is clicked
          conditionalPanel(condition = "input.MR_REGION > 0", 
            #MARGIN (Top: 10px)
            # style = "margin-top: 10px;",
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
            ),
          
          ##### World Map Card #####
          hidden(#Card => Hidden
            card(
              #Card => Headline
              card_header("Interactive World Map"),
              #World Map
              card_body(
                leafletOutput("MR_World_Map", width = "100%", height = 720), #Interactive World Map
                fillable = TRUE, #FLEXBOX CONTAINER
                min_height = "510px", max_height = "510px", #World Map => HEIGHT => !Full_Screen
                max_height_full_screen = "720px", #World Map => HEIGHT => Full_Screen
                padding = 0, #Remove PADDING
                fill = FALSE), #Content => Fixed Size => !Allowed To Scroll
              #World Map => GeoSpatial Data => Source(s)
              card_footer(
                "Sources:", HTML("<a href='https://gis-who.hub.arcgis.com/datasets/95a475ae34af4f54b63ca6e4a6f67fbd_0/explore' target='_blank'>World Health OrGanization (WHO)</a>"), "|", #Link To World Health OrGanization (WHO) - Countries GeoSpatial Data 
                # HTML("<a href='...' target='_blank'>Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques</a>"), "|",
                "Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques", "|", 
                HTML("<a href='https://gadm.org/download_world.html' target='_blank'>Global Administrative Areas (GADM)</a>"), "from", HTML("<a href='https://cran.r-project.org/web/packages/geodata/index.html' target='_blank'>GEODATA</a>")),  #Link To World Health OrGanization (WHO) - Countries GeoSpatial Data 
              #Card => ARGu.
              full_screen = TRUE, #Expand Card To Fit Screen Size
              fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
              id = "MR_World_Map_Card")) #Observe Full Screen State in SHINY
          ),
        
        ##### BIRTH #####
        nav_panel(title = "BIRTH", #tabPanel()
          #MARGIN (Top: 10px)
          tags$div(style = "margin-top: 10px;",
            ##### COUNTRY ACTION BUTTON #####
            actionButton(inputId = "MB_COUNTRY", label = "COUNTRY"),
            ##### REGION ACTION BUTTON #####
            actionButton(inputId = "MB_REGION", label = "REGION")
            ),
          #Conditional Panel visible when REGION ACTION BUTTON is clicked
          conditionalPanel(condition = "input.MB_REGION > 0", 
            #MARGIN (Top: 10px)
            # style = "margin-top: 10px;",
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
  
    ##### CLIMATE #####
    nav_panel(title = "CLIMATE", #tabPanel()
      ##### COUNTRY ACTION BUTTON #####
      actionButton(inputId = "CLIMATE_COUNTRY", label = "COUNTRY"),
      ##### REGION ACTION BUTTON #####
      actionButton(inputId = "CLIMATE_REGION", label = "REGION"), 
      #Conditional Panel visible when REGION ACTION BUTTON is clicked
      conditionalPanel(condition = "input.CLIMATE_REGION > 0", 
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
    
    ##### MIGRATION and CLIMATE #####
    nav_panel(title = "MIGRATION and CLIMATE", #tabPanel()
      #Contains Multiple Tabs
      navset_tab( #tabsetPanel()
        #Selected Tab Panel
        selected = "RESIDENCE",
      
        ##### RESIDENCE #####
        nav_panel(title = "RESIDENCE", #tabPanel()
          #MARGIN (Top: 10px)
          tags$div(style = "margin-top: 10px;",
            ##### COUNTRY ACTION BUTTON #####
            actionButton(inputId = "MCR_COUNTRY", label = "COUNTRY"),
            ##### REGION ACTION BUTTON #####
            actionButton(inputId = "MCR_REGION", label = "REGION")
            ),
          #Conditional Panel visible when REGION ACTION BUTTON is clicked
          conditionalPanel(condition = "input.MCR_REGION > 0", 
            #MARGIN (Top: 10px)
            # style = "margin-top: 10px;",
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
      
        ##### BIRTH #####
        nav_panel(title = "BIRTH", #tabPanel()
          #MARGIN (Top: 10px)
          tags$div(style = "margin-top: 10px;",
            ##### COUNTRY ACTION BUTTON #####
            actionButton(inputId = "MCB_COUNTRY", label = "COUNTRY"),
            ##### REGION ACTION BUTTON #####
            actionButton(inputId = "MCB_REGION", label = "REGION")
            ),
          #Conditional Panel visible when REGION ACTION BUTTON is clicked
          conditionalPanel(condition = "input.MCB_REGION > 0", 
            #MARGIN (Top: 10px)
            # style = "margin-top: 10px;",
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