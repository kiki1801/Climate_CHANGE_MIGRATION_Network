##########################################################################
#####                Climate ChanGe MiGration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   KYLLIAN JAMES | 2023-11-14                   #####
#####                        Mod.: 2024-06-17                        #####
#####                             SERVER                             #####
##########################################################################-

##########################
##### Load Libraries #####
##########################

library(shiny)
library(waiter)
library(shinyjs)
library(leaflet)
library(htmlwidgets)

################
##### DATA #####
################

source("DATA.R")

##################
##### SERVER #####
##################

server <- function(input, output, session) {
  
  ####################
  ##### THEME(s) #####
  ####################
  
  #Switch To Dark Theme When THEME == TRUE
  observe(session$setCurrentTheme(if (isTRUE(input$THEME)) Dark_Mode else Base))
  
  #############################################################################
  ##### World Map PROPERTIES (CSS) => Condition => THEME => TRUE OR FALSE #####
  observe({
    ##### THEME = TRUE #####
    if (isTRUE(input$THEME)) {
      runjs("$('.leaflet-bar button').css({'background-color': '#555555', 'color': 'var(--bs-body-color)'});") #Map View CONTAINER => !HOVER => COLOR(s)
      runjs("$('.leaflet-bar button').hover(function() {$(this).css('background-color', 'var(--bs-secondary)');}, function() {$(this).css('background-color', '#555555');});") #Map View CONTAINER => HOVER => COLOR(s)
      runjs("$('.leaflet-touch .leaflet-bar').attr('style', 'border: 1px solid #4D4D4D !important;');"); #CONTAINER(s) => Map View | Zoom-in | Zoom-out => BORDER => COLOR
      runjs("$('.leaflet-bar a').each(function() {if ($(this).hasClass('leaflet-disabled')) {$(this).css('background-color', 'var(--bs-secondary)');} else {$(this).css('background-color', '#555555');}});") #CONTAINER(s) => Zoom-in | Zoom-out => Disabled | !Disabled => !HOVER => COLOR(s)
      runjs("$('.leaflet-bar a').css({'border-bottom': '1px solid #4D4D4D'});") #Zoom-in CONTAINER => BORDER => COLOR
      runjs("$('.leaflet-bar a:last-child').css({'border-bottom': 'none'});") #Zoom-out CONTAINER => !BORDER
      # runjs("$('.leaflet-bar a').not('.leaflet-disabled').hover(function() {$(this).css('background-color', 'var(--bs-secondary)');}, function() {$(this).css('background-color', '#555555');});") #CONTAINER(s) => Zoom-in | Zoom-out => !Disabled => HOVER => COLOR(s)
      runjs("$('.leaflet-touch .leaflet-control-zoom-in').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23E6E6E6\\' class=\\'bi bi-zoom-in\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5\\'/%3e%3c/svg%3e\")'});"); #Zoom-in CONTAINER => Replace "+" => !Disabled => COLOR
      runjs("$('.leaflet-touch .leaflet-control-zoom-out').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23E6E6E6\\' class=\\'bi bi-zoom-out\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5\\'/%3e%3c/svg%3e\")'});"); #Zoom-out CONTAINER => Replace "-" => !Disabled => COLOR
      runjs("$('.leaflet-bar a.leaflet-control-zoom-in.leaflet-disabled').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23666666\\' class=\\'bi bi-zoom-in\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5\\'/%3e%3c/svg%3e\")'});"); #Zoom-in CONTAINER => Replace "+" => Disabled => COLOR
      runjs("$('.leaflet-bar a.leaflet-control-zoom-out.leaflet-disabled').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23666666\\' class=\\'bi bi-zoom-out\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5\\'/%3e%3c/svg%3e\")'});"); #Zoom-out CONTAINER => Replace "-" => Disabled => COLOR
      #Card Full Screen ENTER (Expand) Appearance
      runjs("$('.bslib-full-screen-enter').css({'background-color': '#555555'});");
      ##### THEME = FALSE ##### 
      } else {
        runjs("$('.leaflet-bar button').css({'background-color': '#FFF', 'color': '#000'});") #Map View CONTAINER => !HOVER => COLOR(s)
        runjs("$('.leaflet-bar button').hover(function() {$(this).css('background-color', '#F4F4F4');}, function() {$(this).css('background-color', '#FFF');});") #Map View CONTAINER => HOVER => COLOR(s)
        runjs("$('.leaflet-touch .leaflet-bar').attr('style', 'border: 1px solid #DDD !important;');"); #Map View | Zoom-in | Zoom-out => CONTAINER(s) => BORDER => COLOR
        runjs("$('.leaflet-bar a').each(function() {if ($(this).hasClass('leaflet-disabled')) {$(this).css('background-color', '#F4F4F4');} else {$(this).css('background-color', '#FFF');}});") #CONTAINER(s) => Zoom-in | Zoom-out => Disabled | !Disabled => !HOVER => COLOR(s)
        runjs("$('.leaflet-bar a').css({'border-bottom': '1px solid #CCC'});") #Zoom-in CONTAINER => BORDER => COLOR
        runjs("$('.leaflet-bar a:last-child').css({'border-bottom': 'none'});") #Zoom-out CONTAINER => !BORDER
        # runjs("$('.leaflet-bar a').not('.leaflet-disabled').hover(function() {$(this).css('background-color', '#F4F4F4');}, function() {$(this).css('background-color', '#FFF');});") #CONTAINER(s) => Zoom-in | Zoom-out => !Disabled => !HOVER => COLOR(s)
        runjs("$('.leaflet-touch .leaflet-control-zoom-in').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'currentColor\\' class=\\'bi bi-zoom-in\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5\\'/%3e%3c/svg%3e\")'});"); #Zoom-in CONTAINER => Replace "+" => !Disabled => COLOR
        runjs("$('.leaflet-touch .leaflet-control-zoom-out').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'currentColor\\' class=\\'bi bi-zoom-out\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5\\'/%3e%3c/svg%3e\")'});"); #Zoom-out CONTAINER => Replace "-" => !Disabled => COLOR
        runjs("$('.leaflet-bar a.leaflet-control-zoom-in.leaflet-disabled').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23CCCCCC\\' class=\\'bi bi-zoom-in\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5\\'/%3e%3c/svg%3e\")'});"); #Zoom-in CONTAINER => Replace "+" => Disabled => COLOR
        runjs("$('.leaflet-bar a.leaflet-control-zoom-out.leaflet-disabled').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23CCCCCC\\' class=\\'bi bi-zoom-out\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5\\'/%3e%3c/svg%3e\")'});"); #Zoom-out CONTAINER => Replace "-" => Disabled => COLOR
        #Card Full Screen ENTER (Expand) Appearance
        runjs("$('.bslib-full-screen-enter').css({'background-color': 'var(--bslib-color-bg, var(--bs-card-bg, var(--bs-body-bg)))'});");
        } 
    })
  
  ######################################################################################
  ##### World Map PROPERTIES (CSS) => Condition(s) => Zoom | THEME (TRUE OR FALSE) #####
  observeEvent(input$MR_World_Map_zoom, {#Execute (JavaScript) Code When ZOOM-IN/ZOOM-OUT           
    ##### THEME = TRUE #####
    if (isTRUE(input$THEME)) {
      runjs("$('.leaflet-bar a').each(function() {if ($(this).hasClass('leaflet-disabled')) {$(this).css('background-color', 'var(--bs-secondary)');} else {$(this).css('background-color', '#555555');}});") #CONTAINER(s) => Zoom-in | Zoom-out => Disabled | !Disabled => !HOVER => COLOR(s) => DYNAMIC
      # runjs("$('.leaflet-bar a').not('.leaflet-disabled').hover(function() {$(this).css('background-color', 'var(--bs-secondary)');}, function() {$(this).css('background-color', '#555555');});") #CONTAINER(s) => Zoom-in | Zoom-out => !Disabled => HOVER => COLOR(s)
      #CONTAINER(s) => Replace "+"/"-" => Disabled | !Disabled => COLOR(s) => DYNAMIC
      runjs("$('.leaflet-bar a').each(function() { \
                  if ($(this).hasClass('leaflet-control-zoom-in') && $(this).hasClass('leaflet-disabled')) { \
                      $(this).css('background-image', 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23666666\\' class=\\'bi bi-zoom-in\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5\\'/%3e%3c/svg%3e\")');} \
                  if ($(this).hasClass('leaflet-control-zoom-in') && !$(this).hasClass('leaflet-disabled')) { \
                      $(this).css('background-image', 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23E6E6E6\\' class=\\'bi bi-zoom-in\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5\\'/%3e%3c/svg%3e\")');} \
                  if ($(this).hasClass('leaflet-control-zoom-out') && $(this).hasClass('leaflet-disabled')) { \
                      $(this).css('background-image', 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23666666\\' class=\\'bi bi-zoom-out\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5\\'/%3e%3c/svg%3e\")');} \
                  if ($(this).hasClass('leaflet-control-zoom-out') && !$(this).hasClass('leaflet-disabled')) { \
                      $(this).css('background-image', 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23E6E6E6\\' class=\\'bi bi-zoom-out\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5\\'/%3e%3c/svg%3e\")');}});")
      ##### THEME = FALSE #####
      } else {
        runjs("$('.leaflet-bar a').each(function() {if ($(this).hasClass('leaflet-disabled')) {$(this).css('background-color', '#F4F4F4');} else {$(this).css('background-color', '#FFF');}});") #CONTAINER(s) => Zoom-in | Zoom-out => Disabled | !Disabled => !HOVER => COLOR(s) => DYNAMIC
        # runjs("$('.leaflet-bar a').not('.leaflet-disabled').hover(function() {$(this).css('background-color', '#F4F4F4');}, function() {$(this).css('background-color', '#FFF');});") #CONTAINER(s) => Zoom-in | Zoom-out => !Disabled => HOVER => COLOR(s)
        #CONTAINER(s) => Replace "+"/"-" => Disabled | !Disabled => COLOR(s) => DYNAMIC
        runjs("$('.leaflet-bar a').each(function() { \
                  if ($(this).hasClass('leaflet-control-zoom-in') && $(this).hasClass('leaflet-disabled')) { \
                      $(this).css('background-image', 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23CCCCCC\\' class=\\'bi bi-zoom-in\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5\\'/%3e%3c/svg%3e\")');} \
                  if ($(this).hasClass('leaflet-control-zoom-in') && !$(this).hasClass('leaflet-disabled')) { \
                      $(this).css('background-image', 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'currentColor\\' class=\\'bi bi-zoom-in\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5\\'/%3e%3c/svg%3e\")');} \
                  if ($(this).hasClass('leaflet-control-zoom-out') && $(this).hasClass('leaflet-disabled')) { \
                      $(this).css('background-image', 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23CCCCCC\\' class=\\'bi bi-zoom-out\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5\\'/%3e%3c/svg%3e\")');} \
                  if ($(this).hasClass('leaflet-control-zoom-out') && !$(this).hasClass('leaflet-disabled')) { \
                      $(this).css('background-image', 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'currentColor\\' class=\\'bi bi-zoom-out\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5\\'/%3e%3c/svg%3e\")');}});")
        }
    })
  
  #####################
  ##### WAITER(s) #####
  #####################
  
  ########################################
  ##### APPLICATION LAUNCH WAITER(s) #####
  
  #CURSOR Class To WAIT
  session$sendCustomMessage(type = "CURSORwithinBODY", message = "wait-cursor")
  
  #!Execution in R For X SECONDs
  Sys.sleep(1)
  
  ##### HIDE WAITER n°1 #####
  waiter_hide() #For waiterShowOnLoad()

  ##### CREATE WAITER n°2 #####
  MWaiter <- Waiter$new(
    html = tagList(
      div(spin_orbiter(), style = "margin-bottom: 10px;"), #SPINNER
      div(span("Load International MiGration Stock Data"))), #Html Text
    color = "#1D1F21", #BackGround Color
    fadeout = 500) #Fade out effect when screen is removed (Boolean OR Numeric)
  
  ##### WAITER n°2 MESSAGE(s) #####
  MESSAGEs <- c("Load GeoSpatial Data")
  
  ##### SHOW WAITER n°2 #####
  MWaiter$show()
  
  #!Execution in R For X SECONDs
  Sys.sleep(1.25)
  
  ##### DYNAMIC UPDATE(s) #####
  for(MESSAGE in 1:length(MESSAGEs)){
    MWaiter$update(html = tagList(
      div(spin_orbiter(), style = "margin-bottom: 10px;"), #SPINNER
      div(span(MESSAGEs[MESSAGE])))) #Html Text
    #!Execution in R For X SECONDs
    Sys.sleep(1.25)}
  
  ##### HIDE WAITER n°2 #####
  MWaiter$hide()
  
  #CURSOR Class To Default
  session$sendCustomMessage(type = "CURSORwithinBODY", message = "default-cursor")
  
  ################################
  ##### Reactive Values (RV) #####
  ################################
  
  ##### World Map Card RV #####
  World_Map_Card_RV <- reactiveValues(
    TilesLoaded = FALSE, #This value is intended to indicate whether World Map Tiles have been loaded or not
    MR_REGION_Click_Count = 0, #Track clicks on REGION ACTION BUTTON
    WM_Card_PROPERTY = FALSE) #New PROPERTY (CSS) on World Map Card => WM_Card_PROPERTY = TRUE
  
  #####################
  ##### MIGRATION #####
  #####################
  
  #####################
  ##### RESIDENCE #####
  
  ##### COUNTRY ACTION BUTTON #####
  observeEvent(input$MR_COUNTRY, { #WHEN COUNTRY ACTION BUTTON is clicked
    
    #Add CSS class from COUNTRY ACTION BUTTON
    shinyjs::addClass(id = "MR_COUNTRY", class = "active")
    #Remove CSS class from REGION ACTION BUTTON
    shinyjs::removeClass(id = "MR_REGION", class = "active")
    #Make REGION CHOICE RADIO BUTTONs invisible
    shinyjs::hide(id = "MRR_REGION_CHOICE", anim = TRUE, animType = "slide", time = 0.25)

    ##### World Map Tiles #####
    if (!World_Map_Card_RV$TilesLoaded) {#Execute Code To load Tiles ONLY if reactive value is FALSE
      
      #Make World Map Card visible
      shinyjs::show(id = "MR_World_Map_Card", anim = TRUE, animType = "fade", time = 0.25)
      
      #World Map
      output$MR_World_Map <- renderLeaflet({
        leaflet() %>% 
          #Add Tile LAYER
          addTiles( 
            attribution = HTML("© <a href='https://www.openstreetmap.org/copyright/' target='_blank'> OpenStreetMap </a>"), #Tile LAYER Attribution
            options = tileOptions(minZoom = 1, maxZoom = 10, maxNativeZoom = 18)) %>% #Min/Max Zoom Levels
          addScaleBar(position = "bottomleft") %>%
          #World Map View => CENTER => LONGITUDE = 0 | LATITUDE = 0 | Zoom Level = 1
          setView(lng = 0, lat = 0, zoom = 1) %>%
          #Execute JavaScript Code When Map is Rendered => Save initial center and zoom level of world map
          onRender(JS("function(el, x){var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>%
          #Add Reset Map View Button
          addEasyButton(easyButton(
            icon = "ion-earth", #EARHT ICON BUTTON from https://ionic.io/ionicons
            title = "Reset Map View", #TEXT SHOW on HOVER
            #JavaScript Code To Run When Button is Clicked
            onClick = JS("function(btn, map){
                            //Reset map view to initial center and zoom level
                            map.setView(map._initialCenter, map._initialZoom);}"),
            position = "topleft", #Button Position
            id = "Reset_Map_View_Button")) #Button IDENTIFIER
        })
      
      #Reactive Value To TRUE => World Map Tiles have been loaded with success
      World_Map_Card_RV$TilesLoaded <- TRUE
      
      #Remove Leaflet Prefix in Attribution Control
      observe({runjs("var CheckMapLoaded = setInterval(function() { //Call Function EVERY X MILLISECONDs
                          var Map = $('#MR_World_Map').data('leaflet-map'); //Retrieve MR_World_Map
                          if (Map) { //Execute Code if Map is retrieved with success
                              Map.attributionControl.setPrefix(false); //Remove default attribution Prefix
                              clearInterval(CheckMapLoaded); //Once Map found and Prefix removed => CheckMapLoaded => !Run ANYMORE
                              }
                          }, 100);")}) #Run Function EVERY 100 MILLISECONDs
      
      #World Map PROPERTIES (CSS) => Condition => THEME == TRUE
      observe({
        THEME_BOOLEAN <- input$THEME #Retrieve THEME Value
        #Formatted Combination => Text and Variable Values
        runjs(sprintf("var CheckMapLoaded = setInterval(function() { //Call Function EVERY X MILLISECONDs
                           var Map = $('#MR_World_Map').data('leaflet-map'); //Retrieve MR_World_Map
                           if (Map) { //Execute Code if Map is retrieved with success
                               if (%s) { //Execute Code if THEME_BOOLEAN == TRUE
                                   $('.leaflet-bar button').css({'background-color': '#555555', 'color': 'var(--bs-body-color)'}); //Map View CONTAINER => !HOVER => COLOR(s)
                                   $('.leaflet-bar button').hover(function() {$(this).css('background-color', 'var(--bs-secondary)');}, function() {$(this).css('background-color', '#555555');}); //Map View CONTAINER => HOVER => COLOR(s)
                                   $('.leaflet-touch .leaflet-bar').attr('style', 'border: 1px solid #4D4D4D !important;'); //CONTAINER(s) => Map View | Zoom-in | Zoom-out => BORDER => COLOR
                                   $('.leaflet-bar a').css({'border-bottom': '1px solid #4D4D4D'}); //Zoom-in CONTAINER => BORDER => COLOR
                                   $('.leaflet-bar a:last-child').css({'border-bottom': 'none'}); //Zoom-out CONTAINER => !BORDER
                                   $('.bslib-full-screen-enter').css({'background-color': '#555555'}); //Card Full Screen ENTER (Expand) Appearance
                                   }
                               clearInterval(CheckMapLoaded); //Once Map found and code executed => CheckMapLoaded => !Run ANYMORE
                               } 
                           }, 100);", #Run Function EVERY 100 MILLISECONDs
                      ifelse(THEME_BOOLEAN, "true", "false")))}) #THEME_BOOLEAN Value in JavaScript Format
      
      }
    
    #PROPERTY (CSS) on World Map Card => Condition(s) => COUNTRY ACTION BUTTON is active/clicked | REGION ACTION BUTTON Click Count > 0
    if (World_Map_Card_RV$MR_REGION_Click_Count > 0) {runjs("$('#MR_World_Map_Card').css('margin-top', '-10px');")}
    
    #Reactive Value To TRUE => World Map Card Have a New PROPERTY
    if (World_Map_Card_RV$MR_REGION_Click_Count > 0) {World_Map_Card_RV$WM_Card_PROPERTY <- TRUE}
    
    })
  
  #Make REGION CHOICE RADIO BUTTONs invisible (first click will also be animated)
  shinyjs::hide(id = "MRR_REGION_CHOICE", anim = FALSE)
  
  ##### REGION ACTION BUTTON #####
  observeEvent(input$MR_REGION, { #WHEN REGION ACTION BUTTON is clicked
    
    #Add CSS class from REGION ACTION BUTTON
    shinyjs::addClass(id = "MR_REGION", class = "active")
    #Remove CSS class from COUNTRY ACTION BUTTON
    shinyjs::removeClass(id = "MR_COUNTRY", class = "active")
    #Make REGION CHOICE RADIO BUTTONs visible
    shinyjs::show(id = "MRR_REGION_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    ##### World Map Tiles #####
    if (!World_Map_Card_RV$TilesLoaded) {#Execute Code To load Tiles ONLY if reactive value is FALSE
      
      #Make World Map Card visible
      shinyjs::show(id = "MR_World_Map_Card", anim = TRUE, animType = "fade", time = 0.25)
      
      #World Map
      output$MR_World_Map <- renderLeaflet({
        leaflet() %>% 
          #Add Tile LAYER
          addTiles( 
            attribution = HTML("© <a href='https://www.openstreetmap.org/copyright/' target='_blank'> OpenStreetMap </a>"), #Tile LAYER Attribution
            options = tileOptions(minZoom = 1, maxZoom = 10, maxNativeZoom = 18)) %>% #Min/Max Zoom Levels
          addScaleBar(position = "bottomleft") %>%
          #World Map View => CENTER => LONGITUDE = 0 | LATITUDE = 0 | Zoom Level = 1
          setView(lng = 0, lat = 0, zoom = 1) %>%
          #Execute JavaScript Code When Map is Rendered => Save initial center and zoom level of world map
          onRender(JS("function(el, x){var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>%
          #Add Reset Map View Button
          addEasyButton(easyButton(
            icon = "ion-earth", #EARHT ICON BUTTON from https://ionic.io/ionicons
            title = "Reset Map View", #TEXT SHOW on HOVER
            #JavaScript Code To Run When Button is Clicked
            onClick = JS("function(btn, map){
                            //Reset map view to initial center and zoom level
                            map.setView(map._initialCenter, map._initialZoom);}"),
            position = "topleft", #Button Position
            id = "Reset_Map_View_Button")) #Button IDENTIFIER
        })
      
      #Reactive Value To TRUE => World Map Tiles have been loaded with success
      World_Map_Card_RV$TilesLoaded <- TRUE
      
      #Remove Leaflet Prefix in Attribution Control
      observe({runjs("var CheckMapLoaded = setInterval(function() { //Call Function EVERY X MILLISECONDs
                          var Map = $('#MR_World_Map').data('leaflet-map'); //Retrieve MR_World_Map
                          if (Map) { //Execute Code if Map is retrieved with success
                              Map.attributionControl.setPrefix(false); //Remove default attribution Prefix
                              clearInterval(CheckMapLoaded); //Once Map found and Prefix removed => CheckMapLoaded => !Run ANYMORE
                              }
                          }, 100);")}) #Run Function EVERY 100 MILLISECONDs
      
      #World Map PROPERTIES (CSS) => Condition => THEME == TRUE
      observe({
        THEME_BOOLEAN <- input$THEME #Retrieve THEME Value
        #Formatted Combination => Text and Variable Values
        runjs(sprintf("var CheckMapLoaded = setInterval(function() { //Call Function EVERY X MILLISECONDs
                           var Map = $('#MR_World_Map').data('leaflet-map'); //Retrieve MR_World_Map
                           if (Map) { //Execute Code if Map is retrieved with success
                               if (%s) { //Execute Code if THEME_BOOLEAN == TRUE
                                   $('.leaflet-bar button').css({'background-color': '#555555', 'color': 'var(--bs-body-color)'}); //Map View CONTAINER => !HOVER => COLOR(s)
                                   $('.leaflet-bar button').hover(function() {$(this).css('background-color', 'var(--bs-secondary)');}, function() {$(this).css('background-color', '#555555');}); //Map View CONTAINER => HOVER => COLOR(s)
                                   $('.leaflet-touch .leaflet-bar').attr('style', 'border: 1px solid #4D4D4D !important;'); //CONTAINER(s) => Map View | Zoom-in | Zoom-out => BORDER => COLOR
                                   $('.leaflet-bar a').css({'border-bottom': '1px solid #4D4D4D'}); //Zoom-in CONTAINER => BORDER => COLOR
                                   $('.leaflet-bar a:last-child').css({'border-bottom': 'none'}); //Zoom-out CONTAINER => !BORDER
                                   $('.bslib-full-screen-enter').css({'background-color': '#555555'}); //Card Full Screen ENTER (Expand) Appearance
                                   }
                               clearInterval(CheckMapLoaded); //Once Map found and code executed => CheckMapLoaded => !Run ANYMORE
                               } 
                           }, 100);", #Run Function EVERY 100 MILLISECONDs
                      ifelse(THEME_BOOLEAN, "true", "false")))}) #THEME_BOOLEAN Value in JavaScript Format
      
      }
    
    #Track clicks on REGION ACTION BUTTON (+1)
    World_Map_Card_RV$MR_REGION_Click_Count <- World_Map_Card_RV$MR_REGION_Click_Count + 1
    
    #PROPERTY (CSS) on World Map Card => Condition(s) => REGION ACTION BUTTON is active/clicked | World Map Card Have a New PROPERTY (WM_Card_PROPERTY = TRUE)
    if (World_Map_Card_RV$WM_Card_PROPERTY) {runjs("$('#MR_World_Map_Card').css('margin-top', '0px');")}
    
    })
  
  #################
  ##### BIRTH #####
  
  ##### COUNTRY ACTION BUTTON #####
  observeEvent(input$MB_COUNTRY, { #WHEN COUNTRY ACTION BUTTON is clicked
    
    #Add CSS class from COUNTRY ACTION BUTTON
    shinyjs::addClass(id = "MB_COUNTRY", class = "active")
    #Remove CSS class from REGION ACTION BUTTON
    shinyjs::removeClass(id = "MB_REGION", class = "active")
    #Make REGION CHOICE RADIO BUTTONs invisible
    shinyjs::hide(id = "MBR_REGION_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  #Make REGION CHOICE RADIO BUTTONs invisible (first click will also be animated)
  shinyjs::hide(id = "MBR_REGION_CHOICE", anim = FALSE)
  
  ##### REGION ACTION BUTTON #####
  observeEvent(input$MB_REGION, { #WHEN REGION ACTION BUTTON is clicked
    
    #Add CSS class from REGION ACTION BUTTON
    shinyjs::addClass(id = "MB_REGION", class = "active")
    #Remove CSS class from COUNTRY ACTION BUTTON
    shinyjs::removeClass(id = "MB_COUNTRY", class = "active")
    #Make REGION CHOICE RADIO BUTTONs visible
    shinyjs::show(id = "MBR_REGION_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  ###################
  ##### CLIMATE #####
  ###################
  
  ##### COUNTRY ACTION BUTTON #####
  observeEvent(input$CLIMATE_COUNTRY, { #WHEN COUNTRY ACTION BUTTON is clicked
    
    #Add CSS class from COUNTRY ACTION BUTTON
    shinyjs::addClass(id = "CLIMATE_COUNTRY", class = "active")
    #Remove CSS class from REGION ACTION BUTTON
    shinyjs::removeClass(id = "CLIMATE_REGION", class = "active")
    #Make REGION CHOICE RADIO BUTTONs invisible
    shinyjs::hide(id = "CLIMATE_R_REGION_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  #Make REGION CHOICE RADIO BUTTONs invisible (first click will also be animated)
  shinyjs::hide(id = "CLIMATE_R_REGION_CHOICE", anim = FALSE)
  
  ##### REGION ACTION BUTTON #####
  observeEvent(input$CLIMATE_REGION, { #WHEN REGION ACTION BUTTON is clicked
    
    #Add CSS class from REGION ACTION BUTTON
    shinyjs::addClass(id = "CLIMATE_REGION", class = "active")
    #Remove CSS class from COUNTRY ACTION BUTTON
    shinyjs::removeClass(id = "CLIMATE_COUNTRY", class = "active")
    #Make REGION CHOICE RADIO BUTTONs visible
    shinyjs::show(id = "CLIMATE_R_REGION_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  ##################################
  #####  MIGRATION and CLIMATE #####
  ##################################
  
  #####################
  ##### RESIDENCE #####
  
  ##### COUNTRY ACTION BUTTON #####
  observeEvent(input$MCR_COUNTRY, { #WHEN COUNTRY ACTION BUTTON is clicked
    
    #Add CSS class from COUNTRY ACTION BUTTON
    shinyjs::addClass(id = "MCR_COUNTRY", class = "active")
    #Remove CSS class from REGION ACTION BUTTON
    shinyjs::removeClass(id = "MCR_REGION", class = "active")
    #Make REGION CHOICE RADIO BUTTONs invisible
    shinyjs::hide(id = "MCRR_REGION_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  #Make REGION CHOICE RADIO BUTTONs invisible (first click will also be animated)
  shinyjs::hide(id = "MCRR_REGION_CHOICE", anim = FALSE)
  
  ##### REGION ACTION BUTTON #####
  observeEvent(input$MCR_REGION, { #WHEN REGION ACTION BUTTON is clicked
    
    #Add CSS class from REGION ACTION BUTTON
    shinyjs::addClass(id = "MCR_REGION", class = "active")
    #Remove CSS class from COUNTRY ACTION BUTTON
    shinyjs::removeClass(id = "MCR_COUNTRY", class = "active")
    #Make REGION CHOICE RADIO BUTTONs visible
    shinyjs::show(id = "MCRR_REGION_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  #################
  ##### BIRTH #####
  
  ##### COUNTRY ACTION BUTTON #####
  observeEvent(input$MCB_COUNTRY, { #WHEN COUNTRY ACTION BUTTON is clicked
    
    #Add CSS class from COUNTRY ACTION BUTTON
    shinyjs::addClass(id = "MCB_COUNTRY", class = "active")
    #Remove CSS class from REGION ACTION BUTTON
    shinyjs::removeClass(id = "MCB_REGION", class = "active")
    #Make REGION CHOICE RADIO BUTTONs invisible
    shinyjs::hide(id = "MCBR_REGION_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  #Make REGION CHOICE RADIO BUTTONs invisible (first click will also be animated)
  shinyjs::hide(id = "MCBR_REGION_CHOICE", anim = FALSE)
  
  ##### REGION ACTION BUTTON #####
  observeEvent(input$MCB_REGION, { #WHEN REGION ACTION BUTTON is clicked
    
    #Add CSS class from REGION ACTION BUTTON
    shinyjs::addClass(id = "MCB_REGION", class = "active")
    #Remove CSS class from COUNTRY ACTION BUTTON
    shinyjs::removeClass(id = "MCB_COUNTRY", class = "active")
    #Make REGION CHOICE RADIO BUTTONs visible
    shinyjs::show(id = "MCBR_REGION_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  }