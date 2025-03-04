##########################################################################
#####                CLIMATE CHANGE MIGRATION NETWORK                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   KYLLIAN JAMES | 2023-11-14                   #####
#####                        Mod.: 2025-03-04                        #####
#####                             SERVER                             #####
##########################################################################

##########################
##### Load Libraries #####
##########################

library(shiny)
library(shinyjs)
library(slickR)
library(waiter)
library(leaflet)
library(htmlwidgets)
library(dplyr)
# library(geosphere)
library(DescTools)
library(plotly)

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
  
  ###########################################################
  ##### CLIMINET => Condition => THEME => TRUE OR FALSE #####
  observe({
    ##### THEME = TRUE #####
    if (isTRUE(input$THEME)) {
      runjs("$(document).ready(function() {$('#climinet').attr('src', 'CLIMINET/Arrows/CLIMINET-WHITE-Arrows.svg'); $('#climinet').attr('width', '32px'); $('#climinet').attr('height', '32px');});") #CLIMINET => LOGO => WHITE/SIZE
      # runjs("$(document).ready(function() {$('#climinet-home').attr('src', 'CLIMINET/Arrows/Climate/CLIMINET-WHITE-Arrows-Climate.svg'); $('#climinet-home').attr('width', '320px'); $('#climinet-home').attr('height', '320px');});") #CLIMINET => HOME => WHITE/SIZE
      runjs("$(document).ready(function() {$('#climinet-home-le').attr('src', 'CLIMINET/Arrows/Climate/CLIMINET-WHITE-Arrows-Climate.svg'); $('#climinet-home-le').attr('width', '256px'); $('#climinet-home-le').attr('height', '256px');});") #CLIMINET => HOME => WHITE/SIZE
      runjs("$(document).ready(function() {$('#climinet-home-ri').attr('src', 'CLIMINET/Arrows/Climate/CLIMINET-WHITE-Arrows-Climate.svg'); $('#climinet-home-ri').attr('width', '256px'); $('#climinet-home-ri').attr('height', '256px');});") #CLIMINET => HOME => WHITE/SIZE
      ##### THEME = FALSE #####
      } else {
        runjs("$(document).ready(function() {$('#climinet').attr('src', 'CLIMINET/Arrows/CLIMINET-GREEN-Arrows.svg'); $('#climinet').attr('width', '32px'); $('#climinet').attr('height', '32px');});") #CLIMINET => LOGO => COLOR(s)/SIZE
        # runjs("$(document).ready(function() {$('#climinet-home').attr('src', 'CLIMINET/Arrows/Climate/CLIMINET-GREEN-Arrows-Climate.svg'); $('#climinet-home').attr('width', '320px'); $('#climinet-home').attr('height', '320px');});") #CLIMINET => HOME => COLOR(s)/SIZE
        runjs("$(document).ready(function() {$('#climinet-home-le').attr('src', 'CLIMINET/Arrows/Climate/CLIMINET-GREEN-Arrows-Climate.svg'); $('#climinet-home-le').attr('width', '256px'); $('#climinet-home-le').attr('height', '256px');});") #CLIMINET => HOME => COLOR(s)/SIZE
        runjs("$(document).ready(function() {$('#climinet-home-ri').attr('src', 'CLIMINET/Arrows/Climate/CLIMINET-GREEN-Arrows-Climate.svg'); $('#climinet-home-ri').attr('width', '256px'); $('#climinet-home-ri').attr('height', '256px');});") #CLIMINET => HOME => COLOR(s)/SIZE
        }
    })
  
  #############################################################################
  ##### HOME => PARTNER(s) Card(s) => Condition => THEME => TRUE OR FALSE #####
  observe({
    ##### THEME = TRUE #####
    if (isTRUE(input$THEME)) {
      runjs("$(document).ready(function() {$('#anr').attr('src', 'Partners/ANR/ANR-WHITE.png'); $('#anr').attr('width', '195px'); $('#anr').attr('height', '95px');});") #ANR Card => LOGO => WHITE/SIZE
      runjs("$(document).ready(function() {$('#belmont-forum').attr('src', 'Partners/BELMONT_FORUM/BELMONT_FORUM-WHITE.png'); $('#belmont-forum').attr('width', '200px'); $('#belmont-forum').attr('height', '60px'); $('#belmont-forum').css('margin-top', '67.5px'); $('#belmont-forum').css('margin-bottom', '67.5px');});") #BELMONT_FORUM Card => LOGO => WHITE/SIZE/POSITION
      runjs("$(document).ready(function() {$('#iai').attr('src', 'Partners/IAI/IAI-WHITE.png'); $('#iai').attr('width', '195px'); $('#iai').attr('height', '195px');});") #IAI Card => LOGO => WHITE/SIZE
      ##### THEME = FALSE #####
      } else {
        runjs("$(document).ready(function() {$('#anr').attr('src', 'Partners/ANR/ANR.jpg'); $('#anr').attr('width', '240px'); $('#anr').attr('height', '90px');});") #ANR Card => LOGO => COLOR(s)/SIZE
        runjs("$(document).ready(function() {$('#belmont-forum').attr('src', 'Partners/BELMONT_FORUM/BELMONT_FORUM.png'); $('#belmont-forum').attr('width', '235px'); $('#belmont-forum').attr('height', '60px'); $('#belmont-forum').css('margin-top', '65px'); $('#belmont-forum').css('margin-bottom', '65px');});") #BELMONT_FORUM Card => LOGO => COLOR(s)/SIZE
        runjs("$(document).ready(function() {$('#iai').attr('src', 'Partners/IAI/IAI.png'); $('#iai').attr('width', '200px'); $('#iai').attr('height', '190px');});") #IAI Card => LOGO => COLOR(s)/SIZE
        }
    })
  
  ###################################################################
  ##### HOME => Carousel => Condition => THEME => TRUE OR FALSE #####
  observe({
    ##### THEME = TRUE #####
    if (isTRUE(input$THEME)) {
      runjs("$(document).ready(function() {$('#home-dea-capture').attr('src', 'Home/DEA-Capture-Dark-75.png'); $('#home-dea-capture').attr('width', '960px'); $('#home-dea-capture').attr('height', '432.5px');});") #DEA-Capture => DARK/SIZE
      runjs("$('#home strong').css({'color': '#E6E6E6'});") #TEXT => WHITE
      runjs("$('.redirection-button').css({'color': '#E6E6E6', 'border-color': '#606163'});") #Default Button Appearance
      runjs("$('.redirection-button').hover(function() {$(this).css('border-color', '#333537');}, function() {$(this).css('border-color', '#606163');});")  #CSS pseudo-class :hover when button is hover
      runjs("$('.sort-button').mousedown(function() {$(this).css('border-color', '#333537');}).mouseup(function() {$(this).css('border-color', '#606163');});") #CSS pseudo-class :active when button is clicked
      ##### THEME = FALSE #####
      } else {
      runjs("$(document).ready(function() {$('#home-dea-capture').attr('src', 'Home/DEA-Capture-75.png'); $('#home-dea-capture').attr('width', '960px'); $('#home-dea-capture').attr('height', '432.5px');});") #DEA-CAPTURE => WHITE/SIZE
      runjs("$('#home strong').css({'color': '#333537'});") #TEXT => BLACK
      runjs("$('.redirection-button').css({'color': '#333537', 'border-color': '#B9BABA'});") #Default Button Appearance
      runjs("$('.redirection-button').hover(function() {$(this).css('border-color', '#E6E6E6');}, function() {$(this).css('border-color', '#B9BABA');});")  #CSS pseudo-class :hover when button is hover
      runjs("$('.sort-button').mousedown(function() {$(this).css('border-color', '#E6E6E6');}).mouseup(function() {$(this).css('border-color', '#B9BABA');});") #CSS pseudo-class :active when button is clicked
      }
    })
  
  #########################################################################
  ##### RESEARCH-RESULT(s) Card(s) => Condition => THEME => TRUE OR FALSE #####
  observe({
    ##### THEME = TRUE #####
    if (isTRUE(input$THEME)) {
      runjs("$('.sort-button').css({'color': '#E6E6E6', 'border-color': '#606163'});") #Default Button Appearance
      runjs("$('.sort-button').hover(function() {$(this).css('background-color', '#404040');}, function() {$(this).css('background-color', 'transparent');});")  #CSS pseudo-class :hover when button is hover
      runjs("$('.sort-button').hover(function() {$(this).css('border-color', '#404040');}, function() {$(this).css('border-color', '#606163');});")  #CSS pseudo-class :hover when button is hover
      runjs("$('.sort-button').mousedown(function() {$(this).css('background-color', '#404040');}).mouseup(function() {$(this).css('background-color', 'transparent');});") #CSS pseudo-class :active when button is clicked
      runjs("$('.sort-button').mousedown(function() {$(this).css('border-color', '#404040');}).mouseup(function() {$(this).css('border-color', '#606163');});") #CSS pseudo-class :active when button is clicked
      # runjs("$(document).ready(function() {$('.ion-person').attr('src', 'Icons/IONICONS/ion-person-outline-white.svg'); $('.ion-person').attr('width', '16px'); $('.ion-person').attr('height', '16px');});") #ICON => PERSON => WHITE/SIZE
      # runjs("$(document).ready(function() {$('.ion-people').attr('src', 'Icons/IONICONS/ion-people-outline-white.svg'); $('.ion-people').attr('width', '16px'); $('.ion-people').attr('height', '16px');});") #ICON => PEOPLE => WHITE/SIZE
      runjs("$(document).ready(function() {$('.ion-newspaper').attr('src', 'Icons/IONICONS/ion-newspaper-outline-white.svg'); $('.ion-newspaper').attr('width', '16px'); $('.ion-newspaper').attr('height', '16px');});") #ICON => NEWSPAPER => WHITE/SIZE
      ##### THEME = FALSE #####
      } else {
        runjs("$('.sort-button').css({'color': '#333537', 'border-color': '#B9BABA'});") #Default Button Appearance
        runjs("$('.sort-button').hover(function() {$(this).css('background-color', '#E6E6E6');}, function() {$(this).css('background-color', 'transparent');});")  #CSS pseudo-class :hover when button is hover
        runjs("$('.sort-button').hover(function() {$(this).css('border-color', '#E6E6E6');}, function() {$(this).css('border-color', '#B9BABA');});")  #CSS pseudo-class :hover when button is hover
        runjs("$('.sort-button').mousedown(function() {$(this).css('background-color', '#E6E6E6');}).mouseup(function() {$(this).css('background-color', 'transparent');});") #CSS pseudo-class :active when button is clicked
        runjs("$('.sort-button').mousedown(function() {$(this).css('border-color', '#E6E6E6');}).mouseup(function() {$(this).css('border-color', '#B9BABA');});") #CSS pseudo-class :active when button is clicked
        # runjs("$(document).ready(function() {$('.ion-person').attr('src', 'Icons/IONICONS/ion-person-outline-black.svg'); $('.ion-person').attr('width', '16px'); $('.ion-person').attr('height', '16px');});") #ICON => PERSON => COLOR(s)/SIZE
        # runjs("$(document).ready(function() {$('.ion-people').attr('src', 'Icons/IONICONS/ion-people-outline-black.svg'); $('.ion-people').attr('width', '16px'); $('.ion-people').attr('height', '16px');});") #ICON => PEOPLE => COLOR(s)/SIZE
        runjs("$(document).ready(function() {$('.ion-newspaper').attr('src', 'Icons/IONICONS/ion-newspaper-outline-black.svg'); $('.ion-newspaper').attr('width', '16px'); $('.ion-newspaper').attr('height', '16px');});") #ICON => NEWSPAPER => COLOR(s)/SIZE
        }
    })
  
  #####################################################################
  ##### MEMBER(s) Card(s) => Condition => THEME => TRUE OR FALSE #####
  observe({
    ##### THEME = TRUE #####
    if (isTRUE(input$THEME)) {
      runjs("$(document).ready(function() {$('#fen-uchile').attr('src', 'Members/Universidad-de-Chile/FEN/FEN-WHITE.png'); $('#fen-uchile').attr('width', '125px'); $('#fen-uchile').attr('height', '70px');});") #FEN-Chile => LOGO => WHITE/SIZE
      runjs("$(document).ready(function() {$('#TSE-T-LAURENT').attr('src', 'Members/Toulouse-School-of-Economics/TSE_NEGATIVE-Small.png'); $('#TSE-T-LAURENT').attr('width', '175px'); $('#TSE-T-LAURENT').attr('height', '60px');});") #TSE => LOGO => WHITE/SIZE
      runjs("$(document).ready(function() {$('#udp').attr('src', 'Members/Universidad-DIEGO-Portales/Universidad-DIEGO-Portales-WHITE.png'); $('#udp').attr('width', '215px'); $('#udp').attr('height', '50px');});") #Universidad DIEGO Portales => LOGO => WHITE/SIZE
      runjs("$(document).ready(function() {$('#TSE-C-THOMAS').attr('src', 'Members/Toulouse-School-of-Economics/TSE_NEGATIVE-Small.png'); $('#TSE-C-THOMAS').attr('width', '175px'); $('#TSE-C-THOMAS').attr('height', '60px');});") #TSE => LOGO => WHITE/SIZE
      runjs("$(document).ready(function() {$('#udesa').attr('src', 'Members/Universidad-de-San-Andres/Universidad-de-San-Andres-WhiteVersion.svg'); $('#udesa').attr('width', '190px'); $('#udesa').attr('height', '45px');});") #UdeSA => LOGO => WHITE/SIZE
      runjs("$(document).ready(function() {$('#uai').attr('src', 'Members/Universidad-Adolfo-Ibanez/Universidad-Adolfo-Ibanez-WHITE.svg'); $('#uai').attr('width', '190px'); $('#uai').attr('height', '40px');});") #UAI => LOGO => WHITE/SIZE
      ##### THEME = FALSE #####
      } else {
        runjs("$(document).ready(function() {$('#fen-uchile').attr('src', 'Members/Universidad-de-Chile/FEN/FEN.png'); $('#fen-uchile').attr('width', '125px'); $('#fen-uchile').attr('height', '70px');});") #FEN-Chile => LOGO => COLOR(s)/SIZE
        runjs("$(document).ready(function() {$('#TSE-T-LAURENT').attr('src', 'Members/Toulouse-School-of-Economics/TSE_MainVersion-Small.png'); $('#TSE-T-LAURENT').attr('width', '175px'); $('#TSE-T-LAURENT').attr('height', '60px');});") #TSE => LOGO => COLOR(s)/SIZE
        runjs("$(document).ready(function() {$('#udp').attr('src', 'Members/Universidad-DIEGO-Portales/Universidad-DIEGO-Portales.png'); $('#udp').attr('width', '215px'); $('#udp').attr('height', '50px');});") #Universidad DIEGO Portales => LOGO => COLOR(s)/SIZE
        runjs("$(document).ready(function() {$('#TSE-C-THOMAS').attr('src', 'Members/Toulouse-School-of-Economics/TSE_MainVersion-Small.png'); $('#TSE-C-THOMAS').attr('width', '175px'); $('#TSE-C-THOMAS').attr('height', '60px');});") #TSE => LOGO => COLOR(s)/SIZE
        runjs("$(document).ready(function() {$('#udesa').attr('src', 'Members/Universidad-de-San-Andres/Universidad-de-San-Andres-MainVersion.svg'); $('#udesa').attr('width', '190px'); $('#udesa').attr('height', '45px');});") #UdeSA => LOGO => COLOR(s)/SIZE
        runjs("$(document).ready(function() {$('#uai').attr('src', 'Members/Universidad-Adolfo-Ibanez/Universidad-Adolfo-Ibanez.svg'); $('#uai').attr('width', '190px'); $('#uai').attr('height', '40px');});") #UAI => LOGO => COLOR(s)/SIZE
        }
    })
  
  #############################################################################
  ##### World Map PROPERTIES (CSS) => Condition => THEME => TRUE OR FALSE #####
  observe({
    ##### THEME = TRUE #####
    if (isTRUE(input$THEME)) {
      runjs("$('.leaflet-bar button').css({'background-color': '#555555', 'color': 'var(--bs-body-color)'});") #Map View CONTAINER => !HOVER => COLOR(s)
      runjs("$('.leaflet-bar button').hover(function() {$(this).css('background-color', 'var(--bs-secondary)');}, function() {$(this).css('background-color', '#555555');});") #Map View CONTAINER => HOVER => COLOR(s)
      runjs("$('.leaflet-touch .leaflet-bar').attr('style', 'border: 1px solid #4D4D4D !important;');") #CONTAINER(s) => Map View | Zoom-in | Zoom-out => BORDER => COLOR
      runjs("$('.leaflet-bar a').each(function() {if ($(this).hasClass('leaflet-disabled')) {$(this).css('background-color', 'var(--bs-secondary)');} else {$(this).css('background-color', '#555555');}});") #CONTAINER(s) => Zoom-in | Zoom-out => Disabled | !Disabled => !HOVER => COLOR(s)
      runjs("$('.leaflet-bar a').css({'border-bottom': '1px solid #4D4D4D'});") #Zoom-in CONTAINER => BORDER => COLOR
      runjs("$('.leaflet-bar a:last-child').css({'border-bottom': 'none'});") #Zoom-out CONTAINER => !BORDER
      # runjs("$('.leaflet-bar a').not('.leaflet-disabled').hover(function() {$(this).css('background-color', 'var(--bs-secondary)');}, function() {$(this).css('background-color', '#555555');});") #CONTAINER(s) => Zoom-in | Zoom-out => !Disabled => HOVER => COLOR(s)
      runjs("$('.leaflet-touch .leaflet-control-zoom-in').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23E6E6E6\\' class=\\'bi bi-zoom-in\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5\\'/%3e%3c/svg%3e\")'});") #Zoom-in CONTAINER => Replace "+" => !Disabled => COLOR
      runjs("$('.leaflet-touch .leaflet-control-zoom-out').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23E6E6E6\\' class=\\'bi bi-zoom-out\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5\\'/%3e%3c/svg%3e\")'});") #Zoom-out CONTAINER => Replace "-" => !Disabled => COLOR
      runjs("$('.leaflet-bar a.leaflet-control-zoom-in.leaflet-disabled').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23666666\\' class=\\'bi bi-zoom-in\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5\\'/%3e%3c/svg%3e\")'});") #Zoom-in CONTAINER => Replace "+" => Disabled => COLOR
      runjs("$('.leaflet-bar a.leaflet-control-zoom-out.leaflet-disabled').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23666666\\' class=\\'bi bi-zoom-out\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5\\'/%3e%3c/svg%3e\")'});") #Zoom-out CONTAINER => Replace "-" => Disabled => COLOR
      runjs("CurrentCSS = $('.leaflet-control-scale-line:not(:first-child)').attr('style');
             $('.leaflet-control-scale-line').attr('style', ($('.leaflet-control-scale-line').attr('style')) + 'border: 1px solid #4D4D4D !important; border-top: none !important;'); //Scale CONTAINER => BORDER-COLOR
             $('.leaflet-control-scale-line:not(:first-child)').attr('style', CurrentCSS + 'border: 1px solid #4D4D4D !important; border-bottom: none !important;');") #Scale CONTAINER => BORDER-COLOR
      runjs("$('.leaflet-control-scale-line').css({'background': 'rgba(85, 85, 85, 0.5)', 'color': '#FCFCFC'});") #Scale CONTAINER => BackGround-COLOR | TEXT-COLOR
      runjs("$('.leaflet-control-attribution.leaflet-control').css({'background': 'rgba(85, 85, 85, 0.75)', 'color': '#FCFCFC'});") #Attribution CONTAINER => BackGround-COLOR | TEXT-COLOR
      runjs("$('.leaflet-control-attribution.leaflet-control a').css({'color': 'rgb(0, 123, 194)'});") #Attribution CONTAINER => TEXT-COLOR
      runjs("$('.info.legend.leaflet-control').css({'background': 'rgba(85, 85, 85, 0.75)', 'color': '#FCFCFC'});") #LN+ => BACKGROUND-COLOR | TEXT-COLOR
      #Card Full Screen ENTER (Expand) Appearance
      runjs("$('.bslib-full-screen-enter').css({'background-color': '#555555'});")
      ##### THEME = FALSE ##### 
      } else {
        runjs("$('.leaflet-bar button').css({'background-color': '#FFF', 'color': '#000'});") #Map View CONTAINER => !HOVER => COLOR(s)
        runjs("$('.leaflet-bar button').hover(function() {$(this).css('background-color', '#F4F4F4');}, function() {$(this).css('background-color', '#FFF');});") #Map View CONTAINER => HOVER => COLOR(s)
        runjs("$('.leaflet-touch .leaflet-bar').attr('style', 'border: 1px solid #DDD !important;');") #Map View | Zoom-in | Zoom-out => CONTAINER(s) => BORDER => COLOR
        runjs("$('.leaflet-bar a').each(function() {if ($(this).hasClass('leaflet-disabled')) {$(this).css('background-color', '#F4F4F4');} else {$(this).css('background-color', '#FFF');}});") #CONTAINER(s) => Zoom-in | Zoom-out => Disabled | !Disabled => !HOVER => COLOR(s)
        runjs("$('.leaflet-bar a').css({'border-bottom': '1px solid #CCC'});") #Zoom-in CONTAINER => BORDER => COLOR
        runjs("$('.leaflet-bar a:last-child').css({'border-bottom': 'none'});") #Zoom-out CONTAINER => !BORDER
        # runjs("$('.leaflet-bar a').not('.leaflet-disabled').hover(function() {$(this).css('background-color', '#F4F4F4');}, function() {$(this).css('background-color', '#FFF');});") #CONTAINER(s) => Zoom-in | Zoom-out => !Disabled => !HOVER => COLOR(s)
        runjs("$('.leaflet-touch .leaflet-control-zoom-in').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'currentColor\\' class=\\'bi bi-zoom-in\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5\\'/%3e%3c/svg%3e\")'});"); #Zoom-in CONTAINER => Replace "+" => !Disabled => COLOR
        runjs("$('.leaflet-touch .leaflet-control-zoom-out').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'currentColor\\' class=\\'bi bi-zoom-out\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5\\'/%3e%3c/svg%3e\")'});"); #Zoom-out CONTAINER => Replace "-" => !Disabled => COLOR
        runjs("$('.leaflet-bar a.leaflet-control-zoom-in.leaflet-disabled').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23CCCCCC\\' class=\\'bi bi-zoom-in\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 3a.5.5 0 0 1 .5.5V6h2.5a.5.5 0 0 1 0 1H7v2.5a.5.5 0 0 1-1 0V7H3.5a.5.5 0 0 1 0-1H6V3.5a.5.5 0 0 1 .5-.5\\'/%3e%3c/svg%3e\")'});"); #Zoom-in CONTAINER => Replace "+" => Disabled => COLOR
        runjs("$('.leaflet-bar a.leaflet-control-zoom-out.leaflet-disabled').css({'background-image': 'url(\"data:image/svg+xml,%3csvg xmlns=\\'http://www.w3.org/2000/svg\\' width=\\'16\\' height=\\'16\\' fill=\\'%23CCCCCC\\' class=\\'bi bi-zoom-out\\' viewBox=\\'0 0 16 16\\'%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M6.5 12a5.5 5.5 0 1 0 0-11 5.5 5.5 0 0 0 0 11M13 6.5a6.5 6.5 0 1 1-13 0 6.5 6.5 0 0 1 13 0\\'/%3e%3cpath d=\\'M10.344 11.742q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1 6.5 6.5 0 0 1-1.398 1.4z\\'/%3e%3cpath fill-rule=\\'evenodd\\' d=\\'M3 6.5a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 0 1h-6a.5.5 0 0 1-.5-.5\\'/%3e%3c/svg%3e\")'});"); #Zoom-out CONTAINER => Replace "-" => Disabled => COLOR
        runjs("CurrentCSS = $('.leaflet-control-scale-line:not(:first-child)').attr('style'); 
               $('.leaflet-control-scale-line').attr('style', ($('.leaflet-control-scale-line').attr('style')) + 'border: 1px solid #777 !important; border-top: none !important;'); //Scale CONTAINER => BORDER-COLOR
               $('.leaflet-control-scale-line:not(:first-child)').attr('style', CurrentCSS + 'border: 1px solid #777 !important; border-bottom: none !important;');") #Scale CONTAINER => BORDER-COLOR
        runjs("$('.leaflet-control-scale-line').css({'background': 'rgba(255, 255, 255, 0.5)', 'color': '#333'});") #Scale CONTAINER => BackGround-COLOR | TEXT-COLOR
        runjs("$('.leaflet-control-attribution.leaflet-control').css({'background': 'rgba(255, 255, 255, 0.75)', 'color': '#333'});") #Attribution CONTAINER => BackGround-COLOR | TEXT-COLOR
        runjs("$('.leaflet-control-attribution.leaflet-control a').css({'color': '#0078A8'});") #Attribution CONTAINER => TEXT-COLOR
        runjs("$('.info.legend.leaflet-control').css({'background': 'rgba(255, 255, 255, 0.75)', 'color': '#555555'});") #LN+ => BackGround-COLOR | TEXT-COLOR
        #Card Full Screen ENTER (Expand) Appearance
        runjs("$('.bslib-full-screen-enter').css({'background-color': 'var(--bslib-color-bg, var(--bs-card-bg, var(--bs-body-bg)))'});");
        } 
    })
  
  ######################################################################################
  ##### World Map PROPERTIES (CSS) => Condition(s) => Zoom | THEME (TRUE OR FALSE) #####
  observeEvent(input$MR_World_Map_zoom, { #Execute (JavaScript) Code When ZOOM-IN/ZOOM-OUT           
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
  
  ############################
  ##### HOME => Carousel #####
  ############################
  
  #############################################
  ##### RESEARCH-RESULT(s) => MOST-RECENT #####
  
  ##### PUBLICATION(s) #####
  runjs("//Retrieve all <div> with class='card'
         var $Cards = $(); //Initialize jQUERY-OBJECT To store card elements
         $('div[data-value]').each(function() { //Iterate over each <div> that have a 'data-value' attribute
           var DataValue = $(this).attr('data-value'); //Retrieve DataValue
           if (DataValue === 'Publications') { //Retrieve Div that have a 'data-value' attribute === PUBLICATION(s)
               var $CardsInDiv = $(this).find('.card'); //Find all child elements with class='card'
               $Cards = $Cards.add($CardsInDiv);}}); //Add CardsInDiv To $Cards
         //Create an ARRAY populated with 'data-date' and 'data-title' atttributes from each card element
         var CardsData = []; //Create an ARRAY
         $Cards.each(function() { //Iterate over each Card in Cards
          var DateValue = $(this).attr('data-date'); //Retrieve DateValue
          var TitleValue = $(this).attr('data-title'); //Retrieve TitleValue
          CardsData.push({Date: DateValue, Title: TitleValue, Element: this});}); //Add DateValue |TitleValue | Element To CardsData
         //Sort ARRAY-CardData BY DATE-TITLE
         CardsData.sort(function(a, b) {
          //Sort BY DATE in reverse-chrono-order
          var DateComparison = a.Date.localeCompare(b.Date);
          if (DateComparison !== 0) {return (-DateComparison)}
          //DATE(s) are equal Then Sort BY TITLE in reverse-chrono-order
          var TitleComparison = a.Title.localeCompare(b.Title);
          if (TitleComparison !== 0) {return (-TitleComparison)};});
         //Select the card with the most recent date
         var MostRecentCard = CardsData[0].Element;
         //Clone MostRecentCard and Replace Attribute-id()
         var ClonedCard = $(MostRecentCard).clone() //Clone MostRecentCard
         ClonedCard.attr('id', 'most-recent-publication'); //Replace Attribute-id()
         //Create a SHINY-SERVER-INPUT
         Shiny.setInputValue('most_recent_publication_card', ClonedCard[0].outerHTML);")
  ##### WORKING-PAPER(s) #####
  runjs("//Retrieve all <div> with class='card'
         var $Cards = $(); //Initialize jQUERY-OBJECT To store card elements
         $('div[data-value]').each(function() { //Iterate over each <div> that have a 'data-value' attribute
           var DataValue = $(this).attr('data-value'); //Retrieve DataValue
           if (DataValue === 'Working Papers') { //Retrieve Div that have a 'data-value' attribute === WORKING-PAPER(s)
               var $CardsInDiv = $(this).find('.card'); //Find all child elements with class='card'
               $Cards = $Cards.add($CardsInDiv);}}); //Add CardsInDiv To $Cards
         //Create an ARRAY populated with 'data-date' and 'data-title' atttributes from each card element
         var CardsData = []; //Create an ARRAY
         $Cards.each(function() { //Iterate over each Card in Cards
          var DateValue = $(this).attr('data-date'); //Retrieve DateValue
          var TitleValue = $(this).attr('data-title'); //Retrieve TitleValue
          CardsData.push({Date: DateValue, Title: TitleValue, Element: this});}); //Add DateValue |TitleValue | Element To CardsData
         //Sort ARRAY-CardData BY DATE-TITLE
         CardsData.sort(function(a, b) {
          //Sort BY DATE in reverse-chrono-order
          var DateComparison = a.Date.localeCompare(b.Date);
          if (DateComparison !== 0) {return (-DateComparison)}
          //DATE(s) are equal Then Sort BY TITLE in reverse-chrono-order
          var TitleComparison = a.Title.localeCompare(b.Title);
          if (TitleComparison !== 0) {return (-TitleComparison)};});
         //Select the card with the most recent date
         var MostRecentCard = CardsData[0].Element;
         //Clone MostRecentCard and Replace Attribute-id()
         var ClonedCard = $(MostRecentCard).clone() //Clone MostRecentCard
         ClonedCard.attr('id', 'most-recent-wpaper'); //Replace Attribute-id()
         //Create a SHINY-SERVER-INPUT
         Shiny.setInputValue('most_recent_wpaper_card', ClonedCard[0].outerHTML);")
  ##### PRESENTATION(s) #####
  runjs("//Retrieve all <div> with class='card'
         var $Cards = $(); //Initialize jQUERY-OBJECT To store card elements
         $('div[data-value]').each(function() { //Iterate over each <div> that have a 'data-value' attribute
           var DataValue = $(this).attr('data-value'); //Retrieve DataValue
           if (DataValue === 'Presentations') { //Retrieve Div that have a 'data-value' attribute === PRESENTATION(s)
               var $CardsInDiv = $(this).find('.card'); //Find all child elements with class='card'
               $Cards = $Cards.add($CardsInDiv);}}); //Add CardsInDiv To $Cards
         //Create an ARRAY populated with 'data-date' and 'data-title' atttributes from each card element
         var CardsData = []; //Create an ARRAY
         $Cards.each(function() { //Iterate over each Card in Cards
          var DateValue = $(this).attr('data-date'); //Retrieve DateValue
          var TitleValue = $(this).attr('data-title'); //Retrieve TitleValue
          CardsData.push({Date: DateValue, Title: TitleValue, Element: this});}); //Add DateValue |TitleValue | Element To CardsData
         //Sort ARRAY-CardData BY DATE-TITLE
         CardsData.sort(function(a, b) {
          //Sort BY DATE in reverse-chrono-order
          var DateComparison = a.Date.localeCompare(b.Date);
          if (DateComparison !== 0) {return (-DateComparison)}
          //DATE(s) are equal Then Sort BY TITLE in reverse-chrono-order
          var TitleComparison = a.Title.localeCompare(b.Title);
          if (TitleComparison !== 0) {return (-TitleComparison)};});
         //Select the card with the most recent date
         var MostRecentCard = CardsData[0].Element;
         //Clone MostRecentCard and Replace Attribute-id()
         var ClonedCard = $(MostRecentCard).clone() //Clone MostRecentCard
         ClonedCard.attr('id', 'most-recent-presentation'); //Replace Attribute-id()
         //Create a SHINY-SERVER-INPUT
         Shiny.setInputValue('most_recent_presentation_card', ClonedCard[0].outerHTML);")

  ####################
  ##### Carousel #####
  
  ##### Run R-Code when an element is clicked [onclick()] #####
  shinyjs::onclick(
    id = "home-dea-capture", #Element-id() => Capture => Data Exploration Application
    expr = { #R Expression To Run
      runjs("
        //Retrieve all <li> with class='nav-item'
        var $NavItems = $(); //Initialize jQUERY-OBJECT To store nav-item elements
        $('li[class]').each(function() { //Iterate over each <li> that have a 'class' attribute
          var Class = $(this).attr('class'); //Retrieve Class
          if (Class === 'nav-item') { //Retrieve li that have a 'class' attribute === nav-item
              var $NavLinksInLi = $(this).find('.nav-link'); //Find all child elements with class='nav-link'
              $NavItems = $NavItems.add($NavLinksInLi);}}); //Add NavLinksInLi To $NavItems
        //Retrieve all <li> with class='dropdown nav-item'
        var $DropdownNavItems = $(); //Initialize jQUERY-OBJECT To store 'dropdown nav-item' elements
        var $DropdownItems = $(); //Initialize jQUERY-OBJECT To store 'dropdown-item' elements
        $('li[class]').each(function() { //Iterate over each <li> that have a 'class' attribute
          var Class = $(this).attr('class'); //Retrieve Class
          if (Class === 'dropdown nav-item') { //Retrieve li that have a 'class' attribute === 'dropdown nav-item'
              var $DropdownNavLinksInLi = $(this).find('.dropdown-toggle.nav-link'); //Find all child elements with class='dropdown-toggle nav-link'
              $DropdownNavItems = $DropdownNavItems.add($DropdownNavLinksInLi); //Add DropdownNavLinksInLi To $DropdownNavItems
              var $DropdownMenu = $(this).find('ul.dropdown-menu'); //Find all <ul> elements with class 'dropdown-menu'
              var $DropdownItemsInLi = $DropdownMenu.find('a.dropdown-item'); //Find all <a> elements with class 'dropdown-item' within $DropdownMenu
              $DropdownItems = $DropdownItems.add($DropdownItemsInLi);}}); //Add DropdownItemsInLi to $DropdownItems
        //Retrieve HOME NavItem
        var CurrentNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Home';});
        //Retrieve DEA_Menu DropdownNavItem
        var DropdownNavItem = $DropdownNavItems.filter(function() {return $(this).attr('data-value') === 'DEA_Menu';});
        //Desactivate CurrentNavItem
        if (CurrentNavItem.hasClass('active')) {CurrentNavItem.removeClass('active');}
        //Activate DropdownNavItem
        if (!DropdownNavItem.hasClass('active')) {DropdownNavItem.addClass('active');}
        //NestedDropdownItems
        var MIGRANT_STOCK_DropdownItem = $DropdownItems.filter(function() {return $(this).attr('data-value') === 'Migrant Stock';});
        var Climate_DropdownItem = $DropdownItems.filter(function() {return $(this).attr('data-value') === 'Climate';});
        var MIGRATION_and_Climate_DropdownItem = $DropdownItems.filter(function() {return $(this).attr('data-value') === 'Migration and Climate';});
        //Activate/Desactivate DropdownItems
        if (!MIGRANT_STOCK_DropdownItem.hasClass('active')) {MIGRANT_STOCK_DropdownItem.addClass('active');}
        if (Climate_DropdownItem.hasClass('active')) {Climate_DropdownItem.removeClass('active');}
        if (MIGRATION_and_Climate_DropdownItem.hasClass('active')) {MIGRATION_and_Climate_DropdownItem.removeClass('active');}
        //Retrieve CurrentTabPane
        var CurrentTabPane = $('#home');
        //Retrieve all <div> with class='tab-pane'
        var $TabPanes = $('div.tab-pane');
        //Retrieve TabPane
        var TabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Migrant Stock';});
        //Desactivate CurrentTabPane
        if (CurrentTabPane.hasClass('active')) {CurrentTabPane.removeClass('active');}
        //Activate TabPane
        if (!TabPane.hasClass('active')) {TabPane.addClass('active');}
        //NestedNavItems
        var ResidenceNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Residence';}).first();
        var BirthNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Birth';}).first();
        //Activate/Desactivate NavItems
        if (!ResidenceNavItem.hasClass('active')) {ResidenceNavItem.addClass('active');}
        if (BirthNavItem.hasClass('active')) {BirthNavItem.removeClass('active');}
        //NestedTabPanes
        var ResidenceTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Residence';}).first();
        var BirthTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Birth';}).first();
        //Activate/Desactivate TabPanes
        if (!ResidenceTabPane.hasClass('active')) {ResidenceTabPane.addClass('active');}
        if (BirthTabPane.hasClass('active')) {BirthTabPane.removeClass('active');}")})
  
  ##### SlickR #####
  output$Carousel_Home <- renderSlickR({ #RENDER => Carousel
    ##### SlickList => DOM-Element(s) #####
    SlickList <- slick_list(
      ##### Div => Data Exploration Application #####
      tags$div(style = "margin-top: 10px;", #MARGIN (Top: 10px)
        #TEXT => COLOR | Bold
        tags$strong(style = "color: #333537 !important; text-decoration: underline !important;", "Explore Our Data"),
        #DEA => Capture
        tags$img(
          style = "margin-top: 10px;", #MARGIN (Top: 10px)
          src = "Home/DEA-Capture-75.png", #PATH OR LINK To CAPTURE
          width = "960px", height = "432.5px", #CAPTURE => WITDH/HEIGHT
          class = "HOME-DEA-CAPTURE", #Clickable CAPTURE Class
          id = "home-dea-capture",  #Allow To Switch CAPTURE (!Dark/Dark Mode) in SERVER.R
          title = "Access Data Exploration Application")), #TEXT SHOW on HOVER
      ##### Div => Most Recent Publication #####
      tags$div(
        #Card => !Full_Width
        style = "width: 50%; min-width: fit-content;",
        #TEXT => COLOR | Bold
        tags$strong(style = "color: #333537; text-decoration: underline !important;", "Most Recent Publication"),
        #Most Recent Publication Card
        HTML(input$most_recent_publication_card),
        #Redirecton Button To Publication(s) NavPanel()
        tags$button(
          type = "button", #Button Element
          style = "margin-top: 0px; margin-bottom: 10px; /*MARGIN(s) => TOP To 0px | BOTTOM To 10px*/
                   padding: 10px 20px; /*PADDING => Top/Bottom => 10px | LEFT/RIGHT => 20px*/
                   border-width: 1px; border-radius: 5px; /*BORDER-WIDTH-and-RADIUS To 1px/5px*/
                   background-color: transparent; /*BACKGROUND To Transparent*/
                   color: #333537; /*TEXT-COLOR*/
                   border: 1px solid #B9BABA;", #BORDER-WIDTH-and-COLOR
          class = "redirection-button", #Button Class
          id = "publications-redirection-button", #Button IDENTIFIER
          onclick = "
            //Retrieve all <li> with class='nav-item'
            var $NavItems = $(); //Initialize jQUERY-OBJECT To store nav-item elements
            $('li[class]').each(function() { //Iterate over each <li> that have a 'class' attribute
              var Class = $(this).attr('class'); //Retrieve Class
              if (Class === 'nav-item') { //Retrieve li that have a 'class' attribute === nav-item
                  var $NavLinksInLi = $(this).find('.nav-link'); //Find all child elements with class='nav-link'
                  $NavItems = $NavItems.add($NavLinksInLi);}}); //Add NavLinksInLi To $NavItems
            //Retrieve HOME NavItem
            var CurrentNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Home';});
            //Retrieve RESEARCH-RESULT(s) NavItem
            //var NavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Research Results';});
            //Retrieve PAPER(s) NavItem
            var NavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Papers';});
            //Desactivate CurrentNavItem
            if (CurrentNavItem.hasClass('active')) {CurrentNavItem.removeClass('active');}
            //Activate NavItem
            if (!NavItem.hasClass('active')) {NavItem.addClass('active');}
            //Retrieve CurrentTabPane
            var CurrentTabPane = $('#home');
            //Retrieve TabPane
            //var TabPane = $('#research-results');
            var TabPane = $('#papers');
            //Desactivate CurrentTabPane
            if (CurrentTabPane.hasClass('active')) {CurrentTabPane.removeClass('active');}
            //Activate TabPane
            if (!TabPane.hasClass('active')) {TabPane.addClass('active');}
            //NestedNavItems
            var PublicationsNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Publications';});
            var WoPapersNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Working Papers';});
            //var PresentationsNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Presentations';});
            //Activate/Desactivate NavItems
            if (!PublicationsNavItem.hasClass('active')) {PublicationsNavItem.addClass('active');}
            if (WoPapersNavItem.hasClass('active')) {WoPapersNavItem.removeClass('active');}
            //if (PresentationsNavItem.hasClass('active')) {PresentationsNavItem.removeClass('active');}
            //Retrieve all <div> with class='tab-pane'
            var $TabPanes = $('div.tab-pane');
            //NestedTabPanes
            var PublicationTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Publications';});
            var WoPapersTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Working Papers';});
            //var PresentationsTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Presentations';});
            //Activate/Desactivate TabPanes
            if (!PublicationTabPane.hasClass('active')) {PublicationTabPane.addClass('active');}
            if (WoPapersTabPane.hasClass('active')) {WoPapersTabPane.removeClass('active');}
            //if (PresentationsTabPane.hasClass('active')) {PresentationsTabPane.removeClass('active');}",
          "View All Publications")), #TEXT
      ##### Div => Most Recent WoPaper #####
      tags$div(
        #Card => !Full_Width
        style = "width: 50%; min-width: fit-content;",
        #TEXT => COLOR | Bold
        tags$strong(style = "color: #333537; text-decoration: underline !important;", "Most Recent Working Paper"), 
        #Most Recent WoPaper Card
        HTML(input$most_recent_wpaper_card), 
        #Redirecton Button To WoPaper(s) NavPanel()
        tags$button(
          type = "button", #Button Element
          style = "margin-top: 0px; margin-bottom: 10px; /*MARGIN(s) => TOP To 0px | BOTTOM To 10px*/
                   padding: 10px 20px; /*PADDING => Top/Bottom => 10px | LEFT/RIGHT => 20px*/
                   border-width: 1px; border-radius: 5px; /*BORDER-WIDTH-and-RADIUS To 1px/5px*/
                   background-color: transparent; /*BACKGROUND To Transparent*/
                   color: #333537; /*TEXT-COLOR*/
                   border: 1px solid #B9BABA;", #BORDER-WIDTH-and-COLOR
          class = "redirection-button", #Button Class
          id = "wpapers-redirection-button", #Button IDENTIFIER
          onclick = "
            //Retrieve all <li> with class='nav-item'
            var $NavItems = $(); //Initialize jQUERY-OBJECT To store nav-item elements
            $('li[class]').each(function() { //Iterate over each <li> that have a 'class' attribute
              var Class = $(this).attr('class'); //Retrieve Class
              if (Class === 'nav-item') { //Retrieve li that have a 'class' attribute === nav-item
                  var $NavLinksInLi = $(this).find('.nav-link'); //Find all child elements with class='nav-link'
                  $NavItems = $NavItems.add($NavLinksInLi);}}); //Add NavLinksInLi To $NavItems
            //Retrieve HOME NavItem
            var CurrentNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Home';});
            //Retrieve RESEARCH-RESULT(s) NavItem
            //var NavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Research Results';});
            //Retrieve PAPER(s) NavItem
            var NavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Papers';});
            //Desactivate CurrentNavItem
            if (CurrentNavItem.hasClass('active')) {CurrentNavItem.removeClass('active');}
            //Activate NavItem
            if (!NavItem.hasClass('active')) {NavItem.addClass('active');}
            //Retrieve CurrentTabPane
            var CurrentTabPane = $('#home');
            //Retrieve TabPane
            //var TabPane = $('#research-results');
            var TabPane = $('#papers');
            //Desactivate CurrentTabPane
            if (CurrentTabPane.hasClass('active')) {CurrentTabPane.removeClass('active');}
            //Activate TabPane
            if (!TabPane.hasClass('active')) {TabPane.addClass('active');}
            //NestedNavItems
            var PublicationsNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Publications';});
            var WoPapersNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Working Papers';});
            //var PresentationsNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Presentations';});
            //Activate/Desactivate NavItems
            if (PublicationsNavItem.hasClass('active')) {PublicationsNavItem.removeClass('active');}
            if (!WoPapersNavItem.hasClass('active')) {WoPapersNavItem.addClass('active');}
            //if (PresentationsNavItem.hasClass('active')) {PresentationsNavItem.removeClass('active');}
            //Retrieve all <div> with class='tab-pane'
            var $TabPanes = $('div.tab-pane');
            //NestedTabPanes
            var PublicationTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Publications';});
            var WoPapersTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Working Papers';});
            //var PresentationsTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Presentations';});
            //Activate/Desactivate TabPanes
            if (PublicationTabPane.hasClass('active')) {PublicationTabPane.removeClass('active');}
            if (!WoPapersTabPane.hasClass('active')) {WoPapersTabPane.addClass('active');}
            //if (PresentationsTabPane.hasClass('active')) {PresentationsTabPane.removeClass('active');}",
          "View All Working Papers")), #TEXT
      ##### Div => Most Recent Presentation #####
      tags$div(
        #Card => !Full_Width
        style = "width: 50%; min-width: fit-content;",
        #TEXT => COLOR | Bold
        tags$strong(style = "color: #333537; text-decoration: underline !important;", "Most Recent Presentation"),
        #Most Recent Presentation Card
        HTML(input$most_recent_presentation_card), 
        #Redirecton Button To Presentation(s) NavPanel()
        tags$button(
          type = "button", #Button Element
          style = "margin-top: 0px; margin-bottom: 10px; /*MARGIN(s) => TOP To 0px | BOTTOM To 10px*/
                   padding: 10px 20px; /*PADDING => Top/Bottom => 10px | LEFT/RIGHT => 20px*/
                   border-width: 1px; border-radius: 5px; /*BORDER-WIDTH-and-RADIUS To 1px/5px*/
                   background-color: transparent; /*BACKGROUND To Transparent*/
                   color: #333537; /*TEXT-COLOR*/
                   border: 1px solid #B9BABA;", #BORDER-WIDTH-and-COLOR
          class = "redirection-button", #Button Class
          id = "presentations-redirection-button", #Button IDENTIFIER
          onclick = "
            //Retrieve all <li> with class='nav-item'
            var $NavItems = $(); //Initialize jQUERY-OBJECT To store nav-item elements
            $('li[class]').each(function() { //Iterate over each <li> that have a 'class' attribute
              var Class = $(this).attr('class'); //Retrieve Class
              if (Class === 'nav-item') { //Retrieve li that have a 'class' attribute === nav-item
                  var $NavLinksInLi = $(this).find('.nav-link'); //Find all child elements with class='nav-link'
                  $NavItems = $NavItems.add($NavLinksInLi);}}); //Add NavLinksInLi To $NavItems
            //Retrieve HOME NavItem
            var CurrentNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Home';});
            //Retrieve RESEARCH-RESULT(s) NavItem
            //var NavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Research Results';});
            //Retrieve PRESENTATION(s) NavItem
            var NavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Presentations';});
            //Desactivate CurrentNavItem
            if (CurrentNavItem.hasClass('active')) {CurrentNavItem.removeClass('active');}
            //Activate NavItem
            if (!NavItem.hasClass('active')) {NavItem.addClass('active');}
            //Retrieve CurrentTabPane
            var CurrentTabPane = $('#home');
            //Retrieve TabPane
            //var TabPane = $('#research-results');
            var TabPane = $('#presentations');
            //Desactivate CurrentTabPane
            if (CurrentTabPane.hasClass('active')) {CurrentTabPane.removeClass('active');}
            //Activate TabPane
            if (!TabPane.hasClass('active')) {TabPane.addClass('active');}
            //NestedNavItems
            //var PublicationsNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Publications';});
            //var WoPapersNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Working Papers';});
            //var PresentationsNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Presentations';});
            //Activate/Desactivate NavItems
            //if (PublicationsNavItem.hasClass('active')) {PublicationsNavItem.removeClass('active');}
            //if (WoPapersNavItem.hasClass('active')) {WoPapersNavItem.removeClass('active');}
            //if (!PresentationsNavItem.hasClass('active')) {PresentationsNavItem.addClass('active');}
            //Retrieve all <div> with class='tab-pane'
            //var $TabPanes = $('div.tab-pane');
            //NestedTabPanes
            //var PublicationTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Publications';});
            //var WoPapersTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Working Papers';});
            //var PresentationsTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Presentations';});
            //Activate/Desactivate TabPanes
            //if (PublicationTabPane.hasClass('active')) {PublicationTabPane.removeClass('active');}
            //if (WoPapersTabPane.hasClass('active')) {WoPapersTabPane.removeClass('active');}
            //if (!PresentationsTabPane.hasClass('active')) {PresentationsTabPane.addClass('active');}",
          "View All Presentations"))) #TEXT
    ##### SlickList in SlickR() #####
    slickR(obj = SlickList, slideId = "carousel-home") + #VECTOR (Element[s]) + IDENTIFIER
      settings( #SlickR() => PARAMETER(s)
        accessibility = TRUE, #ENABLE => TABBING/ARROW-KEY NAVIGATION
        adaptiveHeight = FALSE, #DISABLE => Adaptative HEIGHT For SINGLE Slide Horizontal Carousels
        autoplay = TRUE, autoplaySpeed = 4500, #ENABLE => AUTOPLAY + AUTOPLAY-Speed in milliseconds 
        arrows = TRUE, #ENABLE => Prev/Next Arrows
        centerMode = FALSE, #DISABLE => Centered View with Partial Prev/Next Slides
        dots = FALSE, #DISABLE => Dot-indicators
        draggable = TRUE, #ENABLE => Mouse DRAGGING
        fade = FALSE, #DISABLE => Fade | ENABLE => Slide
        focusOnSelect = FALSE, #DISABLE => Focus on Selected Element (Click)
        infinite = TRUE, #ENABLE => Infinite Loop
        initialSlide = 0, #First Slide
        pauseOnFocus = FALSE, #DISABLE => Pause AUTOPLAY On Focus
        pauseOnHover = TRUE, #ENABLE => Pause AUTOPLAY On Hover
        rows = 1, slidesPerRow = 1, #One-Row | One-SlidePerRow
        slidesToShow = 1, slidesToScroll = 1, #One-SlideToShow and One-SlideToScroll
        speed = 500, #Slide/Fade Animation Speed in milliseconds
        swipe = TRUE, touchMove = TRUE, touchThreshold = 5, #ENABLE => SWIPING | Slide Motion with Touch | For Next Slide => Must Swipe Slider-Width (1/touchThreshold)
        useCSS = TRUE, useTransform = TRUE, #ENABLE => CSS Transitions/Transforms
        variableWidth = FALSE, #DISABLE => Variable Width Slides
        vertical = FALSE, verticalSwiping = FALSE, rtl = FALSE) #DISABLE => Vertical Slide Mode | Vertical Swipe Mode | Slide-Direction To Left-RIGHT
    })
  
  #####################
  ##### WAITER(s) #####
  #####################
  
  ########################################
  ##### DATA EXPLORATION APPLICATION #####
  
  ##### CREATE WAITER #####
  DEA_WAITER <- Waiter$new(
    html = tagList(
      div(spin_orbiter(), style = "margin-bottom: 10px;"), #SPINNER
      div(span("Initialize Data Exploration Application"))), #MESSAGE+
    color = "#1D1F21", #BackGround Color
    fadeout = 500) #Fade out effect when screen is removed (Boolean OR Numeric)
  
  ##### WAITER MESSAGE(s) #####
  MESSAGEs <- c("Load International Migrant Stock Data", "Load Geospatial Data")
  
  ##### DROPDOWN-ITEM(s) #####
  observeEvent(input$DropdownI_Click_COUNTER, { #WHEN DROPDOWN-ITEM(s) are clicked
    
    #DROPDOWN-ITEM(s) Click COUNTER == 1
    if (input$DropdownI_Click_COUNTER == 1) {
      
      #CURSOR Class To WAIT
      session$sendCustomMessage(type = "CURSORwithinBODY", message = "wait-cursor")
      
      ##### SHOW WAITER #####
      DEA_WAITER$show()
      
      #!Execution in R For X SECONDs
      Sys.sleep(1.25)
      
      ##### MESSAGE+ UPDATE(s) #####
      for(MESSAGE in 1:length(MESSAGEs)){
        DEA_WAITER$update(html = tagList(
          div(spin_orbiter(), style = "margin-bottom: 10px;"), #SPINNER
          div(span(MESSAGEs[MESSAGE])))) #TEXT+
        #!Execution in R For X SECONDs
        Sys.sleep(1.25)}
      
      ##### HIDE WAITER #####
      DEA_WAITER$hide()
      
      #CURSOR Class To Default
      session$sendCustomMessage(type = "CURSORwithinBODY", message = "default-cursor")
      
      }
    
    })
  
  ###############################
  ##### World Map WAITER(s) #####
  
  ##### CREATE WAITER #####
  World_Map_WAITER <- Waiter$new(
    # id =  "MR_World_Map", #OVERLAY WAITER on MR_World_Map
    html = tagList(
      div(spin_orbiter(), style = "margin-bottom: 10px;"), #SPINNER
      div(span("Load World Map Tiles"))), #MESSAGE+
    color = "#1D1F21", #BackGround Color
    fadeout = 500) #Fade out effect when screen is removed (Boolean OR Numeric)
  
  ##### WAITER MESSAGE(s) #####
  World_Map_WAITER_MESSAGEs <- c("Load World Map Tiles", "Draw World Administrative Boundaries")
  
  #World Map WAITER => POSITION
  observe({runjs("function WorldMapWAITER_Position() { //Create WorldMapWAITER_Position() Function
                    var Map = document.getElementById('MR_World_Map'); //Retrieve MR_World_Map
                    var WAITER = document.querySelector('.waiter-overlay'); //Retrieve World_Map_WAITER
                    if (Map && WAITER) { //Execute Code if Map/WAITER are retrieved with success
                        var Map_Position = Map.getBoundingClientRect(); //Map POSITION
                        WAITER.style.width = Map_Position.width + 'px'; //WAITER => WIDTH = Map-WIDTH
                        WAITER.style.height = Map_Position.height + 'px'; //WAITER => HEIGHT = Map-HEIGHT
                        WAITER.style.top = Map_Position.top + window.scrollY + 'px'; //WAITER => Top = Map-Top
                        WAITER.style.left = Map_Position.left + window.scrollX + 'px'; //WAITER => LEFT = Map-LEFT
                        }
                    }
                  //Call WorldMapWAITER_Position() Function
                  WorldMapWAITER_Position();
                  //Add EVENT-LISTENER To UPDATE WAITER Position when window is resized
                  window.addEventListener('resize', function() {WorldMapWAITER_Position();});")})
                  # //Add EVENT-LISTENER To UPDATE WAITER Position when the PAGE is scrolled
                  # window.addEventListener('scroll', function() {WorldMapWAITER_Position();});")})
                  # //Add EVENT-LISTENER To UPDATE WAITER Position when the PAGE is scrolled
                  # window.addEventListener('scrollend', function() {WorldMapWAITER_Position();});")})
  
  ##### CREATE WAITER #####
  REGIONs_World_Map_WAITER <- Waiter$new(
    id =  "MR_World_Map", #OVERLAY WAITER on MR_World_Map
    html = tagList(
      div(spin_orbiter(), style = "margin-bottom: 10px;"), #SPINNER
      div(span("Draw World Administrative Boundaries"), style = "font-size: 14px; font-family: DM Sans")), #MESSAGE+ | FONT-SIZE | FONT-FAMILY
    color = "#1D1F21", #BackGround Color
    fadeout = 500) #Fade out effect when screen is removed (Boolean OR Numeric)
  
  ################################
  ##### Reactive Values (RV) #####
  ################################
  
  ##### World Map Card RV #####
  World_Map_Card_RV <- reactiveValues(
    TilesLoaded = FALSE, #This value is intended to indicate whether World Map Tiles have been loaded or not
    MR_REGIONs_Click_COUNTER = 0, #Track clicks on REGIONs ACTION BUTTON
    WM_Card_PROPERTY = FALSE) #New PROPERTY (CSS) on World Map Card => WM_Card_PROPERTY = TRUE
  
  ##### Selected Residence RV #####
  Selected_Residence_RV <- reactiveValues(MR_Residence_View_Choice = "") #This value indicate which action button is active (COUNTRIEs OR REGIONs)
  
  ##### Filtered MIGRANT STOCK(s) DATA RV #####
  Filtered_MIGRANT_STOCKs_DATA_RV <- reactiveValues(
    Selected_Residence_Code_in_MIGRANT_STOCK_DATA = FALSE, #This value indicate whether selected residence code is in iMIGRANT_STOCK_DATA_SHAREs or not
    Filtered_MIGRANT_STOCKs_DATA = data.frame()) #Store Filtered MIGRANT STOCK(s) DATA (Residence)
  
  #########################
  ##### MIGRANT STOCK #####
  #########################
  
  #####################
  ##### RESIDENCE #####
  
  ##### COUNTRIEs ACTION BUTTON #####
  observeEvent(input$MR_COUNTRIEs, { #WHEN COUNTRIEs ACTION BUTTON is clicked
    
    #Add CSS class from COUNTRIEs ACTION BUTTON
    shinyjs::addClass(id = "MR_COUNTRIEs", class = "active")
    #Remove CSS class from REGIONs ACTION BUTTON
    shinyjs::removeClass(id = "MR_REGIONs", class = "active")
    #Make REGIONs CHOICE RADIO BUTTONs invisible
    shinyjs::hide(id = "MRR_REGIONs_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    #Make Selected Residence Card invisible
    shinyjs::hide(id = "MR_Selected_Residence_Card", anim = TRUE, animType = "slide", time = 0.25)
    #Make Top Five BIRTH COUNTRIE(s) CHECKBOX invisible
    shinyjs::hide(id = "MR_Top_Five_BIRTH_Card", anim = TRUE, animType = "slide", time = 0.25)
    #Activate Top Five BIRTH COUNTRIE(s) CHECKBOX
    shinyjs::enable(id = "MR_Top_Five_BIRTH")
    
    #Make ... Card invisible
    shinyjs::hide(id = "MR_MIGRANT_STOCK_DATA_Card", anim = TRUE, animType = "slide", time = 0.25)
    
    #Disable ...
    # shinyjs::disable(id = "MR_BIRTH_View_Choice_REGIONs_CHOICE")
    
    #Top Five BIRTH COUNTRIE(s) CHECKBOX => Replace CHECKBOX Label Value => COUNTRIEs
    updateCheckboxInput(
      session = session, inputId = "MR_Top_Five_BIRTH",
      label = "View Top 5 Birth Countries on World Map (5-Year Period, 1990-2020)",
      value = FALSE) #Replace CHECKBOX Value => FALSE
    
    #CURSOR Class To WAIT
    session$sendCustomMessage(type = "CURSORwithinBODY", message = "wait-cursor")
    
    #Reactive Value To COUNTRIEs => COUNTRIEs Action Button have been clicked
    Selected_Residence_RV$MR_Residence_View_Choice <- "COUNTRIEs"
    
    #Reinitialize Reactive Value To FALSE
    Filtered_MIGRANT_STOCKs_DATA_RV$Selected_Residence_Code_in_MIGRANT_STOCK_DATA <- FALSE
    
    #Reinitialize Reactive Value => DATA.FRAME => No Columns | No Rows
    Filtered_MIGRANT_STOCKs_DATA_RV$Filtered_MIGRANT_STOCKs_DATA <- data.frame()
    
    ##### World Map Tiles #####
    if (!World_Map_Card_RV$TilesLoaded) { #Execute Code To load Tiles ONLY if reactive value is FALSE
      
      #Make World Map Card visible
      shinyjs::show(id = "MR_World_Map_Card", anim = TRUE, animType = "fade", time = 0.25)
      
      ##### SHOW WAITER #####
      World_Map_WAITER$show()
      
      #World Map
      output$MR_World_Map <- renderLeaflet({
        leaflet() %>%
        # leaflet(options = leafletOptions( #Options for Map Creation
        #   minZoom = 1, maxZoom = 10, #Min/Max Zoom Levels
        #   crs = leafletCRS( #Create Custom COORDINATE REFERENCE SYSTEM (CRS)
        #     crsClass = "L.Proj.CRS",
        #     # code = "ESRI:54042", #CODE => WORLD WINKEL-TRIPEL (WINKEL III) - (1921) [PROJ4] | !Supported BY leaflet
        #     # code = "ESRI:54012", #CODE => WORLD ECKERT IV (1906) [PROJ4] | !Supported BY leaflet
        #     code = "ESRI:54030", #CODE => WORLD ROBINSON (1963) [PROJ4]
        #     # proj4def = "+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", #WORLD WINKEL-TRIPEL (WINKEL III) - (1921) [PROJ4] | !Supported BY leaflet
        #     # proj4def = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", #WORLD ECKERT IV (1906) [PROJ4] | !Supported BY leaflet
        #     proj4def = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", #WORLD ROBINSON (1963) [PROJ4]
        #     # resolutions = 32755140.66 / (256 * 2^(0:18))))) %>% #WORLD WINKEL-TRIPEL (WINKEL III) => Resolution = World width in pixels / Tile size in pixels * 2^(Zoom_Level)
        #     # resolutions = 32755140.66 / (256 * 2^(1:10))))) %>% #WORLD WINKEL-TRIPEL (WINKEL III) => Resolution = World width in pixels / Tile size in pixels * 2^(Zoom_Level)
        #     # resolutions = 33842405.84 / (256 * 2^(0:18))))) %>% #WORLD ECKERT IV (1906) => Resolution = World width in pixels / Tile size in pixels * 2^(Zoom_Level)
        #     # resolutions = 33842405.84 / (256 * 2^(1:10))))) %>% #WORLD ECKERT IV (1906) => Resolution = World width in pixels / Tile size in pixels * 2^(Zoom_Level)
        #     # resolutions = 34011666.66 / (256 * 2^(0:18))))) %>% #WORLD ROBINSON (1963) => Resolution = World width in pixels / Tile size in pixels * 2^(Zoom_Level)
        #     resolutions = 34011666.66 / (256 * 2^(1:10))))) %>% #WORLD ROBINSON (1963) => Resolution = World width in pixels / Tile size in pixels * 2^(Zoom_Level)
          #Add Tile LAYER
          # addTiles(
          #   attribution = HTML("© <a href='https://www.openstreetmap.org/copyright/' target='_blank'> OpenStreetMap </a>"), #Tile LAYER Attribution
          #   options = tileOptions(minZoom = 1, maxZoom = 10, maxNativeZoom = 18)) %>% #Min/Max Zoom Levels
          #Add Tile LAYER in WinkelTripel | Eckert IV | Robinson
          # addTiles(
          #   urlTemplate = "https://yourtilesource.com/{z}/{x}/{y}.png", #Tiles Link
          #   attribution = HTML("© <a href='...' target='_blank'> ... </a>"), #Tile LAYER Attribution
          #   options = tileOptions(minZoom = 1, maxZoom = 10, maxNativeZoom = 18)) %>% #Min/Max Zoom Levels
          #Add Tile LAYER From PROVIDER
          addProviderTiles(
            # provider = providers$CartoDB, #PROVIDER-NAME
            # provider = providers$CartoDB.Positron, #PROVIDER-NAME
            # provider = providers$CartoDB.PositronNoLabels, #PROVIDER-NAME
            # provider = providers$CartoDB.DarkMatter, #PROVIDER-NAME
            # provider = providers$CartoDB.DarkMatterNoLabels, #PROVIDER-NAME
            # provider = providers$CartoDB.Voyager, #PROVIDER-NAME
            provider = providers$CartoDB.VoyagerNoLabels, #PROVIDER-NAME
            # provider = providers$CartoDB.VoyagerLabelsUnder, #PROVIDER-NAME
            options = providerTileOptions(
              #Tile LAYER Attribution(s)
              attribution = HTML("© <a href='https://www.openstreetmap.org/copyright/' target='_blank'> OpenStreetMap </a> |
                                  © <a href='https://carto.com/attributions/' target='_blank'> CARTO </a>"),
              minZoom = 1, maxZoom = 10, maxNativeZoom = 20)) %>% #Min/Max Zoom Levels
          #Graticule => !LINE(s) | OCEAN(s)/SEA(s)
          # addGraticule(interval = 0, sphere = TRUE, style = list(color = "#33CCFF", weight = 0, opacity = 0)) %>%
          #Graticule => LINE(s) => 15°
          # addGraticule(interval = 15, sphere = FALSE, style = list(color = "#333333", weight = 0.5, opacity = 0.5)) %>%
          #Graticule => LINE(s) => 30°
          # addGraticule(interval = 30, sphere = FALSE, style = list(color = "#333333", weight = 0.5, opacity = 0.5)) %>%
          #Graticule => LINE(s) => 90°
          # addGraticule(interval = 90, sphere = FALSE, style = list(color = "#333333", weight = 0.5, opacity = 0.5)) %>%
          #Graticule => LINE(s) => 180°
          # addGraticule(interval = 180, sphere = FALSE, style = list(color = "#333333", weight = 0.5, opacity = 0.5)) %>%
          #Graticule => LINE(s) => EQUATOR | PRIME MERIDIAN
          # addGraticule(interval = c(0, 0), sphere = FALSE, style = list(color = "#333333", weight = 0.5, opacity = 0.5)) %>%
          #Make Own Tile LAYER in Robinson FROM POLYGON(s) => COUNTRIE(s)
          # addPolygons(data = GeoDATA, #Add POLYGONs To Map | Keep GeoDATA in WORLD GEODETIC SYSTEM (1984) [PROJ4]
          #             stroke = TRUE, color = "#333333", weight = 0.75, opacity = 0.75, #Stroke Attributes
          #             fill = TRUE, fillColor = "#FAF8F6", fillOpacity = 1, smoothFactor = 1) %>% #Fill Attributes | POLYLINE Simplification
          #Make Own Tile LAYER in Robinson FROM POLYGON(s) => Claimed Areas => Stroke PATTERN+
          # addPolygons(data = GeoDATA %>% filter(is.na(SOVRN) & NAME != "ANTARCTICA"), #Add POLYGONs To Map | Keep GeoDATA in WORLD GEODETIC SYSTEM (1984) [PROJ4]
          #             stroke = TRUE, color = "#FAF8F6", weight = 0.75, opacity = 1, #Stroke Attributes
          #             fill = FALSE , dashArray = "5, 5", smoothFactor = 1) %>% #Fill Attributes | Stroke PATTERN | POLYLINE Simplification
          #Make Own Tile LAYER in Robinson FROM POLYGON(s) => COUNTRIE(s) | Claimed Areas => Label(s)+
          # addLabelOnlyMarkers(data = GeoDATA, lng = ~Centroid_X, lat = ~Centroid_Y, label = ~NAME, #Add Label(s) To Map | COORDINATE(s)
          #                     labelOptions = labelOptions( #Label(s)-Options+
          #                       noHide = TRUE, permanent = TRUE, #Label(s) => !MOUSEOVER
          #                       direction = "center", opacity = 0.75, textsize = "10px", textOnly = TRUE, #Label(s) => POSITION | ... | !BOX
          #                       style = list("..." = "..."))) %>% #...
          addScaleBar(position = "bottomleft") %>%
          #World Map View => CENTER => LONGITUDE = 0 | LATITUDE = 0 | Zoom Level = 1
          setView(lng = 0, lat = 0, zoom = 1) %>%
          #Execute JavaScript Code When Map is Rendered => Save initial center and zoom level of world map
          onRender(JS("function(el, x){var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>%
          #Add Reset Map View Button
          addEasyButton(easyButton(
            icon = "ion-earth", #EARTH ICON BUTTON from https://ionic.io/ionicons
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
      
      } else { #Execute Code To SHOW WAITER ONLY if reactive value is TRUE
        
        ##### SHOW WAITER #####
        World_Map_WAITER$show()
        
        #World Map WAITER => POSITION
        observe({runjs("function WorldMapWAITER_Position() { //Create WorldMapWAITER_Position() Function
                          var Map = document.getElementById('MR_World_Map'); //Retrieve MR_World_Map
                          var WAITER = document.querySelector('.waiter-overlay'); //Retrieve World_Map_WAITER
                          if (Map && WAITER) { //Execute Code if Map/WAITER are retrieved with success
                              var Map_Position = Map.getBoundingClientRect(); //Map POSITION
                              WAITER.style.width = Map_Position.width + 'px'; //WAITER => WIDTH = Map-WIDTH
                              WAITER.style.height = Map_Position.height + 'px'; //WAITER => HEIGHT = Map-HEIGHT
                              WAITER.style.top = Map_Position.top + window.scrollY + 'px'; //WAITER => Top = Map-Top
                              WAITER.style.left = Map_Position.left + window.scrollX + 'px'; //WAITER => LEFT = Map-LEFT
                              }
                          }
                        //Call WorldMapWAITER_Position() Function
                        WorldMapWAITER_Position();
                        //Add EVENT-LISTENER To UPDATE WAITER Position when window is resized
                        setTimeout(function() {window.addEventListener('resize', WorldMapWAITER_Position());}, 225);")}) #DELAY EXECUTION => 225 MILLISECONDs
                        # //Add EVENT-LISTENER To UPDATE WAITER Position when the PAGE is scrolled
                        # setTimeout(function() {window.addEventListener('scroll', WorldMapWAITER_Position());}, 225);")}) #DELAY EXECUTION => 225 MILLISECONDs
                        # //Add EVENT-LISTENER To UPDATE WAITER Position when the PAGE is scrolled
                        # setTimeout(function() {window.addEventListener('scrollend', WorldMapWAITER_Position());}, 225);")}) #DELAY EXECUTION => 225 MILLISECONDs
        
        }
    
    #PROPERTY (CSS) on World Map Card => Condition(s) => COUNTRIEs ACTION BUTTON is active/clicked | REGIONs ACTION BUTTON Click COUNTER > 0
    if (World_Map_Card_RV$MR_REGIONs_Click_COUNTER > 0) {runjs("$('#MR_World_Map_Card').css('margin-top', '-10px');")}
    
    #Reactive Value To TRUE => World Map Card Have a New PROPERTY
    if (World_Map_Card_RV$MR_REGIONs_Click_COUNTER > 0) {World_Map_Card_RV$WM_Card_PROPERTY <- TRUE}
    
    ##### World Administrative Boundaries #####

    #World Map Loaded => Create Reactive Value 'Map_Loaded' == TRUE
    observe({runjs("var CheckMapLoaded = setInterval(function() { //Call Function EVERY X MILLISECONDs
                        var Map = $('#MR_World_Map').data('leaflet-map'); //Retrieve MR_World_Map
                        if (Map) { //Execute Code if Map is retrieved with success
                            Shiny.setInputValue('Map_Loaded', true); //Reactive Value To SERVER
                            //Shiny.setInputValue('Map_Loaded', true, {priority: 'event'}); //Reactive Value To SERVER
                            clearInterval(CheckMapLoaded); //Once Map found and reactive value created => CheckMapLoaded => !Run ANYMORE
                            }
                        }, 100);")}) #Run Function EVERY 100 MILLISECONDs
    
    #Draw World Administrative Boundaries | WAITER => MESSAGE(s)
    observeEvent(input$Map_Loaded, { #WHEN World Map is loaded
      
      #Reactive Value To TRUE => Draw World Administrative Boundaries
      if (input$Map_Loaded) {
        
        ##### MESSAGE+ UPDATE(s) #####
        for(World_Map_WAITER_MESSAGE in 1:length(World_Map_WAITER_MESSAGEs)){
          World_Map_WAITER$update(html = tagList(
            div(spin_orbiter(), style = "margin-bottom: 10px;"), #SPINNER
            div(span(World_Map_WAITER_MESSAGEs[World_Map_WAITER_MESSAGE])))) #TEXT+
          #!Execution in R For X SECONDs
          Sys.sleep(1.25)}
        
        #DEVELOPMENT LEVEL(s) => Group(s)
        # DevLevelsGroups <- c("2 Development Levels", "3 Development Levels", "5 Development Levels")
        
        #COUNTRIEs => Modal Box => HIDE
        runjs("$('#WM_ModalBox').parent().remove();")
        
        #Draw World Administrative Boundaries
        leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
          clearShapes() %>% #Remove all POLYGONs from World Map (REGIONs | Top FIVE BIRTH AREA(s))
          clearControls() %>% #Remove LEGEN. (REGIONs | Top FIVE BIRTH AREA(s))
          # hideGroup(group = DevLevelsGroups) %>% #DEVELOPMENT LEVEL(s) => Group(s) => HIDE POLYGON(s) LAYER FROM World Map
          removeLayersControl() %>% #Remove LAYER(s) Control (DEVELOPMENT LEVEL(s) | Top FIVE BIRTH AREA(s))
          addPolygons(data = GeoDATA, layerId = ~VISUALIZATION_NAME, #Add POLYGONs To Map | Keep GeoDATA in WORLD GEODETIC SYSTEM (1984) [PROJ4]
                      stroke = TRUE, color = "transparent", weight = 1.5, opacity = 0.75, #Stroke Attributes
                      fill = TRUE, fillOpacity = 0.25, smoothFactor = 1, label = ~lapply(Label, HTML), #Fill Attributes | POLYLINE Simplification | HOVER TEXT
                      highlightOptions = highlightOptions( #HIGHLIGHT-Options on Mouse OVER
                        stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75, #Stroke Attributes
                        fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)) #Fill Attributes
        
        } 
      
      })
    
    #World Map Loaded => POLYGONs Rendered => Create Reactive Value 'COUNTRIEs_POLYGONs_Rendered' == TRUE
    observe({runjs("var CheckMapLoaded = setInterval(function() { //Call Function EVERY X MILLISECONDs
                        var Map = $('#MR_World_Map').data('leaflet-map'); //Retrieve MR_World_Map
                        if (Map) { //Execute Code if Map is retrieved with success
                            var LAYER_COUNTER = 0; //Initialize LAYER COUNTER
                            Map.eachLayer(function(LAYER) {LAYER_COUNTER++;}); //+1
                            //console.log('LAYER COUNTER in COUNTRIEs:', LAYER_COUNTER); //Console-support
                            var RPOLYGONs_Rendered = window.REGIONs_POLYGONs_Rendered; //Retrieve REGIONs_POLYGONs_Rendered Global PROPERTY
                            //if (LAYER_COUNTER > 2) { //LAYER COUNTER > 2
                            if (RPOLYGONs_Rendered === undefined && LAYER_COUNTER > 2) { //WHEN We First Click on COUNTRIEs ACTION BUTTON
                                //console.log('Condition n°1'); //Console-support
                                //Shiny.setInputValue('REGIONs_POLYGONs_Rendered', false); //Reactive Value To FALSE => ClearShapes() in World Map => TRIGGER NEXT ObserveEvent() in REGIONs ACTION BUTTON
                                Shiny.setInputValue('COUNTRIEs_POLYGONs_Rendered', true); //Reactive Value To SERVER
                                //Shiny.setInputValue('COUNTRIEs_POLYGONs_Rendered', true, {priority: 'event'}); //Reactive Value To SERVER
                                window.COUNTRIEs_POLYGONs_Rendered = true; //Create COUNTRIEs_POLYGONs_Rendered Global PROPERTY
                                clearInterval(CheckMapLoaded); //Once Map found and reactive value created => CheckMapLoaded => !Run ANYMORE
                                }
                             else if (RPOLYGONs_Rendered === true && LAYER_COUNTER > 4) { //WHEN We Have Clicked on REGIONs ACTION BUTTON
                                      //console.log('Condition n°2'); //Console-support
                                      Shiny.setInputValue('REGIONs_POLYGONs_Rendered', false); //Reactive Value To FALSE => ClearShapes() in World Map => TRIGGER NEXT ObserveEvent() in REGIONs ACTION BUTTON
                                      Shiny.setInputValue('COUNTRIEs_POLYGONs_Rendered', true); //Reactive Value To SERVER
                                      //Shiny.setInputValue('COUNTRIEs_POLYGONs_Rendered', true, {priority: 'event'}); //Reactive Value To SERVER
                                      window.COUNTRIEs_POLYGONs_Rendered = true; //Create COUNTRIEs_POLYGONs_Rendered Global PROPERTY
                                      clearInterval(CheckMapLoaded); //Once Map found and reactive value created => CheckMapLoaded => !Run ANYMORE
                                      }
                            }
                        }, 100);")}) #Run Function EVERY 100 MILLISECONDs
    
    #WAITER => HIDE | CURSOR Class To Default
    observeEvent(input$COUNTRIEs_POLYGONs_Rendered, { #WHEN POLYGONs are rendered
      
      #Reactive Value To TRUE => WAITER => HIDE | CURSOR Class To Default
      if (input$COUNTRIEs_POLYGONs_Rendered) {
        
        ##### HIDE WAITER #####
        World_Map_WAITER$hide()
        
        #CURSOR Class To Default
        session$sendCustomMessage(type = "CURSORwithinBODY", message = "default-cursor")
        
        }
      
      })
    
    })
  
  #Make REGIONs CHOICE RADIO BUTTONs invisible (first click will also be animated)
  shinyjs::hide(id = "MRR_REGIONs_CHOICE", anim = FALSE)
  
  ##### REGIONs ACTION BUTTON #####
  observeEvent(input$MR_REGIONs, { #WHEN REGIONs ACTION BUTTON is clicked
    
    #Add CSS class from REGIONs ACTION BUTTON
    shinyjs::addClass(id = "MR_REGIONs", class = "active")
    #Remove CSS class from COUNTRIEs ACTION BUTTON
    shinyjs::removeClass(id = "MR_COUNTRIEs", class = "active")
    #Make REGIONs CHOICE RADIO BUTTONs visible
    shinyjs::show(id = "MRR_REGIONs_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    #Make Selected Residence Card invisible
    shinyjs::hide(id = "MR_Selected_Residence_Card", anim = TRUE, animType = "slide", time = 0.25)
    #Make Top Five BIRTH AREA(s) CHECKBOX invisible
    shinyjs::hide(id = "MR_Top_Five_BIRTH_Card", anim = TRUE, animType = "slide", time = 0.25)
    #Disable Top Five BIRTH AREA(s) CHECKBOX
    shinyjs::disable(id = "MR_Top_Five_BIRTH")
    
    #Make ... Card invisible
    shinyjs::hide(id = "MR_MIGRANT_STOCK_DATA_Card", anim = TRUE, animType = "slide", time = 0.25)
    
    #Disable ...
    # shinyjs::disable(id = "MR_BIRTH_View_Choice_REGIONs_CHOICE")
    
    #Top Five BIRTH AREA(s) CHECKBOX => Replace CHECKBOX Label Value => REGIONs
    updateCheckboxInput(
      session = session, inputId = "MR_Top_Five_BIRTH",
      label = "View Top 5 Birth Areas on World Map (5-Year Period, 1990-2020)",
      value = FALSE) #Replace CHECKBOX Value => FALSE
    
    #CURSOR Class To WAIT
    session$sendCustomMessage(type = "CURSORwithinBODY", message = "wait-cursor")
    
    #Reactive Value To REGIONs => REGIONs Action Button have been clicked
    Selected_Residence_RV$MR_Residence_View_Choice <- "REGIONs"
    
    #Reinitialize Reactive Value To FALSE
    Filtered_MIGRANT_STOCKs_DATA_RV$Selected_Residence_Code_in_MIGRANT_STOCK_DATA <- FALSE
    
    #Reinitialize Reactive Value => DATA.FRAME => No Columns | No Rows
    Filtered_MIGRANT_STOCKs_DATA_RV$Filtered_MIGRANT_STOCKs_DATA <- data.frame()
    
    ##### World Map Tiles #####
    if (!World_Map_Card_RV$TilesLoaded) { #Execute Code To load Tiles ONLY if reactive value is FALSE
      
      #Make World Map Card visible
      shinyjs::show(id = "MR_World_Map_Card", anim = TRUE, animType = "fade", time = 0.25)
      
      ##### SHOW WAITER #####
      World_Map_WAITER$show()
      
      #World Map
      output$MR_World_Map <- renderLeaflet({
        leaflet() %>% 
        # leaflet(options = leafletOptions( #Options for Map Creation
        #   minZoom = 1, maxZoom = 10, #Min/Max Zoom Levels
        #   crs = leafletCRS( #Create Custom COORDINATE REFERENCE SYSTEM (CRS)
        #     crsClass = "L.Proj.CRS",
        #     # code = "ESRI:54042", #CODE => WORLD WINKEL-TRIPEL (WINKEL III) - (1921) [PROJ4] | !Supported BY leaflet
        #     # code = "ESRI:54012", #CODE => WORLD ECKERT IV (1906) [PROJ4] | !Supported BY leaflet
        #     code = "ESRI:54030", #CODE => WORLD ROBINSON (1963) [PROJ4]
        #     # proj4def = "+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", #WORLD WINKEL-TRIPEL (WINKEL III) - (1921) [PROJ4] | !Supported BY leaflet
        #     # proj4def = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", #WORLD ECKERT IV (1906) [PROJ4] | !Supported BY leaflet
        #     proj4def = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", #WORLD ROBINSON (1963) [PROJ4]
        #     # resolutions = 32755140.66 / (256 * 2^(0:18))))) %>% #WORLD WINKEL-TRIPEL (WINKEL III) => Resolution = World width in pixels / Tile size in pixels * 2^(Zoom_Level)
        #     # resolutions = 32755140.66 / (256 * 2^(1:10))))) %>% #WORLD WINKEL-TRIPEL (WINKEL III) => Resolution = World width in pixels / Tile size in pixels * 2^(Zoom_Level)
        #     # resolutions = 33842405.84 / (256 * 2^(0:18))))) %>% #WORLD ECKERT IV (1906) => Resolution = World width in pixels / Tile size in pixels * 2^(Zoom_Level)
        #     # resolutions = 33842405.84 / (256 * 2^(1:10))))) %>% #WORLD ECKERT IV (1906) => Resolution = World width in pixels / Tile size in pixels * 2^(Zoom_Level)
        #     # resolutions = 34011666.66 / (256 * 2^(0:18))))) %>% #WORLD ROBINSON (1963) => Resolution = World width in pixels / Tile size in pixels * 2^(Zoom_Level)
        #     resolutions = 34011666.66 / (256 * 2^(1:10))))) %>% #WORLD ROBINSON (1963) => Resolution = World width in pixels / Tile size in pixels * 2^(Zoom_Level)
          #Add Tile LAYER
          # addTiles( 
          #   attribution = HTML("© <a href='https://www.openstreetmap.org/copyright/' target='_blank'> OpenStreetMap </a>"), #Tile LAYER Attribution
          #   options = tileOptions(minZoom = 1, maxZoom = 10, maxNativeZoom = 18)) %>% #Min/Max Zoom Levels
          #Add Tile LAYER in WinkelTripel | Eckert IV | Robinson
          # addTiles(
          # urlTemplate = "https://yourtilesource.com/{z}/{x}/{y}.png", #Tiles Link
          # attribution = HTML("© <a href='...' target='_blank'> ... </a>"), #Tile LAYER Attribution
          # options = tileOptions(minZoom = 1, maxZoom = 10, maxNativeZoom = 18)) %>% #Min/Max Zoom Levels
          #Add Tile LAYER From PROVIDER
          addProviderTiles(
            # provider = providers$CartoDB, #PROVIDER-NAME
            # provider = providers$CartoDB.Positron, #PROVIDER-NAME
            # provider = providers$CartoDB.PositronNoLabels, #PROVIDER-NAME
            # provider = providers$CartoDB.DarkMatter, #PROVIDER-NAME
            # provider = providers$CartoDB.DarkMatterNoLabels, #PROVIDER-NAME
            # provider = providers$CartoDB.Voyager, #PROVIDER-NAME
            provider = providers$CartoDB.VoyagerNoLabels, #PROVIDER-NAME
            # provider = providers$CartoDB.VoyagerLabelsUnder, #PROVIDER-NAME
            options = providerTileOptions(
              #Tile LAYER Attribution(s)
              attribution = HTML("© <a href='https://www.openstreetmap.org/copyright/' target='_blank'> OpenStreetMap </a> |
                                  © <a href='https://carto.com/attributions/' target='_blank'> CARTO </a>"),
              minZoom = 1, maxZoom = 10, maxNativeZoom = 20)) %>% #Min/Max Zoom Levels
          #Graticule => !LINE(s) | OCEAN(s)/SEA(s)
          # addGraticule(interval = 0, sphere = TRUE, style = list(color = "#33CCFF", weight = 0, opacity = 0)) %>%
          #Graticule => LINE(s) => 15°
          # addGraticule(interval = 15, sphere = FALSE, style = list(color = "#333333", weight = 0.5, opacity = 0.5)) %>%
          #Graticule => LINE(s) => 30°
          # addGraticule(interval = 30, sphere = FALSE, style = list(color = "#333333", weight = 0.5, opacity = 0.5)) %>%
          #Graticule => LINE(s) => 90°
          # addGraticule(interval = 90, sphere = FALSE, style = list(color = "#333333", weight = 0.5, opacity = 0.5)) %>%
          #Graticule => LINE(s) => 180°
          # addGraticule(interval = 180, sphere = FALSE, style = list(color = "#333333", weight = 0.5, opacity = 0.5)) %>%
          #Graticule => LINE(s) => EQUATOR | PRIME MERIDIAN
          # addGraticule(interval = c(0, 0), sphere = FALSE, style = list(color = "#333333", weight = 0.5, opacity = 0.5)) %>%
          #Make Own Tile LAYER in Robinson FROM POLYGON(s) => COUNTRIE(s)
          # addPolygons(data = GeoDATA, #Add POLYGONs To Map | Keep GeoDATA in WORLD GEODETIC SYSTEM (1984) [PROJ4]
          #             stroke = TRUE, color = "#333333", weight = 0.75, opacity = 0.75, #Stroke Attributes
          #             fill = TRUE, fillColor = "#FAF8F6", fillOpacity = 1, smoothFactor = 1) %>% #Fill Attributes | POLYLINE Simplification
          #Make Own Tile LAYER in Robinson FROM POLYGON(s) => Claimed Areas => Stroke PATTERN+
          # addPolygons(data = GeoDATA %>% filter(is.na(SOVRN) & NAME != "ANTARCTICA"), #Add POLYGONs To Map | Keep GeoDATA in WORLD GEODETIC SYSTEM (1984) [PROJ4]
          #             stroke = TRUE, color = "#FAF8F6", weight = 0.75, opacity = 1, #Stroke Attributes
          #             fill = FALSE , dashArray = "5, 5", smoothFactor = 1) %>% #Fill Attributes | Stroke PATTERN | POLYLINE Simplification
          #Make Own Tile LAYER in Robinson FROM POLYGON(s) => COUNTRIE(s) | Claimed Areas => Label(s)+
          # addLabelOnlyMarkers(data = GeoDATA, lng = ~Centroid_X, lat = ~Centroid_Y, label = ~NAME, #Add Label(s) To Map | COORDINATE(s)
          #                     labelOptions = labelOptions( #Label(s)-Options+
          #                       noHide = TRUE, permanent = TRUE, #Label(s) => !MOUSEOVER
          #                       direction = "center", opacity = 0.75, textsize = "10px", textOnly = TRUE, #Label(s) => POSITION | ... | !BOX
          #                       style = list("..." = "..."))) %>% #..
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
      
      } else { #Execute Code To SHOW WAITER ONLY if reactive value is TRUE
        
        ##### SHOW WAITER #####
        World_Map_WAITER$show()
        
        #World Map WAITER => POSITION
        observe({runjs("function WorldMapWAITER_Position() { //Create WorldMapWAITER_Position() Function
                          var Map = document.getElementById('MR_World_Map'); //Retrieve MR_World_Map
                          var WAITER = document.querySelector('.waiter-overlay'); //Retrieve World_Map_WAITER
                          if (Map && WAITER) { //Execute Code if Map/WAITER are retrieved with success
                              var Map_Position = Map.getBoundingClientRect(); //Map POSITION
                              WAITER.style.width = Map_Position.width + 'px'; //WAITER => WIDTH = Map-WIDTH
                              WAITER.style.height = Map_Position.height + 'px'; //WAITER => HEIGHT = Map-HEIGHT
                              WAITER.style.top = Map_Position.top + window.scrollY + 'px'; //WAITER => Top = Map-Top
                              WAITER.style.left = Map_Position.left + window.scrollX + 'px'; //WAITER => LEFT = Map-LEFT
                              }
                          }
                        //Call WorldMapWAITER_Position() Function
                        WorldMapWAITER_Position();
                        //Add EVENT-LISTENER To UPDATE WAITER Position when window is resized
                        setTimeout(function() {window.addEventListener('resize', WorldMapWAITER_Position());}, 225);")}) #DELAY EXECUTION => 225 MILLISECONDs
                        # //Add EVENT-LISTENER To UPDATE WAITER Position when the PAGE is scrolled
                        # setTimeout(function() {window.addEventListener('scroll', WorldMapWAITER_Position());}, 225);")}) #DELAY EXECUTION => 225 MILLISECONDs
                        # //Add EVENT-LISTENER To UPDATE WAITER Position when the PAGE is scrolled
                        # setTimeout(function() {window.addEventListener('scrollend', WorldMapWAITER_Position());}, 225);")}) #DELAY EXECUTION => 225 MILLISECONDs
        
        }
        
        
    #Track clicks on REGIONs ACTION BUTTON (+1)
    World_Map_Card_RV$MR_REGIONs_Click_COUNTER <- World_Map_Card_RV$MR_REGIONs_Click_COUNTER + 1
    
    #PROPERTY (CSS) on World Map Card => Condition(s) => REGIONs ACTION BUTTON is active/clicked | World Map Card Have a New PROPERTY (WM_Card_PROPERTY = TRUE)
    if (World_Map_Card_RV$WM_Card_PROPERTY) {runjs("$('#MR_World_Map_Card').css('margin-top', '0px');")}
    
    ##### World Administrative Boundaries #####
    
    #World Map Loaded => Create Reactive Value 'Map_Loaded' == TRUE
    observe({runjs("var CheckMapLoaded = setInterval(function() { //Call Function EVERY X MILLISECONDs
                        var Map = $('#MR_World_Map').data('leaflet-map'); //Retrieve MR_World_Map
                        if (Map) { //Execute Code if Map is retrieved with success
                            Shiny.setInputValue('Map_Loaded', true); //Reactive Value To SERVER
                            //Shiny.setInputValue('Map_Loaded', true, {priority: 'event'}); //Reactive Value To SERVER
                            clearInterval(CheckMapLoaded); //Once Map found and reactive value created => CheckMapLoaded => !Run ANYMORE
                            }
                        }, 100);")}) #Run Function EVERY 100 MILLISECONDs
    
    #Draw World Administrative Boundaries | WAITER => MESSAGE(s)
    observeEvent(input$Map_Loaded, { #WHEN World Map is loaded
      
      #Reactive Value To TRUE => Draw World Administrative Boundaries
      if (input$Map_Loaded) {
        
        ##### MESSAGE+ UPDATE(s) #####
        for(World_Map_WAITER_MESSAGE in 1:length(World_Map_WAITER_MESSAGEs)){
          World_Map_WAITER$update(html = tagList(
            div(spin_orbiter(), style = "margin-bottom: 10px;"), #SPINNER
            div(span(World_Map_WAITER_MESSAGEs[World_Map_WAITER_MESSAGE])))) #TEXT+
          #!Execution in R For X SECONDs
          Sys.sleep(1.25)}
        
        ##### REGIONs CHOICE RADIO BUTTONs #####
        observeEvent(input$MRR_REGIONs_CHOICE, { #WHEN REGIONs CHOICE RADIO BUTTONs are clicked
          
          #Make Selected Residence Card invisible
          shinyjs::hide(id = "MR_Selected_Residence_Card", anim = TRUE, animType = "slide", time = 0.25)
          #Make Top Five BIRTH AREA(s) CHECKBOX invisible
          shinyjs::hide(id = "MR_Top_Five_BIRTH_Card", anim = TRUE, animType = "slide", time = 0.25)
          
          #Make ... Card invisible
          shinyjs::hide(id = "MR_MIGRANT_STOCK_DATA_Card", anim = TRUE, animType = "slide", time = 0.25)
          
          #Top Five BIRTH AREA(s) CHECKBOX => Replace CHECKBOX Value => TRUE To FALSE
          if (input$MR_Top_Five_BIRTH) {updateCheckboxInput(session = session, inputId = "MR_Top_Five_BIRTH", value = FALSE)}
          
          #Reinitialize Reactive Value To FALSE
          Filtered_MIGRANT_STOCKs_DATA_RV$Selected_Residence_Code_in_MIGRANT_STOCK_DATA <- FALSE
          
          #Reinitialize Reactive Value => DATA.FRAME => No Columns | No Rows
          Filtered_MIGRANT_STOCKs_DATA_RV$Filtered_MIGRANT_STOCKs_DATA <- data.frame()
          
          #CURSOR Class To WAIT
          session$sendCustomMessage(type = "CURSORwithinBODY", message = "wait-cursor")
          
          ##### SHOW WAITER #####
          REGIONs_World_Map_WAITER$show()
          
          #OVERLAY WAITER on MR_World_Map => FONT-SIZE | FONT-FAMILY
          # runjs("$('.waiter-overlay-content').css({'font-size': '14px', 'font-family': 'DM Sans'});")
          
          #REGIONs => WAITER on World Map => POSITION
          observe({runjs("function REGIONs_WorldMapWAITER_Position() { //Create REGIONs_WorldMapWAITER_Position() Function
                            $('.waiter-overlay.waiter-local').each(function() {
                              var CurrentCSS = $(this).attr('style'); //WAITER => CURRENT-STYLE
                              $(this).attr('style', CurrentCSS + 'position: relative !important; top: 0px !important; left: 0px !important;'); //WAITER => NEW-POSITION
                              })
                            }
                            //Call REGIONs_WorldMapWAITER_Position() Function
                            REGIONs_WorldMapWAITER_Position();
                            //Add EVENT-LISTENER To UPDATE WAITER Position when window is resized
                            window.addEventListener('resize', function() {REGIONs_WorldMapWAITER_Position();});")})
          
          #World Map Loaded => POLYGONs Rendered => Create Reactive Value 'REGIONs_POLYGONs_Rendered' == TRUE
          observe({runjs("var CheckMapLoaded = setInterval(function() { //Call Function EVERY X MILLISECONDs
                          var Map = $('#MR_World_Map').data('leaflet-map'); //Retrieve MR_World_Map
                          if (Map) { //Execute Code if Map is retrieved with success
                              var LAYER_COUNTER = 0; //Initialize LAYER COUNTER
                              Map.eachLayer(function(LAYER) {LAYER_COUNTER++;}); //+1
                              //console.log('LAYER COUNTER n°2:', LAYER_COUNTER); //Console-support
                              if (LAYER_COUNTER > 4) { //LAYER COUNTER > 4
                                  Shiny.setInputValue('REGIONs_POLYGONs_Rendered', false); //Reactive Value To SERVER
                                  Shiny.setInputValue('REGIONs_POLYGONs_Rendered', true); //Reactive Value To SERVER
                                  //Shiny.setInputValue('REGIONs_POLYGONs_Rendered', true, {priority: 'event'}); //Reactive Value To SERVER
                                  window.REGIONs_POLYGONs_Rendered = true; //Create REGIONs_POLYGONs_Rendered Global PROPERTY
                                  clearInterval(CheckMapLoaded); //Once Map found and reactive value created => CheckMapLoaded => !Run ANYMORE
                                  }
                              }
                          }, 100);")}) #Run Function EVERY 100 MILLISECONDs
          
          #REGIONs CHOICE RADIO BUTTONs => Selected REGION
          SelectedREGION <- input$MRR_REGIONs_CHOICE
          
          #Selected REGION => GeoRDATA | World Map => LN-HeadLINE + LAYER-NAME
          if (SelectedREGION == "Continental Regions") {
            
            #Disable Top Five BIRTH AREA(s) CHECKBOX
            shinyjs::disable(id = "MR_Top_Five_BIRTH")
            
            GeoRDATA <- GeoRDATA_ContinentalREGIONs #GeoRDATA
            LNHeadLINE <- "Continental Region" #World Map => LN-HeadLINE
            LAYER <- "ContinentalREGIONs"} #World Map => LAYER-NAME
          else if (SelectedREGION == "Continental Sub-Regions and Intermediate Regions") {
            
            #Activate Top Five BIRTH AREA(s) CHECKBOX
            shinyjs::enable(id = "MR_Top_Five_BIRTH")
            
            GeoRDATA <- GeoRDATA_ContinentalSIREGIONs #GeoRDATA
            LNHeadLINE <- "Continental Sub/Intermediate Region" #World Map => LN-HeadLINE
            LAYER <- "ContinentalSIREGIONs"} #World Map => LAYER-NAME
          else if (SelectedREGION == "Geographic Regions") {
            
            #Disable Top Five BIRTH AREA(s) CHECKBOX
            shinyjs::disable(id = "MR_Top_Five_BIRTH")
            
            GeoRDATA <- GeoRDATA_GeoREGIONs #GeoRDATA
            LNHeadLINE <- "Geographic Region" #World Map => LN-HeadLINE
            LAYER <- "GeoREGIONs"} #World Map => LAYER-NAME
          else if (SelectedREGION == "Development Levels (x3)") {
            
            #Disable Top Five BIRTH AREA(s) CHECKBOX
            shinyjs::disable(id = "MR_Top_Five_BIRTH")
            
            GeoRDATA <- list(GeoRDATA_MoreLess, GeoRDATA_MoreLessLeast, GeoRDATA_LDC_LLDC_SIDS) #GeoRDATA(s)
            POLYGONs_LAYER <- list(GeoRDATA[[1]]$LabelR, GeoRDATA[[2]]$CLRCode, GeoRDATA[[3]]$NAME) #POLYGONs IDENTIFIER
            LNHeadLINE <- "Development Level" #World Map => LN-HeadLINE
            LAYER <- c("MoreLess", "MoreLessLeast", "LDC_LLDC_SIDS")} #World Map =>  LAYER-NAME(s)
          else if (SelectedREGION == "Income Levels") {
            
            #Disable Top Five BIRTH AREA(s) CHECKBOX
            shinyjs::disable(id = "MR_Top_Five_BIRTH")
            
            GeoRDATA <- GeoRDATA_IncomeLevels #GeoRDATA 
            LNHeadLINE <- "Income Level" #World Map => LN-HeadLINE
            LAYER <- "IncomeLevels"} #World Map => LAYER-NAME
          
          #DEVELOPMENT LEVEL(s) => Group(s)
          # DevLevelsGroups <- c("2 Development Levels", "3 Development Levels", "5 Development Levels")
          
          #GeoRDATA => Spatial Features in Class(es)
          if ("sf" %in% class(GeoRDATA)) {
            
            #DEVELOPMENT LEVEL(s) => Group(s)
            # DevLevelsGroups <- c("2 Development Levels", "3 Development Levels", "5 Development Levels")
            
            #REGIONs => SelectedREGION != "Development Levels (x3) => Modal Box => HIDE
            runjs("$('#WM_ModalBox').parent().remove();")
            
            #Maps Values To Colors
            fCLR <- colorFactor(
              #Colors ordered with factor-level(s) in NAME
              palette = GeoRDATA$CLRCode[order(factor(GeoRDATA$NAME, levels = levels(GeoRDATA$NAME)))],
              #Possible Values | NOT-AVAILABLE COLOR
              domain = GeoRDATA$NAME, na.color = "#999999")
            
            #Draw World Administrative Boundaries
            leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
              clearShapes() %>% #Remove all POLYGONs from World Map (COUNTRIEs | REGIONs | Top FIVE BIRTH AREA(s)/COUNTRIE(s))
              clearControls() %>% #Remove LEGEN. (REGIONs | Top FIVE BIRTH AREA(s)/COUNTRIE(s))
              # hideGroup(group = DevLevelsGroups) %>% #DEVELOPMENT LEVEL(s) => Group(s) => HIDE POLYGON(s) LAYER FROM World Map
              removeLayersControl() %>% #Remove LAYER(s) Control (DEVELOPMENT LEVEL(s) | Top FIVE BIRTH AREA(s)/COUNTRIE(s))
              addPolygons(data = GeoRDATA, layerId = ~NAME, #Add POLYGONs To Map
                          stroke = TRUE, color = "#CCCCCC", weight = 0.5, opacity = 0.5, #Stroke Attributes
                          fill = TRUE, fillColor = fCLR(GeoRDATA$NAME), fillOpacity = 0.75, #Fill Attributes
                          smoothFactor = 1.25, label = ~lapply(LabelR, HTML), #POLYLINE Simplification | HOVER TEXT
                          highlightOptions = highlightOptions( #HIGHLIGHT-Options on Mouse OVER
                            stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75, #Stroke Attributes
                            fill = TRUE, fillColor = NULL, fillOpacity = 1)) %>% #Fill Attributes
              addLegend(position = "topright", #Add LN To Map | LN-POSITION
                        pal = fCLR, values = GeoRDATA$NAME, opacity = 1, #COLOR(s) + VALUE(s) + COLOR(s)-OPACITY
                        title = LNHeadLINE, layerId = LAYER, group = "REGIONs") #LN-HeadLINE + LAYER-NAME + Group
            
            #GeoRDATA => Class == LIST(-OF-Spatial-Features)
            } else if (is.list(GeoRDATA)) {
              
                #DEVELOPMENT LEVEL(s) => Group(s)
                DevLevelsGroups <- c("2 Development Levels", "3 Development Levels", "5 Development Levels")
              
                #Maps Values To Colors
                fCLR <- lapply(1:3, function(i) {colorFactor(
                  #Colors ordered with factor-level(s) in NAME
                  palette = GeoRDATA[[i]]$CLRCode[order(factor(GeoRDATA[[i]]$NAME, levels = levels(GeoRDATA[[i]]$NAME)))], 
                  #Possible Values | NOT-AVAILABLE COLOR
                  domain = GeoRDATA[[i]]$NAME, na.color = "#999999")})
                
                #Customize and control a rendered map
                leafletProxy(mapId = "MR_World_Map", session = session) %>% 
                  clearShapes() %>% #Remove => All POLYGONs from World Map (COUNTRIEs | REGIONs| Top FIVE BIRTH AREA(s)/COUNTRIE(s))
                  clearControls() # %>% #Remove => LEGEN. (REGIONs | Top FIVE BIRTH AREA(s)/COUNTRIE(s))
                  #...
                  # removeLayersControl() %>% #Remove LAYER(s) Control (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
                
                #Draw World Administrative Boundaries
                for (i in 1:3) {
            
                  #Draw World Administrative Boundaries => DEVELOPMENT-LEVEL(s)
                  leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
                    addPolygons(data = GeoRDATA[[i]], layerId = POLYGONs_LAYER[[i]], group = DevLevelsGroups[i], #Add POLYGONs To Map | Group+
                                stroke = TRUE, color = "#CCCCCC", weight = 0.5, opacity = 0.5, #Stroke Attributes
                                fill = TRUE, fillColor = fCLR[[i]](GeoRDATA[[i]]$NAME), fillOpacity = 0.75, #Fill Attributes
                                smoothFactor = 1.25, label = ~lapply(LabelR, HTML), #POLYLINE Simplification | HOVER TEXT
                                highlightOptions = highlightOptions( #HIGHLIGHT-Options on Mouse OVER
                                  stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75, #Stroke Attributes
                                  fill = TRUE, fillColor = NULL, fillOpacity = 1)) # %>% #Fill Attributes
                    # addLegend(position = "topright", #Add LN To Map | LN-POSITION
                    #           pal = fCLR[[i]], values = GeoRDATA[[i]]$NAME, opacity = 1, #COLOR(s) + VALUE(s) + COLOR(s)-OPACITY
                    #           title = LNHeadLINE, layerId = LAYER[i], group = DevLevelsGroups[i]) #LN-HeadLINE + LAYER-NAME + Group
                  
                  #DEVELOPMENT LEVEL(s) => Group(s) => HIDE POLYGON(s) LAYER FROM World Map => Keep ONLY => "2 Development Levels" (i == 1)
                  if (i != 1) {leafletProxy(mapId = "MR_World_Map", session = session) %>% hideGroup(group = DevLevelsGroups[i])}
                  #DEVELOPMENT LEVEL(s) => Group(s) => HIDE POLYGON(s) LAYER FROM World Map => Keep ONLY => "2 Development Levels" (i == 1) || . => SHOW POLYGON(s) LAYER on World Map (i == 1)
                  # if (i != 1) {leafletProxy(mapId = "MR_World_Map", session = session) %>% hideGroup(group = DevLevelsGroups[i])} else {leafletProxy(mapId = "MR_World_Map", session = session) %>% showGroup(group = DevLevelsGroups[i])}
                  #DEVELOPMENT LEVEL(s) => Group(s) => Remove LEGEN. (DEVELOPMENT LEVEL(s)) || . => HIDE POLYGON(s) LAYER FROM World Map => Keep ONLY => "2 Development Levels" (i == 1) || . => SHOW POLYGON(s) LAYER on World Map (i == 1)
                  # if (i != 1) {leafletProxy(mapId = "MR_World_Map", session = session) %>% removeControl(layerId = LAYER[i]) %>% hideGroup(group = DevLevelsGroups[i])} else {leafletProxy(mapId = "MR_World_Map", session = session) %>% showGroup(group = DevLevelsGroups[i])}
                  
                  }
                
                #Top FIVE BIRTH COUNTRIE(s) => Group(s)
                TopFiveBirthCountriesGroups <- c("Top 5 Birth Countries - Year 1990", 
                  "Top 5 Birth Countries - Year 1995", "Top 5 Birth Countries - Year 2000", "Top 5 Birth Countries - Year 2005", 
                  "Top 5 Birth Countries - Year 2010", "Top 5 Birth Countries - Year 2015", "Top 5 Birth Countries - Year 2020")
                
                #Top FIVE BIRTH AREA(s) => Group(s)
                TopFiveBirthAreasGroups <- c("Top 5 Birth Areas - Year 1990", 
                  "Top 5 Birth Areas - Year 1995", "Top 5 Birth Areas - Year 2000", "Top 5 Birth Areas - Year 2005", 
                  "Top 5 Birth Areas - Year 2010", "Top 5 Birth Areas - Year 2015", "Top 5 Birth Areas - Year 2020")
                
                #SHOW POLYGON(s) LAYER on World Map For Two Selected Group Values WHEN Back on SelectedREGION == "Development Levels (x3)"
                # if (!is.null(input$MR_World_Map_groups) && input$MR_World_Map_groups != "2 Development Levels") { #WHEN Selected Group Value != "2 Development Levels"
                if (!is.null(input$MR_World_Map_groups) && !"Development Levels" %in% input$MR_World_Map_groups) { #WHEN "2 Development Levels" !in Selected Group Value
                  # leafletProxy(mapId = "MR_World_Map", session = session) %>% showGroup(group = input$MR_World_Map_groups)} #DEVELOPMENT LEVEL(s) => Group(s) => SHOW POLYGON(s) LAYER on World Map
                  # leafletProxy(mapId = "MR_World_Map", session = session) %>% showGroup(group = setdiff(input$MR_World_Map_groups, TopFiveBirthCountriesGroups))} #DEVELOPMENT LEVEL(s) => Group(s) => SHOW POLYGON(s) LAYER on World Map (Top FIVE BIRTH COUNTRIE(s))
                  leafletProxy(mapId = "MR_World_Map", session = session) %>% 
                    showGroup(group = Reduce(f = setdiff, x = list(input$MR_World_Map_groups, TopFiveBirthCountriesGroups, TopFiveBirthAreasGroups)))} #DEVELOPMENT LEVEL(s) => Group(s) => SHOW POLYGON(s) LAYER on World Map (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
              
                #Add LAYER(s) Control
                leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
                  addLayersControl( #Add LAYER(s) Control To Map
                    baseGroups = DevLevelsGroups, #USER CAN CHOOSE ONLY ONE LAYER
                    # overlayGroups = DevLevelsGroups, #USER CAN CHOOSE Several LAYER(s)
                    position = "bottomright", options = layersControlOptions(collapsed = FALSE)) #LAYER(s) Control => POSITION | !Expand on HOVER
                
                #DEVELOPMENT LEVEL(s) => Group(s) => CLEAR/Add LN To Map
                observeEvent(input$MR_World_Map_groups, { #WHEN SWITCH Selected Group in LAYER(s) Control

                  #Make Selected Residence Card invisible
                  shinyjs::hide(id = "MR_Selected_Residence_Card", anim = TRUE, animType = "slide", time = 0.25)
                  #Make Top Five BIRTH AREA(s) CHECKBOX invisible
                  shinyjs::hide(id = "MR_Top_Five_BIRTH_Card", anim = TRUE, animType = "slide", time = 0.25)

                  #Make ... Card invisible
                  shinyjs::hide(id = "MR_MIGRANT_STOCK_DATA_Card", anim = TRUE, animType = "slide", time = 0.25)
                  
                  #Reinitialize Reactive Value To FALSE
                  Filtered_MIGRANT_STOCKs_DATA_RV$Selected_Residence_Code_in_MIGRANT_STOCK_DATA <- FALSE
                  
                  #Reinitialize Reactive Value => DATA.FRAME => No Columns | No Rows
                  Filtered_MIGRANT_STOCKs_DATA_RV$Filtered_MIGRANT_STOCKs_DATA <- data.frame()
                  
                  #LAYER(s) Control => Selected Group Value
                  # SelectedGroup <- input$MR_World_Map_groups
                  
                  #Selected Group Value in 'DevLevelsGroups' => LN+ | Modal Box
                  if (sum(input$MR_World_Map_groups %in% DevLevelsGroups) > 0) {
                    
                    #Top FIVE BIRTH COUNTRIE(s) => Group(s)
                    # TopFiveBirthCountriesGroups <- c("Top 5 Birth Countries - Year 1990",
                    #   "Top 5 Birth Countries - Year 1995", "Top 5 Birth Countries - Year 2000", "Top 5 Birth Countries - Year 2005",
                    #   "Top 5 Birth Countries - Year 2010", "Top 5 Birth Countries - Year 2015", "Top 5 Birth Countries - Year 2020")
                    
                    #LAYER(s) Control => Selected Group Value (Top FIVE BIRTH COUNTRIE(s))
                    # SelectedGroup <- setdiff(input$MR_World_Map_groups, TopFiveBirthCountriesGroups)
                    
                    #LAYER(s) Control => Selected Group Value (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
                    SelectedGroup <- Reduce(f = setdiff, x = list(input$MR_World_Map_groups, TopFiveBirthCountriesGroups, TopFiveBirthAreasGroups))
                    
                    #Customize and control a rendered map | Remove LEGEN. (DEVELOPMENT-LEVEL(s))
                    leafletProxy(mapId = "MR_World_Map", session = session) %>% clearControls()

                    #Selected Group => INDEX in DevLevelsGroups
                    i <- match(SelectedGroup, DevLevelsGroups)

                    #Customize and control a rendered map
                    leafletProxy(mapId = "MR_World_Map", session = session) %>%
                      addLegend(position = "topright", #Add LN To Map | LN-POSITION
                                pal = fCLR[[i]], values = GeoRDATA[[i]]$NAME, opacity = 1, #COLOR(s) + VALUE(s) + COLOR(s)-OPACITY
                                title = LNHeadLINE, layerId = LAYER[i], group = DevLevelsGroups[i]) #LN-HeadLINE + LAYER-NAME + Group

                    #LEAFLET => LN+ => PROPERTIE(s)
                    runjs("let COUNTER = 0; //Initialize COUNTER
                          let X = setInterval(function() { //Call X EVERY X MILLISECONDs
                            COUNTER++; //+1
                            //LEAFLET => LN+ => PADDING+ | FONT-SIZE | FONT-FAMILY | BOX-SHADOW
                            $('.info.legend.leaflet-control').css({
                              'padding': '4px 8px', 'font-size': '12px', 'font-family': 'DM Sans', 'box-shadow': 'none'});
                            //LEAFLET => LN+ => HeadLINE => MARGIN-Top
                            $('.info.legend.leaflet-control div').css({'margin-top': '-2px'});
                            //LEAFLET => LN+ => ELEMENT(s) => MARGIN(s)+
                            $('.leaflet .legend i').css({'margin-right': '2.5px', 'margin-top': '-1.225px'});
                            //COUNTER === 10 => !Run ANYMORE
                            if (COUNTER === 10) {clearInterval(X);}}, 100);") #Run Function EVERY 100 MILLISECONDs

                    #DEVELOPMENT LEVEL(s) => SelectedGroup == "5 Development Levels" => Modal Box => SHOW
                    if (SelectedGroup == "5 Development Levels") { #WHEN Selected Group Value != "5 Development Levels"
                        leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
                          #Add EASY-BUTTON To Map
                          addEasyButton(easyButton( #Create EASY-BUTTON
                            icon = "fa-info-circle", title = "Classification Information", #BUTTON => ICON | HOVER-TEXT
                            onClick = JS("function(btn, map){$('#WorldMap_ModalBox').modal('show');}"), #SHOW Modal Box WHEN BUTTON is clicked
                            position = "topright", id = "WM_ModalBox"))} #BUTTON => POSITION | IDENTIFIER
                        #DEVELOPMENT LEVEL(s) => SelectedGroup != "5 Development Levels" => Modal Box => HIDE
                        else {runjs("$('#WM_ModalBox').parent().remove();")}
                    
                    }

                  })

              }
          
          })

        } 
      
      })
    
    #World Map Loaded => POLYGONs Rendered => Create Reactive Value 'REGIONs_POLYGONs_Rendered' == TRUE
    observe({runjs("var CheckMapLoaded = setInterval(function() { //Call Function EVERY X MILLISECONDs
                        var Map = $('#MR_World_Map').data('leaflet-map'); //Retrieve MR_World_Map
                        if (Map) { //Execute Code if Map is retrieved with success
                            var LAYER_COUNTER = 0; //Initialize LAYER COUNTER
                            Map.eachLayer(function(LAYER) {LAYER_COUNTER++;}); //+1
                            //console.log('LAYER COUNTER n°1:', LAYER_COUNTER); //Console-support
                            var CPOLYGONs_Rendered = window.COUNTRIEs_POLYGONs_Rendered; //Retrieve COUNTRIEs_POLYGONs_Rendered Global PROPERTY
                            //if (LAYER_COUNTER > 2) { //LAYER COUNTER > 2
                            if (CPOLYGONs_Rendered === undefined && LAYER_COUNTER > 2) { //WHEN We First Click on REGIONs ACTION BUTTON
                                //console.log('Condition n°1'); //Console-support
                                //Shiny.setInputValue('COUNTRIEs_POLYGONs_Rendered', false); //Reactive Value To FALSE => ClearShapes() in World Map => TRIGGER NEXT ObserveEvent() in COUNTRIEs ACTION BUTTON
                                Shiny.setInputValue('REGIONs_POLYGONs_Rendered', true); //Reactive Value To SERVER
                                //Shiny.setInputValue('REGIONs_POLYGONs_Rendered', true, {priority: 'event'}); //Reactive Value To SERVER
                                window.REGIONs_POLYGONs_Rendered = true; //Create REGIONs_POLYGONs_Rendered Global PROPERTY
                                clearInterval(CheckMapLoaded); //Once Map found and reactive value created => CheckMapLoaded => !Run ANYMORE
                                }
                             else if (CPOLYGONs_Rendered === true && LAYER_COUNTER > 265) { //WHEN We Have Clicked on COUNTRIEs ACTION BUTTON
                                      //console.log('Condition n°2'); //Console-support
                                      Shiny.setInputValue('COUNTRIEs_POLYGONs_Rendered', false); //Reactive Value To FALSE => ClearShapes() in World Map => TRIGGER NEXT ObserveEvent() in COUNTRIEs ACTION BUTTON
                                      //Shiny.setInputValue('REGIONs_POLYGONs_Rendered', true); //Reactive Value To SERVER (NECESSAIRE?)
                                      //Shiny.setInputValue('REGIONs_POLYGONs_Rendered', true, {priority: 'event'}); //Reactive Value To SERVER
                                      //window.REGIONs_POLYGONs_Rendered = true; //Create REGIONs_POLYGONs_Rendered Global PROPERTY
                                      clearInterval(CheckMapLoaded); //Once Map found and reactive value created => CheckMapLoaded => !Run ANYMORE
                                      }
                            }
                        }, 100);")}) #Run Function EVERY 100 MILLISECONDs
    
    #WAITER => HIDE | CURSOR Class To Default
    observeEvent(input$REGIONs_POLYGONs_Rendered, { #WHEN POLYGONs are rendered
      
      #Reactive Value To TRUE => WAITER => HIDE | CURSOR Class To Default
      if (input$REGIONs_POLYGONs_Rendered) {
      
        #LEAFLET => LN+ => PADDING+ | FONT-SIZE | FONT-FAMILY | BOX-SHADOW
        runjs("$('.info.legend.leaflet-control').css({
                 'padding': '4px 8px', 'font-size': '12px', 'font-family': 'DM Sans', 'box-shadow': 'none'});")
        #LEAFLET => LN+ => HeadLINE => MARGIN-Top
        runjs("$('.info.legend.leaflet-control div').css({'margin-top': '-2px'});")
        #LEAFLET => LN+ => ELEMENT(s) => MARGIN(s)+
        runjs("$('.leaflet .legend i').css({'margin-right': '2.5px', 'margin-top': '-1.225px'});")
        
        ##### HIDE WAITER #####
        World_Map_WAITER$hide()
        
        ##### HIDE WAITER #####
        REGIONs_World_Map_WAITER$hide()
        
        #CURSOR Class To Default
        session$sendCustomMessage(type = "CURSORwithinBODY", message = "default-cursor")
        
        }
      
      })
    
    })
  
  ##### World Map => POLYGON(s) => Click => Reactive Expression => Selected Residence #####
  SelectedResidence <- eventReactive(input$MR_World_Map_shape_click, {ClickedPOLYGON <- (input$MR_World_Map_shape_click)$id})
  
  ##### World Map => POLYGON(s) => Click #####
  observeEvent(input$MR_World_Map_shape_click, { #WHEN POLYGON(s) is clicked
    
    #Make Selected Residence Card visible
    shinyjs::show(id = "MR_Selected_Residence_Card", anim = TRUE, animType = "fade", time = 0.25)
    #Make Top Five BIRTH AREA(s)/COUNTRIE(s) CHECKBOX visible
    shinyjs::show(id = "MR_Top_Five_BIRTH_Card", anim = TRUE, animType = "fade", time = 0.25)
    
    #Make ... Card visible
    shinyjs::show(id = "MR_MIGRANT_STOCK_DATA_Card", anim = TRUE, animType = "fade", time = 0.25)
    
    #Disable ...
    # shinyjs::disable(id = "MR_BIRTH_View_Choice_REGIONs_CHOICE")
    
    #Top Five BIRTH AREA(s)/COUNTRIE(s) CHECKBOX => Replace CHECKBOX Value => TRUE To FALSE
    if (input$MR_Top_Five_BIRTH) {updateCheckboxInput(session = session, inputId = "MR_Top_Five_BIRTH", value = FALSE)}

    #Top Five BIRTH AREA(s)/COUNTRIE(s) CHECKBOX => MARGIN(s)+
    runjs("$('.shiny-input-container .checkbox').css({'margin-bottom': '0px', 'height': '19.986px'});")
    
    #World Map => POLYGON(s) => Click => Selected Residence
    Selected_Residence <- SelectedResidence()

    #Replace Selected Residence Value
    if (startsWith(x = Selected_Residence, prefix = "<b>")) {
      Selected_Residence <- sub(pattern = ".*<b>Development Level: </b>", replacement = "", x = Selected_Residence)}
    else if (Selected_Residence == "#FF9900") {Selected_Residence <- "Least Developed Countries"}
    else if (Selected_Residence == "#FFCC00") {Selected_Residence <- "Less Developed Countries*"}
    else if (Selected_Residence == "#339900") {Selected_Residence <- "More Developed Countries"}
    else if (Selected_Residence == "#999999") {Selected_Residence <- "Other Countries/Territories"}
    
    #Replace Selected Residence Value (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
    if (grepl(pattern = " \\d{4}$", x = Selected_Residence)) {
      Selected_Residence <- sub(pattern = " \\d{4}$", replacement = "", x = Selected_Residence)}

    #CURSOR Class To WAIT
    # session$sendCustomMessage(type = "CURSORwithinBODY", message = "wait-cursor")
    
    #Top FIVE BIRTH COUNTRIE(s) => Group(s)
    TopFiveBirthCountriesGroups <- c("Top 5 Birth Countries - Year 1990", 
      "Top 5 Birth Countries - Year 1995", "Top 5 Birth Countries - Year 2000", "Top 5 Birth Countries - Year 2005", 
      "Top 5 Birth Countries - Year 2010", "Top 5 Birth Countries - Year 2015", "Top 5 Birth Countries - Year 2020")      

    #Top FIVE BIRTH AREA(s) => Group(s)
    TopFiveBirthAreasGroups <- c("Top 5 Birth Areas - Year 1990", 
      "Top 5 Birth Areas - Year 1995", "Top 5 Birth Areas - Year 2000", "Top 5 Birth Areas - Year 2005", 
      "Top 5 Birth Areas - Year 2010", "Top 5 Birth Areas - Year 2015", "Top 5 Birth Areas - Year 2020")

    #Reactive Value == "COUNTRIEs" => GeoDATA | World Map Attributes
    if (Selected_Residence_RV$MR_Residence_View_Choice == "COUNTRIEs") {
      
      #Top FIVE BIRTH COUNTRIE(s) => Group(s)
      # TopFiveBirthCountriesGroups <- c("Top 5 Birth Countries - Year 1990", 
      #   "Top 5 Birth Countries - Year 1995", "Top 5 Birth Countries - Year 2000", "Top 5 Birth Countries - Year 2005", 
      #   "Top 5 Birth Countries - Year 2010", "Top 5 Birth Countries - Year 2015", "Top 5 Birth Countries - Year 2020")
      
      #Clear World Map from Top FIVE BIRTH AREA(s)/COUNTRIE(s)
      leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
        # clearShapes() %>% #Remove all POLYGONs from World Map (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
        clearGroup(group = TopFiveBirthCountriesGroups) %>% #Remove POLYGONs from World Map (Top FIVE BIRTH COUNTRIE(s))
        clearGroup(group = TopFiveBirthAreasGroups) %>% #Remove POLYGONs from World Map (Top FIVE BIRTH AREA(s))
        clearControls() %>% #Remove LEGEN. (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
        removeLayersControl() #Remove LAYER(s) Control (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
      
      #Activate Top Five BIRTH COUNTRIE(s) CHECKBOX
      shinyjs::enable(id = "MR_Top_Five_BIRTH")
      
      Geo <- GeoDATA #GeoDATA
      POLYGONs_LAYER <- Geo$VISUALIZATION_NAME #World Map => POLYGONs => LAYER-NAME
      Group <- NULL #World Map => Group+
      COLOR <- "transparent" #World Map => Stroke Attributes
      STROKE_ATTRIBUTEs <- c(1.5, 0.75, 1.5, 0.75) #World Map => Stroke Attributes
      Fill_COLOR <- c("transparent", "#00CC00") #World Map => Fill Attributes => !Selected Residence | Selected Residence
      Fill_ATTRIBUTEs <- c(0.25, 0.25) #World Map => Fill Attributes
      SMOOTH_FACTOR <- 1 #World Map => POLYLINE Simplification
      LHOVER <- Geo$Label #World Map => HOVER TEXT
      HOVER_Fill_COLOR <- "#0000FF" #HIGHLIGHT-Options on Mouse OVER => Fill Attributes
      HOVER_Fill_ATTRIBUTEs <- 0.25 #HIGHLIGHT-Options on Mouse OVER => Fill Attributes

      #BARPLOT(s) => COUNTRIEs-COUNTRIEs | COUNTRIEs-REGIONs
      # Selected_Residence_Code <- DATA$M49_CODE[which(DATA$NAME == Selected_Residence)]
      #BARPLOT(s) => COUNTRIEs-COUNTRIEs | COUNTRIEs-REGIONs
      Selected_Residence_Code <- DATA %>% filter(VISUALIZATION_NAME == Selected_Residence) %>% pull(M49_CODE)}
    #Reactive Value == "REGIONs" => Selected REGION | GeoRDATA | World Map Attributes
    else if (Selected_Residence_RV$MR_Residence_View_Choice == "REGIONs") {
      #REGIONs CHOICE RADIO BUTTONs => Selected REGION
      Selected_REGION <- input$MRR_REGIONs_CHOICE
      #Selected REGION => GeoRDATA | World Map Attributes
      if (Selected_REGION == "Continental Regions") {
        Geo <- GeoRDATA_ContinentalREGIONs #GeoRDATA
        RDATA <- RDATA_ContinentalREGIONs #RDATA
        POLYGONs_LAYER <- Geo$NAME #World Map => POLYGONs => LAYER-NAME
        Group <- NULL} #World Map => Group+
      else if (Selected_REGION == "Continental Sub-Regions and Intermediate Regions") {
        
        #Clear World Map from Top FIVE BIRTH AREA(s)/COUNTRIE(s)
        leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
          # clearShapes() %>% #Remove all POLYGONs from World Map (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
          clearGroup(group = TopFiveBirthCountriesGroups) %>% #Remove POLYGONs from World Map (Top FIVE BIRTH COUNTRIE(s))
          clearGroup(group = TopFiveBirthAreasGroups) %>% #Remove POLYGONs from World Map (Top FIVE BIRTH AREA(s))
          # clearControls() %>% #Remove LEGEN. (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
          removeControl(layerId = "TopFiveBirthCountries") %>% #Remove LEGEN. (Top FIVE BIRTH COUNTRIE(s))
          removeControl(layerId = "TopFiveBirthAreas") %>% #Remove LEGEN. (Top FIVE BIRTH AREA(s))
          removeLayersControl() #Remove LAYER(s) Control (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
        
        #Activate Top Five BIRTH AREA(s) CHECKBOX
        shinyjs::enable(id = "MR_Top_Five_BIRTH")
        
        Geo <- GeoRDATA_ContinentalSIREGIONs #GeoRDATA
        RDATA <- RDATA_ContinentalSIREGIONs #RDATA
        POLYGONs_LAYER <- Geo$NAME #World Map => POLYGONs => LAYER-NAME
        Group <- NULL} #World Map => Group+
      else if (Selected_REGION == "Geographic Regions") {
        Geo <- GeoRDATA_GeoREGIONs #GeoRDATA
        RDATA <- RDATA_GeoREGIONs #RDATA
        POLYGONs_LAYER <- Geo$NAME #World Map => POLYGONs => LAYER-NAME
        Group <- NULL} #World Map => Group+
      else if (Selected_REGION == "Development Levels (x3)") {
        #LAYER(s) Control => Selected Group Value
        Selected_Group <- input$MR_World_Map_groups
        #Selected Group Value => GeoRDATA | World Map Attributes
        if (Selected_Group == "2 Development Levels") {
          Geo <- GeoRDATA_MoreLess #GeoRDATA
          RDATA <- RDATA_MoreLess #RDATA
          POLYGONs_LAYER <- Geo$LabelR #World Map => POLYGONs => LAYER-NAME
          Group <- "2 Development Levels"} #World Map => Group+
        else if (Selected_Group == "3 Development Levels") {
          Geo <- GeoRDATA_MoreLessLeast #GeoRDATA
          RDATA <- RDATA_MoreLessLeast #RDATA
          POLYGONs_LAYER <- Geo$CLRCode #World Map => POLYGONs => LAYER-NAME
          Group <- "3 Development Levels"} #World Map => Group+
        else if (Selected_Group == "5 Development Levels") {
          Geo <- GeoRDATA_LDC_LLDC_SIDS #GeoRDATA
          RDATA <- RDATA_LDC_LLDC_SIDS #RDATA
          POLYGONs_LAYER <- Geo$NAME #World Map => POLYGONs => LAYER-NAME
          Group <- "5 Development Levels"} #World Map => Group+
        # Group <- "Selected Residence" #World Map => Group+
        }
      else if (Selected_REGION == "Income Levels") {
        Geo <- GeoRDATA_IncomeLevels #GeoRDATA
        RDATA <- RDATA_IncomeLevels #RDATA
        POLYGONs_LAYER <- Geo$NAME #World Map => POLYGONs => LAYER-NAME
        Group <- NULL} #World Map => Group+
      COLOR <- "#CCCCCC" #World Map => Stroke Attributes
      STROKE_ATTRIBUTEs <- c(0.5, 0.5, 1.5, 0.75) #World Map => Stroke Attributes
      #Maps Values To Colors
      fCLR <- colorFactor(
        #Colors ordered with factor-level(s) in NAME
        palette = Geo$CLRCode[order(factor(Geo$NAME, levels = levels(Geo$NAME)))],
        #Possible Values | NOT-AVAILABLE COLOR
        domain = Geo$NAME, na.color = "#999999")
      Fill_COLOR <- list(fCLR(Geo$NAME), fCLR(Geo$NAME)) #World Map => Fill Attributes
      Fill_ATTRIBUTEs <- c(0.75, 1) #World Map => Fill Attributes
      SMOOTH_FACTOR <- 1.25 #World Map => POLYLINE Simplification
      LHOVER <- Geo$LabelR #World Map => HOVER TEXT
      HOVER_Fill_COLOR <- NULL #HIGHLIGHT-Options on Mouse OVER => Fill Attributes
      HOVER_Fill_ATTRIBUTEs <- 1 #HIGHLIGHT-Options on Mouse OVER => Fill Attributes

      #BARPLOT(s) => #REGIONs-COUNTRIEs | REGIONs-REGIONs
      Selected_Residence_Code <- RDATA %>% filter(NAME == Selected_Residence) %>% pull(CODE)}

    #Draw World Administrative Boundaries => Selected Residence
    leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
      addPolygons(data = Geo, layerId = POLYGONs_LAYER, group = Group, #Add POLYGONs To Map | Group+
                  stroke = TRUE, #Stroke Attributes
                  # color = ifelse(POLYGONs_LAYER %in% SelectedResidence(), "#00CC00", COLOR), #Stroke Attributes
                  color = ifelse(POLYGONs_LAYER %in% 
                                   ifelse(grepl(pattern = " \\d{4}$", x = SelectedResidence()), 
                                          sub(pattern = " \\d{4}$", replacement = "", x = SelectedResidence()), SelectedResidence()), "#00CC00", COLOR), #Stroke Attributes (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
                  # weight = ifelse(POLYGONs_LAYER %in% SelectedResidence(), STROKE_ATTRIBUTEs[3], STROKE_ATTRIBUTEs[1]), #Stroke Attributes
                  weight = ifelse(POLYGONs_LAYER %in% 
                                    ifelse(grepl(pattern = " \\d{4}$", x = SelectedResidence()), 
                                           sub(pattern = " \\d{4}$", replacement = "", x = SelectedResidence()), SelectedResidence()), STROKE_ATTRIBUTEs[3], STROKE_ATTRIBUTEs[1]), #Stroke Attributes (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
                  # opacity = ifelse(POLYGONs_LAYER %in% SelectedResidence(), STROKE_ATTRIBUTEs[4], STROKE_ATTRIBUTEs[2]), #Stroke Attributes
                  opacity = ifelse(POLYGONs_LAYER %in% 
                                     ifelse(grepl(pattern = " \\d{4}$", x = SelectedResidence()), 
                                            sub(pattern = " \\d{4}$", replacement = "", x = SelectedResidence()), SelectedResidence()), STROKE_ATTRIBUTEs[4], STROKE_ATTRIBUTEs[2]), #Stroke Attributes (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
                  fill = TRUE, #Fill Attributes
                  # fillColor = ifelse(POLYGONs_LAYER %in% SelectedResidence(), Fill_COLOR[[2]], Fill_COLOR[[1]]), #Fill Attributes
                  fillColor = ifelse(POLYGONs_LAYER %in% 
                                       ifelse(grepl(pattern = " \\d{4}$", x = SelectedResidence()), 
                                              sub(pattern = " \\d{4}$", replacement = "", x = SelectedResidence()), SelectedResidence()), Fill_COLOR[[2]], Fill_COLOR[[1]]), #Fill Attributes (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
                  # fillOpacity = ifelse(POLYGONs_LAYER %in% SelectedResidence(), Fill_ATTRIBUTEs[2], Fill_ATTRIBUTEs[1]), #Fill Attributes
                  fillOpacity = ifelse(POLYGONs_LAYER %in% 
                                         ifelse(grepl(pattern = " \\d{4}$", x = SelectedResidence()), 
                                                sub(pattern = " \\d{4}$", replacement = "", x = SelectedResidence()), SelectedResidence()), Fill_ATTRIBUTEs[2], Fill_ATTRIBUTEs[1]), #Fill Attributes (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
                  smoothFactor = SMOOTH_FACTOR, label = lapply(LHOVER, HTML), #POLYLINE Simplification | HOVER TEXT
                  highlightOptions = highlightOptions( #HIGHLIGHT-Options on Mouse OVER
                    stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75, #Stroke Attributes
                    fill = TRUE, fillColor = HOVER_Fill_COLOR, fillOpacity = HOVER_Fill_ATTRIBUTEs)) # %>% #Fill Attributes
      #Execute JavaScript Code When Map is Rendered => Create a SHINY-SERVER-INPUT => Reactive Value To SERVER
      # onRender(JS("function(el, x) {Shiny.setInputValue('Selected_Residence_Rendered', true);}"))

    #CURSOR Class To Default
    # observeEvent(input$Selected_Residence_Rendered, {session$sendCustomMessage(type = "CURSORwithinBODY", message = "default-cursor")})

    #Make Selected Residence Card visible
    # observeEvent(input$Selected_Residence_Rendered, {shinyjs::show(id = "MR_Selected_Residence_Card", anim = TRUE, animType = "fade", time = 0.25)})

    #Make Top Five BIRTH AREA(s)/COUNTRIE(s) CHECKBOX Card visible
    # observeEvent(input$Selected_Residence_Rendered, {shinyjs::show(id = "MR_Top_Five_BIRTH_Card", anim = TRUE, animType = "fade", time = 0.25)})

    #Activate Top Five BIRTH COUNTRIE(s) CHECKBOX
    # observeEvent(input$Selected_Residence_Rendered, {if (Selected_Residence_RV$MR_Residence_View_Choice == "COUNTRIEs") {shinyjs::enable(id = "MR_Top_Five_BIRTH")}})
    
    #Activate Top Five BIRTH AREA(s) CHECKBOX
    # observeEvent(input$Selected_Residence_Rendered, {if (Selected_Residence_RV$MR_Residence_View_Choice == "REGIONs") {shinyjs::enable(id = "MR_Top_Five_BIRTH")}})
    
    #Selected Residence Code in iMIGRANT_STOCK_DATA_SHAREs => Reactive Value 'Selected_Residence_Code_in_MIGRANT_STOCK_DATA' == TRUE
    if (as.numeric(Selected_Residence_Code) %in% iMIGRANT_STOCK_DATA_SHAREs$ResidenceCode) {
      Filtered_MIGRANT_STOCKs_DATA_RV$Selected_Residence_Code_in_MIGRANT_STOCK_DATA <- TRUE} else {Filtered_MIGRANT_STOCKs_DATA_RV$Selected_Residence_Code_in_MIGRANT_STOCK_DATA <- FALSE}

    #Reactive Value 'Selected_Residence_Code_in_MIGRANT_STOCK_DATA' == TRUE => MIGRANT STOCK DATA => FILTER => RESIDENCE
    if (Filtered_MIGRANT_STOCKs_DATA_RV$Selected_Residence_Code_in_MIGRANT_STOCK_DATA) {
      #MIGRANT STOCK DATA => FILTER => RESIDENCE
      FilteredMIGRANT_STOCKs_DATA <- iMIGRANT_STOCK_DATA_SHAREs %>% 
        filter(ResidenceCode == as.numeric(Selected_Residence_Code))
      #Reactive Value 'Filtered_MIGRANT_STOCKs_DATA' == FilteredMIGRANT_STOCKs_DATA
      Filtered_MIGRANT_STOCKs_DATA_RV$Filtered_MIGRANT_STOCKs_DATA <- FilteredMIGRANT_STOCKs_DATA} else {Filtered_MIGRANT_STOCKs_DATA_RV$Filtered_MIGRANT_STOCKs_DATA <- data.frame()}

    })
  
  ##### Selected Residence #####
  output$MR_Selected_Residence <- renderText({

    #World Map => POLYGON(s) => Click => Selected Residence
    Selected_Residence <- SelectedResidence()

    #Replace Selected Residence Value
    if (startsWith(x = Selected_Residence, prefix = "<b>")) {
      Selected_Residence <- sub(pattern = ".*<b>Development Level: </b>", replacement = "", x = Selected_Residence)}
    else if (Selected_Residence == "#FF9900") {Selected_Residence <- "Least Developed Countries"}
    else if (Selected_Residence == "#FFCC00") {Selected_Residence <- "Less Developed Countries*"}
    else if (Selected_Residence == "#339900") {Selected_Residence <- "More Developed Countries"}
    else if (Selected_Residence == "#999999") {Selected_Residence <- "Other Countries/Territories"}

    #Replace Selected Residence Value (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
    if (grepl(pattern = " \\d{4}$", x = Selected_Residence)) {
      Selected_Residence <- sub(pattern = " \\d{4}$", replacement = "", x = Selected_Residence)}
    
    #Selected Residence Value in Card
    # HTML(Selected_Residence)
    #Selected Residence Value in Card
    HTML(paste("<b>Selected Residence:</b>", Selected_Residence, sep = " "))

    })

  ##### CHECKBOX (Top Five BIRTH AREA(s)/COUNTRIE(s)) => Click #####
  observeEvent(input$MR_Top_Five_BIRTH, { #WHEN CHECKBOX is clicked
    
    #CHECKBOX Value To TRUE => #Draw World Administrative Boundaries => Top Five BIRTH AREA(s)/COUNTRIE(s)
    if (input$MR_Top_Five_BIRTH) {

      #Disable Top Five BIRTH AREA(s)/COUNTRIE(s) CHECKBOX
      shinyjs::disable(id = "MR_Top_Five_BIRTH")
      
      #World Map => POLYGON(s) => Click => Selected Residence
      Selected_Residence <- SelectedResidence()
      
      #Replace Selected Residence Value (Top FIVE BIRTH AREA(s)/COUNTRIE(s))
      if (grepl(pattern = " \\d{4}$", x = Selected_Residence)) {
        Selected_Residence <- sub(pattern = " \\d{4}$", replacement = "", x = Selected_Residence)}
      
      #MIGRANT STOCK DATA => Filtered => RESIDENCE (Reactive Value 'Filtered_MIGRANT_STOCKs_DATA')
      FilteredMIGRANT_STOCKs_DATA <- Filtered_MIGRANT_STOCKs_DATA_RV$Filtered_MIGRANT_STOCKs_DATA
      
      #MIGRANT STOCK => FILTER => BIRTH == World | All YEAR(s)
      FilteredMIGRANT_STOCKs_DATA_World <- FilteredMIGRANT_STOCKs_DATA %>%
        filter(BirthCode == 900, #BIRTH == World
               Year %in% c("1990", "1995", "2000", "2005", "2010", "2015", "2020")) %>% distinct(BirthCode, Year, .keep_all = TRUE)
      
      #Reactive Value == "COUNTRIEs" => MIGRANT STOCK => FILTER => BIRTH
      if (Selected_Residence_RV$MR_Residence_View_Choice == "COUNTRIEs") {
        #MIGRANT STOCK => FILTER => BIRTH == All COUNTRIE(s) | All YEAR(s)
        FilteredMIGRANT_STOCKs_DATA_COUNTRIEs <- FilteredMIGRANT_STOCKs_DATA %>%
          filter(BirthCode < 900, #BIRTH == All COUNTRIE(s)
                 Year %in% c("1990", "1995", "2000", "2005", "2010", "2015", "2020")) %>% distinct(BirthCode, Year, .keep_all = TRUE)} 
      #Reactive Value == "REGIONs" => Selected REGION | MIGRANT STOCK
      else if (Selected_Residence_RV$MR_Residence_View_Choice == "REGIONs") {
        #REGIONs CHOICE RADIO BUTTONs => Selected REGION
        Selected_REGION <- input$MRR_REGIONs_CHOICE
        #Selected REGION => MIGRANT STOCK => FILTER => BIRTH
        if (Selected_REGION == "Continental Sub-Regions and Intermediate Regions") {
          #Continental SIREGIONs CODEs
          ContinentalSIREGIONs_CODEs <- c(910, 911, 912, 913, 914, 
                                          5500, 906, 920, 5501, 922, 
                                          923, 924, 925, 926,
                                          915, 916, 931, 
                                          905, #NORTHERN AMERICA => "Northern America"
                                          927, 928, 954, 957)
          #MIGRANT STOCK => FILTER => BIRTH == All Continental SIREGION(s) | All YEAR(s)
          FilteredMIGRANT_STOCKs_DATA_ContinentalSIREGIONs <- FilteredMIGRANT_STOCKs_DATA %>%
            filter(BirthCode %in% ContinentalSIREGIONs_CODEs, #BIRTH == All Continental SIREGION(s)
                   Year %in% c("1990", "1995", "2000", "2005", "2010", "2015", "2020")) %>% distinct(BirthCode, Year, .keep_all = TRUE)}}
      
      #All Possible YEAR Values
      YEARs <- c("1990", "1995", "2000", "2005", "2010", "2015", "2020")
      
      #Top FIVE BIRTH COUNTRIE(s) => Group(s)
      TopFiveBirthCountriesGroups <- list("1990" = "Top 5 Birth Countries - Year 1990", 
        "1995" = "Top 5 Birth Countries - Year 1995", "2000" = "Top 5 Birth Countries - Year 2000", 
        "2005" = "Top 5 Birth Countries - Year 2005", "2010" = "Top 5 Birth Countries - Year 2010", 
        "2015" = "Top 5 Birth Countries - Year 2015", "2020" = "Top 5 Birth Countries - Year 2020")
      
      #Top FIVE BIRTH AREA(s) => Group(s)
      TopFiveBirthAreasGroups <- list("1990" = "Top 5 Birth Areas - Year 1990", 
        "1995" = "Top 5 Birth Areas - Year 1995", "2000" = "Top 5 Birth Areas - Year 2000", 
        "2005" = "Top 5 Birth Areas - Year 2005", "2010" = "Top 5 Birth Areas - Year 2010", 
        "2015" = "Top 5 Birth Areas - Year 2015", "2020" = "Top 5 Birth Areas - Year 2020")
      
      #Draw World Administrative Boundaries
      for (YEAR in YEARs) {
      
        #MIGRANT STOCK => Filtered => BIRTH == World => YEAR
        MIGRANT_STOCKs_DATA_World_YEAR <- FilteredMIGRANT_STOCKs_DATA_World %>% filter(Year == YEAR)
      
        #Reactive Value == "COUNTRIEs" => Draw World Administrative Boundaries => Top Five BIRTH COUNTRIE(s)
        if (Selected_Residence_RV$MR_Residence_View_Choice == "COUNTRIEs") {
          
          #MIGRANT STOCK => Filtered => BIRTH == All COUNTRIE(s) => YEAR | ORDER | Top FIVE | RANK 
          FilteredMIGRANT_STOCKs_DATA_COUNTRIEs_YEAR <- FilteredMIGRANT_STOCKs_DATA_COUNTRIEs %>%
            filter(Year == YEAR) %>% arrange(desc(iMIGRANT_Stock_Total)) %>% slice(1:5) %>% mutate(Rank = row_number()) %>% select(1:5, 12, 6:11)
          
          #Top FIVE BIRTH COUNTRIE(s) => YEAR
          Top_BIRTH_COUNTRIEs_YEAR <- FilteredMIGRANT_STOCKs_DATA_COUNTRIEs_YEAR %>% pull(Birth)
          # Top_BIRTH_COUNTRIEs_YEAR <- toupper(FilteredMIGRANT_STOCKs_DATA_COUNTRIEs_YEAR %>% pull(Birth))
          
          #Calculate Centroid To Centroid Distance From BIRTH COUNTRIE(s) To Residence => YEAR
          DISTANCEs_YEAR <- DATA %>% 
            filter(VISUALIZATION_NAME %in% c(Selected_Residence, Top_BIRTH_COUNTRIEs_YEAR)) %>% select(1:5, 30:31) %>%
            #Great Circle Distance => Method => Haversine (Spherical Earth => !Ellipsoid)
            # mutate(Distance_To_Residence = round(geosphere::distHaversine( 
            #Great Circle Distance => Method => Meeus (Ellipsoid => Geodetic)
            # mutate(Distance_To_Residence = round(geosphere::distMeeus( 
            #Great Circle Distance => Method => Ellipsoid (Geodesic)
            # mutate(Distance_To_Residence = round(geosphere::distGeo( 
            #Great Circle Distance => Method => VINCENTY (Ellipsoid)
            mutate(Distance_To_Residence = round(geosphere::distVincentyEllipsoid( 
              c(Centroid_X[which(VISUALIZATION_NAME == Selected_Residence)], Centroid_Y[which(VISUALIZATION_NAME == Selected_Residence)]), cbind(Centroid_X, Centroid_Y)) / 1000, 3))
          
          #DATA MANIPULATION(s) on GeoDATA
          GeoDATA_COUNTRIEs_YEAR <- left_join(GeoDATA, FilteredMIGRANT_STOCKs_DATA_COUNTRIEs_YEAR, by = c("VISUALIZATION_NAME" = "Birth")) %>%
            mutate(
              #Replace Values in Label
              Label = case_when(
                #Replace Values in Label => Selected Residence
                VISUALIZATION_NAME == Selected_Residence ~ paste0("<b>Residence: </b>", NAME, "<br>",
                                                                  "<b>Alpha-2 Code (ISO2): </b>", ISO2, "<br>",
                                                                  "<b>Alpha-3 Code (ISO3): </b>", ISO3, "<br>",
                                                                  "<b>Numeric Code (M49 Code): </b>", M49_CODE, "<br>",
                                                                  "<b>Year: </b>", MIGRANT_STOCKs_DATA_World_YEAR$Year, "<br>",
                                                                  "<b>International MIGRANT Stock (Both Sexes Combined): </b>", format(x = MIGRANT_STOCKs_DATA_World_YEAR$iMIGRANT_Stock_Total, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(MIGRANT_STOCKs_DATA_World_YEAR$Share_of_iMIGRANT_Stock_Total, 3), "%)", "<br>",
                                                                  "<b>International MIGRANT Stock (Males): </b>", format(x = MIGRANT_STOCKs_DATA_World_YEAR$iMIGRANT_Stock_Males, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(MIGRANT_STOCKs_DATA_World_YEAR$Share_of_iMIGRANT_Stock_Males, 3), "%)", "<br>",
                                                                  "<b>International MIGRANT Stock (Females): </b>", format(x = MIGRANT_STOCKs_DATA_World_YEAR$iMIGRANT_Stock_Females, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(MIGRANT_STOCKs_DATA_World_YEAR$Share_of_iMIGRANT_Stock_Females, 3), "%)", "<br>",
                                                                  "<b>Top 5 Birth Countries (Cumulative Share, Both Sexes Combined Stock): </b>", round(sum(FilteredMIGRANT_STOCKs_DATA_COUNTRIEs_YEAR$Share_of_iMIGRANT_Stock_Total), 3), "%", "<br>",
                                                                  "<b>Top 5 Birth Countries (Cumulative Share, Males Stock): </b>", round(sum(FilteredMIGRANT_STOCKs_DATA_COUNTRIEs_YEAR$Share_of_iMIGRANT_Stock_Males), 3), "%", "<br>",
                                                                  "<b>Top 5 Birth Countries (Cumulative Share, Females Stock): </b>", round(sum(FilteredMIGRANT_STOCKs_DATA_COUNTRIEs_YEAR$Share_of_iMIGRANT_Stock_Females), 3), "%"),
                #Replace Values in Label => Top BIRTH COUNTRIE(s)
                VISUALIZATION_NAME %in% Top_BIRTH_COUNTRIEs_YEAR ~ paste0("<b>Birth: </b>", NAME, "<br>",
                                                                          "<b>Alpha-2 Code (ISO2): </b>", ISO2, "<br>",
                                                                          "<b>Alpha-3 Code (ISO3): </b>", ISO3, "<br>",
                                                                          "<b>Numeric Code (M49 Code): </b>", M49_CODE, "<br>",
                                                                          "<b>Rank (</b>", YEAR, "<b>): </b>", Rank, "<br>",
                                                                          "<b>International MIGRANT Stock (Both Sexes Combined): </b>", format(x = iMIGRANT_Stock_Total, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(Share_of_iMIGRANT_Stock_Total, 3), "%)", "<br>",
                                                                          "<b>International MIGRANT Stock (Males): </b>", format(x = iMIGRANT_Stock_Males, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(Share_of_iMIGRANT_Stock_Males, 3), "%)", "<br>",
                                                                          "<b>International MIGRANT Stock (Females): </b>", format(x = iMIGRANT_Stock_Females, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(Share_of_iMIGRANT_Stock_Females, 3), "%)","<br>",
                                                                          "<b>Distance To Residence (Centroid To Centroid): </b>", DISTANCEs_YEAR$Distance_To_Residence[match(VISUALIZATION_NAME, DISTANCEs_YEAR$VISUALIZATION_NAME)], " km"), TRUE ~ Label),
              #Replace Values in VISUALIZATION_NAME
              # VISUALIZATION_NAME = paste(VISUALIZATION_NAME, YEAR, sep = " ")) %>% select(1:37)
              #Replace Values in VISUALIZATION_NAME
              # VISUALIZATION_NAME = ifelse(YEAR != 1990, paste(VISUALIZATION_NAME, YEAR, sep = " "), VISUALIZATION_NAME)) %>% select(1:37)
              #Replace Values in VISUALIZATION_NAME
              VISUALIZATION_NAME = case_when(YEAR != 1990 ~ paste(VISUALIZATION_NAME, YEAR, sep = " "), TRUE ~ VISUALIZATION_NAME)) %>% select(1:37)
          
          Geo <- GeoDATA_COUNTRIEs_YEAR #GeoDATA
          POLYGONs_LAYER <- Geo$VISUALIZATION_NAME #World Map => POLYGONs => LAYER-NAME
          Group <- TopFiveBirthCountriesGroups[[YEAR]] #World Map => Group+
          # Selected_Residence_YEAR <- paste(Selected_Residence, YEAR, sep = " ") #Replace Selected Residence Value
          Selected_Residence_YEAR <- ifelse(YEAR != 1990, paste(Selected_Residence, YEAR, sep = " "), Selected_Residence) #Replace Selected Residence Value
          # Top_BIRTH_COUNTRIEs_YEAR <- paste(Top_BIRTH_COUNTRIEs_YEAR, YEAR, sep = " ") #Replace Top BIRTH COUNTRIE(s) Values
          # Top_BIRTH_COUNTRIEs_YEAR <- ifelse(YEAR != 1990, paste(Top_BIRTH_COUNTRIEs_YEAR, YEAR, sep = " "), Top_BIRTH_COUNTRIEs_YEAR) #Replace Top BIRTH COUNTRIE(s) Values
          Top_BIRTH_COUNTRIEs_YEAR <- if (YEAR != 1990) {paste(Top_BIRTH_COUNTRIEs_YEAR, YEAR, sep = " ")} else {Top_BIRTH_COUNTRIEs_YEAR} #Replace Top BIRTH COUNTRIE(s) Values
          COLOR <- "transparent" #World Map => Stroke Attributes
          STROKE_ATTRIBUTEs <- c(1.5, 0.75, 1.5, 0.75) #World Map => Stroke Attributes
          Fill_COLOR <- c("transparent", "#00CC00", "#FED52E") #World Map => Fill Attributes => !Selected Residence | Selected Residence | Top FIVE COUNTRIE(s)
          Fill_ATTRIBUTEs <- c(0.25, 0.25) #World Map => Fill Attributes
          SMOOTH_FACTOR <- 1 #World Map => POLYLINE Simplification
          LHOVER <- Geo$Label #World Map => HOVER TEXT
          HOVER_Fill_COLOR <- "#0000FF" #HIGHLIGHT-Options on Mouse OVER => Fill Attributes
          HOVER_Fill_ATTRIBUTEs <- 0.25 #HIGHLIGHT-Options on Mouse OVER => Fill Attributes
          
          #Draw World Administrative Boundaries => Top Five BIRTH COUNTRIE(s)
          leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
            addPolygons(data = Geo, layerId = POLYGONs_LAYER, group = Group, #Add POLYGONs To Map | Group+
                        stroke = TRUE, #Stroke Attributes
                        color = ifelse(POLYGONs_LAYER %in% Selected_Residence_YEAR, "#00CC00",
                                       ifelse(POLYGONs_LAYER %in% Top_BIRTH_COUNTRIEs_YEAR, "#FED52E", COLOR)), #Stroke Attributes
                        weight = ifelse(POLYGONs_LAYER %in% c(Selected_Residence_YEAR, Top_BIRTH_COUNTRIEs_YEAR), STROKE_ATTRIBUTEs[3], STROKE_ATTRIBUTEs[1]), #Stroke Attributes
                        opacity = ifelse(POLYGONs_LAYER %in% c(Selected_Residence_YEAR, Top_BIRTH_COUNTRIEs_YEAR), STROKE_ATTRIBUTEs[4], STROKE_ATTRIBUTEs[2]), #Stroke Attributes
                        fill = TRUE, #Fill Attributes
                        fillColor = ifelse(POLYGONs_LAYER %in% Selected_Residence_YEAR, Fill_COLOR[[2]],
                                           ifelse(POLYGONs_LAYER %in% Top_BIRTH_COUNTRIEs_YEAR, Fill_COLOR[[3]], Fill_COLOR[[1]])), #Fill Attributes
                        fillOpacity = ifelse(POLYGONs_LAYER %in% c(Selected_Residence_YEAR, Top_BIRTH_COUNTRIEs_YEAR), Fill_ATTRIBUTEs[2], Fill_ATTRIBUTEs[1]), #Fill Attributes
                        smoothFactor = SMOOTH_FACTOR, label = lapply(LHOVER, HTML), #POLYLINE Simplification | HOVER TEXT
                        highlightOptions = highlightOptions( #HIGHLIGHT-Options on Mouse OVER
                          stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75, #Stroke Attributes
                          fill = TRUE, fillColor = HOVER_Fill_COLOR, fillOpacity = HOVER_Fill_ATTRIBUTEs)) #Fill Attributes
          
          #Top FIVE BIRTH COUNTRIE(s) => Group(s) => HIDE POLYGON(s) LAYER FROM World Map => Keep ONLY => "Top 5 Birth Countries - Year 1990" (YEAR == "1990")
          if (YEAR != "1990") {leafletProxy(mapId = "MR_World_Map", session = session) %>% hideGroup(group = TopFiveBirthCountriesGroups[[YEAR]])}
          
          #DEVELOPMENT LEVEL(s) => Group(s)
          DevLevelsGroups <- c("2 Development Levels", "3 Development Levels", "5 Development Levels")
          
          #SHOW POLYGON(s) LAYER on World Map For Six Selected Group Values WHEN CHECKBOX Value is Back To TRUE
          # if (!is.null(input$MR_World_Map_groups) && input$MR_World_Map_groups != "Top 5 Birth Countries - Year 1990") { #WHEN Selected Group Value != "Top 5 Birth Countries - Year 1990"
          if (!is.null(input$MR_World_Map_groups) && !"Top 5 Birth Countries - Year 1990" %in% input$MR_World_Map_groups) { #WHEN "Top 5 Birth Countries - Year 1990" !in Selected Group Value
            # leafletProxy(mapId = "MR_World_Map", session = session) %>% showGroup(group = input$MR_World_Map_groups)} #Top FIVE BIRTH COUNTRIE(s) => Group(s) => SHOW POLYGON(s) LAYER on World Map
            # leafletProxy(mapId = "MR_World_Map", session = session) %>% showGroup(group = setdiff(input$MR_World_Map_groups, DevLevelsGroups))} #Top FIVE BIRTH COUNTRIE(s) => Group(s) => SHOW POLYGON(s) LAYER on World Map
            leafletProxy(mapId = "MR_World_Map", session = session) %>% 
              showGroup(group = Reduce(f = setdiff, x = list(input$MR_World_Map_groups, DevLevelsGroups, TopFiveBirthAreasGroups)))} #Top FIVE BIRTH COUNTRIE(s) => Group(s) => SHOW POLYGON(s) LAYER on World Map (Top FIVE BIRTH AREA(s))
          
          }
        #Reactive Value == "REGIONs" => Selected REGION | Draw World Administrative Boundaries
        else if (Selected_Residence_RV$MR_Residence_View_Choice == "REGIONs") {
          #REGIONs CHOICE RADIO BUTTONs => Selected REGION
          Selected_REGION <- input$MRR_REGIONs_CHOICE
          #Selected REGION => Draw World Administrative Boundaries => Top Five BIRTH AREA(s)
          if (Selected_REGION == "Continental Sub-Regions and Intermediate Regions") {
            
            #MIGRANT STOCK => Filtered => BIRTH == All SIREGION(s) => YEAR | ORDER | Top FIVE | RANK 
            FilteredMIGRANT_STOCKs_DATA_ContinentalSIREGIONs_YEAR <- FilteredMIGRANT_STOCKs_DATA_ContinentalSIREGIONs %>%
              #Replace Values in Birth
              mutate(Birth = case_when(Birth == "NORTHERN AMERICA" ~ "Northern America", TRUE ~ Birth)) %>%
              #FILTER => YEAR | ORDER | Top FIVE | RANK 
              filter(Year == YEAR) %>% arrange(desc(iMIGRANT_Stock_Total)) %>% slice(1:5) %>% mutate(Rank = row_number()) %>% select(1:5, 12, 6:11)
            
            #Top FIVE BIRTH AREA(s) => YEAR
            Top_BIRTH_AREAs_YEAR <- FilteredMIGRANT_STOCKs_DATA_ContinentalSIREGIONs_YEAR %>% pull(Birth)
            # Top_BIRTH_AREAs_YEAR <- toupper(FilteredMIGRANT_STOCKs_DATA_ContinentalSIREGIONs_YEAR %>% pull(Birth))
            
            #Calculate Centroid To Centroid Distance From BIRTH AREA(s) To Residence => YEAR
            DISTANCEs_YEAR <- RDATA_ContinentalSIREGIONs %>% 
              filter(NAME %in% c(Selected_Residence, Top_BIRTH_AREAs_YEAR)) %>% select(1:3, 10:11) %>%
              #Great Circle Distance => Method => Haversine (Spherical Earth => !Ellipsoid)
              # mutate(Distance_To_Residence = round(geosphere::distHaversine( 
              #Great Circle Distance => Method => Meeus (Ellipsoid => Geodetic)
              # mutate(Distance_To_Residence = round(geosphere::distMeeus( 
              #Great Circle Distance => Method => Ellipsoid (Geodesic)
              # mutate(Distance_To_Residence = round(geosphere::distGeo( 
              #Great Circle Distance => Method => VINCENTY (Ellipsoid)
              mutate(Distance_To_Residence = round(geosphere::distVincentyEllipsoid( 
                c(Centroid_X[which(NAME == Selected_Residence)], Centroid_Y[which(NAME == Selected_Residence)]), cbind(Centroid_X, Centroid_Y)) / 1000, 3))
          
            #DATA MANIPULATION(s) on GeoDATA
            GeoRDATA_AREAs_YEAR <- left_join(GeoRDATA_ContinentalSIREGIONs, FilteredMIGRANT_STOCKs_DATA_ContinentalSIREGIONs_YEAR, by = c("NAME" = "Birth")) %>%
              mutate(
                #Replace Values in LabelR
                LabelR = case_when(
                  #Replace Values in LabelR => Selected Residence | Top BIRTH AREA(s)
                  NAME == Selected_Residence & NAME %in% Top_BIRTH_AREAs_YEAR ~ paste0("<b>Residence: </b>", NAME, "<br>",
                                                                                       "<b>Numeric Code (M49 Code): </b>", SIREGION_CODE, "<br>",
                                                                                       "<b>Year: </b>", MIGRANT_STOCKs_DATA_World_YEAR$Year, "<br>",
                                                                                       "<b>International MIGRANT Stock (Both Sexes Combined): </b>", format(x = MIGRANT_STOCKs_DATA_World_YEAR$iMIGRANT_Stock_Total, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(MIGRANT_STOCKs_DATA_World_YEAR$Share_of_iMIGRANT_Stock_Total, 3), "%)", "<br>",
                                                                                       "<b>International MIGRANT Stock (Males): </b>", format(x = MIGRANT_STOCKs_DATA_World_YEAR$iMIGRANT_Stock_Males, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(MIGRANT_STOCKs_DATA_World_YEAR$Share_of_iMIGRANT_Stock_Males, 3), "%)", "<br>",
                                                                                       "<b>International MIGRANT Stock (Females): </b>", format(x = MIGRANT_STOCKs_DATA_World_YEAR$iMIGRANT_Stock_Females, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(MIGRANT_STOCKs_DATA_World_YEAR$Share_of_iMIGRANT_Stock_Females, 3), "%)", "<br>",
                                                                                       "<b>Top 5 Birth Areas (Cumulative Share, Both Sexes Combined Stock): </b>", round(sum(FilteredMIGRANT_STOCKs_DATA_ContinentalSIREGIONs_YEAR$Share_of_iMIGRANT_Stock_Total), 3), "%", "<br>",
                                                                                       "<b>Top 5 Birth Areas (Cumulative Share, Males Stock): </b>", round(sum(FilteredMIGRANT_STOCKs_DATA_ContinentalSIREGIONs_YEAR$Share_of_iMIGRANT_Stock_Males), 3), "%", "<br>",
                                                                                       "<b>Top 5 Birth Areas (Cumulative Share, Females Stock): </b>", round(sum(FilteredMIGRANT_STOCKs_DATA_ContinentalSIREGIONs_YEAR$Share_of_iMIGRANT_Stock_Females), 3), "%", "<br>",
                                                                                       "<br>",
                                                                                       "<b>Birth: </b>", NAME, "<br>",
                                                                                       "<b>Numeric Code (M49 Code): </b>", SIREGION_CODE, "<br>",
                                                                                       "<b>Rank (</b>", YEAR, "<b>): </b>", Rank, "<br>",
                                                                                       "<b>International MIGRANT Stock (Both Sexes Combined): </b>", format(x = iMIGRANT_Stock_Total, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(Share_of_iMIGRANT_Stock_Total, 3), "%)", "<br>",
                                                                                       "<b>International MIGRANT Stock (Males): </b>", format(x = iMIGRANT_Stock_Males, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(Share_of_iMIGRANT_Stock_Males, 3), "%)", "<br>",
                                                                                       "<b>International MIGRANT Stock (Females): </b>", format(x = iMIGRANT_Stock_Females, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(Share_of_iMIGRANT_Stock_Females, 3), "%)","<br>",
                                                                                       "<b>Distance To Residence (Centroid To Centroid): </b>", DISTANCEs_YEAR$Distance_To_Residence[match(NAME, DISTANCEs_YEAR$NAME)], " km"), 
                  #Replace Values in LabelR => Selected Residence
                  NAME == Selected_Residence ~ paste0("<b>Residence: </b>", NAME, "<br>",
                                                      "<b>Numeric Code (M49 Code): </b>", SIREGION_CODE, "<br>",
                                                      "<b>Year: </b>", MIGRANT_STOCKs_DATA_World_YEAR$Year, "<br>",
                                                      "<b>International MIGRANT Stock (Both Sexes Combined): </b>", format(x = MIGRANT_STOCKs_DATA_World_YEAR$iMIGRANT_Stock_Total, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(MIGRANT_STOCKs_DATA_World_YEAR$Share_of_iMIGRANT_Stock_Total, 3), "%)", "<br>",
                                                      "<b>International MIGRANT Stock (Males): </b>", format(x = MIGRANT_STOCKs_DATA_World_YEAR$iMIGRANT_Stock_Males, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(MIGRANT_STOCKs_DATA_World_YEAR$Share_of_iMIGRANT_Stock_Males, 3), "%)", "<br>",
                                                      "<b>International MIGRANT Stock (Females): </b>", format(x = MIGRANT_STOCKs_DATA_World_YEAR$iMIGRANT_Stock_Females, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(MIGRANT_STOCKs_DATA_World_YEAR$Share_of_iMIGRANT_Stock_Females, 3), "%)", "<br>",
                                                      "<b>Top 5 Birth Areas (Cumulative Share, Both Sexes Combined Stock): </b>", round(sum(FilteredMIGRANT_STOCKs_DATA_ContinentalSIREGIONs_YEAR$Share_of_iMIGRANT_Stock_Total), 3), "%", "<br>",
                                                      "<b>Top 5 Birth Areas (Cumulative Share, Males Stock): </b>", round(sum(FilteredMIGRANT_STOCKs_DATA_ContinentalSIREGIONs_YEAR$Share_of_iMIGRANT_Stock_Males), 3), "%", "<br>",
                                                      "<b>Top 5 Birth Areas (Cumulative Share, Females Stock): </b>", round(sum(FilteredMIGRANT_STOCKs_DATA_ContinentalSIREGIONs_YEAR$Share_of_iMIGRANT_Stock_Females), 3), "%"),
                  #Replace Values in LabelR => Top BIRTH AREA(s)
                  NAME %in% Top_BIRTH_AREAs_YEAR ~ paste0("<b>Birth: </b>", NAME, "<br>",
                                                          "<b>Numeric Code (M49 Code): </b>", SIREGION_CODE, "<br>",
                                                          "<b>Rank (</b>", YEAR, "<b>): </b>", Rank, "<br>",
                                                          "<b>International MIGRANT Stock (Both Sexes Combined): </b>", format(x = iMIGRANT_Stock_Total, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(Share_of_iMIGRANT_Stock_Total, 3), "%)", "<br>",
                                                          "<b>International MIGRANT Stock (Males): </b>", format(x = iMIGRANT_Stock_Males, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(Share_of_iMIGRANT_Stock_Males, 3), "%)", "<br>",
                                                          "<b>International MIGRANT Stock (Females): </b>", format(x = iMIGRANT_Stock_Females, trim = TRUE, scientific = FALSE, big.mark = " "), " (", round(Share_of_iMIGRANT_Stock_Females, 3), "%)","<br>",
                                                          "<b>Distance To Residence (Centroid To Centroid): </b>", DISTANCEs_YEAR$Distance_To_Residence[match(NAME, DISTANCEs_YEAR$NAME)], " km"), TRUE ~ LabelR),
                #Replace Values in NAME
                # NAME = paste(NAME, YEAR, sep = " ")) %>% select(1:17)
                #Replace Values in NAME
                # NAME = ifelse(YEAR != 1990, paste(NAME, YEAR, sep = " "), NAME)) %>% select(1:17)
                #Replace Values in NAME
                NAME = case_when(YEAR != 1990 ~ paste(NAME, YEAR, sep = " "), TRUE ~ NAME)) %>% select(1:17)
            
            #NAME => FACTOR-LEVEL(s) => ORDER
            ContinentalSIREGIONs_Levels <- c(
              "Eastern Africa", "Middle Africa", "Northern Africa", "Southern Africa", "Western Africa",
              "Central Asia", "Eastern Asia", "South-Eastern Asia", "Southern Asia", "Western Asia",
              "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe",
              "Caribbean", "Central America", "South America",
              "Northern America",
              "Australia and New Zealand", "Melanesia", "Micronesia", "Polynesia",
              "Other Countries/Territories")
            
            #NAME => FACTOR-LEVEL(s) => World Map
            GeoRDATA_AREAs_YEAR$NAME <- factor(
              x = GeoRDATA_AREAs_YEAR$NAME,
              levels = if (YEAR == 1990) ContinentalSIREGIONs_Levels else paste(ContinentalSIREGIONs_Levels, YEAR, sep = " "))
            
            Geo <- GeoRDATA_AREAs_YEAR #GeoRDATA
            POLYGONs_LAYER <- Geo$NAME #World Map => POLYGONs => LAYER-NAME
            Group <- TopFiveBirthAreasGroups[[YEAR]] #World Map => Group+
            # Selected_Residence_YEAR <- paste(Selected_Residence, YEAR, sep = " ") #Replace Selected Residence Value
            Selected_Residence_YEAR <- ifelse(YEAR != 1990, paste(Selected_Residence, YEAR, sep = " "), Selected_Residence) #Replace Selected Residence Value
            # Top_BIRTH_AREAs_YEAR <- paste(Top_BIRTH_AREAs_YEAR, YEAR, sep = " ") #Replace Top BIRTH AREA(s) Values
            # Top_BIRTH_AREAs_YEAR <- ifelse(YEAR != 1990, paste(Top_BIRTH_AREAs_YEAR, YEAR, sep = " "), Top_BIRTH_AREAs_YEAR) #Replace Top BIRTH AREA(s) Values
            Top_BIRTH_AREAs_YEAR <- if (YEAR != 1990) {paste(Top_BIRTH_AREAs_YEAR, YEAR, sep = " ")} else {Top_BIRTH_AREAs_YEAR} #Replace Top BIRTH AREA(s) Values
            COLOR <- "#CCCCCC" #World Map => Stroke Attributes
            STROKE_ATTRIBUTEs <- c(0.5, 0.5, 1.5, 0.75) #World Map => Stroke Attributes
            #Maps Values To Colors
            fCLR <- colorFactor(
              #Colors ordered with factor-level(s) in NAME
              palette = Geo$CLRCode[order(factor(Geo$NAME, levels = levels(Geo$NAME)))],
              #Possible Values | NOT-AVAILABLE COLOR
              domain = Geo$NAME, na.color = "#999999")
            Fill_COLOR <- list(fCLR(Geo$NAME), fCLR(Geo$NAME), fCLR(Geo$NAME), fCLR(Geo$NAME)) #World Map => Fill Attributes
            Fill_ATTRIBUTEs <- c(0.75, 1) #World Map => Fill Attributes
            SMOOTH_FACTOR <- 1.25 #World Map => POLYLINE Simplification
            # SMOOTH_FACTOR <- 1.50 #World Map => POLYLINE Simplification
            LHOVER <- Geo$LabelR #World Map => HOVER TEXT
            HOVER_Fill_COLOR <- NULL #HIGHLIGHT-Options on Mouse OVER => Fill Attributes
            HOVER_Fill_ATTRIBUTEs <- 1 #HIGHLIGHT-Options on Mouse OVER => Fill Attributes
            
            #Draw World Administrative Boundaries => Top Five BIRTH AREA(s)
            leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
              addPolygons(data = Geo, layerId = POLYGONs_LAYER, group = Group, #Add POLYGONs To Map | Group+
                          stroke = TRUE, #Stroke Attributes
                          color = ifelse(POLYGONs_LAYER == Selected_Residence_YEAR & POLYGONs_LAYER %in% Top_BIRTH_AREAs_YEAR, "#7FD017",
                                         ifelse(POLYGONs_LAYER %in% Selected_Residence_YEAR, "#00CC00", ifelse(POLYGONs_LAYER %in% Top_BIRTH_AREAs_YEAR, "#FED52E", COLOR))), #Stroke Attributes
                          weight = ifelse(POLYGONs_LAYER %in% c(Selected_Residence_YEAR, Top_BIRTH_AREAs_YEAR), STROKE_ATTRIBUTEs[3], STROKE_ATTRIBUTEs[1]), #Stroke Attributes
                          opacity = ifelse(POLYGONs_LAYER %in% c(Selected_Residence_YEAR, Top_BIRTH_AREAs_YEAR), STROKE_ATTRIBUTEs[4], STROKE_ATTRIBUTEs[2]), #Stroke Attributes
                          fill = TRUE, #Fill Attributes
                          # fillColor = ifelse(POLYGONs_LAYER %in% Selected_Residence_YEAR, Fill_COLOR[[2]],
                          #                    ifelse(POLYGONs_LAYER %in% Top_BIRTH_AREAs_YEAR, Fill_COLOR[[3]], Fill_COLOR[[1]])), #Fill Attributes
                          fillColor = ifelse(POLYGONs_LAYER == Selected_Residence_YEAR & POLYGONs_LAYER %in% Top_BIRTH_AREAs_YEAR, Fill_COLOR[[2]],
                                             ifelse(POLYGONs_LAYER %in% Selected_Residence_YEAR, Fill_COLOR[[3]], ifelse(POLYGONs_LAYER %in% Top_BIRTH_AREAs_YEAR, Fill_COLOR[[4]], Fill_COLOR[[1]]))), #Fill Attributes
                          fillOpacity = ifelse(POLYGONs_LAYER %in% c(Selected_Residence_YEAR, Top_BIRTH_AREAs_YEAR), Fill_ATTRIBUTEs[2], Fill_ATTRIBUTEs[1]), #Fill Attributes
                          smoothFactor = SMOOTH_FACTOR, label = lapply(LHOVER, HTML), #POLYLINE Simplification | HOVER TEXT
                          highlightOptions = highlightOptions( #HIGHLIGHT-Options on Mouse OVER
                            stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75, #Stroke Attributes
                            fill = TRUE, fillColor = HOVER_Fill_COLOR, fillOpacity = HOVER_Fill_ATTRIBUTEs)) #Fill Attributes
            
            #Top FIVE BIRTH AREA(s) => Group(s) => HIDE POLYGON(s) LAYER FROM World Map => Keep ONLY => "Top 5 Birth Areas - Year 1990" (YEAR == "1990")
            if (YEAR != "1990") {leafletProxy(mapId = "MR_World_Map", session = session) %>% hideGroup(group = TopFiveBirthAreasGroups[[YEAR]])}
            
            #DEVELOPMENT LEVEL(s) => Group(s)
            DevLevelsGroups <- c("2 Development Levels", "3 Development Levels", "5 Development Levels")
            
            #SHOW POLYGON(s) LAYER on World Map For Six Selected Group Values WHEN CHECKBOX Value is Back To TRUE
            # if (!is.null(input$MR_World_Map_groups) && input$MR_World_Map_groups != "Top 5 Birth Areas - Year 1990") { #WHEN Selected Group Value != "Top 5 Birth Areas - Year 1990"
            if (!is.null(input$MR_World_Map_groups) && !"Top 5 Birth Areas - Year 1990" %in% input$MR_World_Map_groups) { #WHEN "Top 5 Birth Areas - Year 1990" !in Selected Group Value
              # leafletProxy(mapId = "MR_World_Map", session = session) %>% showGroup(group = input$MR_World_Map_groups)} #Top FIVE BIRTH AREA(s) => Group(s) => SHOW POLYGON(s) LAYER on World Map
              # leafletProxy(mapId = "MR_World_Map", session = session) %>% showGroup(group = setdiff(input$MR_World_Map_groups, DevLevelsGroups))} #Top FIVE BIRTH AREA(s) => Group(s) => SHOW POLYGON(s) LAYER on World Map
              leafletProxy(mapId = "MR_World_Map", session = session) %>% 
                showGroup(group = Reduce(f = setdiff, x = list(input$MR_World_Map_groups, TopFiveBirthCountriesGroups)))} #Top FIVE BIRTH AREA(s) => Group(s) => SHOW POLYGON(s) LAYER on World Map (Top FIVE BIRTH COUNTRIE(s))
            
            }
          }

        }
      
      #Reactive Value == "COUNTRIEs" => LAYER(s) Control | LN+
      if (Selected_Residence_RV$MR_Residence_View_Choice == "COUNTRIEs") {
        
        #Add LAYER(s) Control
        leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
          addLayersControl( #Add LAYER(s) Control To Map
            baseGroups = unlist(x = TopFiveBirthCountriesGroups, use.names = FALSE), #USER CAN CHOOSE ONLY ONE LAYER
            # overlayGroups = unlist(x = TopFiveBirthCountriesGroups, use.names = FALSE), #USER CAN CHOOSE Several LAYER(s)
            position = "bottomright", options = layersControlOptions(collapsed = FALSE)) #LAYER(s) Control => POSITION | !Expand on HOVER
        
        #Top FIVE BIRTH COUNTRIE(s) => Group(s) => CLEAR/Add LN To Map
        observeEvent(input$MR_World_Map_groups, { #WHEN SWITCH Selected Group in LAYER(s) Control
          
          #LN+ => REMOVE => DEVELOPMENT-LEVEL(s)
          leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
            removeControl(layerId = "MoreLess") %>% removeControl(layerId = "MoreLessLeast") %>% removeControl(layerId = "LDC_LLDC_SIDS")
          
          #COUNTRIEs => Modal Box => HIDE
          runjs("$('#WM_ModalBox').parent().remove();")
          
          #Add LN To Map
          leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
            addLegend(position = "topright", #LN-POSITION
                      colors = c("#00CC00", "#FED52E"), labels = c("Selected Residence", "Top 5 Birth Countries"), opacity = 1, #COLOR(s) + Label(s) + COLOR(s)-OPACITY
                      layerId = "TopFiveBirthCountries") #LAYER-NAME
          
          #LEAFLET => LN+ => PROPERTIE(s)
          runjs("let COUNTER = 0; //Initialize COUNTER
                 let X = setInterval(function() { //Call X EVERY X MILLISECONDs
                   COUNTER++; //+1
                   //LEAFLET => LN+ => PADDING+ | FONT-SIZE | FONT-FAMILY | BOX-SHADOW
                   $('.info.legend.leaflet-control').css({
                     'padding': '4px 8px', 'font-size': '12px', 'font-family': 'DM Sans', 'box-shadow': 'none'});
                   //LEAFLET => LN+ => ELEMENT(s) => MARGIN(s)+
                   $('.leaflet .legend i').css({'margin-right': '2.5px'});
                   //COUNTER === 10 => !Run ANYMORE
                   if (COUNTER === 10) {clearInterval(X);}}, 100);") #Run Function EVERY 100 MILLISECONDs
          
          })
        
        }
      #Reactive Value == "REGIONs" => Selected REGION | LAYER(s) Control | LN+
      else if (Selected_Residence_RV$MR_Residence_View_Choice == "REGIONs") {
        #REGIONs CHOICE RADIO BUTTONs => Selected REGION
        Selected_REGION <- input$MRR_REGIONs_CHOICE
        #Selected REGION => LAYER(s) Control | LN+
        if (Selected_REGION == "Continental Sub-Regions and Intermediate Regions") {
          
          #Add LAYER(s) Control
          leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
            addLayersControl( #Add LAYER(s) Control To Map
              baseGroups = unlist(x = TopFiveBirthAreasGroups, use.names = FALSE), #USER CAN CHOOSE ONLY ONE LAYER
              # overlayGroups = unlist(x = TopFiveBirthAreasGroups, use.names = FALSE), #USER CAN CHOOSE Several LAYER(s)
              position = "bottomleft", options = layersControlOptions(collapsed = FALSE)) #LAYER(s) Control => POSITION | !Expand on HOVER
          
          #Top FIVE BIRTH AREA(s) => Group(s) => CLEAR/Add LN To Map
          observeEvent(input$MR_World_Map_groups, { #WHEN SWITCH Selected Group in LAYER(s) Control
            
            #LN+ => REMOVE => DEVELOPMENT-LEVEL(s)
            leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
              removeControl(layerId = "MoreLess") %>% removeControl(layerId = "MoreLessLeast") %>% removeControl(layerId = "LDC_LLDC_SIDS")
            
            #REGIONs => Modal Box => HIDE
            runjs("$('#WM_ModalBox').parent().remove();")
            
            #Add LN To Map
            leafletProxy(mapId = "MR_World_Map", session = session) %>% #Customize and control a rendered map
              # addLegend(position = "topleft", #LN-POSITION
              addLegend(position = "bottomleft", #LN-POSITION
                        colors = c("#00CC00", "#FED52E"), labels = c("Selected Residence", "Top 5 Birth Areas"), opacity = 1, #COLOR(s) + Label(s) + COLOR(s)-OPACITY
                        layerId = "TopFiveBirthAreas") #LAYER-NAME
            
            #LEAFLET => LN+ => PROPERTIE(s)
            runjs("let COUNTER = 0; //Initialize COUNTER
                   let X = setInterval(function() { //Call X EVERY X MILLISECONDs
                     COUNTER++; //+
                     //LEAFLET => LN+ => POSITION
                     //$('.leaflet-top.leaflet-left .info.legend.leaflet-control').css({'top': '-113.5px', 'right': '-42.5px'});
                     //LEAFLET => LN+ => PADDING+ | FONT-SIZE | FONT-FAMILY | BOX-SHADOW
                     $('.info.legend.leaflet-control').css({
                       'padding': '4px 8px', 'font-size': '12px', 'font-family': 'DM Sans', 'box-shadow': 'none'});
                     //LEAFLET => LN+ => ELEMENT(s) => MARGIN(s)+
                     $('.leaflet .legend i').css({'margin-right': '2.5px'});
                     //LEAFLET => LN+ => ELEMENT(s) => n°0 => BACKGround+  BORDER
                     $('.leaflet-bottom.leaflet-left .info.legend.leaflet-control i').eq(0).css({'background': 'none', 'border': '2.5px solid #00CC00'});
                     //$('.leaflet-bottom.leaflet-left .info.legend.leaflet-control i').eq(0).css({'background': 'none', 'border': '2.5px solid #00CC00', 'border-radius': '10%'});
                     //LEAFLET => LN+ => ELEMENT(s) => n°1 => BACKGround+  BORDER
                     $('.leaflet-bottom.leaflet-left .info.legend.leaflet-control i').eq(1).css({'background': 'none', 'border': '2.5px solid #FED52E'});
                     //$('.leaflet-bottom.leaflet-left .info.legend.leaflet-control i').eq(1).css({'background': 'none', 'border': '2.5px solid #FED52E', 'border-radius': '10%'});
                     //COUNTER === 10 => !Run ANYMORE
                     if (COUNTER === 10) {clearInterval(X);}}, 100);") #Run Function EVERY 100 MILLISECONDs
            
            })
          
          }
        }
      
      }

    })
  
  ##### ... => Reactive Expression => ... #####
  SelectedBirthView <- eventReactive(input$MR_BIRTH_View_Choice, {Selected_Birth_View <- input$MR_BIRTH_View_Choice})
  
  ##### ... => Reactive Expression => ... #####
  SelectedClassification <- eventReactive(input$MR_BIRTH_View_Choice_REGIONs_CHOICE, {Selected_Classification <- input$MR_BIRTH_View_Choice_REGIONs_CHOICE})
  
  ##### ... => Reactive Expression => ... #####
  SelectedVariable <- eventReactive(input$MR_Variable_Choice, {Selected_Variable <- input$MR_Variable_Choice})
  
  ##### ... => Reactive Expression => ... #####
  DecimalChoice <- eventReactive(input$MR_Decimal_Choice, {Decimal_Choice <- input$MR_Decimal_Choice})
  
  #### ... #####
  observeEvent(input$MR_BIRTH_View_Choice, {
    if (input$MR_BIRTH_View_Choice == "Countries") {shinyjs::disable(id = "MR_BIRTH_View_Choice_REGIONs_CHOICE")}
    else if (input$MR_BIRTH_View_Choice == "Regions") {shinyjs::enable(id = "MR_BIRTH_View_Choice_REGIONs_CHOICE")}})
  
  ##### ... #####
  output$MR_Stacked_BARs_COUNTs <- renderPlotly({
    
    if (Filtered_MIGRANT_STOCKs_DATA_RV$Selected_Residence_Code_in_MIGRANT_STOCK_DATA) {
    
      FilteredMIGRANT_STOCKs_DATA <- Filtered_MIGRANT_STOCKs_DATA_RV$Filtered_MIGRANT_STOCKs_DATA
      
      if (SelectedBirthView() == "Countries") {
        
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs <- FilteredMIGRANT_STOCKs_DATA %>%
          filter(BirthCode < 900 | BirthCode == 2003, Year == c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) %>% distinct(BirthCode, Year, .keep_all = TRUE)
        
        COUNTRIEs_COLORs_LVs <- iMIGRANT_STOCK_DATA_SHAREs %>%
          distinct(Residence, ResidenceCode) %>% filter(ResidenceCode < 900) %>%
          inner_join(DATA %>% select(VISUALIZATION_NAME, M49_CODE, ContinentalREGION, CLRCode) , by = c("Residence" = "VISUALIZATION_NAME")) %>%
          arrange(ContinentalREGION, Residence) %>%
          add_row(Residence = "Other", ResidenceCode = 2003, M49_CODE = "2003", ContinentalREGION = NA, CLRCode = "#999999")

        LVs <- COUNTRIEs_COLORs_LVs$Residence
        COLORs <- COUNTRIEs_COLORs_LVs$CLRCode
        
        X <- paste(Mode((FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs %>% group_by(Year) %>% summarize(Count = n_distinct(BirthCode)))$Count), "Countries")
        
        }
      else if (SelectedBirthView() == "Regions") {
        
        if (SelectedClassification() == "Continental Regions") {
          REGIONs_CODEs = c(903, 935, 908, 904, 905, 909)
          LVs <- c(GeoRDATA_ContinentalREGIONs$NAME, 'OTHER')
          COLORs <- c(GeoRDATA_ContinentalREGIONs$CLRCode, "#999999")
          X <- "6 Continental Regions"}
        else if (SelectedClassification() == "Continental Sub-Regions and Intermediate Regions") {
          REGIONs_CODEs <- c(910, 911, 912, 913, 914, 
                             5500, 906, 920, 5501, 922, 
                             923, 924, 925, 926,
                             915, 916, 931, 
                             905, #NORTHERN AMERICA => "Northern America"
                             927, 928, 954, 957)
          LVs <- c(levels(GeoRDATA_ContinentalSIREGIONs$NAME), 'Other')
          LVs <- LVs[LVs != 'Other Countries/Territories']
          COLORs <- c(GeoRDATA_ContinentalSIREGIONs$CLRCode[order(GeoRDATA_ContinentalSIREGIONs$NAME)])
          X <- "22 Continental Sub-Regions and Intermediate Regions"}
        else if (SelectedClassification() == "Geographic Regions") {
          REGIONs_CODEs = c(927, 921, 1832, 1829, 1830, 1833, 1835, 947)
          LVs <- c(levels(GeoRDATA_GeoREGIONs$NAME), 'Other')
          LVs <- LVs[LVs != 'Other Countries/Territories']
          COLORs <- c(GeoRDATA_GeoREGIONs$CLRCode[order(GeoRDATA_GeoREGIONs$NAME)])
          X <- "8 Geographic Regions"}
        else if (SelectedClassification() == "2 Development Levels") {
          REGIONs_CODEs = c(901, 902)
          LVs <- c(levels(GeoRDATA_MoreLess$NAME), 'Other')
          LVs <- LVs[LVs != 'Other Countries/Territories']
          COLORs <- c(GeoRDATA_MoreLess$CLRCode[order(GeoRDATA_MoreLess$NAME)])
          X <- "2 Development Levels"}
        else if (SelectedClassification() == "3 Development Levels") {
          REGIONs_CODEs = c(901, 934, 941)
          LVs <- c(levels(GeoRDATA_MoreLessLeast$NAME), 'Other')
          LVs <- LVs[LVs != 'Other Countries/Territories']
          COLORs <- c(GeoRDATA_MoreLessLeast$CLRCode[order(GeoRDATA_MoreLessLeast$NAME)])
          X <- "3 Development Levels"}
        else if (SelectedClassification() == "5 Development Levels") {
          REGIONs_CODEs = c(942, 1638, 1639, 1640, 1641)
          LVs <- c(levels(GeoRDATA_LDC_LLDC_SIDS$NAME), 'Other', 'Non-OHRLLS Countries')
          LVs <- LVs[LVs != 'Other Countries/Territories']
          COLORs <- c("#F1C40F", "#E74C3C", "#5B92E5", "#FF9900", "#72C02C", "#999999", "#555555")
          X <- "5 Development Levels"}
        else if (SelectedClassification() == "Income Levels") {
          REGIONs_CODEs = c(1503, 1502, 1501, 1500)
          LVs <- c(levels(GeoRDATA_IncomeLevels$NAME), 'Other')
          LVs <- LVs[LVs != 'Other Countries/Territories']
          COLORs <- c(GeoRDATA_IncomeLevels$CLRCode[order(GeoRDATA_IncomeLevels$NAME)])
          X <- "4 Income Levels"}
        
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs <- FilteredMIGRANT_STOCKs_DATA %>%
          filter(BirthCode %in% REGIONs_CODEs | BirthCode == 2003, Year == c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) %>% distinct(BirthCode, Year, .keep_all = TRUE)
        
        if (SelectedClassification() == "5 Development Levels") {
        
          FilteredMIGRANT_STOCKs_DATA_World <- FilteredMIGRANT_STOCKs_DATA %>%
            filter(BirthCode == 900, #BIRTH == World
                   Year %in% c("1990", "1995", "2000", "2005", "2010", "2015", "2020")) %>% distinct(BirthCode, Year, .keep_all = TRUE)
          
          Selected_Residence <- FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs %>% pull(Residence) %>% first()
          
          Selected_Residence_Code <- FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs %>% pull(ResidenceCode) %>% first()
          
          FilteredMIGRANT_STOCKs_DATA_NON_OHRLLS_COUNTRIEs <- FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs %>% group_by(Year) %>% 
            summarise(iMIGRANT_Stock_Total = sum(iMIGRANT_Stock_Total, na.rm = TRUE), iMIGRANT_Stock_Males = sum(iMIGRANT_Stock_Males, na.rm = TRUE), iMIGRANT_Stock_Females = sum(iMIGRANT_Stock_Females, na.rm = TRUE), 
                      Share_of_iMIGRANT_Stock_Total = 100 - sum(Share_of_iMIGRANT_Stock_Total, na.rm = TRUE), Share_of_iMIGRANT_Stock_Males = 100 - sum(Share_of_iMIGRANT_Stock_Males, na.rm = TRUE), Share_of_iMIGRANT_Stock_Females = 100 - sum(Share_of_iMIGRANT_Stock_Females, na.rm = TRUE)) %>%
            mutate(iMIGRANT_Stock_Total =  FilteredMIGRANT_STOCKs_DATA_World$iMIGRANT_Stock_Total[match(Year, FilteredMIGRANT_STOCKs_DATA_World$Year)] - iMIGRANT_Stock_Total,
                   iMIGRANT_Stock_Males =  FilteredMIGRANT_STOCKs_DATA_World$iMIGRANT_Stock_Males[match(Year, FilteredMIGRANT_STOCKs_DATA_World$Year)] - iMIGRANT_Stock_Males,
                   iMIGRANT_Stock_Females =  FilteredMIGRANT_STOCKs_DATA_World$iMIGRANT_Stock_Females[match(Year, FilteredMIGRANT_STOCKs_DATA_World$Year)] - iMIGRANT_Stock_Females,
                   Residence = Selected_Residence, ResidenceCode = Selected_Residence_Code, Birth = "Non-OHRLLS Countries", BirthCode = 2004) %>% select(8:11, 1:7)
          
          FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs <- rbind(FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs, FilteredMIGRANT_STOCKs_DATA_NON_OHRLLS_COUNTRIEs)
          
          }
        
        }
      
      if (SelectedClassification() == "Continental Regions") {
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs$Birth <- gsub(pattern = "Other", replacement = "OTHER", x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs$Birth)}
      else if (SelectedClassification() == "Continental Sub-Regions and Intermediate Regions") {
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs$Birth <- gsub(pattern = "NORTHERN AMERICA", replacement = "Northern America", x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs$Birth)}
      else if (SelectedClassification() == "Geographic Regions (SDG)") {
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs$Birth <- gsub(pattern = "Oceania \\(excluding Australia and New Zealand\\)", replacement = "Oceania*", x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs$Birth)}
      else if (SelectedClassification() == "3 Development Levels") {
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs$Birth <- gsub(pattern = "Less Developed Countries \\(Least Developed Countries excluded\\)", replacement = "Less Developed Countries*", x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs$Birth)
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs$Birth <- gsub(pattern = "Least Developed Countries \\(LDC\\)", replacement = "Least Developed Countries", x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs$Birth)}

      #Residence-Birth/Year To Year/Residence/Birth
      FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs <- FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs %>%
        group_by(Year, Residence, ResidenceCode, Birth, BirthCode) %>% summarise(
          iMIGRANT_Stock_Total, iMIGRANT_Stock_Males, iMIGRANT_Stock_Females, Share_of_iMIGRANT_Stock_Total, Share_of_iMIGRANT_Stock_Males, Share_of_iMIGRANT_Stock_Females)
      
      if (SelectedVariable() == "Both Sexes Combined") {
        Selected_Variable <- "iMIGRANT_Stock_Total"
        Selected_Group <- "Both Sexes Combined"}
      else if (SelectedVariable() == "Males") {
        Selected_Variable <- "iMIGRANT_Stock_Males"
        Selected_Group <- "Males"}
      else if (SelectedVariable() == "Females") {
        Selected_Variable <- "iMIGRANT_Stock_Females"
        Selected_Group <- "Females"}
      
      FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs$Birth <- factor(x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs$Birth, levels = rev(LVs))
      
      plot_ly(data = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs, x = ~Year, y = as.formula(paste0("~", Selected_Variable)),
              color = ~Birth, type = "bar", colors = rev(COLORs), legendgroup = 'Birth',
              text = with(data = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs, expr = paste0("<b>Residence: </b>", Residence, "<br>", 
                                                                                                "<b>Birth: </b>", Birth, "<br>", 
                                                                                                "<b>Year: </b>", Year, "<br>", 
                                                                                                "<b>International MIGRANT Stock (", Selected_Group, "): </b>", format(x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_COUNTs[[Selected_Variable]], trim = TRUE, scientific = FALSE, big.mark = " ")))) %>%
      
        style(textposition = "none", hoverinfo = "text") %>%
        layout(barmode = "stack",
               yaxis = list(title = paste0("International Migrant Stock (", Selected_Group,")")),
               title = paste("International Migrant Stocks in", SelectedResidence(), "from", X, "(1990-2020)"),
               legend = list(title = list(text = '<b>Birth</b>'), x=1, y=0.5),
               margin = list(t = 30))
      
      }
    
    })
  
  ##### ... #####
  output$MR_Stacked_BARs_SHAREs <- renderPlotly({
    
    if (Filtered_MIGRANT_STOCKs_DATA_RV$Selected_Residence_Code_in_MIGRANT_STOCK_DATA) {
    
      FilteredMIGRANT_STOCKs_DATA <- Filtered_MIGRANT_STOCKs_DATA_RV$Filtered_MIGRANT_STOCKs_DATA
      
      if (SelectedBirthView() == "Countries") {
        
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs <- FilteredMIGRANT_STOCKs_DATA %>%
          filter(BirthCode < 900 | BirthCode == 2003, Year == c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) %>% distinct(BirthCode, Year, .keep_all = TRUE)
        
        COUNTRIEs_COLORs_LVs <- iMIGRANT_STOCK_DATA_SHAREs %>%
          distinct(Residence, ResidenceCode) %>% filter(ResidenceCode < 900) %>%
          inner_join(DATA %>% select(VISUALIZATION_NAME, M49_CODE, ContinentalREGION, CLRCode) , by = c("Residence" = "VISUALIZATION_NAME")) %>%
          arrange(ContinentalREGION, Residence) %>%
          add_row(Residence = "Other", ResidenceCode = 2003, M49_CODE = "2003", ContinentalREGION = NA, CLRCode = "#999999")

        LVs <- COUNTRIEs_COLORs_LVs$Residence
        COLORs <- COUNTRIEs_COLORs_LVs$CLRCode
        
        X <- paste(Mode((FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs %>% group_by(Year) %>% summarize(Count = n_distinct(BirthCode)))$Count), "Countries")
        
        }
      else if (SelectedBirthView() == "Regions") {
        
        if (SelectedClassification() == "Continental Regions") {
          REGIONs_CODEs = c(903, 935, 908, 904, 905, 909)
          LVs <- c(GeoRDATA_ContinentalREGIONs$NAME, 'OTHER')
          COLORs <- c(GeoRDATA_ContinentalREGIONs$CLRCode, "#999999")
          X <- "6 Continental Regions"}
        else if (SelectedClassification() == "Continental Sub-Regions and Intermediate Regions") {
          REGIONs_CODEs <- c(910, 911, 912, 913, 914, 
                             5500, 906, 920, 5501, 922, 
                             923, 924, 925, 926,
                             915, 916, 931, 
                             905, #NORTHERN AMERICA => "Northern America"
                             927, 928, 954, 957)
          LVs <- c(levels(GeoRDATA_ContinentalSIREGIONs$NAME), 'Other')
          LVs <- LVs[LVs != 'Other Countries/Territories']
          COLORs <- c(GeoRDATA_ContinentalSIREGIONs$CLRCode[order(GeoRDATA_ContinentalSIREGIONs$NAME)])
          X <- "22 Continental Sub-Regions and Intermediate Regions"}
        else if (SelectedClassification() == "Geographic Regions") {
          REGIONs_CODEs = c(927, 921, 1832, 1829, 1830, 1833, 1835, 947)
          LVs <- c(levels(GeoRDATA_GeoREGIONs$NAME), 'Other')
          LVs <- LVs[LVs != 'Other Countries/Territories']
          COLORs <- c(GeoRDATA_GeoREGIONs$CLRCode[order(GeoRDATA_GeoREGIONs$NAME)])
          X <- "8 Geographic Regions"}
        else if (SelectedClassification() == "2 Development Levels") {
          REGIONs_CODEs = c(901, 902)
          LVs <- c(levels(GeoRDATA_MoreLess$NAME), 'Other')
          LVs <- LVs[LVs != 'Other Countries/Territories']
          COLORs <- c(GeoRDATA_MoreLess$CLRCode[order(GeoRDATA_MoreLess$NAME)])
          X <- "2 Development Levels"}
        else if (SelectedClassification() == "3 Development Levels") {
          REGIONs_CODEs = c(901, 934, 941)
          LVs <- c(levels(GeoRDATA_MoreLessLeast$NAME), 'Other')
          LVs <- LVs[LVs != 'Other Countries/Territories']
          COLORs <- c(GeoRDATA_MoreLessLeast$CLRCode[order(GeoRDATA_MoreLessLeast$NAME)])
          X <- "3 Development Levels"}
        else if (SelectedClassification() == "5 Development Levels") {
          REGIONs_CODEs = c(942, 1638, 1639, 1640, 1641)
          LVs <- c(levels(GeoRDATA_LDC_LLDC_SIDS$NAME), 'Other', 'Non-OHRLLS Countries')
          LVs <- LVs[LVs != 'Other Countries/Territories']
          COLORs <- c("#F1C40F", "#E74C3C", "#5B92E5", "#FF9900", "#72C02C", "#999999", "#555555")
          X <- "5 Development Levels"}
        else if (SelectedClassification() == "Income Levels") {
          REGIONs_CODEs = c(1503, 1502, 1501, 1500)
          LVs <- c(levels(GeoRDATA_IncomeLevels$NAME), 'Other')
          LVs <- LVs[LVs != 'Other Countries/Territories']
          COLORs <- c(GeoRDATA_IncomeLevels$CLRCode[order(GeoRDATA_IncomeLevels$NAME)])
          X <- "4 Income Levels"}
        
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs <- FilteredMIGRANT_STOCKs_DATA %>%
          filter(BirthCode %in% REGIONs_CODEs | BirthCode == 2003, Year == c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) %>% distinct(BirthCode, Year, .keep_all = TRUE)
        
        if (SelectedClassification() == "5 Development Levels") {
          
          FilteredMIGRANT_STOCKs_DATA_World <- FilteredMIGRANT_STOCKs_DATA %>%
            filter(BirthCode == 900, #BIRTH == World
                   Year %in% c("1990", "1995", "2000", "2005", "2010", "2015", "2020")) %>% distinct(BirthCode, Year, .keep_all = TRUE)
          
          Selected_Residence <- FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs %>% pull(Residence) %>% first()
          
          Selected_Residence_Code <- FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs %>% pull(ResidenceCode) %>% first()
          
          FilteredMIGRANT_STOCKs_DATA_NON_OHRLLS_COUNTRIEs <- FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs %>% group_by(Year) %>% 
            summarise(iMIGRANT_Stock_Total = sum(iMIGRANT_Stock_Total, na.rm = TRUE), iMIGRANT_Stock_Males = sum(iMIGRANT_Stock_Males, na.rm = TRUE), iMIGRANT_Stock_Females = sum(iMIGRANT_Stock_Females, na.rm = TRUE), 
                      Share_of_iMIGRANT_Stock_Total = 100 - sum(Share_of_iMIGRANT_Stock_Total, na.rm = TRUE), Share_of_iMIGRANT_Stock_Males = 100 - sum(Share_of_iMIGRANT_Stock_Males, na.rm = TRUE), Share_of_iMIGRANT_Stock_Females = 100 - sum(Share_of_iMIGRANT_Stock_Females, na.rm = TRUE)) %>%
            mutate(iMIGRANT_Stock_Total =  FilteredMIGRANT_STOCKs_DATA_World$iMIGRANT_Stock_Total[match(Year, FilteredMIGRANT_STOCKs_DATA_World$Year)] - iMIGRANT_Stock_Total,
                   iMIGRANT_Stock_Males =  FilteredMIGRANT_STOCKs_DATA_World$iMIGRANT_Stock_Males[match(Year, FilteredMIGRANT_STOCKs_DATA_World$Year)] - iMIGRANT_Stock_Males,
                   iMIGRANT_Stock_Females =  FilteredMIGRANT_STOCKs_DATA_World$iMIGRANT_Stock_Females[match(Year, FilteredMIGRANT_STOCKs_DATA_World$Year)] - iMIGRANT_Stock_Females,
                   Residence = Selected_Residence, ResidenceCode = Selected_Residence_Code, Birth = "Non-OHRLLS Countries", BirthCode = 2004) %>% select(8:11, 1:7)
          
          FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs <- rbind(FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs, FilteredMIGRANT_STOCKs_DATA_NON_OHRLLS_COUNTRIEs)
          
          }
        
        }
      
      if (SelectedClassification() == "Continental Regions") {
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs$Birth <- gsub(pattern = "Other", replacement = "OTHER", x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs$Birth)}
      else if (SelectedClassification() == "Continental Sub-Regions and Intermediate Regions") {
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs$Birth <- gsub(pattern = "NORTHERN AMERICA", replacement = "Northern America", x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs$Birth)}
      else if (SelectedClassification() == "Geographic Regions (SDG)") {
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs$Birth <- gsub(pattern = "Oceania \\(excluding Australia and New Zealand\\)", replacement = "Oceania*", x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs$Birth)}
      else if (SelectedClassification() == "3 Development Levels") {
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs$Birth <- gsub(pattern = "Less Developed Countries \\(Least Developed Countries excluded\\)", replacement = "Less Developed Countries*", x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs$Birth)
        FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs$Birth <- gsub(pattern = "Least Developed Countries \\(LDC\\)", replacement = "Least Developed Countries", x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs$Birth)}

      #Residence-Birth/Year To Year/Residence/Birth
      FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs <- FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs %>%
        group_by(Year, Residence, ResidenceCode, Birth, BirthCode) %>% summarise(
          iMIGRANT_Stock_Total, iMIGRANT_Stock_Males, iMIGRANT_Stock_Females, Share_of_iMIGRANT_Stock_Total, Share_of_iMIGRANT_Stock_Males, Share_of_iMIGRANT_Stock_Females)
      
      if (SelectedVariable() == "Both Sexes Combined") {
        Selected_Variable <- "Share_of_iMIGRANT_Stock_Total"
        Selected_Group <- "Both Sexes Combined"}
      else if (SelectedVariable() == "Males") {
        Selected_Variable <- "Share_of_iMIGRANT_Stock_Males"
        Selected_Group <- "Males"}
      else if (SelectedVariable() == "Females") {
        Selected_Variable <- "Share_of_iMIGRANT_Stock_Females"
        Selected_Group <- "Females"}
      
      FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs$Birth <- factor(x = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs$Birth, levels = rev(LVs))
      
      plot_ly(data = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs, x = ~Year, y = as.formula(paste0("~I(", Selected_Variable, "/100)")),
              color = ~Birth, type = "bar", colors = rev(COLORs), legendgroup = 'Birth',
              text = with(data = FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs, expr = paste0("<b>Residence: </b>", Residence, "<br>", 
                                                                                                "<b>Birth: </b>", Birth, "<br>", 
                                                                                                "<b>Year: </b>", Year, "<br>", 
                                                                                                "<b>International MIGRANT Stock (", Selected_Group, "): </b>", round(FilteredMIGRANT_STOCKs_DATA_Stacked_BARs_SHAREs[[Selected_Variable]], DecimalChoice()), "%"))) %>%
        style(textposition = "none", hoverinfo = "text") %>%
        layout(barmode = "stack",
               yaxis = list(title = paste0("International Migrant Stock (", Selected_Group,")"), tickformat = ".0%"),
               title = paste("Shares of International Migrant Stocks in", SelectedResidence(), "from", X, "(1990-2020)"),
               legend = list(title = list(text = '<b>Birth</b>'), x=1, y=0.5),
               margin = list(t = 30))
      
      }
    
    })
  
  #################
  ##### BIRTH #####
  
  ##### COUNTRIEs ACTION BUTTON #####
  observeEvent(input$MB_COUNTRIEs, { #WHEN COUNTRIEs ACTION BUTTON is clicked
    
    #Add CSS class from COUNTRIEs ACTION BUTTON
    shinyjs::addClass(id = "MB_COUNTRIEs", class = "active")
    #Remove CSS class from REGIONs ACTION BUTTON
    shinyjs::removeClass(id = "MB_REGIONs", class = "active")
    #Make REGIONs CHOICE RADIO BUTTONs invisible
    shinyjs::hide(id = "MBR_REGIONs_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  #Make REGIONs CHOICE RADIO BUTTONs invisible (first click will also be animated)
  shinyjs::hide(id = "MBR_REGIONs_CHOICE", anim = FALSE)
  
  ##### REGIONs ACTION BUTTON #####
  observeEvent(input$MB_REGIONs, { #WHEN REGIONs ACTION BUTTON is clicked
    
    #Add CSS class from REGIONs ACTION BUTTON
    shinyjs::addClass(id = "MB_REGIONs", class = "active")
    #Remove CSS class from COUNTRIEs ACTION BUTTON
    shinyjs::removeClass(id = "MB_COUNTRIEs", class = "active")
    #Make REGIONs CHOICE RADIO BUTTONs visible
    shinyjs::show(id = "MBR_REGIONs_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  ###################
  ##### CLIMATE #####
  ###################
  
  ##### COUNTRIEs ACTION BUTTON #####
  observeEvent(input$CLIMATE_COUNTRIEs, { #WHEN COUNTRIEs ACTION BUTTON is clicked
    
    #Add CSS class from COUNTRIEs ACTION BUTTON
    shinyjs::addClass(id = "CLIMATE_COUNTRIEs", class = "active")
    #Remove CSS class from REGIONs ACTION BUTTON
    shinyjs::removeClass(id = "CLIMATE_REGIONs", class = "active")
    #Make REGIONs CHOICE RADIO BUTTONs invisible
    shinyjs::hide(id = "CLIMATE_R_REGIONs_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  #Make REGIONs CHOICE RADIO BUTTONs invisible (first click will also be animated)
  shinyjs::hide(id = "CLIMATE_R_REGIONs_CHOICE", anim = FALSE)
  
  ##### REGIONs ACTION BUTTON #####
  observeEvent(input$CLIMATE_REGIONs, { #WHEN REGIONs ACTION BUTTON is clicked
    
    #Add CSS class from REGIONs ACTION BUTTON
    shinyjs::addClass(id = "CLIMATE_REGIONs", class = "active")
    #Remove CSS class from COUNTRIEs ACTION BUTTON
    shinyjs::removeClass(id = "CLIMATE_COUNTRIEs", class = "active")
    #Make REGIONs CHOICE RADIO BUTTONs visible
    shinyjs::show(id = "CLIMATE_R_REGIONs_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  ##################################
  #####  MIGRATION and CLIMATE #####
  ##################################
  
  #####################
  ##### RESIDENCE #####
  
  ##### COUNTRIEs ACTION BUTTON #####
  observeEvent(input$MCR_COUNTRIEs, { #WHEN COUNTRIEs ACTION BUTTON is clicked
    
    #Add CSS class from COUNTRIEs ACTION BUTTON
    shinyjs::addClass(id = "MCR_COUNTRIEs", class = "active")
    #Remove CSS class from REGIONs ACTION BUTTON
    shinyjs::removeClass(id = "MCR_REGIONs", class = "active")
    #Make REGIONs CHOICE RADIO BUTTONs invisible
    shinyjs::hide(id = "MCRR_REGIONs_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  #Make REGIONs CHOICE RADIO BUTTONs invisible (first click will also be animated)
  shinyjs::hide(id = "MCRR_REGIONs_CHOICE", anim = FALSE)
  
  ##### REGIONs ACTION BUTTON #####
  observeEvent(input$MCR_REGIONs, { #WHEN REGIONs ACTION BUTTON is clicked
    
    #Add CSS class from REGIONs ACTION BUTTON
    shinyjs::addClass(id = "MCR_REGIONs", class = "active")
    #Remove CSS class from COUNTRIEs ACTION BUTTON
    shinyjs::removeClass(id = "MCR_COUNTRIEs", class = "active")
    #Make REGIONs CHOICE RADIO BUTTONs visible
    shinyjs::show(id = "MCRR_REGIONs_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  #################
  ##### BIRTH #####
  
  ##### COUNTRIEs ACTION BUTTON #####
  observeEvent(input$MCB_COUNTRIEs, { #WHEN COUNTRIEs ACTION BUTTON is clicked
    
    #Add CSS class from COUNTRIEs ACTION BUTTON
    shinyjs::addClass(id = "MCB_COUNTRIEs", class = "active")
    #Remove CSS class from REGIONs ACTION BUTTON
    shinyjs::removeClass(id = "MCB_REGIONs", class = "active")
    #Make REGIONs CHOICE RADIO BUTTONs invisible
    shinyjs::hide(id = "MCBR_REGIONs_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  #Make REGIONs CHOICE RADIO BUTTONs invisible (first click will also be animated)
  shinyjs::hide(id = "MCBR_REGIONs_CHOICE", anim = FALSE)
  
  ##### REGIONs ACTION BUTTON #####
  observeEvent(input$MCB_REGIONs, { #WHEN REGIONs ACTION BUTTON is clicked
    
    #Add CSS class from REGIONs ACTION BUTTON
    shinyjs::addClass(id = "MCB_REGIONs", class = "active")
    #Remove CSS class from COUNTRIEs ACTION BUTTON
    shinyjs::removeClass(id = "MCB_COUNTRIEs", class = "active")
    #Make REGIONs CHOICE RADIO BUTTONs visible
    shinyjs::show(id = "MCBR_REGIONs_CHOICE", anim = TRUE, animType = "slide", time = 0.25)
    
    })
  
  }