##########################################################################
#####                Climate ChanGe MiGration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   KYLLIAN JAMES | 2023-11-14                   #####
#####                        Mod.: 2024-10-02                        #####
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
  # shinyjs::onclick(
  #   id = "home-dea-capture", #Element-id() => Capture => Data Exploration Application
  #   expr = { #R Expression To Run
  #     runjs("
  #       //Retrieve all <li> with class='nav-item'
  #       var $NavItems = $(); //Initialize jQUERY-OBJECT To store nav-item elements
  #       $('li[class]').each(function() { //Iterate over each <li> that have a 'class' attribute
  #         var Class = $(this).attr('class'); //Retrieve Class
  #         if (Class === 'nav-item') { //Retrieve li that have a 'class' attribute === nav-item
  #             var $NavLinksInLi = $(this).find('.nav-link'); //Find all child elements with class='nav-link'
  #             $NavItems = $NavItems.add($NavLinksInLi);}}); //Add NavLinksInLi To $NavItems
  #       //Retrieve all <li> with class='dropdown nav-item'
  #       var $DropdownNavItems = $(); //Initialize jQUERY-OBJECT To store 'dropdown nav-item' elements
  #       var $DropdownItems = $(); //Initialize jQUERY-OBJECT To store 'dropdown-item' elements
  #       $('li[class]').each(function() { //Iterate over each <li> that have a 'class' attribute
  #         var Class = $(this).attr('class'); //Retrieve Class
  #         if (Class === 'dropdown nav-item') { //Retrieve li that have a 'class' attribute === 'dropdown nav-item'
  #             var $DropdownNavLinksInLi = $(this).find('.dropdown-toggle.nav-link'); //Find all child elements with class='dropdown-toggle nav-link'
  #             $DropdownNavItems = $DropdownNavItems.add($DropdownNavLinksInLi); //Add DropdownNavLinksInLi To $DropdownNavItems
  #             var $DropdownMenu = $(this).find('ul.dropdown-menu'); //Find all <ul> elements with class 'dropdown-menu'
  #             var $DropdownItemsInLi = $DropdownMenu.find('a.dropdown-item'); //Find all <a> elements with class 'dropdown-item' within $DropdownMenu
  #             $DropdownItems = $DropdownItems.add($DropdownItemsInLi);}}); //Add DropdownItemsInLi to $DropdownItems
  #       //Retrieve HOME NavItem
  #       var CurrentNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Home';});
  #       //Retrieve DEA_Menu DropdownNavItem
  #       var DropdownNavItem = $DropdownNavItems.filter(function() {return $(this).attr('data-value') === 'DEA_Menu';});
  #       //Desactivate CurrentNavItem
  #       if (CurrentNavItem.hasClass('active')) {CurrentNavItem.removeClass('active');}
  #       //Activate DropdownNavItem
  #       if (!DropdownNavItem.hasClass('active')) {DropdownNavItem.addClass('active');}
  #       //NestedDropdownItems
  #       var MIGRATION_DropdownItem = $DropdownItems.filter(function() {return $(this).attr('data-value') === 'Migration';});
  #       var Climate_DropdownItem = $DropdownItems.filter(function() {return $(this).attr('data-value') === 'Climate';});
  #       var MIGRATION_and_Climate_DropdownItem = $DropdownItems.filter(function() {return $(this).attr('data-value') === 'Migration and Climate';});
  #       //Activate/Desactivate DropdownItems
  #       if (!MIGRATION_DropdownItem.hasClass('active')) {MIGRATION_DropdownItem.addClass('active');}
  #       if (Climate_DropdownItem.hasClass('active')) {Climate_DropdownItem.removeClass('active');}
  #       if (MIGRATION_and_Climate_DropdownItem.hasClass('active')) {MIGRATION_and_Climate_DropdownItem.removeClass('active');}
  #       //Retrieve CurrentTabPane
  #       var CurrentTabPane = $('#home');
  #       //Retrieve all <div> with class='tab-pane'
  #       var $TabPanes = $('div.tab-pane');
  #       //Retrieve TabPane
  #       var TabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Migration';});
  #       //Desactivate CurrentTabPane
  #       if (CurrentTabPane.hasClass('active')) {CurrentTabPane.removeClass('active');}
  #       //Activate TabPane
  #       if (!TabPane.hasClass('active')) {TabPane.addClass('active');}
  #       //NestedNavItems
  #       var ResidenceNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Residence';}).first();
  #       var BirthNavItem = $NavItems.filter(function() {return $(this).attr('data-value') === 'Birth';}).first();
  #       //Activate/Desactivate NavItems
  #       if (!ResidenceNavItem.hasClass('active')) {ResidenceNavItem.addClass('active');}
  #       if (BirthNavItem.hasClass('active')) {BirthNavItem.removeClass('active');}
  #       //NestedTabPanes
  #       var ResidenceTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Residence';}).first();
  #       var BirthTabPane = $TabPanes.filter(function() {return $(this).attr('data-value') === 'Birth';}).first();
  #       //Activate/Desactivate TabPanes
  #       if (!ResidenceTabPane.hasClass('active')) {ResidenceTabPane.addClass('active');}
  #       if (BirthTabPane.hasClass('active')) {BirthTabPane.removeClass('active');}")})
  
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
  
  ################################
  ##### Reactive Values (RV) #####
  ################################
  
  ##### World Map Card RV #####
  World_Map_Card_RV <- reactiveValues(
    TilesLoaded = FALSE, #This value is intended to indicate whether World Map Tiles have been loaded or not
    MR_REGIONs_Click_Count = 0, #Track clicks on REGIONs ACTION BUTTON
    WM_Card_PROPERTY = FALSE) #New PROPERTY (CSS) on World Map Card => WM_Card_PROPERTY = TRUE
  
  #####################
  ##### MIGRATION #####
  #####################
  
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
      
      }
  
                         
    #PROPERTY (CSS) on World Map Card => Condition(s) => COUNTRIEs ACTION BUTTON is active/clicked | REGIONs ACTION BUTTON Click Count > 0
    if (World_Map_Card_RV$MR_REGIONs_Click_Count > 0) {runjs("$('#MR_World_Map_Card').css('margin-top', '-10px');")}
    
    #Reactive Value To TRUE => World Map Card Have a New PROPERTY
    if (World_Map_Card_RV$MR_REGIONs_Click_Count > 0) {World_Map_Card_RV$WM_Card_PROPERTY <- TRUE}
    
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
    
    #Track clicks on REGIONs ACTION BUTTON (+1)
    World_Map_Card_RV$MR_REGIONs_Click_Count <- World_Map_Card_RV$MR_REGIONs_Click_Count + 1
    
    #PROPERTY (CSS) on World Map Card => Condition(s) => REGIONs ACTION BUTTON is active/clicked | World Map Card Have a New PROPERTY (WM_Card_PROPERTY = TRUE)
    if (World_Map_Card_RV$WM_Card_PROPERTY) {runjs("$('#MR_World_Map_Card').css('margin-top', '0px');")}
    
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