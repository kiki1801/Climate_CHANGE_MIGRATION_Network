##########################################################################
#####                Climate ChanGe MiGration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   KYLLIAN JAMES | 2023-11-14                   #####
#####                        Mod.: 2024-09-10                        #####
#####                               UI                               #####
##########################################################################

##########################
##### Load Libraries #####
##########################

library(bslib)
library(shiny)
library(shinyjs)
library(slickR)
library(waiter)
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
  "card-bg" = "#FEFEFE", #From "#FCFCFC" To "#FEFEFE"
  "dropdown-bg" = "var(--bs-light)", #From "#FCFCFC" To "#E6E6E6"
  "dropdown-border-color" = "none", #Remove Dropdown Menu BORDER
  "dropdown-border-radius" = "5px", #Dropdown Menu Border Radius from 3px To 5px
  "dropdown-link-color" = "var(--bs-nav-link-color)", #Dropdown ITEM(s) => TEXT => COLOR => !Active
  "dropdown-link-hover-color" = "var(--bs-nav-link-hover-color)", #Dropdown ITEM(s) => TEXT => COLOR => HOVER
  "dropdown-link-active-color" = "var(--bs-navbar-active-color)", #Dropdown ITEM(s) => TEXT => COLOR => Active
  "dropdown-link-active-bg" = "#B0B0B0") #Dropdown ITEM(s) => TEXT => BACKGROUND => COLOR => Active

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
  "card-bg" = "var(--bs-secondary)", #From "#1D1F21" To "#404040"
  "dropdown-bg" = "var(--bs-light)", #From "#1D1F21" To "#??????"
  "dropdown-border-color" = "none", #Remove Dropdown Menu BORDER
  "dropdown-border-radius" = "5px", #Dropdown Menu Border Radius from 3px To 5px
  "dropdown-link-color" = "var(--bs-nav-link-color)", #Dropdown ITEM(s) => TEXT => COLOR => !Active
  "dropdown-link-hover-color" = "var(--bs-nav-link-hover-color)", #Dropdown ITEM(s) => TEXT => COLOR => HOVER
  "dropdown-link-active-color" = "var(--bs-navbar-active-color)", #Dropdown ITEM(s) => TEXT => COLOR => Active
  "dropdown-link-active-bg" = "#B0B0B0") #Dropdown ITEM(s) => TEXT => BACKGROUND => COLOR => Active

#######################
##### FUNCTION(s) #####
#######################

#PARTNER Card Function
PARTNER_Card <- function(LINK, SOURCE, WIDTH, HEIGHT, id_attribute, PARTNER) {
  card(
    #PARTNER => LOGO + LINK
    card_body(
      #PARTNER => LOGO => Clickable
      tags$a(
        href = LINK, #Link To PARTNER WEBSITE
        target = "_blank", #Open a new tab when SOURCE (i.e. LOGO) is clicked
        # style = "padding-bottom: 20px", #LOGO => POSITION
        #PARTNER => LOGO
        tags$img(
          src = SOURCE, #PATH OR LINK To LOGO
          width = WIDTH, height = HEIGHT, #LOGO => WITDH/HEIGHT
          class = "PARTNER-LOGO", #Clickable LOGO Class
          id = id_attribute, #Allow To Switch LOGO (!Dark/Dark Mode) in SERVER.R
          title = "Visit Partner's Website")), #TEXT SHOW on HOVER
      #PARTNER => NAME => Clickable
      tags$a(
        href = LINK, #Link To PARTNER WEBSITE
        target = "_blank", #Open a new tab when NAME/ICON is clicked
        # style = "text-decoration: none; color: var(--bs-body-color); position: absolute; bottom: 16px;", #TEXT/ICON => COLOR | POSITION
        style = "text-decoration: none; color: var(--bs-body-color);", #TEXT/ICON => COLOR
        class = "PARTNER-NAME", #Clickable text/icon class
        title = "Visit Partner's Website", #TEXT SHOW on HOVER
        #BOOTSTRAP_ICON
        bsicons::bs_icon(
          name = "globe2", #Globe ICON from https://icons.getbootstrap.com/
          size = "16px", #ICON => HEIGHT and WIDTH
          title = "Visit Partner's Website"), #TEXT SHOW on HOVER
        PARTNER), #PARTNER => NAME
      #Card_BODY => ARGu.
      fillable = TRUE, #FLEXBOX CONTAINER
      min_height = "260px", max_height = "260px", #Card => HEIGHT => !Full_Screen
      padding = c(16, 24, 16, 24), #PADDING => TOP/BOTTOM To 16px | LEFT/RIGHT To 24px
      fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
      # style = "align-items: center; justify-content: center; overflow: hidden"), #ITEM(s) => CENTER || Remove SCROLLING BAR(s)
      style = "align-items: center; justify-content: space-between; overflow: hidden"), #ITEM(s) => CENTER || Remove SCROLLING BAR(s)
    #Card => ARGu.
    full_screen = FALSE, #!Expand Card To Fit Screen Size
    fill = FALSE)} #Content => Fixed Size => !Allowed To Scroll

#PUBLICATION Card Function
PUBLICATION_Card <- function(
    ARTICLE, AUTHORs, #Related To ARTICLE
    PUBLISHER, VOLUME, ISSUE, PAGEs, #Related To PUBLISHER
    DATE, DOI) { #DATE + LINK
  #Convert DATE To Sortable Format (YYYY-MM)
  SORTABLE_DATE <- format(lubridate::parse_date_time(
    x = DATE, #CHARACTER VECTOR (INPUT)
    orders = "my"),  #CHARACTER VECTOR => Date-Time Formats
    format = "%Y-%m") #VECTOR => Formats(OUTPUT)
  #AUTHORs => NUMBER
  AUTHORs_NUMBER <- length(AUTHORs)
  #PAGEs => NUMBER
  PAGEs_NUMBER <- length(PAGEs)
  #PUBLICATION => Card
  card(
    #Attribute 'data-date' To Sort Card(s)
    `data-date` = SORTABLE_DATE,
    #Attribute 'data-title' To Sort Card(s)
    `data-title` = ARTICLE,
    #Card => Headline => Full ARTICLE Title
    card_header(ARTICLE),
    #ARTICLE => INFORMATION
    card_body(
      #AUTHORs => ICON + NAMEs
      tags$div(
        #FLEXBOX CONTAINER
        style = "display: flex",
        #AUTHORs => ICON
        # AUTHORs_ICON <- if (AUTHORs_NUMBER > 1) {
        #   tags$img(
        #     src = "Icons/IONICONS/ion-people-outline-black.svg", #PATH OR LINK To ICON
        #     width = "16px", height = "16px", #ICON => WITDH/HEIGHT
        #     class = "ion-people")} else { #Allow To Switch ICON (!Dark/Dark Mode) in SERVER.R + POSITION
        #       tags$img(
        #         src = "Icons/IONICONS/ion-person-outline-black.svg", #PATH OR LINK To ICON
        #         width = "16px", height = "16px", #ICON => WITDH/HEIGHT
        #         class = "ion-person")}, #Allow To Switch ICON (!Dark/Dark Mode) in SERVER.R + POSITION
        AUTHORs_ICON <- if (AUTHORs_NUMBER > 1) {
          bsicons::bs_icon(
            name = "people", #People ICON from https://icons.getbootstrap.com/
            size = "16px")} else { #ICON => HEIGHT and WIDTH
              bsicons::bs_icon(
                name = "person", #Person ICON from https://icons.getbootstrap.com/
                size = "16px")}, #ICON => HEIGHT and WIDTH
        #AUTHORs => NAMEs
        tags$span(
          #MARGIN (Left: 5px)
          style = "margin-left: 5px;",
          #Formatted NAMEs
          AUTHORs_NAMEs <- if (AUTHORs_NUMBER > 1) {
            paste(paste(AUTHORs[1:(AUTHORs_NUMBER-1)], collapse = ", "), "and", AUTHORs[AUTHORs_NUMBER])} else {AUTHORs})),
      #PUBLISHER => ICON + NAME + VOLUME + ISSUE + PAGE(s)
      tags$div(
        #FLEXBOX CONTAINER
        style = "display: flex",
        #PUBLISHER => ICON
        tags$img(
          src = "Icons/IONICONS/ion-newspaper-outline-black.svg", #PATH OR LINK To ICON
          width = "16px", height = "16px", #ICON => WITDH/HEIGHT
          class = "ion-newspaper"), #Allow To Switch ICON (!Dark/Dark Mode) in SERVER.R + POSITION
        #PUBLISHER => NAME + VOLUME + ISSUE + PAGE(s)
        tags$span(
          #MARGIN (Left: 5px)
          style = "margin-left: 5px;",
          #Formatted => PUBLISHER => NAME + VOLUME + ISSUE + PAGE(s)
          HTML(
            paste0(
              #PUBLISHER => NAME
              tags$i(PUBLISHER),
              #PUBLISHER => VOLUME
              if (!is.null(VOLUME)) {paste0(", vol. ", VOLUME)} else {""},
              #PUBLISHER => ISSUE
              if (!is.null(ISSUE)) {paste0(", no. ", ISSUE)} else {""},
              #PUBLISHER => PAGE(s)
              if (!is.null(PAGEs)) {
                if (PAGEs_NUMBER > 1) {
                  paste0(", pp. ", PAGEs[1], "-", PAGEs[2])} else {paste0(", p. ", PAGEs)}} else {""})))),
      #DATE => ICON + (MONTH + YEAR)
      tags$div(
        #FLEXBOX CONTAINER
        style = "display: flex",
        #DATE => BOOTSTRAP_ICON
        bsicons::bs_icon(
          name = "calendar4-event", #Calendar ICON from https://icons.getbootstrap.com/
          size = "16px"), #ICON => HEIGHT and WIDTH
        #DATE => MONTH + YEAR | MARGIN (Left: 5px)
        tags$span(style = "margin-left: 5px;", DATE)),
      #DOI => ICON + LINK => Clickable
      if (!is.null(DOI)) { #Add DOI Link ONLY if exists
        tags$a(
          href = DOI, #Link To PUBLICATION
          target = "_blank", #Open a new tab when text/icon is clicked
          style = "text-decoration: none; color: var(--bs-body-color);", #TEXT/ICON => COLOR
          class = "PUBLICATION-DOI", #Clickable text/icon class
          title = "Access Full Publication", #TEXT SHOW on HOVER
          tags$div(
            #FLEXBOX CONTAINER
            style = "display: flex",
            #DOI => BOOTSTRAP_ICON
            bsicons::bs_icon(
              name = "link", #Link ICON from https://icons.getbootstrap.com/
              size = "16px", #ICON => HEIGHT and WIDTH
              title = "Access Full Publication"), #TEXT SHOW on HOVER
            #DOI => LINK | MARGIN (Left: 5px)
            tags$span(style = "margin-left: 5px;", DOI)))},
      #Card_BODY => ARGu.
      fillable = TRUE, #FLEXBOX CONTAINER
      min_height = "115px", max_height = "120px", #Card => HEIGHT => !Full_Screen
      padding = c(0, 16, 8, 16), #PADDING => TOP To 0px | BOTTOM To 8px | LEFT/RIGHT To 16px
      fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
      style = "justify-content: flex-start; overflow: hidden"), #ITEM(s) => CENTER || Remove SCROLLING BAR(s)
    #Card => ARGu.
    full_screen = FALSE, #!Expand Card To Fit Screen Size
    fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
    style = "margin-top: 10px")} #MARGIN (Top: 10px) + ROW-GAP To 20px

#WORKING-PAPER Card Function
WORKING.PAPER_Card <- function(
    WORKING_PAPER, AUTHORs, WORKING_PAPER_SERIEs, WORKING_PAPER_NUMBER, #Related To WORKING-PAPER
    INSTITUTION_LINK, INSTITUTION, DATE, LINK) { #INSTITUTION + DATE + LINK
  #Convert DATE To Sortable Format (YYYY-MM)
  SORTABLE_DATE <- format(lubridate::parse_date_time(
    x = DATE, #CHARACTER VECTOR (INPUT)
    orders = "my"),  #CHARACTER VECTOR => Date-Time Formats
    format = "%Y-%m") #VECTOR => Formats(OUTPUT)
  #AUTHORs => NUMBER
  AUTHORs_NUMBER <- length(AUTHORs)
  #WORKING-PAPER => Card
  card(
    #Attribute 'data-date' To Sort Card(s)
    `data-date` = SORTABLE_DATE,
    #Attribute 'data-title' To Sort Card(s)
    `data-title` = WORKING_PAPER,
    #Card => Headline => Full WORKING-PAPER Title
    card_header(WORKING_PAPER),
    #WORKING-PAPER => INFORMATION
    card_body(
      #AUTHORs => ICON + NAMEs
      tags$div(
        #FLEXBOX CONTAINER
        style = "display: flex",
        #AUTHORs => ICON
        # AUTHORs_ICON <- if (AUTHORs_NUMBER > 1) {
        #   tags$img(
        #     src = "Icons/IONICONS/ion-people-outline-black.svg", #PATH OR LINK To ICON
        #     width = "16px", height = "16px", #ICON => WITDH/HEIGHT
        #     class = "ion-people")} else { #Allow To Switch ICON (!Dark/Dark Mode) in SERVER.R + POSITION
        #       tags$img(
        #         src = "Icons/IONICONS/ion-person-outline-black.svg", #PATH OR LINK To ICON
        #         width = "16px", height = "16px", #ICON => WITDH/HEIGHT
        #         class = "ion-person")}, #Allow To Switch ICON (!Dark/Dark Mode) in SERVER.R + POSITION
        AUTHORs_ICON <- if (AUTHORs_NUMBER > 1) {
          bsicons::bs_icon(
            name = "people", #People ICON from https://icons.getbootstrap.com/
            size = "16px")} else { #ICON => HEIGHT and WIDTH
              bsicons::bs_icon(
                name = "person", #Person ICON from https://icons.getbootstrap.com/
                size = "16px")}, #ICON => HEIGHT and WIDTH
        #AUTHORs => NAMEs
        tags$span(
          #MARGIN (Left: 5px)
          style = "margin-left: 5px;",
          #Formatted NAMEs
          AUTHORs_NAMEs <- if (AUTHORs_NUMBER > 1) {
            paste(paste(AUTHORs[1:(AUTHORs_NUMBER-1)], collapse = ", "), "and", AUTHORs[AUTHORs_NUMBER])} else {AUTHORs})),
      #ICON + WORKING-PAPER-SERIEs + NUMBER
      tags$div(
        #FLEXBOX CONTAINER
        style = "display: flex",
        #WORKING-PAPER-SERIEs => BOOTSTRAP_ICON
        bsicons::bs_icon(
          name = "file-earmark-text", #WORKING-PAPER ICON from https://icons.getbootstrap.com/
          size = "16px"), #ICON => HEIGHT and WIDTH
        #WORKING-PAPER-SERIEs + NUMBER
        tags$span(
          #MARGIN (Left: 5px)
          style = "margin-left: 5px;",
          #Formatted => WORKING-PAPER-SERIEs + NUMBER
          paste0(
            #WORKING-PAPER-SERIEs
            WORKING_PAPER_SERIEs, 
            #WORKING-PAPER => NUMBER
            if (!is.null(WORKING_PAPER_NUMBER)) {paste0(", no. ", WORKING_PAPER_NUMBER)} else {""}))),
      #ICON + INSTITUTION
      tags$a(
        href = INSTITUTION_LINK, #Link To INSTITUTION
        target = "_blank", #Open a new tab when text/icon is clicked
        style = "text-decoration: none; color: var(--bs-body-color);", #TEXT/ICON => COLOR
        class = "INSTITUTION", #Clickable text/icon class
        title = "Visit Website", #TEXT SHOW on HOVER
        tags$div(
          #FLEXBOX CONTAINER
          style = "display: flex",
          #INSTITUTION => BOOTSTRAP_ICON
          bsicons::bs_icon(
            name = "bank", #Institution ICON from https://icons.getbootstrap.com/
            size = "16px", #ICON => HEIGHT and WIDTH
            title = "Visit Website"), #TEXT SHOW on HOVER
          #LINK | MARGIN (Left: 5px)
          tags$span(style = "margin-left: 5px;", INSTITUTION))),
      #DATE => ICON + (MONTH + YEAR)
      tags$div(
        #FLEXBOX CONTAINER
        style = "display: flex",
        #DATE => BOOTSTRAP_ICON
        bsicons::bs_icon(
          name = "calendar4-event", #Calendar ICON from https://icons.getbootstrap.com/
          size = "16px"), #ICON => HEIGHT and WIDTH
        #DATE => MONTH + YEAR | MARGIN (Left: 5px)
        tags$span(style = "margin-left: 5px;", DATE)),
      #ICON + LINK => Clickable
      if (!is.null(LINK)) { #Add Link ONLY if exists
        tags$a(
          href = LINK, #Link To WORKING-PAPER
          target = "_blank", #Open a new tab when text/icon is clicked
          style = "text-decoration: none; color: var(--bs-body-color);", #TEXT/ICON => COLOR
          class = "WORKING-PAPER-LINK", #Clickable text/icon class
          title = "Access Full Text", #TEXT SHOW on HOVER
          tags$div(
            #FLEXBOX CONTAINER
            style = "display: flex",
            #LINK => BOOTSTRAP_ICON
            bsicons::bs_icon(
              name = "link", #Link ICON from https://icons.getbootstrap.com/
              size = "16px", #ICON => HEIGHT and WIDTH
              title = "Access Full Text"), #TEXT SHOW on HOVER
            #LINK | MARGIN (Left: 5px)
            tags$span(style = "margin-left: 5px;", LINK)))},
      #Card_BODY => ARGu.
      fillable = TRUE, #FLEXBOX CONTAINER
      min_height = "145px", max_height = "150px", #Card => HEIGHT => !Full_Screen
      padding = c(0, 16, 8, 16), #PADDING => TOP To 0px | BOTTOM To 8px | LEFT/RIGHT To 16px
      fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
      style = "justify-content: flex-start; overflow: hidden"), #ITEM(s) => CENTER || Remove SCROLLING BAR(s)
    #Card => ARGu.
    full_screen = FALSE, #!Expand Card To Fit Screen Size
    fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
    style = "margin-top: 10px")} #MARGIN (Top: 10px) + ROW-GAP To 20px

#PRESENTATION Card Function
PRESENTATION_Card <- function(
    PRESENTATION, SPEAKERs, #Related To PRESENTATION
    ORGANIZERs_LINK, ORGANIZERs, #Related To ORGANIZER(s)
    DATE, LOCATION, EVENT_LINK, EVENT) {#Related To EVENT
  #Replace DATE-RANGE BY FIRST-DAY
  DATE_Cleaned <- stringr::str_replace(
    string = DATE, #CHARACTER VECTOR (INPUT)
    pattern = "(\\d+)-\\d+ ", #REGEX
    replacement = "\\1 ") #Replacement Value
  #Convert DATE To Sortable Format (YYYY-MM-DD)
  SORTABLE_DATE <- format(lubridate::parse_date_time(
    x = DATE_Cleaned, #CHARACTER VECTOR (INPUT)
    orders = "dmy"),  #CHARACTER VECTOR => Date-Time Formats
    format = "%Y-%m-%d") #VECTOR => Formats(OUTPUT)
  #SPEAKERs => NUMBER
  SPEAKERs_NUMBER <- length(SPEAKERs)
  #ORGANIZER(s) => NUMBER
  ORGANIZERs_NUMBER <- length(ORGANIZERs)
  #PRESENTATION => Card
  card(
    #Attribute 'data-date' To Sort Card(s)
    `data-date` = SORTABLE_DATE,
    #Attribute 'data-title' To Sort Card(s)
    `data-title` = PRESENTATION,
    #Card => Headline => Full PRESENTATION Title
    card_header(PRESENTATION),
    #PRESENTATION => INFORMATION
    card_body(
      #SPEAKERs => ICON + NAMEs
      tags$div(
        #FLEXBOX CONTAINER
        style = "display: flex",
        #SPEAKERs => ICON
        # SPEAKERs_ICON <- if (SPEAKERs_NUMBER > 1) {
        #   tags$img(
        #     src = "Icons/IONICONS/ion-people-outline-black.svg", #PATH OR LINK To ICON
        #     width = "16px", height = "16px", #ICON => WITDH/HEIGHT
        #     class = "ion-people")} else { #Allow To Switch ICON (!Dark/Dark Mode) in SERVER.R + POSITION
        #       tags$img(
        #         src = "Icons/IONICONS/ion-person-outline-black.svg", #PATH OR LINK To ICON
        #         width = "16px", height = "16px", #ICON => WITDH/HEIGHT
        #         class = "ion-person")}, #Allow To Switch ICON (!Dark/Dark Mode) in SERVER.R + POSITION
        SPEAKERs_ICON <- if (SPEAKERs_NUMBER > 1) {
          bsicons::bs_icon(
            name = "people", #People ICON from https://icons.getbootstrap.com/
            size = "16px")} else { #ICON => HEIGHT and WIDTH
              bsicons::bs_icon(
                name = "person", #Person ICON from https://icons.getbootstrap.com/
                size = "16px")}, #ICON => HEIGHT and WIDTH
        #SPEAKERs => NAMEs
        tags$span(
          #MARGIN (Left: 5px)
          style = "margin-left: 5px;",
          #Formatted NAMEs
          SPEAKERs_NAMEs <- if (SPEAKERs_NUMBER > 1) {
            paste(paste(AUTHORs[1:(SPEAKERs_NAMEs-1)], collapse = ", "), "and", SPEAKERs[SPEAKERs_NUMBER])} else {SPEAKERs})),
      #ICON + ORGANIZER(s)
      # tags$a(
      #   href = ORGANIZERs_LINK, #Link To ORGANIZER
      #   target = "_blank", #Open a new tab when text/icon is clicked
      #   style = "text-decoration: none; color: var(--bs-body-color);", #TEXT/ICON => COLOR
      #   class = "ORGANIZER", #Clickable text/icon class
      #   title = "Visit Website", #TEXT SHOW on HOVER
      #   tags$div(
      #     #FLEXBOX CONTAINER
      #     style = "display: flex",
      #     #ORGANIZER => BOOTSTRAP_ICON
      #     bsicons::bs_icon(
      #       name = "bank", #Institution ICON from https://icons.getbootstrap.com/
      #       size = "16px", #ICON => HEIGHT and WIDTH
      #       title = "Visit Website"), #TEXT SHOW on HOVER
      #     #LINK | MARGIN (Left: 5px)
      #     tags$span(style = "margin-left: 5px;", ORGANIZERs))),
      if (ORGANIZERs_NUMBER > 1) {
        tags$div(
          #FLEXBOX CONTAINER
          style = "display: flex",
          #ORGANIZER(s) => ICON
          bsicons::bs_icon(
            name = "bank", #Institution ICON from https://icons.getbootstrap.com/
            size = "16px"), #ICON => HEIGHT and WIDTH
          #ORGANIZER(s) => NAME(s)
          tags$span(
            #MARGIN (Left: 5px)
            style = "margin-left: 5px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
            #Formatted NAME(s)
            # ORGANIZERs_NAMEs <- paste(ORGANIZERs, collapse = " | ")
            tags$a(
              href = ORGANIZERs_LINK[1], #Link To ORGANIZER
              target = "_blank", #Open a new tab when text is clicked
              style = "text-decoration: none; color: var(--bs-body-color);", #TEXT => COLOR
              class = "ORGANIZER", #Clickable text class
              title = "Visit Website", #TEXT SHOW on HOVER
              ORGANIZERs[1]), " | ",
            tags$a(
              href = ORGANIZERs_LINK[2], #Link To ORGANIZER
              target = "_blank", #Open a new tab when text is clicked
              style = "text-decoration: none; color: var(--bs-body-color);", #TEXT => COLOR
              class = "ORGANIZER", #Clickable text class
              title = "Visit Website", #TEXT SHOW on HOVER
              ORGANIZERs[2])))} else {
                                  tags$a(
                                    href = ORGANIZERs_LINK, #Link To ORGANIZER
                                    target = "_blank", #Open a new tab when text/icon is clicked
                                    style = "text-decoration: none; color: var(--bs-body-color);", #TEXT/ICON => COLOR
                                    class = "ORGANIZER", #Clickable text/icon class
                                    title = "Visit Website", #TEXT SHOW on HOVER
                                    tags$div(
                                      #FLEXBOX CONTAINER
                                      style = "display: flex",
                                      #ORGANIZER => BOOTSTRAP_ICON
                                      bsicons::bs_icon(
                                        name = "bank", #Institution ICON from https://icons.getbootstrap.com/
                                        size = "16px", #ICON => HEIGHT and WIDTH
                                        title = "Visit Website"), #TEXT SHOW on HOVER
                                      #LINK | MARGIN (Left: 5px)
                                      tags$span(style = "margin-left: 5px;", ORGANIZERs)))},
      #DATE => ICON + (DAY(s) + MONTH + YEAR) || ICON + LOCATION
      tags$div(
        #FLEXBOX CONTAINER
        style = "display: flex",
        #DATE => BOOTSTRAP_ICON
        bsicons::bs_icon(
          name = "calendar4-event", #Calendar ICON from https://icons.getbootstrap.com/
          size = "16px"), #ICON => HEIGHT and WIDTH
        #DATE => MONTH + YEAR | MARGINs (Left: 5px | RIGHT: 15px)
        tags$span(style = "margin-left: 5px; margin-right: 15px;", DATE),
        #LOCATION => BOOTSTRAP_ICON
        bsicons::bs_icon(
          name = "geo", #Location ICON from https://icons.getbootstrap.com/
          size = "16px"), #ICON => HEIGHT and WIDTH
        #LOCATION | MARGIN (Left: 5px)
        tags$span(style = "margin-left: 5px;", LOCATION)),
      #EVENT => ICON + LINK => Clickable
      if (!is.null(EVENT_LINK)) { #Add Link ONLY if exists
        tags$a(
          href = EVENT_LINK, #Link To EVENT
          target = "_blank", #Open a new tab when text/icon is clicked
          style = "text-decoration: none; color: var(--bs-body-color);", #TEXT/ICON => COLOR
          class = "EVENT-LINK", #Clickable text/icon class
          title = "Learn More", #TEXT SHOW on HOVER
          tags$div(
            #FLEXBOX CONTAINER
            style = "display: flex",
            #LINK => BOOTSTRAP_ICON
            bsicons::bs_icon(
              name = "info-circle", #INFO ICON from https://icons.getbootstrap.com/
              size = "16px", #ICON => HEIGHT and WIDTH
              title = "Learn More"), #TEXT SHOW on HOVER
            #EVENT => NAME | MARGIN (Left: 5px)
            tags$span(style = "margin-left: 5px;", EVENT)))},
      #Card_BODY => ARGu.
      fillable = TRUE, #FLEXBOX CONTAINER
      # min_height = "145px", max_height = "150px", #Card => HEIGHT => !Full_Screen
      padding = c(0, 16, 8, 16), #PADDING => TOP To 0px | BOTTOM To 8px | LEFT/RIGHT To 16px
      fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
      style = "justify-content: flex-start; overflow: hidden"), #ITEM(s) => CENTER || Remove SCROLLING BAR(s)
    #Card => ARGu.
    full_screen = FALSE, #!Expand Card To Fit Screen Size
    fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
    style = "margin-top: 10px")} #MARGIN (Top: 10px) + ROW-GAP To 20px

#MEMBER Card Function
MEMBER_Card <- function(
    MEMBER_SOURCE, MEMBER_WIDTH, MEMBER_HEIGHT, #Related To MEMBER_PHOTO
    NAME, POSITION, ROLE, Email, Pronoun, MEMBER_LINK, #Related To MEMBER
    AFFILIATION, AFFILIATION_LINK, AFFILIATION_SOURCE, AFFILIATION_WIDTH, AFFILIATION_HEIGHT, id_attribute) { #Related To MEMBER_AFFILIATION
  card(
    #MEMBER => PHOTO
    card_body(
      #MEMBER => PHOTO
      tags$img(
        src = MEMBER_SOURCE, #PATH OR LINK To PHOTO
        width = MEMBER_WIDTH, height = MEMBER_HEIGHT), #PHOTO => WITDH/HEIGHT
      #Card_BODY => ARGu.
      fillable = TRUE, #FLEXBOX CONTAINER
      min_height = "360px", max_height = "360px", #Card => HEIGHT => !Full_Screen
      padding = 0, #Remove PADDING
      fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
      style = "align-items: center; justify-content: center; overflow: hidden"), #ITEM(s) => CENTER || Remove SCROLLING BAR(s)
    #MEMBER => (NAME + ROLE) | POSITION | AFFILIATION (LOGO + LINK) | Email + Personal WEBSITE
    card_body(
      #MEMBER => NAME
      # tags$strong(NAME), #MEMBER => NAME => Bold
      #MEMBER => NAME + ROLE
      if (is.null(ROLE)) {
        tags$strong(NAME)} else { #MEMBER => NAME => Bold
          tags$strong(paste0(NAME, " (", ROLE, ")"))}, #MEMBER => NAME + ROLE => Bold
      #MEMBER => POSITION
      # POSITION, 
      #MEMBER => POSITION + AFFILIATION
      # paste(POSITION, AFFILIATION, sep = " - "),
      #MEMBER => ICON + POSITION
      tags$div(
        #FLEXBOX CONTAINER
        style = "display: flex",
        #POSITION => BOOTSTRAP_ICON
        bsicons::bs_icon(
          name = "person-badge", #POSITION ICON from https://icons.getbootstrap.com/
          size = "16px"), #ICON => HEIGHT and WIDTH
        #MEMBER => POSITION | MARGIN (Left: 2.25px)
        tags$span(style = "margin-left: 2.25px;", POSITION)),
      #AFFILIATION => LOGO => Clickable
      tags$a(
        href = AFFILIATION_LINK, #Link To AFFILIATION WEBSITE
        target = "_blank", #Open a new tab when AFFILIATION_SOURCE (i.e. LOGO) is clicked
        #AFFILIATION => LOGO
        tags$img(
          src = AFFILIATION_SOURCE, #PATH OR LINK To LOGO
          width = AFFILIATION_WIDTH, height = AFFILIATION_HEIGHT, #LOGO => WITDH/HEIGHT
          class = "AFFILIATION-LOGO", #Clickable LOGO Class
          id = id_attribute, #Allow To Switch LOGO (!Dark/Dark Mode) in SERVER.R
          title = "Visit Website")), #TEXT SHOW on HOVER
      #MEMBER => Email => Clickable
      tags$a(
        href = paste0("mailto:", Email), #Click on link => Open default email client
        style = "text-decoration: none; color: var(--bs-body-color);", #TEXT/ICON => COLOR
        class = "MEMBER-Email", #Clickable text/icon class
        title = paste("Contact", Pronoun), #TEXT SHOW on HOVER
        #BOOTSTRAP_ICON
        bsicons::bs_icon(
          name = "envelope-at", #Email ICON from https://icons.getbootstrap.com/
          size = "16px", #ICON => HEIGHT and WIDTH
          title = paste("Contact", Pronoun)), #TEXT SHOW on HOVER
        Email), #MEMBER => Email
      #MEMBER => Personal WEBSITE => Clickable
      if (!is.null(MEMBER_LINK)) { #Add Personal WEBSITE Link ONLY if exists
        tags$a(
          href = MEMBER_LINK, #Link To MEMBER Personal WEBSITE
          target = "_blank", #Open a new tab when text/icon is clicked
          style = "text-decoration: none; color: var(--bs-body-color);", #TEXT/ICON => COLOR
          class = "MEMBER-WEBSITE", #Clickable text/icon class
          title = "Visit Member's Website", #TEXT SHOW on HOVER
          #BOOTSTRAP_ICON
          bsicons::bs_icon(
            name = "globe2", #Globe ICON from https://icons.getbootstrap.com/
            size = "16px", #ICON => HEIGHT and WIDTH
            title = "Visit Member's Website"), #TEXT SHOW on HOVER
          "Personal Website")}, #MEMBER => Personal WEBSITE
      #Card_BODY => ARGu.
      fillable = TRUE, #FLEXBOX CONTAINER
      min_height = "225px", max_height = "225px", #Card => HEIGHT => !Full_Screen
      padding = c(16, 24, 16, 24), #PADDING => TOP/BOTTOM To 16px | LEFT/RIGHT To 24px
      fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
      # style = "align-items: center; justify-content: center; overflow: hidden"), #ITEM(s) => CENTER || Remove SCROLLING BAR(s)
      # style = "align-items: center; justify-content: space-evenly; overflow: hidden"), #ITEM(s) => CENTER || Remove SCROLLING BAR(s)
      # style = "align-items: center; justify-content: flex-start; overflow: hidden"), #ITEM(s) => CENTER || Remove SCROLLING BAR(s)
      style = "align-items: center; justify-content: space-between; overflow: hidden"), #ITEM(s) => CENTER || Remove SCROLLING BAR(s)
    #Card => ARGu.
    full_screen = FALSE, #!Expand Card To Fit Screen Size
    fill = FALSE)} #Content => Fixed Size => !Allowed To Scroll

##############
##### UI #####
##############

ui <- fluidPage(
  
  ##### Browser Window #####
  # titlePanel("Climate Change Migration Network"),
  
  ##### BSLIB+ THEME #####
  theme = Base,
  
  ##### Include WAITER Dependencie(s) #####
  useWaiter(),
  
  ##### SHINY JavaScript Initialization #####
  useShinyjs(),
  
  ##### CSS Code #####
  tags$head(
    #WebPAGE => TITLE
    tags$title("CLIMINET"),
    #Favicon => Next To WebPAGE-TITLE
    tags$link(id = "favicon", rel = "icon", type = "image/svg+xml", href = "CLIMINET/Arrows/CLIMINET-GREEN-Arrows.svg"),
    tags$style(
      #HTML and BODY Minimum Width (don't shrink browser window title) (972px + 20px from @media)
      "html {min-width: 992px !important;}", "body {min-width: 992px !important;}",
      #CURSOR Classes => WAIT | Default
      # "body.wait-cursor {cursor: wait !important;}",
      # "body.default-cursor {cursor: default !important;}",
      "body.wait-cursor, body.wait-cursor * {cursor: wait !important;}", #CSS Rule => All elements within BODY
      "body.default-cursor, body.default-cursor * {cursor: default !important;}", #CSS Rule => All elements within BODY
      #BSLIB => ROW => Gap from 1rem To 10px
      ".bslib-gap-spacing {row-gap: 10px !important;}",
      #CLIMINET => LOGO => POSITION
      "img#climinet {margin-top: 2.5px !important;}",
      "@media (max-width: 992px) {.navbar-header {max-height: 36.75px !important;}}", #TOGGLER Visible
      #CLIMINET => Clickable LOGO
      "img.CLIMINET-LOGO:hover {opacity: 0.75 !important;}", #LOGO => HOVER => OPACITY
      #HOME => Carousel => SlickSlide => Appearance (BackGround-Color + HEIGHT + CENTER)
      ".slick-slide {background-color: transparent !important; height: 472.5px !important; display: flex !important; justify-content: center !important; align-items: center !important;}",
      #HOME => Carousel => Prev/Next Arrows => POSITION
      ".slick-prev {left: 0 !important; z-index: 1 !important;}",
      ".slick-next {right: 0 !important;}",
      #HOME => Carousel => DEA-Capture => Browser => SHRINK
      "@media (max-width: 1080px) {img.HOME-DEA-CAPTURE {width: 855px !important; height: 385px !important;}}",
      #HOME => Carousel => Clickable DEA-Capture
      # "img.HOME-DEA-CAPTURE:hover {opacity: 0.75 !important;}", #LOGO => HOVER => OPACITY
      #HOME => Carousel => Most Recent Card(s) => Card-Header => POSITION
      "#home .card-header {display: flex !important;}",
      #HOME => Carousel => REDIRECTION => BUTTON => Appearance
      ".redirection-button:hover {background-color: #007BC2 !important; color: #FCFCFC !important; border-color: #E6E6E6;}", #CSS pseudo-class :hover when button is hover
      ".redirection-button:active {background-color: #00629B !important; color: #FCFCFC !important; border-color: #E6E6E6; outline: 5px auto -webkit-focus-ring-color !important; outline-offset: -2px !important;}", #CSS pseudo-class :active when button is clicked
      #Overview Card => Clickable ICON
      "a.clickable-icon svg.bi.bi-info-circle {vertical-align: -3.5px !important; margin-top: 0px !important;}", #ICON => POSITION
      "a.clickable-icon svg.bi.bi-info-circle:hover {color: #00629B !important;}", #HOVER => COLOR
      #Overview Card => HTML => <ol>...</ol> => MARGIN (Bottom: 0px)
      "ol {margin-bottom: 0px !important;}",
      #PARTNER(s) Grid => SHRINK => Window < 580px => ROW => TWO 
      "@media (max-width: 580px) {.tab-pane[data-value='Overview and Partners'].active .bslib-grid.bslib-mb-spacing {grid-template-columns: 1fr 1fr !important;}}",
      #PARTNER(s) Card => AFFILIATION => LOGO => POSITION
      "img#anr {margin-top: 50px !important; margin-bottom: 50px !important;}", #ANR
      "img#belmont-forum {margin-top: 65px; margin-bottom: 65px;}", #BELMONT-FORUM
      #PARTNER(s) Card => Clickable LOGO
      "img.PARTNER-LOGO:hover {opacity: 0.75 !important;}", #LOGO => HOVER => OPACITY
      #PARTNER(s) Card => Clickable TEXT/ICON
      "a.PARTNER-NAME:hover {color: #00629B !important;}", #TEXT/ICON => HOVER => COLOR
      #PARTNER(s) Card => Clickable ICON
      "a.PARTNER-NAME svg.bi.bi-globe2 {vertical-align: -3.5px !important;}", #ICON => POSITION
      #RESEARCH-RESULT(s) => FILTER => BUTTON => ICON => POSITION
      "svg.bi.bi-funnel {margin-right: 3.5px !important; margin-top: 0.5px !important;}",
      #RESEARCH-RESULT(s) => FILTER => BUTTON => Appearance
      ".sort-button:hover {background-color: #E6E6E6; border-color: #E6E6E6;}", #CSS pseudo-class :hover when button is hover
      ".sort-button:active {background-color: #E6E6E6; border-color: #E6E6E6; outline: 5px auto -webkit-focus-ring-color !important; outline-offset: -2px !important;}", #CSS pseudo-class :active when button is clicked
      #RESEARCH-RESULT(s) Card => ICON => POSITION
      "svg.bi.bi-person {margin-top: 1.5px !important;}",
      "svg.bi.bi-people {margin-top: 1.5px !important;}",
      "img.ion-newspaper {margin-top: 0.5px !important;}",
      "svg.bi.bi-file-earmark-text {margin-top: 0.5px !important;}",
      "svg.bi.bi-bank {margin-top: 0.5px !important;}",
      #RESEARCH-RESULT(s) Card => Clickable TEXT/ICON
      "a.INSTITUTION:hover {color: #00629B !important;}", #TEXT/ICON => HOVER => COLOR
      "a.ORGANIZER:hover {color: #00629B !important;}", #TEXT/ICON => HOVER => COLOR
      #RESEARCH-RESULT(s) Card => ICON => POSITION
      "svg.bi.bi-calendar4-event {margin-top: 1px !important;}",
      "svg.bi.bi-geo {margin-top: 1px !important;}",
      "svg.bi.bi-link {margin-top: 1.5px !important;}",
      #RESEARCH-RESULT(s) Card => Clickable TEXT/ICON
      "a.PUBLICATION-DOI:hover {color: #00629B !important;}", #TEXT/ICON => HOVER => COLOR
      "a.WORKING-PAPER-LINK:hover {color: #00629B !important;}", #TEXT/ICON => HOVER => COLOR
      "a.EVENT-LINK:hover {color: #00629B !important;}", #TEXT/ICON => HOVER => COLOR
      #RESEARCH-RESULT(s) Card => ICON => POSITION
      "svg.bi.bi-info-circle {margin-top: 1px !important;}",
      #MEMBER(s) Grid => SHRINK => Window < 580px => ROW => TWO 
      "@media (max-width: 580px) {.tab-pane[data-value='Members'].active .bslib-grid.bslib-mb-spacing {grid-template-columns: 1fr 1fr 1fr !important;}}",
      #MEMBER(s) Card => POSITION => ICON => POSITION
      "svg.bi.bi-person-badge {margin-top: 1px !important;}", #ICON => POSITION
      #MEMBER(s) Card => AFFILIATION => LOGO => POSITION
      # "img#fen-uchile {margin-top: -10px !important;}", #FEN-Chile
      "img#TSE-T-LAURENT {margin-top: 5px !important; margin-bottom: 5px !important;}", #TSE => T-LAURENT
      "img#udp {margin-top: 10px !important; margin-bottom: 10px !important;}", #Universidad DIEGO Portales
      "img#TSE-C-THOMAS {margin-top: 5px !important; margin-bottom: 5px !important;}", #TSE => C-THOMAS
      "img#udesa {margin-top: 12.5px !important; margin-bottom: 12.5px !important;}", #UdeSA
      "img#uai {margin-top: 15px !important; margin-bottom: 15px !important;}", #UAI
      #MEMBER(s) Card => Clickable LOGO
      "img.AFFILIATION-LOGO:hover {opacity: 0.75 !important;}", #LOGO => HOVER => OPACITY
      #MEMBER(s) Card => Clickable TEXT/ICON
      "a.MEMBER-Email:hover {color: #00629B !important;}", #TEXT/ICON => HOVER => COLOR
      "a.MEMBER-WEBSITE:hover {color: #00629B !important;}", #TEXT/ICON => HOVER => COLOR
      #MEMBER(s) Card => Clickable ICON
      "a.MEMBER-Email svg.bi.bi-envelope-at {vertical-align: -3.5px !important;}", #ICON => POSITION
      "a.MEMBER-WEBSITE svg.bi.bi-globe2 {vertical-align: -3.5px !important;}", #ICON => POSITION
      #NAVIGATION BAR Appearance
      ".bslib-grid {white-space: nowrap !important;}", #CCMN + Dark Mode (don't shrink browser window title)
      ".bslib-grid.grid {grid-template-rows: 1fr !important; grid-auto-rows: 0 !important; row-gap: 0 !important; max-height: 35px !important; overflow: hidden !important;}", #CCMN + Dark Mode => SHRINK => ROW => ONE
      ".bslib-grid.grid.bslib-mb-spacing.html-fill-item {margin-top: 20px !important; margin-bottom: 10px !important;}", #CCMN + Dark Mode => POSITION
      "h2 {margin-bottom: 2.5px !important; font-size: 30px !important; font-weight: 500 !important; line-height: 1.1 !important;}", #Climate ChanGe MiGration Network Appearance
      ".g-col-md-4.bslib-grid-item.bslib-gap-spacing.html-fill-container {display: flex !important; justify-content: center !important;}", #Dark Mode Label + TOGGLE SWITCH Button => POSITION
      ".g-col-md-4.bslib-grid-item.bslib-gap-spacing.html-fill-container .shiny-input-container:not(.shiny-input-container-inline) {margin-left: auto !important;}", #Dark Mode Label + TOGGLE SWITCH Button => POSITION
      ".navbar {--bs-navbar-brand-font-size: 18px !important;}", #CLIMINET => Font Size from 1.09375rem To 18px
      ".navbar-brand {margin-left: -12.5px !important; margin-right: 10px !important;}", #CLIMINET => LOGO => POSITION
      "@media (min-width: 992px) {.navbar-nav .nav-item .nav-link.active {background-color: #B0B0B0 !important; height: calc(100% + 16px) !important; margin-top: -8px !important; display: flex !important; align-items: center !important;}}", #NavPanel => BackGround COLOR (TOGGLER !Visible) => Active
      "@media (max-width: 992px) {.navbar-nav .nav-item .nav-link.active {background-color: #B0B0B0 !important; width: calc(100% + 45px) !important; margin-left: -22.5px !important; padding-left: 22.5px !important;}}", #NavPanel => BackGround COLOR (TOGGLER Visible) => Active
      ".navbar {--bs-navbar-color: var(--bs-link-color) !important;}", #NavPanel => TEXT COLOR => !Active
      ".navbar {--bs-navbar-hover-color: var(--bs-link-hover-color) !important;}", #NavPanel => TEXT COLOR => HOVER
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
      #NAVIGATION BAR MENU Appearance
      ".dropdown-toggle::after {margin-left: 2.5px !important; vertical-align: 1.5px !important; border-top: 5px solid !important; border-right: 5px solid transparent !important; border-left: 5px solid transparent !important;}", #DROPDOWN => TOGGLE => CARET => POSITION | SIZE
      "@media (min-width: 992px) {a.dropdown-toggle.nav-link.active::after {margin-left: 6px !important;}}", #DROPDOWN => TOGGLE => CARET => POSITION on TOGGLE.Active (TOGGLER !Visible)
      ".dropdown-menu.show {margin-top: 0px !important;}", #DROPDOWN => MENU => SHOW => POSITION
      "a.dropdown-toggle.nav-link.show {color: var(--bs-nav-link-hover-color) !important;}", #DROPDOWN => TOGGLE => TEXT => SHOW => COLOR
      "a.dropdown-toggle.nav-link.active.show {color: var(--bs-navbar-active-color) !important;}", #DROPDOWN => TOGGLE => TEXT => ACTIVE + SHOW => COLOR
      "@media (min-width: 992px) {.dropdown-toggle.nav-link:hover + .dropdown-menu, .dropdown-menu:hover {display: block !important; margin-top: 0px !important;}}", #DROPDOWN => MENU => SHOW on TOGGLE:HOVER + POSITION (TOGGLER !Visible)
      "@media (max-width: 992px) {.dropdown-toggle.nav-link:hover + .dropdown-menu, .dropdown-menu:hover {display: block !important; margin-top: -8px !important;}}", #DROPDOWN => MENU => SHOW on TOGGLE:HOVER + POSITION (TOGGLER Visible)
      ".dropdown-toggle.nav-link.active:not(:hover) + .dropdown-menu {display: none !important;}", #DROPDOWN => MENU => !SHOW on TOGGLE.Active:NOT(:HOVER)
      ".dropdown-toggle.nav-link.active:not(:hover) + .dropdown-menu:hover {display: block !important; margin-top: -8px !important;}", #DROPDOWN => MENU => SHOW on TOGGLE.Active:NOT(:HOVER) + MENU:HOVER (POSITION)
      ".dropdown-toggle.nav-link.active:hover + .dropdown-menu, .dropdown-menu:hover {margin-top: -8px !important;}", #DROPDOWN => MENU => POSITION on TOGGLE.Active:HOVER
      "@media (min-width: 992px) {.dropdown-toggle.nav-link.active.show:not(:hover) + .dropdown-menu, .dropdown-menu:hover {margin-top: 0px !important;}}", #DROPDOWN => MENU => POSITION on TOGGLE.Active.SHOW:NOT(:HOVER) (TOGGLER !Visible)
      "@media (min-width: 992px) {.dropdown-toggle.nav-link.active.show:hover + .dropdown-menu, .dropdown-menu:hover {margin-top: 0px !important;}}", #DROPDOWN => MENU => POSITION on TOGGLE.Active.SHOW:HOVER (TOGGLER !Visible)
      ".dropdown-toggle.nav-link.active + .dropdown-menu {background-color: #B0B0B0 !important;}", #DROPDOWN => MENU => COLOR  on TOGGLE.Active
      ".dropdown-menu {padding: 8px 41px 0px 0px !important;}", #DROPDOWN => MENU => SIZE
      "@media (max-width: 992px) {.dropdown-menu {margin: 0px -22px 0px -22px !important;}}", #DROPDOWN => MENU => SIZE (TOGGLER Visible)
      "a.dropdown-item {width: calc(100% + 41px) !important;}", #Dropdown ITEM(s) => SIZE
      "@media (max-width: 992px) {a.dropdown-item {padding-left: 30px !important;}}", #Dropdown ITEM(s) => SIZE (TOGGLER Visible)
      ".dropdown-toggle.nav-link + .dropdown-menu .dropdown-item:hover {border-radius: var(--bs-dropdown-item-border-radius, 5px) !important;}", #Dropdown ITEM(s) => BORDER => RADIUS => HOVER
      ".dropdown-toggle.nav-link + .dropdown-menu.show .dropdown-item:hover {border-radius: var(--bs-dropdown-item-border-radius, 5px) !important;}", #Dropdown ITEM(s) => BORDER => RADIUS => HOVER
      ".dropdown-toggle.nav-link.active + .dropdown-menu .dropdown-item:hover {background-color : #B0B0B0 !important; border-radius: var(--bs-dropdown-item-border-radius, 5px) !important;}", #Dropdown ITEM(s) => HOVER => BACKGROUND_COLOR and BORDER_RADIUS on TOGGLE.Active
      ".dropdown-toggle.nav-link.active + .dropdown-menu.show .dropdown-item:hover {background-color : #B0B0B0 !important; border-radius: var(--bs-dropdown-item-border-radius, 5px) !important;}", #Dropdown ITEM(s) => HOVER => BACKGROUND_COLOR and BORDER_RADIUS on TOGGLE.Active
      ".dropdown-toggle.nav-link.active + .dropdown-menu .dropdown-item.active {border-radius: var(--bs-dropdown-item-border-radius, 5px) !important;}", #Dropdown ITEM(s) => BORDER => RADIUS => ACTIVE | !HOVER on TOGGLE.Active
      #WAITER => SPINNER => COLOR
      # ".orbiter-spinner .orbiter:nth-child(1) {border-bottom: 3px solid #FCFCFC !important;}",
      # ".orbiter-spinner .orbiter:nth-child(2) {border-right: 3px solid #FCFCFC !important;}",
      # ".orbiter-spinner .orbiter:nth-child(3) {border-top: 3px solid #FCFCFC !important;}",
      #WAITER => TEXT => COLOR
      # ".waiter-overlay {color: #FCFCFC !important;}",
      #NAVIGATION Panel Appearance
      ".nav-tabs .nav-link {padding: 12px 20px !important;}",
      #Action Button Appearance
      ".btn {padding: 10px 20px !important;}", #Action Button PADDING Rule
      ".tab-pane.html-fill-container.active.show[data-value='Climate'] {display: inline-block !important;}", #Action Button(s) in CLIMATE => ROW => ONE
      ".btn-default {--bs-btn-color: var(--bs-body-color) !important; --bs-btn-border-color: var(--bs-border-color) !important}", #CSS class default when button with default class
      ".btn-default {--bs-btn-hover-bg: var(--bs-primary) !important; --bs-btn-hover-color: #FCFCFC !important; --bs-btn-hover-border-color: var(--bs-light) !important;}", #CSS pseudo-class :hover when button is hover
      ".btn:active {background-color: var(--bs-link-hover-color) !important; color: #FCFCFC !important; outline: 5px auto -webkit-focus-ring-color !important; outline-offset: -2px !important;}", #CSS pseudo-class :active when button is clicked
      ".btn-default {--bs-btn-active-border-color: var(--bs-light) !important;}", #CSS pseudo-class :active and CSS class .active => BORDER => COLOR
      ".btn.active {background-color: var(--bs-primary) !important; color: #FCFCFC !important; outline: none !important;}", #CSS class .active when button with active class
      #Radio Button(s) Appearance
      ".shiny-input-container-inline .shiny-options-group .radio-inline {padding-left: 20px !important; padding-top: 2.75px !important;}", #Radio Button(s) => POSITION
      ".tab-pane:is([data-value='Climate']) .shiny-input-container-inline .shiny-options-group .radio-inline {padding-top: 1px !important;}", #Radio Button(s) => POSITION in CLIMATE
      ".shiny-input-container-inline .shiny-options-group {column-gap: 10px !important; margin-left: 1.5px !important;}", #Radio Button(s) | Circle(s) => POSITION
      ".form-group.shiny-input-radiogroup.shiny-input-container.shiny-input-container-inline.shinyjs-resettable.shiny-bound-input {margin-bottom: 0px !important;}", #Radio Button(s) => MARGIN
      ".tab-pane:is([data-value='Migration'], [data-value='Migration and Climate']) .shiny-input-container-inline .shiny-options-group .radio-inline input {margin-top: 1px !important;}", #Radio Button(s) => Circle(s)  => POSITION (!in CLIMATE)
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
      ".bslib-full-screen-exit svg {margin-left: 5px !important;}", #Close BUTTON => MARGIN
      #Animation => Fade => From 0 To 1
      "@keyframes FadeIn {from {opacity: 0;} to {opacity: 1;}",
      "-webkit-@keyframes FadeIn {from {opacity: 0;} to {opacity: 1;}" #Set Animation For All Browser (GOOGLE | SAFARI | IOS)
      ),
    tags$script(
      #Browser in Dark Mode => Favicon in WHITE
      "function DarkMode() {return window.matchMedia('(prefers-color-scheme: dark)').matches;} //Function To Detect Dark Mode
       function Favicon() { //Function To Switch Favicon
         var dark_mode = DarkMode(); //Variable dark_mode == true OR false
         var favicon = document.getElementById('favicon'); //Retrieve Favicon Element
         if (dark_mode) {favicon.href = 'CLIMINET/Arrows/CLIMINET-WHITE-Arrows.svg';} else {favicon.href = 'CLIMINET/Arrows/CLIMINET-GREEN-Arrows.svg';}} //Switch Favicon based on dark_mode value
       //Call Favicon() on load
       Favicon();
       //Event Listener To Detect ANY CHANGE in Theme Preference
       window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', function () {Favicon();})",
      #NavPanel() => Attribute-id => NavPanel-TITLE
      "$(document).ready(function() {
         //Replace Attributes-id()
         $('div[data-value]').each(function() { //Iterate over each <div> that have a 'data-value' attribute
            var DataValue = $(this).attr('data-value'); //Retrieve Text Content (= NavPanel-TITLE)
            var New_id = DataValue.toLowerCase().replace(/ /g, '-'); //Construct Attribute-id
            //Replace Attribute-id()
            $(this).attr('id', New_id);});});",
      #NavPanel() => Attribute => HREF() => DOMAIN + NavPanel-TITLE
      # "$(document).ready(function() {
      #    //Replace HREF() Attributes
      #    $('.nav.navbar-nav li a').each(function() { //Iterate over each <a> within NAVIGATION-BAR items
      #       var NavPanelTitle = $(this).text().trim(); //Retrieve Text Content (= NavPanel-TITLE)
      #       var DOMAIN = 'https://www.climinet.com'; //DOMAIN-NAME
      #       var LINK = DOMAIN + '/' + NavPanelTitle.toLowerCase().replace(/ /g, '-'); //Construct HREF() Attribute
      #       //Replace HREF() Attribute
      #       $(this).attr('href', LINK);});});",
      #NavPanel() => Attribute => HREF() => # + NavPanel-TITLE
      "$(document).ready(function() {
         //Replace HREF() Attributes
         $('.nav.navbar-nav li a').each(function() { //Iterate over each <a> within NAVIGATION-BAR items
            var NavPanelTitle = $(this).text().trim(); //Retrieve Text Content (= NavPanel-TITLE)
            var LINK = '#' + NavPanelTitle.toLowerCase().replace(/ /g, '-'); //Construct HREF() Attribute
            //Replace HREF() Attribute
            $(this).attr('href', LINK);});});",
      #Nested-NavPanel() => Attribute => HREF() => # + Main-NavPanel-TITLE + Nested-NavPanel-TITLE
      "$(document).ready(function() {
         //Replace Attributes-id()
         $('ul.nav.nav-tabs[data-tabsetid]').each(function() { //Iterate over each <nav-tabs> that have a 'data-tabsetid' attribute
            var ParentPanel = $(this).closest('div[data-value]'); //Retrieve Main-NavPanel()
            var MainNavPanelTitle = ParentPanel.attr('data-value'); //Retrieve Text Content (= Main-NavPanel-TITLE)
            var New_NavTab_id = MainNavPanelTitle.toLowerCase().replace(/ /g, '-'); //Construct Attribute-id
            //Replace Attribute-id()
            $(this).attr('data-tabsetid', New_NavTab_id);
            //Replace HREF() Attributes
            $(this).find('a').each(function() { //Iterate over each <a> within <nav><nav-tabs> items
              var NestedDataValue = $(this).attr('data-value'); //Retrieve Text Content (= Nested-NavPanel-TITLE)
              var NestedLINK = '#' + New_NavTab_id + '/' + NestedDataValue.toLowerCase().replace(/ /g, '-'); //Construct HREF() Attribute
              //Replace HREF() Attribute
              $(this).attr('href', NestedLINK);
              //Replace Attributes-id()
              $('div[data-value]').each(function() { //Iterate over each <div> that have a 'data-value' attribute
                 if ($(this).attr('data-value') === NestedDataValue) { //Retrieve Div that have a 'data-value' attribute === NestedDataValue
                     var Nested_New_id = New_NavTab_id + '/' + NestedDataValue.toLowerCase().replace(/ /g, '-'); //Construct Attribute-id
                     //Replace Attribute-id()
                     $(this).attr('id', Nested_New_id);}});});});});",
      #NavMenu() + Nested-NavPanel() => Attribute => HREF() + id() => # + DropdownMenu-TITLE + DropdownItem-TITLE + Nested-NavPanel-TITLE
      "$(document).ready(function() {
         //DropdownMenu-TITLE
         var New_Dropdown_id = 'data-exploration-application';
         //Replace Attributes-id()
         $('ul.dropdown-menu[data-tabsetid]').each(function() { //Iterate over each <dropdown-menu> that have a 'data-tabsetid' attribute
            //Replace Attribute-id()
            $(this).attr('data-tabsetid', New_Dropdown_id);
            //Replace HREF() Attributes
            $(this).find('a').each(function() { //Iterate over each <a> within <dropdown-menu> items
              var DropdownItemDataValue = $(this).attr('data-value'); //Retrieve Text Content (= DropdownItem-TITLE)
              var DropdownItemLINK = '#' + New_Dropdown_id + '/' + DropdownItemDataValue.toLowerCase().replace(/ /g, '-'); //Construct HREF() Attribute
              //Replace HREF() Attribute
              $(this).attr('href', DropdownItemLINK);
              //Replace Attributes-id()
              $('div[data-value]').each(function() { //Iterate over each <div> that have a 'data-value' attribute
                 if ($(this).attr('data-value') === DropdownItemDataValue) { //Retrieve Div that have a 'data-value' attribute === DropdownItemDataValue
                     var DropdownTab_New_id = New_Dropdown_id + '/' + DropdownItemDataValue.toLowerCase().replace(/ /g, '-'); //Construct Attribute-id
                     //Replace Attribute-id()
                     $(this).attr('id', DropdownTab_New_id);}});});});
          //Replace HREF() Attributes
          $('ul.nav.nav-tabs[data-tabsetid]').each(function() { //Iterate over each <nav-tabs> that have a 'data-tabsetid' attribute
             var ParentPanel = $(this).closest('div[data-value]'); //Retrieve Main-NavPanel()
             var MainNavPanel_id = ParentPanel.attr('id'); //Retrieve Text Content (= Main-NavPanel-id)
             var New_NavTab_id = MainNavPanel_id; //Construct Attribute-id
             //Replace Attribute-id()
             $(this).attr('data-tabsetid', New_NavTab_id);
             //Replace HREF() Attributes
             $(this).find('a').each(function() { //Iterate over each <a> within <nav><nav-tabs> items
               var NestedDataValue = $(this).attr('data-value'); //Retrieve Text Content (= Nested-NavPanel-TITLE)
               var NestedLINK = '#' + New_NavTab_id + '/' + NestedDataValue.toLowerCase().replace(/ /g, '-'); //Construct HREF() Attribute
               //Replace HREF() Attribute
               $(this).attr('href', NestedLINK);
               //Replace Attributes-id()
               $('div[data-value]').each(function() { //Iterate over each <div> that have a 'data-value' attribute
                 if ($(this).attr('data-value') === NestedDataValue) { //Retrieve Div that have a 'data-value' attribute === NestedDataValue
                     var ParentNavTab = $(this).parent().siblings('ul.nav.nav-tabs') //Retrieve <nav-tabs> which is in same <div> as <div[data-value]>
                     var NavTab_id = ParentNavTab.attr('data-tabsetid'); //Retrieve Text Content (= NavTab-id)
                     var Nested_New_id = NavTab_id + '/' + NestedDataValue.toLowerCase().replace(/ /g, '-'); //Construct Attribute-id
                     //Replace Attribute-id()
                     $(this).attr('id', Nested_New_id);}});});});});",
      #CURSOR Class To Default
      "$(document).ready(function(){$('body').addClass('default-cursor');});",
      #Handle CUSTOM CURSOR Classes MESSAGE from server-side
      "Shiny.addCustomMessageHandler('CURSORwithinBODY', function(MESSAGE) {document.body.className = MESSAGE;});",
      #Disabled Data Exploration Application (DEA) NavPanel => DEA => !READY
      "$(document).ready(function() {$('a[data-value=\"DEA_Menu\"]').addClass('disabled');});",
      #FILTER => PUBLICATION(s) Card(s) => DATE
      "$(document).ready(function() {
         //Click-Event-Handler For Element with a certain id 
         $('#publications-sort-button').click(function() { //When element is clicked, function is executed
           //console.log('Button Clicked'); //Button Clicked To Console To Confirm-button-click-was-detected
           //Retrieve data-order-attribute current value from clicked button
           var CurrentOrder = $(this).attr('data-order');
           //console.log('Current Order:', CurrentOrder); //Console-support
           //NewOrder Variable To 'reverse-order' (CurrrentOrder = 'chrono-order') OR To 'chrono-order' (CurrrentOrder = 'reverse-order')
           var NewOrder = (CurrentOrder === 'chrono-order') ? 'reverse-order' : 'chrono-order';
           //Replace Attribute-data-order()
           $(this).attr('data-order', NewOrder);
           //console.log('New Order:', NewOrder); //Console-support
           //Retrieve all <div> with class='card'
           var $Cards = $(); //Initialize jQUERY-OBJECT To store card elements
           $('div[data-value]').each(function() { //Iterate over each <div> that have a 'data-value' attribute
             var DataValue = $(this).attr('data-value'); //Retrieve DataValue
             if (DataValue === 'Publications') { //Retrieve Div that have a 'data-value' attribute === PUBLICATION(s)
                 var $CardsInDiv = $(this).find('.card'); //Find all child elements with class='card' 
                 $Cards = $Cards.add($CardsInDiv);}}); //Add CardsInDiv To $Cards
           //console.log('Number of Cards:', $Cards.length); //Console-support
           //console.log('First Card:', $Cards.first().attr('data-date')); //Console-support
           //Create an ARRAY populated with 'data-date' atttributes from each card element
           //var Dates = [];
           //$Cards.each(function(index) {
            //var DateValue = $(this).attr('data-date');
            //Dates.push(DateValue);});
           //console.log(Dates);
           //Create an ARRAY populated with 'data-date' and 'data-title atttributes from each card element
           var CardsData = []; //Create an ARRAY
           $Cards.each(function() { //Iterate over each Card in Cards
            var DateValue = $(this).attr('data-date'); //Retrieve DateValue
            var TitleValue = $(this).attr('data-title'); //Retrieve TitleValue
            CardsData.push({Date: DateValue, Title: TitleValue});}); //Add DateValue and TitleValue To CardsData
           //console.log(CardsData); //Console-support
           //Sort ARRAY-Dates in 'chrono-order' OR 'reverse-order'
           //Dates.sort(function(a, b) {
            //if (NewOrder === 'chrono-order') {
                //return a.localeCompare(b);} else {return b.localeCompare(a);}});
           //console.log(Dates);
           //Sort ARRAY-CardData BY DATE-TITLE
           CardsData.sort(function(a, b) {
            //Sort BY DATE based on NewOrder Value
            var DateComparison = a.Date.localeCompare(b.Date);
            if (DateComparison !== 0) {
                return (NewOrder === 'chrono-order') ? DateComparison : -DateComparison;}
            //DATE(s) are equal Then Sort BY TITLE
            return a.Title.localeCompare(b.Title);});
           //console.log(CardsData); //Console-support
           //Create a DICTIONNATY To map each date to its position in SORTED-ARRAY
           //var DatesIndex = {};
           //Dates.forEach(function(Date, index) {
            //DatesIndex[Date] = index + 1;});
           //console.log(DatesIndex);
           //Create a DICTIONNATY To map each date-title combination to its position in SORTED-ARRAY
           var CardsIndex = {}; //Create a DICTIONNARY
           CardsData.forEach(function(Card, index) { //Iterate over each element in CardsData
            var KEY = Card.Date + ' - ' + Card.Title; //Create a KEY
            CardsIndex[KEY] = index + 1;}); //Add KEY-index
           //console.log(CardsIndex); //Console-support
           //CSS-ORDER-PROPERTY of each card based on its date (DatesIndex)
           //$Cards.each(function() {
             //var DateValue = $(this).attr('data-date');
             //var index = DatesIndex[DateValue];
             //$(this).css('order', index);});
           //CSS-ORDER-PROPERTY of each card based on its date-title (CardsIndex)
           $Cards.each(function() { //Iterate over each Card in Cards
             var DateValue = $(this).attr('data-date'); //Retrieve DateValue
             var TitleValue = $(this).attr('data-title'); //Retrieve TitleValue
             var KEY = DateValue + ' - ' + TitleValue; //Create a KEY
             var index = CardsIndex[KEY]; //Retrieve index for a KEY
             $(this).css('order', index);});});});",  #Add OR Replace CSS-ORDER-PROPERTY
      #FILTER => WORKING-PAPER(s) Card(s) => DATE
      "$(document).ready(function() {
         //Click-Event-Handler For Element with a certain id 
         $('#working-papers-sort-button').click(function() { //When element is clicked, function is executed
           //Retrieve data-order-attribute current value from clicked button
           var CurrentOrder = $(this).attr('data-order');
           //NewOrder Variable To 'reverse-order' (CurrrentOrder = 'chrono-order') OR To 'chrono-order' (CurrrentOrder = 'reverse-order')
           var NewOrder = (CurrentOrder === 'chrono-order') ? 'reverse-order' : 'chrono-order';
           //Replace Attribute-data-order()
           $(this).attr('data-order', NewOrder);
           //Retrieve all <div> with class='card'
           var $Cards = $(); //Initialize jQUERY-OBJECT To store card elements
           $('div[data-value]').each(function() { //Iterate over each <div> that have a 'data-value' attribute
             var DataValue = $(this).attr('data-value'); //Retrieve DataValue
             if (DataValue === 'Working Papers') { //Retrieve Div that have a 'data-value' attribute === WORKING-PAPER(s)
                 var $CardsInDiv = $(this).find('.card'); //Find all child elements with class='card' 
                 $Cards = $Cards.add($CardsInDiv);}}); //Add CardsInDiv To $Cards
           //Create an ARRAY populated with 'data-date' and 'data-title atttributes from each card element
           var CardsData = []; //Create an ARRAY
           $Cards.each(function() { //Iterate over each Card in Cards
            var DateValue = $(this).attr('data-date'); //Retrieve DateValue
            var TitleValue = $(this).attr('data-title'); //Retrieve TitleValue
            CardsData.push({Date: DateValue, Title: TitleValue});}); //Add DateValue and TitleValue To CardsData
           //Sort ARRAY-CardData BY DATE-TITLE
           CardsData.sort(function(a, b) {
            //Sort BY DATE based on NewOrder Value
            var DateComparison = a.Date.localeCompare(b.Date);
            if (DateComparison !== 0) {
                return (NewOrder === 'chrono-order') ? DateComparison : -DateComparison;}
            //DATE(s) are equal Then Sort BY TITLE
            return a.Title.localeCompare(b.Title);});
           //Create a DICTIONNATY To map each date-title combination to its position in SORTED-ARRAY
           var CardsIndex = {}; //Create a DICTIONNARY
           CardsData.forEach(function(Card, index) { //Iterate over each element in CardsData
            var KEY = Card.Date + ' - ' + Card.Title; //Create a KEY
            CardsIndex[KEY] = index + 1;}); //Add KEY-index
           //CSS-ORDER-PROPERTY of each card based on its date-title (CardsIndex)
           $Cards.each(function() { //Iterate over each Card in Cards
             var DateValue = $(this).attr('data-date'); //Retrieve DateValue
             var TitleValue = $(this).attr('data-title'); //Retrieve TitleValue
             var KEY = DateValue + ' - ' + TitleValue; //Create a KEY
             var index = CardsIndex[KEY]; //Retrieve index for a KEY
             $(this).css('order', index);});});});", #Add OR Replace CSS-ORDER-PROPERTY
      #FILTER => PRESENTATION(s) Card(s) => DATE
      "$(document).ready(function() {
         //Click-Event-Handler For Element with a certain id
         $('#presentations-sort-button').click(function() { //When element is clicked, function is executed
           //Retrieve data-order-attribute current value from clicked button
           var CurrentOrder = $(this).attr('data-order');
           //NewOrder Variable To 'reverse-order' (CurrrentOrder = 'chrono-order') OR To 'chrono-order' (CurrrentOrder = 'reverse-order')
           var NewOrder = (CurrentOrder === 'chrono-order') ? 'reverse-order' : 'chrono-order';
           //Replace Attribute-data-order()
           $(this).attr('data-order', NewOrder);
           //Retrieve all <div> with class='card'
           var $Cards = $(); //Initialize jQUERY-OBJECT To store card elements
           $('div[data-value]').each(function() { //Iterate over each <div> that have a 'data-value' attribute
             var DataValue = $(this).attr('data-value'); //Retrieve DataValue
             if (DataValue === 'Presentations') { //Retrieve Div that have a 'data-value' attribute === PRESENTATION(s)
                 var $CardsInDiv = $(this).find('.card'); //Find all child elements with class='card'
                 $Cards = $Cards.add($CardsInDiv);}}); //Add CardsInDiv To $Cards
           //Create an ARRAY populated with 'data-date' and 'data-title atttributes from each card element
           var CardsData = []; //Create an ARRAY
           $Cards.each(function() { //Iterate over each Card in Cards
            var DateValue = $(this).attr('data-date'); //Retrieve DateValue
            var TitleValue = $(this).attr('data-title'); //Retrieve TitleValue
            CardsData.push({Date: DateValue, Title: TitleValue});}); //Add DateValue and TitleValue To CardsData
           //Sort ARRAY-CardData BY DATE-TITLE
           CardsData.sort(function(a, b) {
            //Sort BY DATE based on NewOrder Value
            var DateComparison = a.Date.localeCompare(b.Date);
            if (DateComparison !== 0) {
                return (NewOrder === 'chrono-order') ? DateComparison : -DateComparison;}
            //DATE(s) are equal Then Sort BY TITLE
            return a.Title.localeCompare(b.Title);});
           //Create a DICTIONNATY To map each date-title combination to its position in SORTED-ARRAY
           var CardsIndex = {}; //Create a DICTIONNARY
           CardsData.forEach(function(Card, index) { //Iterate over each element in CardsData
            var KEY = Card.Date + ' - ' + Card.Title; //Create a KEY
            CardsIndex[KEY] = index + 1;}); //Add KEY-index
           //CSS-ORDER-PROPERTY of each card based on its date-title (CardsIndex)
           $Cards.each(function() { //Iterate over each Card in Cards
             var DateValue = $(this).attr('data-date'); //Retrieve DateValue
             var TitleValue = $(this).attr('data-title'); //Retrieve TitleValue
             var KEY = DateValue + ' - ' + TitleValue; //Create a KEY
             var index = CardsIndex[KEY]; //Retrieve index for a KEY
             $(this).css('order', index);});});});" #Add OR Replace CSS-ORDER-PROPERTY
      ),
    ),
  
  ##### Responsive 12-Column Grid #####
  layout_columns(
    ##### Browser Window #####
    titlePanel("Climate Change Migration Network"), 
    ##### Dark Mode Button (TOGGLE SWITCH) #####
    input_switch(id = "THEME", label = "Dark Mode", value = FALSE, width = "125px"),
    ##### Columns #####
    col_widths = c(8, 4)),

  ##### NAVIGATION BAR #####
  page_navbar( #navbarPage()
    #NAVIGATION BAR HEADLINE
    # title = "CLIMINET",
    #NAVIGATION BAR HEADLINE => CLIMINET => LOGO => Clickable
    title = tags$a(
      href = "https://www.climinet.com", #Link To CLIMINET WEBSITE
      # href = "https://www.climinet.com/#home", #Link To CLIMINET WEBSITE
      #CLIMINET => LOGO
      tags$img(
        src = "CLIMINET/Arrows/CLIMINET-GREEN-Arrows.svg", #PATH OR LINK To LOGO
        width = "32px", height = "32px", #LOGO => WITDH/HEIGHT
        class = "CLIMINET-LOGO", #Clickable LOGO Class
        id = "climinet", #Allow To Switch LOGO (!Dark/Dark Mode) in SERVER.R
        title = "Return To Home")), #TEXT SHOW on HOVER
    #Selected Tab Panel
    selected = "Home",
    #NAVIGATION BAR POSITION ("static-top", "fixed-top", "fixed-bottom")
    position = "static-top",
    #!UNDERLINE Selected Tab Panel when active
    underline = FALSE,
    #Collapse NAVIGATION Elements into an expandable menu on mobile devices or narrow browser window
    collapsible = TRUE,
    
    ##### HOME #####
    nav_panel(title = "Home", #tabPanel()
      ##### WELCOME - MESSAGE #####
      # tags$strong( #MESSAGE => Animated (FadeIn)
      #   style = "text-align: center; font-size: 24px; padding: 8px 16px; animation: FadeIn 1.75s;", #TEXT => POSITION | FONT-SIZE
      #   "Welcome to the CLIMINET Website"), #TEXT => Bold
      #CLIMINET => HOME => LOGO
      # tags$img( #CLIMINET-HOME => CENTER + ANIMATION
      #   style = "margin-left: auto; margin-right: auto; opacity: 0; animation: FadeIn 2.25s 0.75s forwards;",
      #   src = "CLIMINET/Arrows/Climate/CLIMINET-GREEN-Arrows-Climate.svg", #PATH OR LINK To LOGO
      #   width = "320px", height = "320px", #LOGO => WITDH/HEIGHT
      #   class = "CLIMINET-HOME", #Clickable LOGO Class
      #   id = "climinet-home"), #Allow To Switch LOGO (!Dark/Dark Mode) in SERVER.R
      ##### WELCOME - MESSAGE #####
      tags$div(#CLIMINET-HOME + MESSAGE + CLIMINET-HOME => CENTER
        style = "display: flex; justify-content: space-evenly; align-items: center;",
        #CLIMINET => HOME => LOGO
        tags$img( #CLIMINET-HOME => Animated (FadeIn)
          style = "padding: 8px 16px; opacity: 0; animation: FadeIn 2.25s 0.75s forwards;", #PADDING
          src = "CLIMINET/Arrows/Climate/CLIMINET-GREEN-Arrows-Climate.svg", #PATH OR LINK To LOGO
          width = "256px", height = "256px", #LOGO => WITDH/HEIGHT
          class = "CLIMINET-HOME", #Clickable LOGO Class
          id = "climinet-home-le"), #Allow To Switch LOGO (!Dark/Dark Mode) in SERVER.R
        #MESSAGE
        tags$strong( #MESSAGE => Animated (FadeIn)
          style = "font-size: 24px; padding: 8px 16px; animation: FadeIn 1.75s;", #TEXT => FONT-SIZE || PADDING
          "Welcome to the CLIMINET Website"), #TEXT => Bold
        #CLIMINET => HOME => LOGO
        tags$img( #CLIMINET-HOME => Animated (FadeIn)
          style = "padding: 8px 16px; opacity: 0; animation: FadeIn 2.25s 0.75s forwards;", #PADDING
          src = "CLIMINET/Arrows/Climate/CLIMINET-GREEN-Arrows-Climate.svg", #PATH OR LINK To LOGO
          width = "256px", height = "256px", #LOGO => WITDH/HEIGHT
          class = "CLIMINET-HOME", #Clickable LOGO Class
          id = "climinet-home-ri")), #Allow To Switch LOGO (!Dark/Dark Mode) in SERVER.R
      ##### Carousel => DEA + MOST-RECENT-RESEARCH-RESULT(s) #####
      div( #Carousel => Animated (FadeIn)
        style = "opacity: 0; animation: FadeIn 2.25s 0.75s forwards;", #ANIMATION
        slickROutput(outputId = "Carousel_Home", width = "100%", height = "472.5px")) #Carousel
      ),

    ##### OVERVIEW and PARTNER(s) #####
    nav_panel(title = "Overview and Partners", id = "overview-and-partners", #tabPanel()
      ##### Overview Card #####
      card(
        #Card => Headline
        card_header(
          #Full PROJECT Title
          "International Migration, Climate Change and Network effects: A Worldwide Study",
          #Clickable ICON
          tags$a(
            href = 'https://www.belmontforum.org/archives/projects/international-migration-climate-change-and-network-effects-a-worldwide-study', #Link To PROJECT Profile on Belmont Forum
            target = '_blank', #Open a new tab when icon is clicked
            style = "color: var(--bs-body-color);", #ICON => COLOR
            class = "clickable-icon", #Clickable icon class
            #BOOTSTRAP_ICON
            bsicons::bs_icon(
              name = "info-circle", #INFO ICON from https://icons.getbootstrap.com/
              size = "16px", #ICON => HEIGHT and WIDTH
              title = "Full Profile"))), #TEXT SHOW on HOVER
        #Overview
        card_body(
          #PROJECT OBJECTIVE from https://www.belmontforum.org/archives/projects/international-migration-climate-change-and-network-effects-a-worldwide-study
          # HTML(
          #   "<p>The goal of this project is to investigate two important aspects of international migration.</p>
          #   <p>First, the project proposes a global framework (including more than 150 countries all over the world) to study the influence of climate change on migration. Our dataset spans the 30-year period over 1990-2020. To characterize countries, we include proxies for fast-and slow-onset meteorological events as a way of measuring countries' exposure to climate change, and a large set of country-specific socio-economic, health, political, and governance factors.</p>
          #   <p>Second, the project analyzes the role of neighboring countries on migration flows. To identify neighboring countries, we allow not only for geographical proximity, but also for economic, and cultural proximity between pairs of countries.</p>
          #   <p>The main contributions of this project are the following:</p>
          #   <ol>
          #     <li>We include a wide set of environmental variables with a global coverage which allows us to have a higher capability to incorporate climate change aspects into the migration analysis.</li>
          #     <li>Considering a large set of country-specific socio-economic, political, and demographic characteristics will allow us to:
          #       <ul>
          #         <li>Explore the contextual causes of migration and the dynamics of these;</li>
          #         <li>Disentangle the patterns distinguishing developed and developing economies through time;</li>
          #         <li>Quantify the feedback effects from socio-economic shocks affecting migration and reversely.</li>
          #       </ul>
          #     </li>
          #     <li>We propose a methodology to predict and simulate the expected impact on migration of major shocks (e.g., the Ukrainian war) and of various simulated patterns of climate change.</li>
          #     <li>The existing procedures typically assume linear relationships between the socio-economic, demographic, political, environmental, weather and climate change determinants of migration flows. However, the connectivity migration matrices and the inter-relations among them are non-linear. To account for these non-linear relations, we rely on functional connectivity, and high-order functional connectivity, to characterize the collective dynamics in the migration networks.</li>
          #   </ol>"),
          #PROJECT OBJECTIVE from https://www.belmontforum.org/archives/projects/international-migration-climate-change-and-network-effects-a-worldwide-study
          HTML(
            "<p style='font-weight: 600;'>Goals:</p>
            <ol>
              <li>We propose a global framework (150+ countries) to examine the influence of climate change on migration:
                <ul>
                  <li>Our data spans the period from 1990 to 2020;</li>
                  <li>To characterize countries, we include proxies for meteorological events to measure their exposure to climate change, and a set of country-specific socio-economic, health, political, and governance factors.</li>
                </ul>
              </li>
              <li>We analyze the role of neighboring countries on migration flows:
                <ul>
                  <li>To identify these countries, we take into account geographical proximity, but also economic, and cultural proximity between pairs of countries.</li>
                </ul>
              </li>
            </ol>
            <p style='font-weight: 600;'>Main Contributions:</p>
            <ol>
              <li>We include a wide set of environmental variables with a global coverage which enables us to better incorporate climate change aspects into the migration analysis.</li>
              <li>Considering a broad set of country-specific socio-economic, political, and demographic characteristics will allow us to:
                <ul>
                  <li>Explore the contextual causes of migration and the dynamics of these;</li>
                  <li>Disentangle the patterns distinguishing developed and developing economies through time;</li>
                  <li>Quantify the feedback effects from socio-economic shocks affecting migration and vice versa.</li>
                </ul>
              </li>
              <li>We propose a methodology to predict and simulate the expected impact on migration of major shocks and various simulated patterns of climate change.</li>
              <li>The current procedures often assume linear relationships between the socio-economic, demographic, political, environmental, weather and climate change determinants of migration flows:
                <ul>
                  <li>However, the connectivity migration matrices and the inter-relations between them are non-linear;</li>
                  <li>To account for these non-linear relations, we use functional connectivity (incl. high-order functional connectivity) to characterize the collective dynamics in the migration networks.</li>
                </ul>
              </li>
            </ol>
            <p><span style='font-weight: 600;'>Duration:</span> April 2023 - March 2026 (36 Months)</p>"),
          fillable = TRUE, #FLEXBOX CONTAINER
          padding = c(0, 16, 8, 16), #PADDING => TOP To 0px | BOTTOM To 8px | LEFT/RIGHT To 16px
          fill = FALSE #Content => Fixed Size => !Allowed To Scroll
          ),
        #Card => ARGu.
        full_screen = FALSE, #!Expand Card To Fit Screen Size
        fill = FALSE), #Content => Fixed Size => !Allowed To Scroll
      ##### PARTNER(s) Card(s) #####
      layout_column_wrap(
        ##### ANR Card #####
        PARTNER_Card( #PARTNER Card Function
          LINK = "https://anr.fr/fr/", #Link To ANR
          SOURCE = "Partners/ANR/ANR.jpg", #PATH To LOGO
          WIDTH = "240px", HEIGHT = "90px", #LOGO => WITDH/HEIGHT
          id_attribute = "anr", #ANR => LOGO => id_attribute
          PARTNER = "Agence Nationale de la Recherche"), #PARTNER Full Name
        ##### Belmont Forum Card #####
        PARTNER_Card( #PARTNER Card Function
          LINK = "https://www.belmontforum.org/", #Link To BELMONT_FORUM
          SOURCE = "Partners/BELMONT_FORUM/BELMONT_FORUM.png", #LINK To LOGO
          WIDTH = "235px", HEIGHT = "60px", #LOGO => WITDH/HEIGHT
          id_attribute = "belmont-forum", #BALMONT_FORUM => LOGO => id_attribute
          PARTNER = "Belmont Forum"), #PARTNER Full Name
        ##### IAI Card #####
        PARTNER_Card( #PARTNER Card Function
          LINK = "https://www.iai.int/en/#", #Link To IAI
          SOURCE = "Partners/IAI/IAI.png", #LINK To LOGO
          WIDTH = "200px", HEIGHT = "190px", #LOGO => WITDH/HEIGHT
          id_attribute = "iai", #IAI => LOGO => id_attribute
          PARTNER = "Inter-American Institute for Global Change Research"), #PARTNER Full Name
        #Function => ARGu.
        width = "410px", #Card => Minimum WIDTH 
        fixed_width = FALSE, #Column => Minimum Size = WIDTH
        heights_equal = "all", #Card => EVERY Row => SAME HEIGHT
        fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
        fillable = TRUE, #FLEXBOX CONTAINER
        min_height = "260px", #Card HEIGHT
        max_height = "540px",  #2x Card HEIGHT + Gap 
        gap = "20px", #COLUMN/ROW-Gap
        style = "margin-top: 10px;") #Grid => POSITION
      ),
    
    ##### DATA EXPLORATION APPLICATION (MENU) #####
    nav_menu(#navbarMenu()
      #NAVIGATION BAR MENU HEADLINE
      title = "Data Exploration Application",
      #NAVIGATION BAR MENU VALUE
      value = "DEA_Menu",
      #Dropdown Menu => Horizontal ALIGNMENT => Relative To Dropdown TOGGLE 
      align = "left",
      
      ##### MIGRATION #####
      nav_panel(title = "Migration", #tabPanel()
        #Contains Multiple Tabs
        navset_tab( #tabsetPanel()
          #Selected Tab Panel
          selected = "Residence",
      
          ##### RESIDENCE #####
          nav_panel(title = "Residence", #tabPanel()
            #MARGIN (Top: 10px)
            tags$div(style = "margin-top: 10px;",
              ##### COUNTRIEs ACTION BUTTON #####
              actionButton(inputId = "MR_COUNTRIEs", label = "Countries"),
              ##### REGIONs ACTION BUTTON #####
              actionButton(inputId = "MR_REGIONs", label = "Regions")
              ),
            #Conditional Panel visible when REGIONs ACTION BUTTON is clicked
            conditionalPanel(condition = "input.MR_REGIONs > 0", 
              #MARGIN (Top: 10px)
              # style = "margin-top: 10px;",
              ##### REGIONs CHOICE RADIO BUTTONs #####
              div(style = "min-width: 1296px;", #RADIO BUTTONs' container minimum width (don't shrink choices)
                radioButtons(inputId = "MRR_REGIONs_CHOICE", label  = NULL, 
                  #International MiGration Stock Data Classifications
                  choices = c(
                    "Continental Regions", "Continental Sub-Regions and Intermediate Regions", "Geographic Regions", "Income Levels", "Development Levels (x3)"),
                  #Selected Classification | Render choices inline
                  selected = "Continental Regions", inline = TRUE
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
                  "Sources:", HTML("<a href='https://gis-who.hub.arcgis.com/datasets/95a475ae34af4f54b63ca6e4a6f67fbd_0/explore' target='_blank'>World Health Organization (WHO)</a>"), "|", #Link To World Health OrGanization (WHO) - Countries GeoSpatial Data 
                  # HTML("<a href='...' target='_blank'>Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques</a>"), "|",
                  "Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques", "|", 
                  HTML("<a href='https://gadm.org/download_world.html' target='_blank'>Global Administrative Areas (GADM)</a>"), "from", HTML("<a href='https://cran.r-project.org/web/packages/geodata/index.html' target='_blank'>GEODATA</a>")),  #Link To Global Administrative Areas (GADM) GeoSpatial Data 
                #Card => ARGu.
                full_screen = TRUE, #Expand Card To Fit Screen Size
                fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
                id = "MR_World_Map_Card")) #Observe Full Screen State in SHINY
            ),
        
          ##### BIRTH #####
          nav_panel(title = "Birth", #tabPanel()
            #MARGIN (Top: 10px)
            tags$div(style = "margin-top: 10px;",
              ##### COUNTRIEs ACTION BUTTON #####
              actionButton(inputId = "MB_COUNTRIEs", label = "Countries"),
              ##### REGIONs ACTION BUTTON #####
              actionButton(inputId = "MB_REGIONs", label = "Regions")
              ),
            #Conditional Panel visible when REGIONs ACTION BUTTON is clicked
            conditionalPanel(condition = "input.MB_REGIONs > 0", 
              #MARGIN (Top: 10px)
              # style = "margin-top: 10px;",
              ##### REGIONs CHOICE RADIO BUTTONs #####
              div(style = "min-width: 1296px;", #RADIO BUTTONs' container minimum width (don't shrink choices)
                radioButtons(inputId = "MBR_REGIONs_CHOICE", label  = NULL, 
                  #International MiGration Stock Data Classifications
                  choices = c(
                    "Continental Regions", "Continental Sub-Regions and Intermediate Regions", "Geographic Regions", "Income Levels", "Development Levels (x3)"),
                  #Selected Classification | Render choices inline
                  selected = "Continental Regions", inline = TRUE
                  )
                )
              )
            )
          )
        ),
      
      ##### CLIMATE #####
      nav_panel(title = "Climate", #tabPanel()
        ##### COUNTRIEs ACTION BUTTON #####
        actionButton(inputId = "CLIMATE_COUNTRIEs", label = "Countries"),
        ##### REGIONs ACTION BUTTON #####
        actionButton(inputId = "CLIMATE_REGIONs", label = "Regions"), 
        #Conditional Panel visible when REGIONs ACTION BUTTON is clicked
        conditionalPanel(condition = "input.CLIMATE_REGIONs > 0", 
          #MARGIN (Top: 10px)
          style = "margin-top: 10px;",
          ##### REGIONs CHOICE RADIO BUTTONs #####
          div(style = "min-width: 1296px;", #RADIO BUTTONs' container minimum width (don't shrink choices)
            radioButtons(inputId = "CLIMATE_R_REGIONs_CHOICE", label  = NULL, 
              #International MiGration Stock Data Classifications
              choices = c(
                "Continental Regions", "Continental Sub-Regions and Intermediate Regions", "Geographic Regions", "Income Levels", "Development Levels (x3)"),
              #Selected Classification | Render choices inline
              selected = "Continental Regions", inline = TRUE
              )
            )
          )
        ),
    
      ##### MIGRATION and CLIMATE #####
      nav_panel(title = "Migration and Climate", #tabPanel()
        #Contains Multiple Tabs
        navset_tab( #tabsetPanel()
          #Selected Tab Panel
          selected = "Residence",
      
          ##### RESIDENCE #####
          nav_panel(title = "Residence", #tabPanel()
            #MARGIN (Top: 10px)
            tags$div(style = "margin-top: 10px;",
              ##### COUNTRIEs ACTION BUTTON #####
              actionButton(inputId = "MCR_COUNTRIEs", label = "Countries"),
              ##### REGIONs ACTION BUTTON #####
              actionButton(inputId = "MCR_REGIONs", label = "Regions")
              ),
            #Conditional Panel visible when REGIONs ACTION BUTTON is clicked
            conditionalPanel(condition = "input.MCR_REGIONs > 0", 
              #MARGIN (Top: 10px)
              # style = "margin-top: 10px;",
              ##### REGIONs CHOICE RADIO BUTTONs #####
              div(style = "min-width: 1296px;", #RADIO BUTTONs' container minimum width (don't shrink choices)
                radioButtons(inputId = "MCRR_REGIONs_CHOICE", label  = NULL, 
                  #International MiGration Stock Data Classifications
                  choices = c(
                    "Continental Regions", "Continental Sub-Regions and Intermediate Regions", "Geographic Regions", "Income Levels", "Development Levels (x3)"),
                  #Selected Classification | Render choices inline
                  selected = "Continental Regions", inline = TRUE
                  )
                )
              )
            ),
      
          ##### BIRTH #####
          nav_panel(title = "Birth", #tabPanel()
            #MARGIN (Top: 10px)
            tags$div(style = "margin-top: 10px;",
              ##### COUNTRIEs ACTION BUTTON #####
              actionButton(inputId = "MCB_COUNTRIEs", label = "Countries"),
              ##### REGIONs ACTION BUTTON #####
              actionButton(inputId = "MCB_REGIONs", label = "Regions")
              ),
            #Conditional Panel visible when REGIONs ACTION BUTTON is clicked
            conditionalPanel(condition = "input.MCB_REGIONs > 0", 
              #MARGIN (Top: 10px)
              # style = "margin-top: 10px;",
              ##### REGIONs CHOICE RADIO BUTTONs #####
              div(style = "min-width: 1296px;", #RADIO BUTTONs' container minimum width (don't shrink choices)
                radioButtons(inputId = "MCBR_REGIONs_CHOICE", label  = NULL, 
                  #International MiGration Stock Data Classifications
                  choices = c(
                    "Continental Regions", "Continental Sub-Regions and Intermediate Regions", "Geographic Regions", "Income Levels", "Development Levels (x3)"),
                  #Selected Classification | Render choices inline
                  selected = "Continental Regions", inline = TRUE
                  )
                )
              )
            )
          )
        )
      ), 
    
    ##### RESEARCH-RESULT(s) #####
    nav_panel(title = "Research Results", #tabPanel()
        #Contains Multiple Tabs
        navset_tab( #tabsetPanel()
          #Selected Tab Panel
          selected = "Publications",
      
          ##### PUBLICATION(s) #####
          nav_panel(title = "Publications", #tabPanel(),
            #Button To Sort Card
            tags$button(
              type = "button", #Button Element
              style = "display: flex; /*FLEXBOX CONTAINER*/
                       margin-top: 10px; margin-bottom: -10px; /*MARGIN(s) => TOP To 10px | BOTTOM To -10px*/
                       padding: 10px 20px; /*PADDING => Top/Bottom => 10px | LEFT/RIGHT => 20px*/
                       width: 125px; /*WIDTH To 125px*/
                       border-width: 1px; border-radius: 5px; /*BORDER-WIDTH-and-RADIUS To 1px/5px*/
                       background-color: transparent; /*BACKGROUND To Transparent*/
                       color: #333537; /*TEXT-COLOR*/
                       border: 1px solid #B9BABA;", #BORDER-WIDTH-and-COLOR
              class = "sort-button", #Button Class
              id = "publications-sort-button", #Button IDENTIFIER
              `data-order` = "chrono-order", #Default Value of Attribute 'data-order' = "chrono-order" 
              #BOOTSTRAP_ICON
              bsicons::bs_icon(
                name = "funnel", #Filter ICON from https://icons.getbootstrap.com/
                size = "16px"), #ICON => HEIGHT and WIDTH
              "Date Sort"), #TEXT on RIGHT-SIDE
            ##### AUTHOR(s)-YEAR #####
            PUBLICATION_Card( #PUBLICATION Card Function
              ARTICLE = "Neighbouring Countries and Bilateral Remittances: A Global Study", #ARTICLE => TITLE
              AUTHORs = c("Thibault Laurent", "Paula Margaretic", "Christine Thomas-Agnan"), #AUTHOR(s) => NAME(s)
              PUBLISHER = "Spatial Economic Analysis", #PUBLISHER => NAME  
              VOLUME = "17", #PUBLICATION => VOLUME => NUMBER 
              ISSUE = "4", #PUBLICATION => ISSUE => NUMBER
              PAGEs = c("557", "584"), #PUBLICATION => PAGE(s)
              DATE = "June 2022", #PUBLICATION => DATE
              DOI = "https://doi.org/10.1080/17421772.2022.2070656"), #Link To PUBLICATION
            ##### AUTHOR(s)-YEAR #####
            PUBLICATION_Card( #PUBLICATION Card Function
              ARTICLE = "Generalizing Impact Computations for the Autoregressive Spatial Interaction Model", #ARTICLE => TITLE
              AUTHORs = c("Thibault Laurent", "Paula Margaretic", "Christine Thomas-Agnan"), #AUTHOR(s) => NAME(s)
              PUBLISHER = "Geographical Analysis", #PUBLISHER => NAME  
              VOLUME = "55", #PUBLICATION => VOLUME => NUMBER 
              ISSUE = "4", #PUBLICATION => ISSUE => NUMBER
              PAGEs = c("728", "758"), #PUBLICATION => PAGE(s)
              DATE = "October 2023", #PUBLICATION => DATE
              DOI = "https://doi.org/10.1111/gean.12358") #Link To PUBLICATION
            ),
          
          ##### WORKING-PAPER(s) #####
          nav_panel(title = "Working Papers", #tabPanel(),
            #Button To Sort Card
            tags$button(
              type = "button", #Button Element
              style = "display: flex; /*FLEXBOX CONTAINER*/
                       margin-top: 10px; margin-bottom: -10px; /*MARGIN(s) => TOP To 10px | BOTTOM To -10px*/
                       padding: 10px 20px; /*PADDING => Top/Bottom => 10px | LEFT/RIGHT => 20px*/
                       width: 125px; /*WIDTH To 125px*/
                       border-width: 1px; border-radius: 5px; /*BORDER-WIDTH-and-RADIUS To 1px/5px*/
                       background-color: transparent; /*BACKGROUND To Transparent*/
                       color: #333537; /*TEXT-COLOR*/
                       border: 1px solid #B9BABA;", #BORDER-WIDTH-and-COLOR
              class = "sort-button", #Button Class
              id = "working-papers-sort-button", #Button IDENTIFIER
              `data-order` = "chrono-order", #Default Value of Attribute 'data-order' = "chrono-order" 
              #BOOTSTRAP_ICON
              bsicons::bs_icon(
                name = "funnel", #Filter ICON from https://icons.getbootstrap.com/
                size = "16px"), #ICON => HEIGHT and WIDTH
              "Date Sort"), #TEXT on RIGHT-SIDE
            ##### AUTHOR(s)-YEAR #####
            # WORKING.PAPER_Card( #WORKING-PAPER Card Function
            #   WORKING_PAPER = "Full WORKING-PAPER Title", #WORKING-PAPER => TITLE
            #   AUTHORs = "Author n°1", #AUTHOR(s) => NAME(s)
            #   WORKING_PAPER_SERIEs = "WORKING-PAPER-Series", #WORKING-PAPER => SERIEs
            #   WORKING_PAPER_NUMBER = "X", #WORKING-PAPER => NUMBER
            #   INSTITUTION_LINK = "http://www.example.com", #Link To INSTITUTION
            #   INSTITUTION = "Full INSTITUTION Name", #INSTITUTION => NAME  
            #   DATE = "Month Year", #WORKING-PAPER => DATE
            #   LINK = "http://www.example.com"), #Link To WORKING-PAPER
            ##### AUTHOR(s)-YEAR #####
            # WORKING.PAPER_Card( #WORKING-PAPER Card Function
            #   WORKING_PAPER = "Full WORKING-PAPER Title", #WORKING-PAPER => TITLE
            #   AUTHORs = c("Author n°1", "Author n°2"), #AUTHOR(s) => NAME(s)
            #   WORKING_PAPER_SERIEs = "WORKING-PAPER-Series", #WORKING-PAPER => SERIEs
            #   WORKING_PAPER_NUMBER = "X", #WORKING-PAPER => NUMBER
            #   INSTITUTION_LINK = "http://www.example.com", #Link To INSTITUTION
            #   INSTITUTION = "Full INSTITUTION Name", #INSTITUTION => NAME  
            #   DATE = "Month Year", #WORKING-PAPER => DATE
            #   LINK = "http://www.example.com"), #Link To WORKING-PAPER
            ##### AUTHOR(s)-YEAR #####
            # WORKING.PAPER_Card( #WORKING-PAPER Card Function
            #   WORKING_PAPER = "Full WORKING-PAPER Title", #WORKING-PAPER => TITLE
            #   AUTHORs = c("Author n°1", "Author n°2", "Author n°3"), #AUTHOR(s) => NAME(s)
            #   WORKING_PAPER_SERIEs = "WORKING-PAPER-Series", #WORKING-PAPER => SERIEs
            #   WORKING_PAPER_NUMBER = "X", #WORKING-PAPER => NUMBER
            #   INSTITUTION_LINK = "http://www.example.com", #Link To INSTITUTION
            #   INSTITUTION = "Full INSTITUTION Name", #INSTITUTION => NAME  
            #   DATE = "Month Year", #WORKING-PAPER => DATE
            #   LINK = "http://www.example.com") #Link To WORKING-PAPER
            ##### AUTHOR(s)-YEAR #####
            WORKING.PAPER_Card( #WORKING-PAPER Card Function
              WORKING_PAPER = "Climate, Conflict and International Migration", #WORKING-PAPER => TITLE
              AUTHORs = c("Evangelina A. Dardati", "Thibault Laurent", "Paula Margaretic", "Christine Thomas-Agnan"), #AUTHOR(s) => NAME(s)
              WORKING_PAPER_SERIEs = "TSE Working Paper", #WORKING-PAPER => SERIEs
              WORKING_PAPER_NUMBER = "X", #WORKING-PAPER => NUMBER
              INSTITUTION_LINK = "https://www.tse-fr.eu/", #Link To INSTITUTION
              INSTITUTION = "Toulouse School of Economics (TSE)", #INSTITUTION => NAME
              DATE = "August 2024", #WORKING-PAPER => DATE
              LINK = "http://www.example.com"), #Link To WORKING-PAPER
            ),
          
          ##### PRESENTATION(s) #####
          nav_panel(title = "Presentations", #tabPanel(),
            #Button To Sort Card
            tags$button(
              type = "button", #Button Element
              style = "display: flex; /*FLEXBOX CONTAINER*/
                       margin-top: 10px; margin-bottom: -10px; /*MARGIN(s) => TOP To 10px | BOTTOM To -10px*/
                       padding: 10px 20px; /*PADDING => Top/Bottom => 10px | LEFT/RIGHT => 20px*/
                       width: 125px; /*WIDTH To 125px*/
                       border-width: 1px; border-radius: 5px; /*BORDER-WIDTH-and-RADIUS To 1px/5px*/
                       background-color: transparent; /*BACKGROUND To Transparent*/
                       color: #333537; /*TEXT-COLOR*/
                       border: 1px solid #B9BABA;", #BORDER-WIDTH-and-COLOR
              class = "sort-button", #Button Class
              id = "presentations-sort-button", #Button IDENTIFIER
              `data-order` = "chrono-order", #Default Value of Attribute 'data-order' = "chrono-order" 
              #BOOTSTRAP_ICON
              bsicons::bs_icon(
                name = "funnel", #Filter ICON from https://icons.getbootstrap.com/
                size = "16px"), #ICON => HEIGHT and WIDTH
              "Date Sort"), #TEXT on RIGHT-SIDE
            ##### CLIMB-2023-T-LAURENT #####
            PRESENTATION_Card( #PRESENTATION Card Function
              PRESENTATION = "Exploring the Different International Migration Flow Estimates", #PRESENTATION => TITLE
              SPEAKERs = "Thibault Laurent", #SPEAKER(s) => NAME(s)
              ORGANIZERs_LINK = "http://climbproject.org/", #Link To ORGANIZER
              ORGANIZERs = "Climate-Induced Migration: Big Data & Predictive Analytics (CLIMB)", #ORGANIZER => NAME
              DATE = "16-17 October 2023", #PRESENTATION => DATE
              LOCATION = "Malmö - Sweden", #PRESENTATION => LOCATION
              EVENT_LINK = "http://climbproject.org/2023/10/01/workshop-on-climate-induced-migration/", #Link To EVENT
              EVENT = "Workshop on Climate-Induced Migration: New Perspectives and Methodological Innovations"), #EVENT => NAME  
            ##### SEA-2023-P-MARGARETIC #####
            PRESENTATION_Card( #PRESENTATION Card Function
              PRESENTATION = "International Mobility and Environmental Factors", #PRESENTATION => TITLE
              SPEAKERs = "Paula Margaretic", #SPEAKER(s) => NAME(s)
              ORGANIZERs_LINK = "https://www.spatialeconometricsassociation.org/", #Link To ORGANIZER
              ORGANIZERs = "Spatial Econometrics Association (SEA)", #ORGANIZER => NAME
              DATE = "16-17 November 2023", #PRESENTATION => DATE
              LOCATION = "San Diego - United States of America (USA)", #PRESENTATION => LOCATION
              EVENT_LINK = "https://www.spatialeconometricsassociation.org/2023-annual-sea-conference-san-diego/", #Link To EVENT
              EVENT = "2023 Annual SEA Conference"), #EVENT => NAME  
            ##### SEA-2023-T-LAURENT #####
            PRESENTATION_Card( #PRESENTATION Card Function
              PRESENTATION = "Visualizing Migration Data with R", #PRESENTATION => TITLE
              SPEAKERs = "Thibault Laurent", #SPEAKER(s) => NAME(s)
              ORGANIZERs_LINK = "https://www.spatialeconometricsassociation.org/", #Link To ORGANIZER
              ORGANIZERs = "Spatial Econometrics Association (SEA)", #ORGANIZER => NAME
              DATE = "16-17 November 2023", #PRESENTATION => DATE
              LOCATION = "San Diego - United States of America (USA)", #PRESENTATION => LOCATION
              EVENT_LINK = "https://www.spatialeconometricsassociation.org/2023-annual-sea-conference-san-diego/", #Link To EVENT
              EVENT = "2023 Annual SEA Conference"), #EVENT => NAME
            ##### SECHI-2024-E-DARDATI #####
            PRESENTATION_Card( #PRESENTATION Card Function
              PRESENTATION = "Climate, Conflict and International Migration", #PRESENTATION => TITLE
              SPEAKERs = "Evangelina A. Dardati", #SPEAKER(s) => NAME(s)
              ORGANIZERs_LINK = "https://www.sechi.cl/", #Link To ORGANIZER
              ORGANIZERs = "Sociedad de Economía de Chile (SECHI)", #ORGANIZER => NAME
              DATE = "4-6 September 2024", #PRESENTATION => DATE
              LOCATION = "Concepción - Chile", #PRESENTATION => LOCATION
              EVENT_LINK = "https://www.sechi.cl/encuentro-anual/encuentro-2024/", #Link To EVENT
              EVENT = "Encuentro Anual SECHI 2024"), #EVENT => NAME
            ##### LACEA-LAMES-2024-E-DARDATI #####
            PRESENTATION_Card( #PRESENTATION Card Function
              PRESENTATION = "Climate, Conflict and International Migration", #PRESENTATION => TITLE
              SPEAKERs = "Evangelina A. Dardati", #SPEAKER(s) => NAME(s)
              ORGANIZERs_LINK = c("https://www.lacea.org/portal/", "https://www.econometricsociety.org/"), #Link(s) To ORGANIZER(s)
              ORGANIZERs = c("Latin American and Caribbean Economic Association (LACEA)", "Latin American and Caribbean Chapter of the Econometric Society (LAMES)"), #ORGANIZER(s) => NAME(s)
              DATE = "14-16 November 2024", #PRESENTATION => DATE
              LOCATION = "Montevideo - Uruguay", #PRESENTATION => LOCATION
              EVENT_LINK = "https://www.lacealames2024.org/", #Link To EVENT
              EVENT = "LACEA-LAMES 2024 Annual Meeting") #EVENT => NAME
            )
          )
        ),
    
    ##### EVENT(s) #####
    nav_panel(title = "Events", #tabPanel()
      ##### INFORMATION - MESSAGE #####
      tags$div(style = "text-align: center; font-size: 18px; padding: 16px 24px;", #TEXT => POSITION | FONT-SIZE
        tags$strong("Future events information will be available soon."), #TEXT => Bold
        HTML("<br><br>"), #HTML => Line Break (x2)
        tags$strong("Please check back later.")) #TEXT => Bold
      ),
    
    ##### MEMBER(s) #####
    nav_panel(title = "Members", #tabPanel()
      ##### MEMBER(s) Card(s) #####
      layout_column_wrap( 
        ##### PAULA MARGARETIC #####
        MEMBER_Card( #PARTNER Card Function
          MEMBER_SOURCE = "Members/P-MARGARETIC/P-MARGARETIC.jpg", #PATH To PHOTO
          MEMBER_WIDTH = "360px", MEMBER_HEIGHT = "360px", #PHOTO => WITDH/HEIGHT
          NAME = "Paula Margaretic", #MEMBER Full Name
          POSITION = "Economist", #MEMBER Position
          ROLE = "PI", #MEMBER Role
          Email = "paumargaretic@gmail.com", #MEMBER Email
          Pronoun = "Her", #MEMBER Pronoun
          MEMBER_LINK = "https://www.paulamargaretic.com/", #MEMBER Personal WEBSITE
          # AFFILIATION = "Universidad de Chile", #MEMBER AFFILIATION
          AFFILIATION_LINK = "https://fen.uchile.cl/es", #Link To FEN-Chile
          AFFILIATION_SOURCE = "Members/Universidad-de-Chile/FEN/FEN.png", #PATH To LOGO
          AFFILIATION_WIDTH = "125px", AFFILIATION_HEIGHT = "70px", #LOGO => WITDH/HEIGHT
          id_attribute = "fen-uchile"), #UAI => LOGO => id_attribute
        ##### THIBAULT LAURENT #####
        MEMBER_Card( #PARTNER Card Function
          MEMBER_SOURCE = "Members/T-LAURENT/T-LAURENT-Small.jpeg", #PATH To PHOTO
          MEMBER_WIDTH = "360px", MEMBER_HEIGHT = "360px", #PHOTO => WITDH/HEIGHT
          NAME = "Thibault Laurent", #MEMBER Full Name
          POSITION = "Statistician", #MEMBER Position
          ROLE = "Co-PI", #MEMBER Role
          Email = "thibault.laurent@tse-fr.eu", #MEMBER Email
          Pronoun = "Him", #MEMBER Pronoun
          MEMBER_LINK = "http://www.thibault.laurent.free.fr/", #MEMBER Personal WEBSITE
          # AFFILIATION = "Toulouse School of Economics", #MEMBER AFFILIATION
          AFFILIATION_LINK = "https://www.tse-fr.eu/", #Link To TSE
          AFFILIATION_SOURCE = "Members/Toulouse-School-of-Economics/TSE_MainVersion-Small.png", #PATH To LOGO
          AFFILIATION_WIDTH = "175px", AFFILIATION_HEIGHT = "60px", #LOGO => WITDH/HEIGHT
          id_attribute = "TSE-T-LAURENT"), #TSE => LOGO => id_attribute
        ##### EVANGELINA DARDATI #####
        MEMBER_Card( #PARTNER Card Function
          MEMBER_SOURCE = "Members/E-DARDATI/E-DARDATI.jpg", #PATH To PHOTO
          MEMBER_WIDTH = "545px", MEMBER_HEIGHT = "360px", #PHOTO => WITDH/HEIGHT
          NAME = "Evangelina A. Dardati", #MEMBER Full Name
          POSITION = "Economist", #MEMBER Position
          ROLE = NULL, #MEMBER Role
          Email = "edardati@gmail.com", #MEMBER Email
          Pronoun = "Her", #MEMBER Pronoun
          MEMBER_LINK = "https://sites.google.com/site/edardati/", #MEMBER Personal WEBSITE
          # AFFILIATION = "Universidad Diego Portales", #MEMBER AFFILIATION
          AFFILIATION_LINK = "https://www.udp.cl/", #Link To Universidad DIEGO Portales
          AFFILIATION_SOURCE = "Members/Universidad-DIEGO-Portales/Universidad-DIEGO-Portales.png", #PATH To LOGO
          AFFILIATION_WIDTH = "215px", AFFILIATION_HEIGHT = "50px", #LOGO => WITDH/HEIGHT
          id_attribute = "udp"), #Universidad DIEGO Portales => LOGO => id_attribute
        ##### CHRISTINE THOMAS-AGNAN #####
        MEMBER_Card( #PARTNER Card Function
          MEMBER_SOURCE = "Members/C-THOMAS-AGNAN/C-THOMAS-AGNAN-Pro-Small.jpg", #PATH To PHOTO
          MEMBER_WIDTH = "360px", MEMBER_HEIGHT = "360px", #PHOTO => WITDH/HEIGHT
          NAME = "Christine Thomas-Agnan", #MEMBER Full Name
          POSITION = "Statistician", #MEMBER Position
          ROLE = NULL, #MEMBER Role
          Email = "christine.thomas@tse-fr.eu", #MEMBER Email
          Pronoun = "Her", #MEMBER Pronoun
          # MEMBER_LINK = NULL, #MEMBER Personal WEBSITE
          MEMBER_LINK = "https://www.tse-fr.eu/people/christine-thomas-agnan", #MEMBER Personal WEBSITE
          # AFFILIATION = "Toulouse School of Economics", #MEMBER AFFILIATION
          AFFILIATION_LINK = "https://www.tse-fr.eu/", #Link To TSE
          AFFILIATION_SOURCE = "Members/Toulouse-School-of-Economics/TSE_MainVersion-Small.png", #PATH To LOGO
          AFFILIATION_WIDTH = "175px", AFFILIATION_HEIGHT = "60px", #LOGO => WITDH/HEIGHT
          id_attribute = "TSE-C-THOMAS"), #TSE => LOGO => id_attribute
        ##### ADOLFO GARCIA #####
        MEMBER_Card( #PARTNER Card Function
          MEMBER_SOURCE = "Members/A-GARCIA/A-GARCIA-Small.jpg", #PATH To PHOTO
          MEMBER_WIDTH = "360px", MEMBER_HEIGHT = "360px", #PHOTO => WITDH/HEIGHT
          NAME = "Adolfo M. García", #MEMBER Full Name
          POSITION = "Neuroscientist", #MEMBER Position
          ROLE = NULL, #MEMBER Role
          Email = "adolfo.garcia@gbhi.org", #MEMBER Email
          Pronoun = "Him", #MEMBER Pronoun
          MEMBER_LINK = "https://adolfogarcia.com.ar/", #MEMBER Personal WEBSITE
          # AFFILIATION = "Universidad de San Andrés", #MEMBER AFFILIATION
          AFFILIATION_LINK = "https://udesa.edu.ar/", #Link To Universidad de San Andrés
          AFFILIATION_SOURCE = "Members/Universidad-de-San-Andres/Universidad-de-San-Andres-MainVersion.svg", #PATH To LOGO
          AFFILIATION_WIDTH = "190px", AFFILIATION_HEIGHT = "45px", #LOGO => WITDH/HEIGHT
          id_attribute = "udesa"), #Universidad de San Andrés => LOGO => id_attribute
        ##### AGUSTIN IBANEZ #####
        MEMBER_Card( #PARTNER Card Function
          MEMBER_SOURCE = "Members/A-IBANEZ/A-IBANEZ.jpg", #PATH To PHOTO
          MEMBER_WIDTH = "360px", MEMBER_HEIGHT = "360px", #PHOTO => WITDH/HEIGHT
          NAME = "Agustin Ibáñez", #MEMBER Full Name
          POSITION = "Neuroscientist", #MEMBER Position
          ROLE = NULL, #MEMBER Role
          Email = "agustin.ibanez@gbhi.org", #MEMBER Email
          Pronoun = "Him", #MEMBER Pronoun
          MEMBER_LINK = "https://brainlat.uai.cl/autoridad/agustin-ibanez/", #MEMBER Personal WEBSITE
          # AFFILIATION = "Universidad Adolfo Ibáñez", #MEMBER AFFILIATION
          AFFILIATION_LINK = "https://www.uai.cl/", #Link To UAI
          AFFILIATION_SOURCE = "Members/Universidad-Adolfo-Ibanez/Universidad-Adolfo-Ibanez.svg", #PATH To LOGO
          AFFILIATION_WIDTH = "190px", AFFILIATION_HEIGHT = "40px", #LOGO => WITDH/HEIGHT
          id_attribute = "uai"), #UAI => LOGO => id_attribute
        #Function => ARGu.
        width = "260px", #Card => Minimum WIDTH
        fixed_width = FALSE, #Column => Minimum Size = WIDTH
        heights_equal = "all", #Card => EVERY Row => SAME HEIGHT
        fill = FALSE, #Content => Fixed Size => !Allowed To Scroll
        fillable = TRUE, #FLEXBOX CONTAINER
        min_height = "585px", #Card(s) HEIGHT
        max_height = "1190px",  #2x Card(s) HEIGHT + Gap
        gap = "20px"), #COLUMN/ROW-Gap
      ) 
    )

  )