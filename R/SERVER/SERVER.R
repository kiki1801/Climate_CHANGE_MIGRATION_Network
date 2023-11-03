##########################################################################
#####                Climate Change Migration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   Kyllian James | 2023-10-24                   #####
#####                             SERVER                             #####
##########################################################################

###########################
##### Load Librairies #####
###########################

library(shiny)
library(readxl)
library(sf)
library(dplyr)
library(leaflet)

####################################
##### Load IMS.DATA | GeoDATA #####
####################################

IMS.DATA <- read_xlsx("Données/IMS.xlsx")

GeoDATA <- read_sf("Données/Geo/RIMS_WAB_UNSD_WorldBank.shp") %>%
  rename(Residence = 1, ResidenceCode = 2, Continent = 5, ContinentCode = 6, 
         SRegion = 8, SRegionCode = 9, IRegin = 10, IRegionCode= 11, 
         LDC = 12, LLDC = 13, SIDS = 14, IncomeLevel = 15, S = 16, 
         ColorCode = 17, ContinentalRegion = 19, ClasseSDG = 20, 
         MoreLess = 21, DevtLevel = 22)

DATA <- GeoDATA %>% st_drop_geometry()

#######################
##### GeoDATA.CRS #####
#######################

GeoDATA.WGS84 <- st_transform(GeoDATA, crs = '+proj=longlat +datum=WGS84')

#############################
##### DATA.Manipulation #####
#############################

Label <- lapply(seq(nrow(DATA)), function(i) {
  paste0("<b>Name : </b>", DATA[i, 1], "<br>", 
         "<b>ISO Num. : </b>", DATA[i, 2],"<br>",
         "<b>ISO2 : </b>", DATA[i, 4],"<br>",
         "<b>ISO3 : </b>", DATA[i, 3])
})

##################
##### SERVER #####
##################

serverWGS84 <- function(input, output) {
  observeEvent(input$countries, {
    output$world_map <- renderLeaflet({
      leaflet() %>%
        addTiles(attribution = HTML("© <a href='https://www.openstreetmap.org/copyright/' target='_blank'>OpenStreetMap</a>, 
                                    <a href='https://opendatacommons.org/licenses/odbl/' target='_blank'>ODbL</a> | 
                                    <a href='https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/information/' target='_blank'>OpenDataSoft</a> | 
                                    <a href='https://cran.r-project.org/web/packages/rnaturalearth/index.html' target='_blank'>RNaturalEarth</a>")) %>%
        addPolygons(data = GeoDATA.WGS84, layerId = ~Residence,
                    color = "blue", stroke = 1, weight = 1.5, opacity = 0.75, fillOpacity = 0.25, 
                    label = lapply(Label, HTML)) %>%
        addScaleBar(position = "bottomleft")
      })
    
    SelectedResidence <- eventReactive(input$world_map_shape_click, {
      event <- input$world_map_shape_click
      clickedPolygon <- event$id
    })
    
    output$selected_residence <- renderText({
      SelectedR <- SelectedResidence()
      paste("Selected Residence:", SelectedR)
    })
  })
}

serverWR <- function(input, output) {
  observeEvent(input$countries, {
    output$world_map <- renderLeaflet({
      leaflet(options = leafletOptions(
        crs = leafletCRS(
          crsClass = "L.Proj.CRS", 
          code = "ESRI:54030", 
          proj4def = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", 
          scales = c(8192, 4096, 2048, 1024, 512)))) %>%
        addTiles(attribution = HTML("© <a href='https://www.openstreetmap.org/copyright/' target='_blank'>OpenStreetMap</a>,
                                    <a href='https://opendatacommons.org/licenses/odbl/' target='_blank'>ODbL</a> |
                                    <a href='https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/information/' target='_blank'>OpenDataSoft</a> |
                                    <a href='https://cran.r-project.org/web/packages/rnaturalearth/index.html' target='_blank'>RNaturalEarth</a>")) %>%
        addPolygons(data = GeoDATA.WGS84,
                    color = "blue", stroke = 1, weight = 1.5, opacity = 0.75, fillOpacity = 0.25, 
                    label = ~Residence) %>%
        addScaleBar(position = "bottomleft")
    })
  })
}