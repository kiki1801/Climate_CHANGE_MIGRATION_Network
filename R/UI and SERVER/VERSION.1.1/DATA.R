##########################################################################
#####                Climate Change Migration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   Kyllian James | 2023-11-14                   #####
#####                        Mod.: 2023-11-20                        #####
#####                              DATA                              #####
##########################################################################

###########################
##### Load Librairies #####
###########################

library(readxl)
library(sf)

#########################
##### Load IMS.DATA #####
#########################

IMS.DATA <- read_xlsx("Données/IMS.xlsx")

########################
##### Load GeoDATA #####
########################

GeoDATA <- read_sf("Données/Geo/Countries/RIMS_WAB_UNSD_WorldBank.geojson", 
                   crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
DATA <- read_xlsx("Données/Geo/Countries/DATA.xlsx")

#########################
##### Load GeoRDATA #####
#########################

GeoRDATA.CRs <- read_sf("Données/Geo/Regions/CRs/GeoRDATA.CRs.geojson", 
                        crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
GeoRDATA.CRs$Residence <- factor(GeoRDATA.CRs$Residence)
RDATA.CRs <- read_xlsx("Données/Geo/Regions/CRs/RDATA.CRs.xlsx")

GeoRDATA.CSRs <- read_sf("Données/Geo/Regions/CSRs/GeoRDATA.CSRs.geojson", 
                         crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
GeoRDATA.CSRs$SIRegion <- factor(GeoRDATA.CSRs$SIRegion, 
                                 levels = c("Eastern Africa", "Middle Africa", "Northern Africa", "Southern Africa", 'Western Africa', 
                                            "Central Asia", "Eastern Asia", "South-Eastern Asia", "Southern Asia", "Western Asia", 
                                            "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe", 
                                            "Caribbean", "Central America", "South America", "Northern America", "Australia and New Zealand", "Melanesia", "Micronesia", "Polynesia"))
RDATA.CSRs <- read_xlsx("Données/Geo/Regions/CSRs/RDATA.CSRs.xlsx")

GeoRDATA.GRSDG <- read_sf("Données/Geo/Regions/GRSDG/GeoRDATA.GRSDG.geojson", 
                          crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
GeoRDATA.GRSDG$ClasseSDG <- factor(GeoRDATA.GRSDG$ClasseSDG)
RDATA.GRSDG <- read_xlsx("Données/Geo/Regions/GRSDG/RDATA.GRSDG.xlsx")

GeoRDATA.IncomeLevel <- read_sf("Données/Geo/Regions/IncomeLevel/GeoRDATA.IncomeLevel.geojson", 
                                crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
GeoRDATA.IncomeLevel$Residence <- factor(GeoRDATA.IncomeLevel$Residence, 
                                         levels = c("High-Income Countries", "Upper-Middle-Income Countries", 
                                                    "Lower-Middle-Income Countries", "Low-Income Countries", 
                                                    "Not Available"))
RDATA.IncomeLevel <- read_xlsx("Données/Geo/Regions/IncomeLevel/RDATA.IncomeLevel.xlsx")

#######################
##### GeoDATA.CRS #####
#######################

GeoDATA.WGS84 <- st_transform(GeoDATA, crs = '+proj=longlat +datum=WGS84')

GeoRDATA.CRs.WGS84 <- st_transform(GeoRDATA.CRs, crs = '+proj=longlat +datum=WGS84')

GeoRDATA.CSRs.WGS84 <- st_transform(GeoRDATA.CSRs, crs = '+proj=longlat +datum=WGS84')

GeoRDATA.GRSDG.WGS84 <- st_transform(GeoRDATA.GRSDG, crs = '+proj=longlat +datum=WGS84')

GeoRDATA.IncomeLevel.WGS84 <- st_transform(GeoRDATA.IncomeLevel, crs = '+proj=longlat +datum=WGS84')