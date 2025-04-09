##########################################################################
#####                CLIMATE CHANGE MIGRATION NETWORK                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   KYLLIAN JAMES | 2023-11-14                   #####
#####                        Mod.: 2025-04-09                        #####
#####                              DATA                              #####
##########################################################################

##########################
##### Load Libraries #####
##########################

library(readxl)
library(sf)

#################################################
##### Load International MIGRANT Stock Data #####
#################################################

# iMIGRANT_STOCK_DATA_SHAREs <- read_xlsx("Données/ims_sex_residence_and_birth_with_shares_2020.xlsx")
iMIGRANT_STOCK_DATA_SHAREs <- readRDS("Données/ims_sex_residence_and_birth_with_shares_2020.rds") #SHINYApps.io

################################
##### Load GeoSpatial Data #####
################################

##### COORDINATE REFERENCE SYSTEM (CRS) #####

#WORLD GEODETIC SYSTEM (1984) [PROJ4]
EPSG_4326 <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

#WORLD ROBINSON (1963) [PROJ4]
ESRI_54030 <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"

#WORLD WINKEL-TRIPEL (WINKEL III) - (1921) [PROJ4]
# ESRI_54042 <- "+proj=wintri +lon_0=0 +lat_1=50.467 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"

##### COUNTRIEs #####
# GeoDATA <- read_sf("Données/Geo/COUNTRIEs/CCMN_GeoDATA.geojson", crs = EPSG_4326) #WORLD GEODETIC SYSTEM (1984)
# GeoDATA <- readRDS("Données/Geo/COUNTRIEs/CCMN_GeoDATA.RDS") #SHINYApps.io
GeoDATA <- readRDS("Données/Geo/COUNTRIEs/CCMN_GeoDATA_Reduced.RDS") #SHINYApps.io
GeoDATA_Global_Climate <- readRDS("Données/Geo/COUNTRIEs/CCMN_GeoDATA_Reduced_Global_Climate.RDS") #SHINYApps.io + Global Climate
DATA <- read_xlsx("Données/Geo/COUNTRIEs/CCMN_DATA.xlsx")
# GeoDATA_WinkelTripel <- read_sf( #WORLD WINKEL-TRIPEL (WINKEL III) - (1921)
#   "Données/Geo/COUNTRIEs/World_Winkel_Tripel/CCMN_GeoDATA_WinkelTripel.geojson", crs = ESRI_54042)
# DATA_WinkelTripel <- read_xlsx("Données/Geo/COUNTRIEs/World_Winkel_Tripel/CCMN_DATA_WinkelTripel.xlsx")

##### Continental REGIONs #####
# GeoRDATA_ContinentalREGIONs <- read_sf( #WORLD GEODETIC SYSTEM (1984)
#   "Données/Geo/REGIONs/ContinentalREGIONs/CCMN_GeoRDATA_ContinentalREGIONs.geojson", crs = EPSG_4326)
GeoRDATA_ContinentalREGIONs <- readRDS("Données/Geo/REGIONs/ContinentalREGIONs/CCMN_GeoRDATA_ContinentalREGIONs_Reduced.RDS") #SHINYApps.io
RDATA_ContinentalREGIONs <- read_xlsx("Données/Geo/REGIONs/ContinentalREGIONs/CCMN_RDATA_ContinentalREGIONs.xlsx")
# GeoRDATA_ContinentalREGIONs_WinkelTripel <- read_sf(
#   "Données/Geo/REGIONs/ContinentalREGIONs/World_Winkel_Tripel/CCMN_GeoRDATA_ContinentalREGIONs_WinkelTripel.geojson",
#   crs = ESRI_54042) #WORLD WINKEL-TRIPEL (WINKEL III) - (1921)
# RDATA_ContinentalREGIONs_WinkelTripel <- read_xlsx(
#   "Données/Geo/REGIONs/ContinentalREGIONs/World_Winkel_Tripel/CCMN_RDATA_ContinentalREGIONs_WinkelTripel.xlsx")

##### Continental SIREGIONs #####
# GeoRDATA_ContinentalSIREGIONs <- read_sf( #WORLD GEODETIC SYSTEM (1984)
#   "Données/Geo/REGIONs/ContinentalSIREGIONs/CCMN_GeoRDATA_ContinentalSIREGIONs.geojson", crs = EPSG_4326)
#NAME => FACTOR-LEVEL(s) => World Map
# GeoRDATA_ContinentalSIREGIONs$NAME <- factor(
#   x = GeoRDATA_ContinentalSIREGIONs$NAME,
#   levels = c(
#     "Eastern Africa", "Middle Africa", "Northern Africa", "Southern Africa", "Western Africa",
#     "Central Asia", "Eastern Asia", "South-Eastern Asia", "Southern Asia", "Western Asia",
#     "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe",
#     "Caribbean", "Central America", "South America",
#     "Northern America",
#     "Australia and New Zealand", "Melanesia", "Micronesia", "Polynesia",
#     "Other Countries/Territories"))
GeoRDATA_ContinentalSIREGIONs <- readRDS("Données/Geo/REGIONs/ContinentalSIREGIONs/CCMN_GeoRDATA_ContinentalSIREGIONs_Reduced.RDS") #SHINYApps.io
RDATA_ContinentalSIREGIONs <- read_xlsx("Données/Geo/REGIONs/ContinentalSIREGIONs/CCMN_RDATA_ContinentalSIREGIONs.xlsx")
# GeoRDATA_ContinentalSIREGIONs_WinkelTripel <- read_sf(
#   "Données/Geo/REGIONs/ContinentalSIREGIONs/World_Winkel_Tripel/CCMN_GeoRDATA_ContinentalSIREGIONs_WinkelTripel.geojson",
#   crs = ESRI_54042) #WORLD WINKEL-TRIPEL (WINKEL III) - (1921)
# RDATA_ContinentalSIREGIONs_WinkelTripel <- read_xlsx(
#   "Données/Geo/REGIONs/ContinentalSIREGIONs/World_Winkel_Tripel/CCMN_RDATA_ContinentalSIREGIONs_WinkelTripel.xlsx")

##### Geo. REGIONs #####
# GeoRDATA_GeoREGIONs <- read_sf( #WORLD GEODETIC SYSTEM (1984)
#   "Données/Geo/REGIONs/GeoREGIONs/CCMN_GeoRDATA_GeoREGIONs.geojson", crs = EPSG_4326)
#NAME => FACTOR-LEVEL(s) => World Map
# GeoRDATA_GeoREGIONs$NAME <- factor(
#   x = GeoRDATA_GeoREGIONs$NAME,
#   levels = c("Australia and New Zealand", "Central and Southern Asia", "Eastern and South-Eastern Asia",
#              "Europe and Northern America", "Latin America and the Caribbean", "Northern Africa and Western Asia",
#              "Oceania*", "Sub-Saharan Africa", "Other Countries/Territories"))
GeoRDATA_GeoREGIONs <- readRDS("Données/Geo/REGIONs/GeoREGIONs/CCMN_GeoRDATA_GeoREGIONs_Reduced.RDS") #SHINYApps.io
RDATA_GeoREGIONs <- read_xlsx("Données/Geo/REGIONs/GeoREGIONs/CCMN_RDATA_GeoREGIONs.xlsx")
# GeoRDATA_GeoREGIONs_WinkelTripel <- read_sf(
#   "Données/Geo/REGIONs/GeoREGIONs/World_Winkel_Tripel/CCMN_GeoRDATA_GeoREGIONs_WinkelTripel.geojson",
#   crs = ESRI_54042) #WORLD WINKEL-TRIPEL (WINKEL III) - (1921)
# RDATA_GeoREGIONs_WinkelTripel <- read_xlsx(
#   "Données/Geo/REGIONs/GeoREGIONs/World_Winkel_Tripel/CCMN_RDATA_GeoREGIONs_WinkelTripel.xlsx")

##### More/Less Developed COUNTRIEs #####
# GeoRDATA_MoreLess <- read_sf( #WORLD GEODETIC SYSTEM (1984)
#   "Données/Geo/REGIONs/DevLevels/MoreLess/CCMN_GeoRDATA_MoreLess.geojson", crs = EPSG_4326)
#NAME => FACTOR-LEVEL(s) => World Map
# GeoRDATA_MoreLess$NAME <- factor(
#   x = GeoRDATA_MoreLess$NAME,
#   levels = c("More Developed Countries", "Less Developed Countries", "Other Countries/Territories"))
GeoRDATA_MoreLess <- readRDS("Données/Geo/REGIONs/DevLevels/MoreLess/CCMN_GeoRDATA_MoreLess_Reduced.RDS") #SHINYApps.io
RDATA_MoreLess <- read_xlsx("Données/Geo/REGIONs/DevLevels/MoreLess/CCMN_RDATA_MoreLess.xlsx")
# GeoRDATA_MoreLess_WinkelTripel <- read_sf(
#   "Données/Geo/REGIONs/DevLevels/MoreLess/World_Winkel_Tripel/CCMN_GeoRDATA_MoreLess_WinkelTripel.geojson",
#   crs = ESRI_54042) #WORLD WINKEL-TRIPEL (WINKEL III) - (1921)
# RDATA_MoreLess_WinkelTripel <- read_xlsx(
#   "Données/Geo/REGIONs/DevLevels/MoreLess/World_Winkel_Tripel/CCMN_RDATA_MoreLess_WinkelTripel.xlsx")

##### More/Less/Least Developed COUNTRIEs #####
# GeoRDATA_MoreLessLeast <- read_sf( #WORLD GEODETIC SYSTEM (1984)
#   "Données/Geo/REGIONs/DevLevels/MoreLessLeast/CCMN_GeoRDATA_MoreLessLeast.geojson", crs = EPSG_4326)
#NAME => FACTOR-LEVEL(s) => World Map
# GeoRDATA_MoreLessLeast$NAME <- factor(
#   x = GeoRDATA_MoreLessLeast$NAME,
#   levels = c(
#     "More Developed Countries", "Less Developed Countries*", "Least Developed Countries", "Other Countries/Territories"))
GeoRDATA_MoreLessLeast <- readRDS("Données/Geo/REGIONs/DevLevels/MoreLessLeast/CCMN_GeoRDATA_MoreLessLeast_Reduced.RDS") #SHINYApps.io
RDATA_MoreLessLeast <- read_xlsx("Données/Geo/REGIONs/DevLevels/MoreLessLeast/CCMN_RDATA_MoreLessLeast.xlsx")
# GeoRDATA_MoreLessLeast_WinkelTripel <- read_sf(
#   "Données/Geo/REGIONs/DevLevels/MoreLessLeast/World_Winkel_Tripel/CCMN_GeoRDATA_MoreLessLeast_WinkelTripel.geojson",
#   crs = ESRI_54042) #WORLD WINKEL-TRIPEL (WINKEL III) - (1921)
# RDATA_MoreLessLeast_WinkelTripel <- read_xlsx(
#   "Données/Geo/REGIONs/DevLevels/MoreLessLeast/World_Winkel_Tripel/CCMN_RDATA_MoreLessLeast_WinkelTripel.xlsx")

##### Dev. Level(s) #####
# GeoRDATA_LDC_LLDC_SIDS <- read_sf( #WORLD GEODETIC SYSTEM (1984)
#   "Données/Geo/REGIONs/DevLevels/LDC_LLDC_SIDS/CCMN_GeoRDATA_LDC_LLDC_SIDS.geojson", crs = EPSG_4326)
#NAME => FACTOR-LEVEL(s) => World Map
# GeoRDATA_LDC_LLDC_SIDS$NAME <- factor(
#   x = GeoRDATA_LDC_LLDC_SIDS$NAME,
#   levels = c("Least Developed Countries* (LDC*)",
#              "Land-Locked Developing Countries* (LLDC*)",
#              "Small Island Developing States* (SIDS*)",
#              "LDC | LLDC", "LDC | SIDS", "Other Countries/Territories"))
GeoRDATA_LDC_LLDC_SIDS <- readRDS("Données/Geo/REGIONs/DevLevels/LDC_LLDC_SIDS/CCMN_GeoRDATA_LDC_LLDC_SIDS_Reduced.RDS") #SHINYApps.io
RDATA_LDC_LLDC_SIDS <- read_xlsx("Données/Geo/REGIONs/DevLevels/LDC_LLDC_SIDS/CCMN_RDATA_LDC_LLDC_SIDS.xlsx")
# GeoRDATA_LDC_LLDC_SIDS_WinkelTripel <- read_sf(
#   "Données/Geo/REGIONs/DevLevels/LDC_LLDC_SIDS/World_Winkel_Tripel/CCMN_GeoRDATA_LDC_LLDC_SIDS_WinkelTripel.geojson",
#   crs = ESRI_54042) #WORLD WINKEL-TRIPEL (WINKEL III) - (1921)
# RDATA_LDC_LLDC_SIDS_WinkelTripel <- read_xlsx(
#   "Données/Geo/REGIONs/DevLevels/LDC_LLDC_SIDS/World_Winkel_Tripel/CCMN_RDATA_LDC_LLDC_SIDS_WinkelTripel.xlsx")

##### Income Level(s) #####
# GeoRDATA_IncomeLevels <- read_sf( #WORLD GEODETIC SYSTEM (1984)
#   "Données/Geo/REGIONs/IncomeLevels/CCMN_GeoRDATA_IncomeLevels.geojson", crs = EPSG_4326)
#NAME => FACTOR-LEVEL(s) => World Map
# GeoRDATA_IncomeLevels$NAME <- factor(
#   x = GeoRDATA_IncomeLevels$NAME,
#   levels = c("High-Income Countries",
#              "Upper-Middle-Income Countries", "Lower-Middle-Income Countries",
#              "Low-Income Countries", "Other Countries/Territories"))
GeoRDATA_IncomeLevels <- readRDS("Données/Geo/REGIONs/IncomeLevels/CCMN_GeoRDATA_IncomeLevels_Reduced.RDS") #SHINYApps.io
RDATA_IncomeLevels <- read_xlsx("Données/Geo/REGIONs/IncomeLevels/CCMN_RDATA_IncomeLevels.xlsx")
# GeoRDATA_IncomeLevels_WinkelTripel <- read_sf(
#   "Données/Geo/REGIONs/IncomeLevels/World_Winkel_Tripel/CCMN_GeoRDATA_IncomeLevels_WinkelTripel.geojson",
#   crs = ESRI_54042) #WORLD WINKEL-TRIPEL (WINKEL III) - (1921)
# RDATA_IncomeLevels_WinkelTripel <- read_xlsx(
#   "Données/Geo/REGIONs/IncomeLevels/World_Winkel_Tripel/CCMN_RDATA_IncomeLevels_WinkelTripel.xlsx")

#############################
##### Load Climate Data #####
#############################

#### Climate Indicators => 3 Files #####
load("Données/Climate/Reduced_Climate_Data/Climate_Indicators/Climate_Indicators.RData")

#### ... #####
# Global_Climate_Maps <- readRDS("Données/Climate/Reduced_Climate_Data/Climate_Indicators/Global_Climate_Maps.RDS") #SHINYApps.io