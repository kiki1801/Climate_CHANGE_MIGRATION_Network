##########################################################################
#####                Climate Change Migration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   Kyllian James | 2023-10-24                   #####
#####                        Mod.: 2023-11-14                        #####
#####                              DATA                              #####
##########################################################################

###########################
##### Load Librairies #####
###########################

library(readxl)
library(sf)
library(dplyr)
library(DescTools)

##############################
##### Load IMS | GeoDATA #####
##############################

IMS.DATA <- read_xlsx("Données/IMS.xlsx")

IMS.DATA <- IMS.DATA %>%
  mutate(Residence = case_when(
    Residence == "High-income countries" ~ "High-Income Countries",
    Residence == "Upper-middle-income countries" ~ "Upper-Middle-Income Countries",
    Residence == "Lower-middle-income countries" ~ "Lower-Middle-Income Countries",
    Residence == "Low-income countries" ~ "Low-Income Countries",
    TRUE ~ Residence), 
    Birth = case_when(
      Birth == "High-income countries" ~ "High-Income Countries",
      Birth == "Upper-middle-income countries" ~ "Upper-Middle-Income Countries",
      Birth == "Lower-middle-income countries" ~ "Lower-Middle-Income Countries",
      Birth == "Low-income countries" ~ "Low-Income Countries",
      TRUE ~ Birth)
    )

GeoDATA <- read_sf("Données/Geo/RIMS_WAB_UNSD_WorldBank.shp") %>%
  rename(Residence = 1, ResidenceCode = 2, Continent = 5, ContinentCode = 6, 
         SRegion = 8, SRegionCode = 9, IRegion = 10, IRegionCode= 11, 
         LDC = 12, LLDC = 13, SIDS = 14, IncomeLevel = 15, S = 16, 
         ColorCode = 17, ContinentalRegion = 19, ClasseSDG = 20, 
         MoreLess = 21, DevtLevel = 22)

DATA <- GeoDATA %>% st_drop_geometry()

####################
##### GeoRDATA #####
####################

GeoRDATA.CRs <- GeoDATA %>%
  group_by(ContinentalRegion) %>%
  summarize(
    Continent = Mode(Continent),
    ContinentCode = Mode(ContinentCode),
    geometry = st_combine(geometry)) %>%
  mutate(
    Residence = case_when(
      ContinentalRegion == "Africa" ~ "AFRICA",
      ContinentalRegion == "Asia" ~ "ASIA",
      ContinentalRegion == "Europe" ~ "EUROPE",
      ContinentalRegion == "Latin America and the Caribbean" ~ "LATIN AMERICA AND THE CARIBBEAN",
      ContinentalRegion == "Northern America" ~ "NORTHERN AMERICA",
      ContinentalRegion == "Oceania" ~ "OCEANIA"
    ), 
    ResidenceCode = case_when(
      ContinentalRegion == "Africa" ~ 903,
      ContinentalRegion == "Asia" ~ 935,
      ContinentalRegion == "Europe" ~ 908,
      ContinentalRegion == "Latin America and the Caribbean" ~ 904,
      ContinentalRegion == "Northern America" ~ 905,
      ContinentalRegion == "Oceania" ~ 909
    )) %>%
  select(1, 5:6, 2:4)

RDATA.CRs <- GeoRDATA.CRs %>% st_drop_geometry()

GeoRDATA.CSRs <- GeoDATA %>%
  group_by(Region) %>%
  summarize(
    Continent = Mode(Continent),
    ContinentCode = Mode(ContinentCode),
    SRegion = Mode(SRegion), 
    SRegionCode = Mode(SRegionCode), 
    IRegionCode = Mode(IRegionCode),
    geometry = st_combine(geometry)) %>%
  rename(SIRegion = Region) %>%
  mutate(
    SIRegionCode = ifelse(is.na(IRegionCode), SRegionCode, IRegionCode), 
    ResidenceCode = case_when(
      SIRegionCode == 53 ~ 927,
      SIRegionCode == 29 ~ 915,
      SIRegionCode == 13 ~ 916,
      SIRegionCode == 143 ~ 5500,
      SIRegionCode == 14 ~ 910,
      SIRegionCode == 30 ~ 906,
      SIRegionCode == 151 ~ 923,
      SIRegionCode == 54 ~ 928,
      SIRegionCode == 57 ~ 954,
      SIRegionCode == 17 ~ 911,
      SIRegionCode == 15 ~ 912,
      SIRegionCode == 21 ~ 905,
      SIRegionCode == 154 ~ 924,
      SIRegionCode == 61 ~ 957,
      SIRegionCode == 5 ~ 931,
      SIRegionCode == 35 ~ 920,
      SIRegionCode == 18 ~ 913,
      SIRegionCode == 34 ~ 5501,
      SIRegionCode == 39 ~ 925,
      SIRegionCode == 11 ~ 914,
      SIRegionCode == 145 ~ 922,
      SIRegionCode == 155 ~ 926
    )) %>%
  select(1, 8:9, 2:7)

RDATA.CSRs <- GeoRDATA.CSRs %>% st_drop_geometry()

GeoRDATA.GRSDG <- GeoDATA %>%
  group_by(ClasseSDG) %>%
  summarize(geometry = st_combine(geometry)) %>%
  mutate(ResidenceCode = case_when(
    ClasseSDG == "Australia and New Zealand" ~ 927,
    ClasseSDG == "Central and Southern Asia" ~ 921,
    ClasseSDG == "Eastern and South-Eastern Asia" ~ 1832,
    ClasseSDG == "Europe and Northern America" ~ 1829,
    ClasseSDG == "Latin America and the Caribbean" ~ 1830,
    ClasseSDG == "Northern Africa and Western Asia" ~ 1833, 
    ClasseSDG == "Oceania*" ~ 1835, 
    ClasseSDG == "Sub-Saharan Africa" ~ 947,
  )) %>%
  select(1, 3, 2)

RDATA.GRSDG <- GeoRDATA.GRSDG %>% st_drop_geometry()

GeoRDATA.IncomeLevel <- GeoDATA %>%
  group_by(IncomeLevel) %>%
  summarize(geometry = st_combine(geometry)) %>%
  mutate(
    Residence = case_when(
      IncomeLevel == "H" ~ "High-Income Countries",
      IncomeLevel == "L" ~ "Low-Income Countries",
      IncomeLevel == "LM" ~ "Lower-Middle-Income Countries",
      IncomeLevel == "UM" ~ "Upper-Middle-Income Countries",
      IncomeLevel == NA ~ NA
    ),
    ResidenceCode = case_when(
      IncomeLevel == "H" ~ 1503,
      IncomeLevel == "L" ~ 1500,
      IncomeLevel == "LM" ~ 1501,
      IncomeLevel == "UM" ~ 1502,
      IncomeLevel == NA ~ NA
    )) %>%
  select(1, 3:4, 2) %>%
  arrange(desc(ResidenceCode))

RDATA.IncomeLevel <- GeoRDATA.IncomeLevel %>% st_drop_geometry()

#######################
##### GeoDATA.CRS #####
#######################

GeoDATA.WGS84 <- st_transform(GeoDATA, crs = '+proj=longlat +datum=WGS84')

GeoRDATA.CRs.WGS84 <- st_transform(GeoRDATA.CRs, crs = '+proj=longlat +datum=WGS84')

GeoRDATA.CSRs.WGS84 <- st_transform(GeoRDATA.CSRs, crs = '+proj=longlat +datum=WGS84')

GeoRDATA.GRSDG.WGS84 <- st_transform(GeoRDATA.GRSDG, crs = '+proj=longlat +datum=WGS84')

GeoRDATA.IncomeLevel.WGS84 <- st_transform(GeoRDATA.IncomeLevel, crs = '+proj=longlat +datum=WGS84')

#############################
##### DATA.MANIPULATION #####
#############################

Label <- lapply(seq(nrow(DATA)), function(i) {
  paste0("<b>Name: </b>", DATA[i, 1], "<br>", 
         "<b>ISO Num.: </b>", DATA[i, 2],"<br>",
         "<b>ISO2: </b>", DATA[i, 3],"<br>",
         "<b>ISO3: </b>", DATA[i, 4])
})

LabelR.CRs <- lapply(seq(nrow(RDATA.CRs)), function(i) {
  paste0("<b>Continental Region: </b>", RDATA.CRs[i, 1], "<br>", 
         "<b>Continent: </b>", RDATA.CRs[i, 4],"<br>",
         "<b>M49 Code: </b>", RDATA.CRs[i, 5],"<br>")
})

LabelR.CSRs <- lapply(seq(nrow(RDATA.CSRs)), function(i) {
  paste0("<b>Continental Sub-Region: </b>", RDATA.CSRs[i, 1], "<br>",
         "<b>M49 Code: </b>", RDATA.CSRs[i, 2],"<br>")
})

LabelR.GRSDG <- lapply(seq(nrow(RDATA.GRSDG)), function(i) {
  paste0("<b>Geographic Region: </b>", RDATA.GRSDG[i, 1])
})

LabelR.IncomeLevel <- lapply(seq(nrow(RDATA.IncomeLevel)), function(i) {
  paste0("<b>Income Level (2020): </b>", RDATA.IncomeLevel[i, 2])
})

####################################
##### COUNTRIE(s) | COLOR.CODE #####
####################################

CAF <- DATA$Residence[which(DATA$ContinentalRegion == "Africa")[order(DATA$Residence[which(DATA$ContinentalRegion == "Africa")])]]
ColorCode <- colorRampPalette(c("#CCFF33", "#336600"))(length(CAF))
ColorCodeCAF <- data.frame(Residence = CAF, CLRCode = ColorCode)

AFC <- DATA$Residence[which(DATA$ContinentalRegion == "Asia")[order(DATA$Residence[which(DATA$ContinentalRegion == "Asia")])]]
ColorCode <- colorRampPalette(c("#FF9900", "#FF6600"))(length(AFC[1:25]))
ColorCodeAFCOne <- data.frame(Residence = AFC[1:25], CLRCode = ColorCode)
ColorCode <- colorRampPalette(c("#FFCC99", "#996600"))(length(AFC[26:55]))
ColorCodeAFCTwo <- data.frame(Residence = AFC[26:55], CLRCode = ColorCode)

Europe <- DATA$Residence[which(DATA$ContinentalRegion == "Europe")[order(DATA$Residence[which(DATA$ContinentalRegion == "Europe")])]]
ColorCode <- colorRampPalette(c("#33CCFF", "#003366"))(length(Europe))
ColorCodeEU <- data.frame(Residence = Europe, CLRCode = ColorCode)

LAC <- DATA$Residence[which(DATA$ContinentalRegion == "Latin America and the Caribbean")[order(DATA$Residence[which(DATA$ContinentalRegion == "Latin America and the Caribbean")])]]
ColorCode <- colorRampPalette(c("#FF0000", "#660000"))(length(LAC))
ColorCodeLAC <- data.frame(Residence = LAC, CLRCode = ColorCode)

CONCACAF <- DATA$Residence[which(DATA$ContinentalRegion == "Northern America")[order(DATA$Residence[which(DATA$ContinentalRegion == "Northern America")])]]
ColorCode <- colorRampPalette(c("#CC99FF", "#663399"))(length(CONCACAF))
ColorCodeCONCACAF <- data.frame(Residence = CONCACAF, CLRCode = ColorCode)

OFC <- DATA$Residence[which(DATA$ContinentalRegion == "Oceania")[order(DATA$Residence[which(DATA$ContinentalRegion == "Oceania")])]]
ColorCode <- colorRampPalette(c("#FFCC00", "#FFFFCC"))(length(OFC))
ColorCodeOFC <- data.frame(Residence = OFC, CLRCode = ColorCode)

CountriesColorCode <- do.call(rbind, list(ColorCodeCAF, ColorCodeAFCOne, ColorCodeAFCTwo, ColorCodeEU, 
                                          ColorCodeLAC, ColorCodeCONCACAF, ColorCodeOFC))
CountriesColorCode <- left_join(CountriesColorCode, select(DATA, Residence, ResidenceCode), by = "Residence")
CountriesColorCode <- left_join(CountriesColorCode, select(
  (IMS.DATA %>% distinct(ResidenceCode, .keep_all = TRUE)), 
  Residence, ResidenceCode), by = "ResidenceCode")
CountriesColorCodeIMS <- CountriesColorCode[!is.na(CountriesColorCode$Residence.y), ] %>%
  rename(ResidenceCC = Residence.x, Residence = Residence.y)
CountriesColorCodeIMS <- rbind(CountriesColorCodeIMS, list(NA, "#999999", 2003, "Other"))
