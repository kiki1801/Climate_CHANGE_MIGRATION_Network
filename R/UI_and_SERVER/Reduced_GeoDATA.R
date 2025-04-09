#Load GeoSpatial Data
GeoDATA <- readRDS("../Desktop/R/UI_and_SERVER/CLIMINET/Données/Geo/COUNTRIEs/CCMN_GeoDATA.RDS")
DATA <- GeoDATA %>% st_drop_geometry()

GeoDATA_0.05 <- rmapshaper::ms_simplify(GeoDATA, keep = 0.05, keep_shapes = TRUE)
object.size(GeoDATA_0.05)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA_0.05, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25, label = ~lapply(Label, HTML))

GeoDATA_0.05 <- rmapshaper::ms_simplify(GeoDATA, keep = 0.05, keep_shapes = TRUE, explode = TRUE)
GeoDATA_0.05 <- GeoDATA_0.05 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(GeoDATA_0.05)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA_0.05, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)

GeoDATA_0.25 <- rmapshaper::ms_simplify(GeoDATA, keep = 0.25, keep_shapes = TRUE, explode = TRUE)
GeoDATA_0.25 <- GeoDATA_0.25 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(GeoDATA_0.25)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA_0.25, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)

GeoDATA_0.50 <- rmapshaper::ms_simplify(GeoDATA, keep = 0.50, keep_shapes = TRUE, explode = TRUE)
GeoDATA_0.50 <- GeoDATA_0.50 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(GeoDATA_0.50)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA_0.50, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)

GeoDATA_0.95 <- GeoDATA %>% rmapshaper::ms_simplify(keep = 0.95, keep_shapes = TRUE, explode = TRUE)
GeoDATA_0.95 <- GeoDATA_0.95 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(GeoDATA_0.95)/1024^2

GeoDATA_Small_Areas <- GeoDATA %>%
  filter(AREA < quantile(AREA, probs = 0.25)) %>% arrange(AREA)
object.size(GeoDATA_Small_Areas)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA_Small_Areas, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25, label = ~lapply(Label, HTML))

# NEIGHBORs <- st_touches(GeoDATA_0.95)
# X <- sapply(NEIGHBORs, length) > 0
# COUNTRIEs_with_NEIGHBORs <- GeoDATA$NAME[X]
# COUNTRIEs_without_NEIGHBORs <- GeoDATA$NAME[!X]

# library(spdep)
# NEIGHBORs <- poly2nb(GeoDATA_0.95)
# COUNTRIEs_with_NEIGHBORs <- rep(FALSE, length(NEIGHBORs))
# COUNTRIEs_without_NEIGHBORs <- rep(FALSE, length(NEIGHBORs))
# for (i in seq_along(NEIGHBORs)) {
#   if (length(NEIGHBORs[[i]]) == 1 && NEIGHBORs[[i]][1] == 0) {COUNTRIEs_without_NEIGHBORs[i] <- TRUE}
#   else if ((length(NEIGHBORs[[i]]) == 1 && NEIGHBORs[[i]][1] != 0) || length(NEIGHBORs[[i]]) > 1) {COUNTRIEs_with_NEIGHBORs[i] <- TRUE}}
# COUNTRIEs_with_NEIGHBORs <- GeoDATA$NAME[COUNTRIEs_with_NEIGHBORs]
# COUNTRIEs_without_NEIGHBORs <- GeoDATA$NAME[COUNTRIEs_without_NEIGHBORs]

X <- c(1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 
       1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 
       1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 
       0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
       1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 
       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1,
       0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
       0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 
       1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 
       1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
       0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 
       1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 
       1, 0)
X <- data.frame(NAME = DATA$NAME, NEIGHBORs = X)
COUNTRIEs_with_NEIGHBORs <- X %>% filter(NEIGHBORs == 1) %>% pull(NAME)
COUNTRIEs_without_NEIGHBORs <- X %>% filter(NEIGHBORs == 0) %>% pull(NAME)

GeoDATA_Small_Areas_Islands <- GeoDATA_Small_Areas %>% filter(NAME %in% COUNTRIEs_without_NEIGHBORs)
object.size(GeoDATA_Small_Areas_Islands)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA_Small_Areas_Islands, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25, label = ~lapply(Label, HTML))

GeoDATA_Small_Areas_Countries <- GeoDATA_Small_Areas %>% filter(NAME %in% COUNTRIEs_with_NEIGHBORs)
object.size(GeoDATA_Small_Areas_Countries)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA_Small_Areas_Countries, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25, label = ~lapply(Label, HTML))

GeoDATA_Islands <- GeoDATA %>% filter(NAME %in% COUNTRIEs_without_NEIGHBORs)
object.size(GeoDATA_Islands)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA_Islands, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25, label = ~lapply(Label, HTML))
GeoDATA_Countries <- GeoDATA %>% filter(NAME %in% COUNTRIEs_with_NEIGHBORs)
object.size(GeoDATA_Countries)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA_Countries, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25, label = ~lapply(Label, HTML))

GeoDATA_Countries_0.25 <- rmapshaper::ms_simplify(GeoDATA_Countries, keep = 0.25, keep_shapes = TRUE, explode = TRUE)
GeoDATA_Countries_0.25 <- GeoDATA_Countries_0.25 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(GeoDATA_Countries_0.25)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA_Countries_0.25, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)

GeoDATA_Countries_0.10_Global_Climate <- rmapshaper::ms_simplify(GeoDATA_Countries, keep = 0.10, keep_shapes = TRUE)
# GeoDATA_Countries_0.10_Global_Climate <- GeoDATA_Countries_0.10_Global_Climate %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(GeoDATA_Countries_0.10_Global_Climate)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA_Countries_0.10_Global_Climate, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)

SIZEs <- numeric(nrow(GeoDATA_Islands))
for (i in 1:nrow(GeoDATA_Islands)) {SIZEs[i] <- object.size(st_geometry(GeoDATA_Islands)[i]) / 1024^2}
DATA_Islands <- GeoDATA_Islands %>% mutate(SIZE = SIZEs) %>% arrange(SIZEs) %>% st_drop_geometry()

Geo_1 <- GeoDATA_Islands %>% filter(NAME == "ÅLAND ISLANDS")
object.size(Geo_1)
Geo_0 <- rmapshaper::ms_simplify(Geo_1, keep = 0.25, keep_shapes = TRUE, explode = TRUE)
Geo_0 <- Geo_0 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(Geo_0)
leaflet() %>% addTiles() %>% addPolygons(data = Geo_0, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)
Geo_1_Global_Climate <- GeoDATA_Islands %>% filter(NAME == "ÅLAND ISLANDS")
object.size(Geo_1_Global_Climate)
Geo_0 <- rmapshaper::ms_simplify(Geo_1_Global_Climate, keep = 0.10, keep_shapes = TRUE)
# Geo_0 <- Geo_0 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(Geo_0)
leaflet() %>% addTiles() %>% addPolygons(data = Geo_0, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)
Geo_1_Global_Climate <- st_set_geometry(Geo_1_Global_Climate, st_geometry(Geo_0))
Geo_2 <- GeoDATA_Islands %>% filter(NAME == "ANTARCTICA")
object.size(Geo_2)
Geo_0 <- rmapshaper::ms_simplify(Geo_2, keep = 0.10, keep_shapes = TRUE, explode = TRUE)
Geo_0 <- Geo_0 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(Geo_0)
leaflet() %>% addTiles() %>% addPolygons(data = Geo_0, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)
Geo_2 <- st_set_geometry(Geo_2, st_geometry(Geo_0))
Geo_2_Global_Climate <- GeoDATA_Islands %>% filter(NAME == "ANTARCTICA")
object.size(Geo_2_Global_Climate)
Geo_0 <- rmapshaper::ms_simplify(Geo_2_Global_Climate, keep = 0.10, keep_shapes = TRUE)
# Geo_0 <- Geo_0 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(Geo_0)
leaflet() %>% addTiles() %>% addPolygons(data = Geo_0, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)
Geo_2_Global_Climate <- st_set_geometry(Geo_2_Global_Climate, st_geometry(Geo_0))
Geo_3 <- GeoDATA_Islands %>% filter(NAME == "AUSTRALIA")
object.size(Geo_3)
Geo_0 <- rmapshaper::ms_simplify(Geo_3, keep = 0.10, keep_shapes = TRUE, explode = TRUE)
Geo_0 <- Geo_0 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(Geo_0)
leaflet() %>% addTiles() %>% addPolygons(data = Geo_0, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)
Geo_3 <- st_set_geometry(Geo_3, st_geometry(Geo_0))
Geo_3_Global_Climate <- GeoDATA_Islands %>% filter(NAME == "AUSTRALIA")
object.size(Geo_3_Global_Climate)
Geo_0 <- rmapshaper::ms_simplify(Geo_3_Global_Climate, keep = 0.10, keep_shapes = TRUE)
# Geo_0 <- Geo_0 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(Geo_0)
leaflet() %>% addTiles() %>% addPolygons(data = Geo_0, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)
Geo_3_Global_Climate <- st_set_geometry(Geo_3_Global_Climate, st_geometry(Geo_0))
Geo_4 <- GeoDATA_Islands %>% filter(NAME == "JAPAN")
object.size(Geo_4)
Geo_0 <- rmapshaper::ms_simplify(Geo_4, keep = 0.25, keep_shapes = TRUE, explode = TRUE)
Geo_0 <- Geo_0 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(Geo_0)
leaflet() %>% addTiles() %>% addPolygons(data = Geo_0, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)
Geo_4 <- st_set_geometry(Geo_4, st_geometry(Geo_0))
Geo_4_Global_Climate <- GeoDATA_Islands %>% filter(NAME == "JAPAN")
object.size(Geo_4_Global_Climate)
Geo_0 <- rmapshaper::ms_simplify(Geo_4_Global_Climate, keep = 0.10, keep_shapes = TRUE)
# Geo_0 <- Geo_0 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(Geo_0)
leaflet() %>% addTiles() %>% addPolygons(data = Geo_0, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)
Geo_4_Global_Climate <- st_set_geometry(Geo_4_Global_Climate, st_geometry(Geo_0))
Geo_5 <- GeoDATA_Islands %>% filter(NAME == "PHILIPPINES")
object.size(Geo_5)
Geo_0 <- rmapshaper::ms_simplify(Geo_5, keep = 0.25, keep_shapes = TRUE, explode = TRUE)
Geo_0 <- Geo_0 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(Geo_0)
leaflet() %>% addTiles() %>% addPolygons(data = Geo_0, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)
Geo_5 <- st_set_geometry(Geo_5, st_geometry(Geo_0))
Geo_5_Global_Climate <- GeoDATA_Islands %>% filter(NAME == "PHILIPPINES")
object.size(Geo_5_Global_Climate)
Geo_0 <- rmapshaper::ms_simplify(Geo_5_Global_Climate, keep = 0.10, keep_shapes = TRUE)
# Geo_0 <- Geo_0 %>% group_by(NAME) %>% summarise(geometry = st_union(geometry))
object.size(Geo_0)
leaflet() %>% addTiles() %>% addPolygons(data = Geo_0, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25)
Geo_5_Global_Climate <- st_set_geometry(Geo_5_Global_Climate, st_geometry(Geo_0))
GeoDATA_Islands_Global_Climate <- GeoDATA_Islands %>% filter(!NAME %in% c("ÅLAND ISLANDS", "ANTARCTICA", "AUSTRALIA", "JAPAN", "PHILIPPINES"))
GeoDATA_Islands_Global_Climate <- rbind(GeoDATA_Islands_Global_Climate, 
  Geo_1_Global_Climate, Geo_2_Global_Climate, Geo_3_Global_Climate, Geo_4_Global_Climate, Geo_5_Global_Climate)
object.size(GeoDATA_Islands_Global_Climate)/1024^2
GeoDATA_Islands <- GeoDATA_Islands %>% filter(!NAME %in% c("ÅLAND ISLANDS", "ANTARCTICA", "AUSTRALIA", "JAPAN", "PHILIPPINES"))
GeoDATA_Islands <- rbind(GeoDATA_Islands, Geo_1, Geo_2, Geo_3, Geo_4, Geo_5)
object.size(GeoDATA_Islands)/1024^2

SIZEs <- numeric(nrow(GeoDATA_Small_Areas_Islands))
for (i in 1:nrow(GeoDATA_Small_Areas_Islands)) {SIZEs[i] <- object.size(st_geometry(GeoDATA_Small_Areas_Islands)[i]) / 1024^2}
DATA_Small_Areas_Islands <- GeoDATA_Small_Areas_Islands %>% mutate(SIZE = SIZEs) %>% arrange(SIZEs) %>% st_drop_geometry()

SIZEs <- numeric(nrow(GeoDATA_Small_Areas_Countries))
for (i in 1:nrow(GeoDATA_Small_Areas_Countries)) {SIZEs[i] <- object.size(st_geometry(GeoDATA_Small_Areas_Countries)[i]) / 1024^2}
DATA_Small_Areas_Countries <- GeoDATA_Small_Areas_Countries %>% mutate(SIZE = SIZEs) %>% arrange(SIZEs) %>% st_drop_geometry()

GeoDATA_Islands <- GeoDATA_Islands %>% select(NAME, geometry)
GeoDATA_Reduced <- rbind(GeoDATA_Countries_0.25, GeoDATA_Islands)
object.size(GeoDATA_Reduced)/1024^2
GeoDATA <- GeoDATA %>% arrange(NAME)
GeoDATA_Reduced <- GeoDATA_Reduced %>% arrange(NAME)
GeoDATA$geometry <- GeoDATA_Reduced$geometry
object.size(GeoDATA)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25, label = ~lapply(Label, HTML))
GeoDATA_Islands_Global_Climate <- GeoDATA_Islands_Global_Climate %>% select(NAME, geometry)
GeoDATA_Countries_0.10_Global_Climate <- GeoDATA_Countries_0.10_Global_Climate %>% select(NAME, geometry)
GeoDATA_Reduced_Global_Climate <- rbind(GeoDATA_Countries_0.10_Global_Climate, GeoDATA_Islands_Global_Climate)
object.size(GeoDATA_Reduced_Global_Climate)/1024^2
GeoDATA_Global_Climate <- GeoDATA %>% arrange(NAME)
GeoDATA_Reduced_Global_Climate <- GeoDATA_Reduced_Global_Climate %>% arrange(NAME)
GeoDATA_Global_Climate$geometry <- GeoDATA_Reduced_Global_Climate$geometry
object.size(GeoDATA_Global_Climate)/1024^2
leaflet() %>% addTiles() %>% addPolygons(data = GeoDATA_Global_Climate, 
                                         stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                                         fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25, label = ~lapply(Label, HTML))

Geo_1 <- GeoDATA %>% filter(NAME == "ARGENTINA")
Geo_2 <- GeoDATA %>% filter(NAME == "BRAZIL")
Geo_3 <- GeoDATA %>% filter(NAME == "URUGUAY")
Geo_4 <- GeoDATA %>% filter(NAME == "BRAZILIAN ISLAND")
Geo_5 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "BRAZILIAN ISLAND")
Geo_0 <- st_union(Geo_3, st_geometry(Geo_4))
Geo_3 <- st_set_geometry(Geo_3, st_geometry(Geo_0))
Geometric_Operations <- function(COUNTRY, Geo) {
  if (st_is_valid(COUNTRY) == FALSE) {COUNTRY <- st_make_valid(COUNTRY)}
  if (st_is_valid(COUNTRY) == FALSE) {Geo <- st_make_valid(Geo)}
  Geo_0 <- st_difference(st_geometry(COUNTRY), st_geometry(Geo))
  if (length(Geo_0) > 1) {Geo_0 <- Geo_0[st_geometry_type(Geo_0) %in% c("POLYGON", "MULTIPOLYGON")]}
  if (st_is_valid(Geo_0) == FALSE) {Geo_0 <- st_make_valid(Geo_0)}
  return(Geo_0)}
Geo_0 <- Geometric_Operations(Geo_1, Geo_5)
Geo_1 <- st_set_geometry(Geo_1, Geo_0)
Geo_0 <- Geometric_Operations(Geo_2, Geo_5)
Geo_2 <- st_set_geometry(Geo_2, Geo_0)
Geo_0 <- Geometric_Operations(Geo_3, Geo_5)
Geo_3 <- st_set_geometry(Geo_3, Geo_0)
GeoDATA <- GeoDATA %>% filter(!NAME %in% c("ARGENTINA", "BRAZIL", "URUGUAY", "BRAZILIAN ISLAND"))
GeoDATA <- rbind(GeoDATA, Geo_1, Geo_2, Geo_3, Geo_5)
object.size(GeoDATA)/1024^2
Geo_1 <- GeoDATA_Global_Climate %>% filter(NAME == "ARGENTINA")
Geo_2 <- GeoDATA_Global_Climate %>% filter(NAME == "BRAZIL")
Geo_3 <- GeoDATA_Global_Climate %>% filter(NAME == "URUGUAY")
Geo_4 <- GeoDATA_Global_Climate %>% filter(NAME == "BRAZILIAN ISLAND")
Geo_5 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "BRAZILIAN ISLAND")
Geo_0 <- st_union(Geo_1, st_geometry(Geo_4))
Geo_1 <- st_set_geometry(Geo_1, st_geometry(Geo_0))
Geo_0 <- st_union(Geo_3, st_geometry(Geo_4))
Geo_3 <- st_set_geometry(Geo_3, st_geometry(Geo_0))
Geometric_Operations <- function(COUNTRY, Geo) {
  if (st_is_valid(COUNTRY) == FALSE) {COUNTRY <- st_make_valid(COUNTRY)}
  if (st_is_valid(COUNTRY) == FALSE) {Geo <- st_make_valid(Geo)}
  Geo_0 <- st_difference(st_geometry(COUNTRY), st_geometry(Geo))
  if (length(Geo_0) > 1) {Geo_0 <- Geo_0[st_geometry_type(Geo_0) %in% c("POLYGON", "MULTIPOLYGON")]}
  if (st_is_valid(Geo_0) == FALSE) {Geo_0 <- st_make_valid(Geo_0)}
  return(Geo_0)}
Geo_0 <- Geometric_Operations(Geo_1, Geo_3)
Geo_1 <- st_set_geometry(Geo_1, Geo_0)
Geo_0 <- Geometric_Operations(Geo_1, Geo_5)
Geo_1 <- st_set_geometry(Geo_1, Geo_0)
Geo_0 <- Geometric_Operations(Geo_2, Geo_5)
Geo_2 <- st_set_geometry(Geo_2, Geo_0)
Geo_0 <- Geometric_Operations(Geo_3, Geo_5)
Geo_3 <- st_set_geometry(Geo_3, Geo_0)
GeoDATA_Global_Climate <- GeoDATA_Global_Climate %>% filter(!NAME %in% c("ARGENTINA", "BRAZIL", "URUGUAY", "BRAZILIAN ISLAND"))
GeoDATA_Global_Climate <- rbind(GeoDATA_Global_Climate, Geo_1, Geo_2, Geo_3, Geo_5)
object.size(GeoDATA_Global_Climate)/1024^2

Geo_1 <- GeoDATA %>% filter(NAME == "AUSTRIA")
Geo_2 <- GeoDATA %>% filter(NAME == "SWITZERLAND")
Geo_3 <- GeoDATA %>% filter(NAME == "LIECHTENSTEIN")
Geo_4 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "LIECHTENSTEIN")
Geo_0 <- st_union(Geo_2, st_geometry(Geo_3))
Geo_2 <- st_set_geometry(Geo_2, st_geometry(Geo_0))
Geo_0 <- Geometric_Operations(Geo_1, Geo_4)
Geo_1 <- st_set_geometry(Geo_1, Geo_0)
Geo_0 <- Geometric_Operations(Geo_2, Geo_4)
Geo_2 <- st_set_geometry(Geo_2, Geo_0)
GeoDATA <- GeoDATA %>% filter(!NAME %in% c("AUSTRIA", "SWITZERLAND", "LIECHTENSTEIN"))
GeoDATA <- rbind(GeoDATA, Geo_1, Geo_2, Geo_4)
object.size(GeoDATA)/1024^2

Geo_1 <- GeoDATA %>% filter(NAME == "SPAIN")
Geo_2 <- GeoDATA %>% filter(NAME == "GIBRALTAR")
Geo_3 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "GIBRALTAR")
Geo_0 <- Geometric_Operations(Geo_1, Geo_3)
Geo_1 <- st_set_geometry(Geo_1, Geo_0)
GeoDATA <- GeoDATA %>% filter(!NAME %in% c("SPAIN", "GIBRALTAR"))
GeoDATA <- rbind(GeoDATA, Geo_1, Geo_3)
object.size(GeoDATA)/1024^2
Geo_1 <- GeoDATA_Global_Climate %>% filter(NAME == "SPAIN")
Geo_2 <- GeoDATA_Global_Climate %>% filter(NAME == "GIBRALTAR")
Geo_3 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "GIBRALTAR")
Geo_0 <- Geometric_Operations(Geo_1, Geo_3)
Geo_1 <- st_set_geometry(Geo_1, Geo_0)
GeoDATA_Global_Climate <- GeoDATA_Global_Climate %>% filter(!NAME %in% c("SPAIN", "GIBRALTAR"))
GeoDATA_Global_Climate <- rbind(GeoDATA_Global_Climate, Geo_1, Geo_3)
object.size(GeoDATA_Global_Climate)/1024^2

Geo_1 <- GeoDATA %>% filter(NAME == "ITALY")
Geo_2 <- GeoDATA %>% filter(NAME == "HOLY SEE")
Geo_3 <- GeoDATA %>% filter(NAME == "SAN MARINO")
Geo_4 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "HOLY SEE")
Geo_5 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "SAN MARINO")
Geo_0 <- st_union(Geo_1, st_geometry(Geo_2))
Geo_1 <- st_set_geometry(Geo_1, st_geometry(Geo_0))
Geo_0 <- st_union(Geo_1, st_geometry(Geo_3))
Geo_1 <- st_set_geometry(Geo_1, st_geometry(Geo_0))
Geo_0 <- Geometric_Operations(Geo_1, Geo_4)
Geo_1 <- st_set_geometry(Geo_1, Geo_0)
Geo_0 <- Geometric_Operations(Geo_1, Geo_5)
Geo_1 <- st_set_geometry(Geo_1, Geo_0)
GeoDATA <- GeoDATA %>% filter(!NAME %in% c("ITALY", "HOLY SEE", "SAN MARINO"))
GeoDATA <- rbind(GeoDATA, Geo_1, Geo_4, Geo_5)
object.size(GeoDATA)/1024^2
Geo_1 <- GeoDATA_Global_Climate %>% filter(NAME == "ITALY")
Geo_2 <- GeoDATA_Global_Climate %>% filter(NAME == "HOLY SEE")
Geo_3 <- GeoDATA_Global_Climate %>% filter(NAME == "SAN MARINO")
Geo_4 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "HOLY SEE")
Geo_5 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "SAN MARINO")
Geo_0 <- st_union(Geo_1, st_geometry(Geo_2))
Geo_1 <- st_set_geometry(Geo_1, st_geometry(Geo_0))
Geo_0 <- st_union(Geo_1, st_geometry(Geo_3))
Geo_1 <- st_set_geometry(Geo_1, st_geometry(Geo_0))
Geo_0 <- Geometric_Operations(Geo_1, Geo_4)
Geo_1 <- st_set_geometry(Geo_1, Geo_0)
Geo_0 <- Geometric_Operations(Geo_1, Geo_5)
Geo_1 <- st_set_geometry(Geo_1, Geo_0)
GeoDATA_Global_Climate <- GeoDATA_Global_Climate %>% filter(!NAME %in% c("ITALY", "HOLY SEE", "SAN MARINO"))
GeoDATA_Global_Climate <- rbind(GeoDATA_Global_Climate, Geo_1, Geo_4, Geo_5)
object.size(GeoDATA_Global_Climate)/1024^2

Geo_1 <- GeoDATA %>% filter(NAME == "SINT MAARTEN")
Geo_2 <- GeoDATA %>% filter(NAME == "SAINT MARTIN")
Geo_3 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "SINT MAARTEN")
Geo_4 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "SAINT MARTIN")
GeoDATA <- GeoDATA %>% filter(!NAME %in% c("SINT MAARTEN", "SAINT MARTIN"))
GeoDATA <- rbind(GeoDATA, Geo_3, Geo_4)
object.size(GeoDATA)/1024^2
Geo_1 <- GeoDATA_Global_Climate %>% filter(NAME == "SINT MAARTEN")
Geo_2 <- GeoDATA_Global_Climate %>% filter(NAME == "SAINT MARTIN")
Geo_3 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "SINT MAARTEN")
Geo_4 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "SAINT MARTIN")
GeoDATA_Global_Climate <- GeoDATA_Global_Climate %>% filter(!NAME %in% c("SINT MAARTEN", "SAINT MARTIN"))
GeoDATA_Global_Climate <- rbind(GeoDATA_Global_Climate, Geo_3, Geo_4)
object.size(GeoDATA_Global_Climate)/1024^2

Geo_1 <- GeoDATA %>% filter(NAME == "CHINA")
Geo_2 <- GeoDATA %>% filter(NAME == "MACAO SAR")
Geo_3 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "MACAO SAR")
Geo_0 <- Geometric_Operations(Geo_1, Geo_3)
Geo_1 <- st_set_geometry(Geo_1, Geo_0)
GeoDATA <- GeoDATA %>% filter(!NAME %in% c("CHINA", "MACAO SAR"))
GeoDATA <- rbind(GeoDATA, Geo_1, Geo_3)
object.size(GeoDATA)/1024^2
Geo_1 <- GeoDATA_Global_Climate %>% filter(NAME == "CHINA")
Geo_2 <- GeoDATA_Global_Climate %>% filter(NAME == "MACAO SAR")
Geo_3 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "MACAO SAR")
Geo_0 <- st_union(Geo_1, st_geometry(Geo_2))
Geo_1 <- st_set_geometry(Geo_1, st_geometry(Geo_0))
Geo_0 <- Geometric_Operations(Geo_1, Geo_3)
Geo_1 <- st_set_geometry(Geo_1, Geo_0)
GeoDATA_Global_Climate <- GeoDATA_Global_Climate %>% filter(!NAME %in% c("CHINA", "MACAO SAR"))
GeoDATA_Global_Climate <- rbind(GeoDATA_Global_Climate, Geo_1, Geo_3)
object.size(GeoDATA_Global_Climate)/1024^2

Geo_1 <- GeoDATA %>% filter(NAME == "CYPRUS")
Geo_2 <- GeoDATA_Countries %>% filter(NAME == "CYPRUS")
Geo_3 <- GeoDATA %>% filter(NAME == "AKROTIRI AND DHEKELIA (SBA)")
Geo_4 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "AKROTIRI AND DHEKELIA (SBA)")
GeoDATA <- GeoDATA %>% filter(!NAME %in% c("CYPRUS", "AKROTIRI AND DHEKELIA (SBA)"))
GeoDATA <- rbind(GeoDATA, Geo_2, Geo_4)
object.size(GeoDATA)/1024^2
Geo_1 <- GeoDATA_Global_Climate %>% filter(NAME == "CYPRUS")
Geo_2 <- GeoDATA_Countries %>% filter(NAME == "CYPRUS")
Geo_3 <- GeoDATA_Global_Climate %>% filter(NAME == "AKROTIRI AND DHEKELIA (SBA)")
Geo_4 <- GeoDATA_Small_Areas_Countries %>% filter(NAME == "AKROTIRI AND DHEKELIA (SBA)")
GeoDATA_Global_Climate <- GeoDATA_Global_Climate %>% filter(!NAME %in% c("CYPRUS", "AKROTIRI AND DHEKELIA (SBA)"))
GeoDATA_Global_Climate <- rbind(GeoDATA_Global_Climate, Geo_2, Geo_4)
object.size(GeoDATA_Global_Climate)/1024^2

if ("sfc_GEOMETRY" %in% class(GeoDATA$geometry)) {GeoDATA$geometry <- st_cast(GeoDATA$geometry, "MULTIPOLYGON")}
if ("sfc_GEOMETRY" %in% class(GeoDATA_Global_Climate$geometry)) {GeoDATA_Global_Climate$geometry <- st_cast(GeoDATA_Global_Climate$geometry, "MULTIPOLYGON")}

saveRDS(GeoDATA, "R/UI_and_SERVER/CLIMINET-SHINYApps/Données/Geo/COUNTRIEs/CCMN_GeoDATA_Reduced.RDS")
saveRDS(GeoDATA_Global_Climate, "R/UI_and_SERVER/CLIMINET-SHINYApps/Données/Geo/COUNTRIEs/CCMN_GeoDATA_Reduced_Global_Climate.RDS")

# GeoDATA_0.05 <- rmapshaper::ms_simplify(GeoDATA, keep = 0.05, method = "vis", weighting = 0.7, keep_shapes = TRUE)
# GeoDATA_0.05 <- rmapshaper::ms_simplify(GeoDATA, keep = 0.05, method = "vis", weighting = 0.7, keep_shapes = TRUE, explode = TRUE)
# GeoDATA_0.05 <- rmapshaper::ms_simplify(GeoDATA, keep = 0.05, method = "vis", weighting = 0.5, keep_shapes = TRUE)
# GeoDATA_0.05 <- rmapshaper::ms_simplify(GeoDATA, keep = 0.05, method = "vis", weighting = 0.5, keep_shapes = TRUE, explode = TRUE)
# GeoDATA_0.05 <- rmapshaper::ms_simplify(GeoDATA, keep = 0.05, method = "dp", keep_shapes

# GeoDATA_0.05 <- rmapshaper::ms_simplify(GeoDATA, keep = 0.05, method = "dp", keep_shapes = TRUE, explode = TRUE)

GeoDATA <- readRDS("R/UI_and_SERVER/CLIMINET-SHINYApps/Données/Geo/COUNTRIEs/CCMN_GeoDATA_Reduced.RDS")

##### Continental REGIONs #####
GeoRDATA_ContinentalREGIONs <- GeoDATA %>% 
  #DATA MANIPULATIONs
  mutate(
    #Replace Values in ContinentalREGION
    ContinentalREGION = case_when(is.na(ContinentalREGION) ~ "Antarctica", TRUE ~ ContinentalREGION)) %>%
  #DATA MANIPULATIONs
  group_by(ContinentalREGION) %>%
  #DATA MANIPULATIONs
  summarize(
    #New Variable => CONTINENT
    CONTINENT = as.character(Mode(CONTINENT)),
    #New Variable => CONTINENT_CODE
    CONTINENT_CODE = as.character(Mode(CONTINENT_CODE)),
    #New Variable => AREA
    AREA = sum(AREA, na.rm = TRUE),
    #New Variable => GEOMETRY+
    geometry = st_combine(geometry)) %>%
  #Rename Column(s)
  rename(VISUALIZATION_NAME = 1) %>%
  #DATA MANIPULATIONs
  mutate(
    #New Variable => NAME
    NAME = toupper(VISUALIZATION_NAME),
    #New Variable => CODE
    CODE = case_when(
      NAME == "AFRICA" ~ 903, 
      NAME == "ASIA" ~ 935, 
      NAME == "EUROPE" ~ 908,
      NAME == "LATIN AMERICA AND THE CARIBBEAN" ~ 904,
      NAME == "NORTHERN AMERICA" ~ 905,
      NAME == "OCEANIA" ~ 909),
    #Replace Values in CONTINENT
    CONTINENT = case_when(is.na(CONTINENT) ~ "ANTARCTICA", TRUE ~ CONTINENT),
    #Replace Values in CONTINENT_CODE
    CONTINENT_CODE = case_when(is.na(CONTINENT_CODE) ~ "010", TRUE ~ CONTINENT_CODE),
    #New Variable => LabelR
    LabelR = paste0("<b>Continental REGION: </b>", NAME, "<br>",
                    "<b>Continent: </b>", CONTINENT, "<br>",
                    "<b>Numeric Code (M49 Code): </b>", CONTINENT_CODE),
    #New Variable => CLRCode
    CLRCode = c(
      "#339900", "#FAFAFA", "#FF9900", "#003366", "#FF0000", "#663399", "#FFCC00"),
    #New Variable => SOURCE
    SOURCE = case_when(
      NAME == "ANTARCTICA" ~ "Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques",
      NAME %in% c("AFRICA", "EUROPE") ~ "World Health OrGanization (WHO) - Countries and Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques",
      TRUE ~ "World Health OrGanization (WHO) - Countries and Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques and Global Administrative Areas (GADM)")) %>% 
  select(6, 1, 7, 2:3, 8:9, 4, 10, 5) #Columns ORDER

CCMN_GeoRDATA_ContinentalREGIONs <- readRDS("R/UI_and_SERVER/CLIMINET-SHINYApps/Données/Geo/REGIONs/ContinentalREGIONs/CCMN_GeoRDATA_ContinentalREGIONs.RDS")
GeoRDATA_ContinentalREGIONs <- GeoRDATA_ContinentalREGIONs %>% arrange(NAME)
CCMN_GeoRDATA_ContinentalREGIONs <- CCMN_GeoRDATA_ContinentalREGIONs %>% arrange(NAME)
CCMN_GeoRDATA_ContinentalREGIONs$geometry <- GeoRDATA_ContinentalREGIONs$geometry
saveRDS(CCMN_GeoRDATA_ContinentalREGIONs, "R/UI_and_SERVER/CLIMINET-SHINYApps/Données/Geo/REGIONs/ContinentalREGIONs/CCMN_GeoRDATA_ContinentalREGIONs_Reduced.RDS")

##### Continental SIREGIONs #####
GeoRDATA_ContinentalSIREGIONs <- GeoDATA %>% 
  #DATA MANIPULATIONs
  mutate(
    #Replace Values in SIREGION
    SIREGION = case_when(is.na(SIREGION) ~ "Other Countries/Territories", TRUE ~ SIREGION)) %>%
  #DATA MANIPULATIONs
  group_by(SIREGION) %>%
  #DATA MANIPULATIONs
  summarize(
    #New Variable => SIREGION_CODE
    SIREGION_CODE = as.character(Mode(SIREGION_CODE)),
    #New Variable => CONTINENT
    CONTINENT = as.character(Mode(CONTINENT)),
    #New Variable => CONTINENT_CODE
    CONTINENT_CODE = as.character(Mode(CONTINENT_CODE)),
    #New Variable => AREA
    AREA = sum(AREA, na.rm = TRUE),
    #New Variable => GEOMETRY+
    geometry = st_combine(geometry)) %>%
  #Rename Column(s)
  rename(NAME = 1) %>%
  #DATA MANIPULATIONs
  mutate(
    #New Variable => CODE
    CODE = case_when(
      NAME == "Australia and New Zealand" ~ 927, 
      NAME == "Caribbean" ~ 915, 
      NAME == "Central America" ~ 916,
      NAME == "Central Asia" ~ 5500,
      NAME == "Eastern Africa" ~ 910,
      NAME == "Eastern Asia" ~ 906,
      NAME == "Eastern Europe" ~ 923, 
      NAME == "Melanesia" ~ 928, 
      NAME == "Micronesia" ~ 954,
      NAME == "Middle Africa" ~ 911,
      NAME == "Northern Africa" ~ 912,
      NAME == "Northern America" ~ 905,
      NAME == "Northern Europe" ~ 923, 
      NAME == "Polynesia" ~ 957, 
      NAME == "South America" ~ 931,
      NAME == "South-Eastern Asia" ~ 920,
      NAME == "Southern Africa" ~ 913,
      NAME == "Southern Asia" ~ 5501,
      NAME == "Southern Europe" ~ 925, 
      NAME == "Western Africa" ~ 914, 
      NAME == "Western Asia" ~ 922,
      NAME == "Western Europe" ~ 926),
    #New Variable => LabelR
    LabelR = paste0("<b>Continental Sub-REGION/Intermediate REGION: </b>", NAME, "<br>",
                    "<b>Numeric Code (M49 Code): </b>", SIREGION_CODE),
    #New Variable => CLRCode => colorRampPalette(c("#CCFF33", "#336600"))(5)
    CLRCode = c(
      "#F1C40F", "#FF0000", "#B20000", "#FFCC99", "#CCFF33", "#FFB24C", "#33CCFF", 
      "#F5D74E", "#FAEB8D", "#A5D826", "#7FB219", "#663399", "#2299CC", "#999999", "#FFFFCC", 
      "#660000", "#FF9900", "#598C0C", "#F08116", "#116699", "#336600", "#E16A2D", "#003366"),
    #New Variable => SOURCE
    SOURCE = case_when(
      NAME %in% c("Central America", "Southern Africa") ~ "World Health OrGanization (WHO) - Countries",
      NAME == "Other Countries/Territories" ~ "Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques",
      NAME %in% c(
        "Australia and New Zealand", "Central Asia", "Eastern Africa", "Eastern Europe", "Melanesia", 
        "Middle Africa", "Northern Africa", "Northern Europe", "South America", "Southern Asia", "Southern Europe", 
        "Western Africa", "Western Asia", "Western Europe") ~ "World Health OrGanization (WHO) - Countries and Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques",
      TRUE ~ "World Health OrGanization (WHO) - Countries and Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques and Global Administrative Areas (GADM)")) %>% 
  select(1:2, 7, 3:4, 8:9, 5, 10, 6) #Columns ORDER

CCMN_GeoRDATA_ContinentalSIREGIONs <- readRDS("R/UI_and_SERVER/CLIMINET/Données/Geo/REGIONs/ContinentalSIREGIONs/CCMN_GeoRDATA_ContinentalSIREGIONs.RDS")
# GeoRDATA_ContinentalSIREGIONs <- GeoRDATA_ContinentalSIREGIONs %>% arrange(NAME)
GeoRDATA_ContinentalSIREGIONs <- GeoRDATA_ContinentalSIREGIONs[order(CCMN_GeoRDATA_ContinentalSIREGIONs$NAME), ]
CCMN_GeoRDATA_ContinentalSIREGIONs <- CCMN_GeoRDATA_ContinentalSIREGIONs %>% arrange(NAME)
CCMN_GeoRDATA_ContinentalSIREGIONs$geometry <- GeoRDATA_ContinentalSIREGIONs$geometry
saveRDS(CCMN_GeoRDATA_ContinentalSIREGIONs, "R/UI_and_SERVER/CLIMINET-SHINYApps/Données/Geo/REGIONs/ContinentalSIREGIONs/CCMN_GeoRDATA_ContinentalSIREGIONs_Reduced.RDS")

##### Geo. REGIONs #####
GeoRDATA_GeoREGIONs <- GeoDATA %>% 
  #DATA MANIPULATIONs
  mutate(
    #Replace Values in GeoREGION
    GeoREGION = case_when(is.na(GeoREGION) ~ "Other Countries/Territories", TRUE ~ GeoREGION)) %>%
  #DATA MANIPULATIONs
  group_by(GeoREGION) %>%
  #DATA MANIPULATIONs
  summarize(
    #New Variable => AREA
    AREA = sum(AREA, na.rm = TRUE),
    #New Variable => GEOMETRY+
    geometry = st_combine(geometry)) %>%
  #Rename Column(s)
  rename(NAME = 1) %>%
  #DATA MANIPULATIONs
  mutate(
    #New Variable => CODE
    CODE = case_when(
      NAME == "Australia and New Zealand" ~ 927,
      NAME == "Central and Southern Asia" ~ 921,
      NAME == "Eastern and South-Eastern Asia" ~ 1832,
      NAME == "Europe and Northern America" ~ 1829,
      NAME == "Latin America and the Caribbean" ~ 1830,
      NAME == "Northern Africa and Western Asia" ~ 1833,
      NAME == "Oceania*" ~ 1835,
      NAME == "Sub-Saharan Africa" ~ 947),
    #New Variable => LabelR
    LabelR = paste0("<b>Geo. REGION: </b>", NAME),
    #New Variable => CLRCode
    CLRCode = c(
      "#FF0000", "#FF6600", "#339900", "#003366", "#33CCFF", "#FF9900", "#CC0000", "#999999", "#FF3399"),
    #New Variable => SOURCE
    SOURCE = case_when(
      NAME == "Other Countries/Territories" ~ "Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques",
      NAME %in% c(
        "Central and Southern Asia", "Northern Africa and Western Asia", "Sub-Saharan Africa") ~ "World Health OrGanization (WHO) - Countries and Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques",
      TRUE ~ "World Health OrGanization (WHO) - Countries and Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques and Global Administrative Areas (GADM)")) %>%
  select(1, 4:6, 2, 7, 3) #Columns ORDER

CCMN_GeoRDATA_GeoREGIONs <- readRDS("R/UI_and_SERVER/CLIMINET/Données/Geo/REGIONs/GeoREGIONs/CCMN_GeoRDATA_GeoREGIONs.RDS")
# GeoRDATA_GeoREGIONs <- GeoRDATA_GeoREGIONs %>% arrange(NAME)
GeoRDATA_GeoREGIONs <- GeoRDATA_GeoREGIONs[order(CCMN_GeoRDATA_GeoREGIONs$NAME), ]
CCMN_GeoRDATA_GeoREGIONs <- CCMN_GeoRDATA_GeoREGIONs %>% arrange(NAME)
CCMN_GeoRDATA_GeoREGIONs$geometry <- GeoRDATA_GeoREGIONs$geometry
saveRDS(CCMN_GeoRDATA_GeoREGIONs, "R/UI_and_SERVER/CLIMINET-SHINYApps/Données/Geo/REGIONs/GeoREGIONs/CCMN_GeoRDATA_GeoREGIONs_Reduced.RDS")

##### MoreLess #####
GeoRDATA_MoreLess <- GeoDATA %>% 
  #DATA MANIPULATIONs
  mutate(
    #Replace Values in MoreLess
    MoreLess = case_when(is.na(MoreLess) ~ "Other Countries/Territories", TRUE ~ MoreLess)) %>%
  #DATA MANIPULATIONs
  group_by(MoreLess) %>%
  #DATA MANIPULATIONs
  summarize(
    #New Variable => AREA
    AREA = sum(AREA, na.rm = TRUE),
    #New Variable => GEOMETRY+
    geometry = st_combine(geometry)) %>%
  #Rename Column(s)
  rename(NAME = 1) %>%
  #DATA MANIPULATIONs
  mutate(
    #Replace Values in NAME
    NAME = case_when(
      NAME == "More Developed" ~ "More Developed Countries",
      NAME == "Less Developed" ~ "Less Developed Countries",
      NAME == "Other Countries/Territories" ~ "Other Countries/Territories"),
    #New Variable => CODE
    CODE = case_when(
      NAME == "More Developed Countries" ~ 901, 
      NAME == "Less Developed Countries" ~ 902),
    #New Variable => LabelR
    LabelR = paste0("<b>Development Level: </b>", NAME),
    #New Variable => CLRCode
    CLRCode = c("#FFCC00", "#339900", "#999999"),
    #New Variable => SOURCE
    SOURCE = case_when(
      NAME == "Other Countries/Territories" ~ "Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques",
      TRUE ~ "World Health OrGanization (WHO) - Countries and Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques and Global Administrative Areas (GADM)")) %>% 
  select(1, 4:6, 2, 7, 3) #Columns ORDER

CCMN_GeoRDATA_MoreLess <- readRDS("R/UI_and_SERVER/CLIMINET/Données/Geo/REGIONs/DevLevels/MoreLess/CCMN_GeoRDATA_MoreLess.RDS")
# GeoRDATA_MoreLess <- GeoRDATA_MoreLess %>% arrange(NAME)
GeoRDATA_MoreLess <- GeoRDATA_MoreLess[order(CCMN_GeoRDATA_MoreLess$NAME), ]
CCMN_GeoRDATA_MoreLess <- CCMN_GeoRDATA_MoreLess %>% arrange(NAME)
CCMN_GeoRDATA_MoreLess$geometry <- GeoRDATA_MoreLess$geometry
saveRDS(CCMN_GeoRDATA_MoreLess, "R/UI_and_SERVER/CLIMINET-SHINYApps/Données/Geo/REGIONs/DevLevels/MoreLess/CCMN_GeoRDATA_MoreLess_Reduced.RDS")

##### MoreLessLeast #####
GeoRDATA_MoreLessLeast <- GeoDATA %>% 
  #DATA MANIPULATIONs
  mutate(
    #Replace Values in MoreLessLeast
    MoreLessLeast = case_when(is.na(MoreLessLeast) ~ "Other Countries/Territories", TRUE ~ MoreLessLeast)) %>%
  #DATA MANIPULATIONs
  group_by(MoreLessLeast) %>%
  #DATA MANIPULATIONs
  summarize(
    #New Variable => AREA
    AREA = sum(AREA, na.rm = TRUE),
    #New Variable => GEOMETRY+
    geometry = st_combine(geometry)) %>%
  #Rename Column(s)
  rename(NAME = 1) %>%
  #DATA MANIPULATIONs
  mutate(
    #Replace Values in NAME
    NAME = case_when(
      NAME == "More Developed" ~ "More Developed Countries",
      NAME == "Less Developed*" ~ "Less Developed Countries*",
      NAME == "Least Developed" ~ "Least Developed Countries",
      NAME == "Other Countries/Territories" ~ "Other Countries/Territories"),
    #New Variable => CODE
    CODE = case_when(
      NAME == "More Developed Countries" ~ 901, 
      NAME == "Less Developed Countries*" ~ 934, 
      NAME == "Least Developed Countries" ~ 941),
    #New Variable => LabelR
    LabelR = paste0("<b>Development Level: </b>", NAME),
    #New Variable => CLRCode
    CLRCode = c("#FF9900", "#FFCC00", "#339900", "#999999"),
    #New Variable => SOURCE
    SOURCE = case_when(
      NAME == "Other Countries/Territories" ~ "Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques",
      NAME == "Least Developed Countries" ~ "World Health OrGanization (WHO) - Countries and Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques",
      TRUE ~ "World Health OrGanization (WHO) - Countries and Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques and Global Administrative Areas (GADM)")) %>% 
  select(1, 4:6, 2, 7, 3) #Columns ORDER

CCMN_GeoRDATA_MoreLessLeast <- readRDS("R/UI_and_SERVER/CLIMINET/Données/Geo/REGIONs/DevLevels/MoreLessLeast/CCMN_GeoRDATA_MoreLessLeast.RDS")
# GeoRDATA_MoreLessLeast <- GeoRDATA_MoreLessLeast %>% arrange(NAME)
GeoRDATA_MoreLessLeast <- GeoRDATA_MoreLessLeast[order(CCMN_GeoRDATA_MoreLessLeast$NAME), ]
CCMN_GeoRDATA_MoreLessLeast <- CCMN_GeoRDATA_MoreLessLeast %>% arrange(NAME)
CCMN_GeoRDATA_MoreLessLeast$geometry <- GeoRDATA_MoreLessLeast$geometry
saveRDS(CCMN_GeoRDATA_MoreLessLeast, "R/UI_and_SERVER/CLIMINET-SHINYApps/Données/Geo/REGIONs/DevLevels/MoreLessLeast/CCMN_GeoRDATA_MoreLessLeast_Reduced.RDS")

##### LDC_LLDC_SIDS #####
GeoRDATA_LDC_LLDC_SIDS <- GeoDATA %>% 
  #DATA MANIPULATIONs
  mutate(
    #Replace Values in DevLevel
    DevLevel = case_when(is.na(DevLevel) ~ "Other Countries/Territories", TRUE ~ DevLevel)) %>%
  #DATA MANIPULATIONs
  group_by(DevLevel) %>%
  #DATA MANIPULATIONs
  summarize(
    #New Variable => AREA
    AREA = sum(AREA, na.rm = TRUE),
    #New Variable => GEOMETRY+
    geometry = st_combine(geometry)) %>%
  #Rename Column(s)
  rename(NAME = 1) %>%
  #DATA MANIPULATIONs
  mutate(
    #Replace Values in NAME
    NAME = case_when(
      NAME == "LDC*" ~ "Least Developed Countries* (LDC*)",
      NAME == "LLDC*" ~ "Land-Locked Developing Countries* (LLDC*)",
      NAME == "SIDS*" ~ "Small Island Developing States* (SIDS*)",
      NAME == "LDC | LLDC" ~ "LDC | LLDC",
      NAME == "LDC | SIDS" ~ "LDC | SIDS",
      NAME == "Other Countries/Territories" ~ "Other Countries/Territories"),
    #New Variable => CODE
    CODE = case_when(
      NAME == "Least Developed Countries* (LDC*)" ~ 942, 
      NAME == "Land-Locked Developing Countries* (LLDC*)" ~ 1638, 
      NAME == "Small Island Developing States* (SIDS*)" ~ 1639,
      NAME == "LDC | LLDC" ~ 1640, 
      NAME == "LDC | SIDS" ~ 1641),
    #New Variable => LabelR
    LabelR = paste0("<b>Development Level: </b>", NAME),
    #New Variable => CLRCode
    CLRCode = c("#FF9900", "#72C02C", "#F1C40F", "#E74C3C", "#999999", "#5B92E5"),
    #New Variable => SOURCE
    SOURCE = case_when(
      NAME %in% c(
        "Least Developed Countries* (LDC*)", "Land-Locked Developing Countries* (LLDC*)", 
        "LDC | LLDC", "LDC | SIDS") ~ "World Health OrGanization (WHO) - Countries and Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques",
      TRUE ~ "World Health OrGanization (WHO) - Countries and Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques and Global Administrative Areas (GADM)")) %>% 
  select(1, 4:6, 2, 7, 3) #Columns ORDER

CCMN_GeoRDATA_LDC_LLDC_SIDS <- readRDS("R/UI_and_SERVER/CLIMINET/Données/Geo/REGIONs/DevLevels/LDC_LLDC_SIDS/CCMN_GeoRDATA_LDC_LLDC_SIDS.RDS")
# GeoRDATA_LDC_LLDC_SIDS <- GeoRDATA_LDC_LLDC_SIDS %>% arrange(NAME)*
GeoRDATA_LDC_LLDC_SIDS <- GeoRDATA_LDC_LLDC_SIDS[order(CCMN_GeoRDATA_LDC_LLDC_SIDS$NAME), ]
CCMN_GeoRDATA_LDC_LLDC_SIDS <- CCMN_GeoRDATA_LDC_LLDC_SIDS %>% arrange(NAME)
CCMN_GeoRDATA_LDC_LLDC_SIDS$geometry <- GeoRDATA_LDC_LLDC_SIDS$geometry
saveRDS(CCMN_GeoRDATA_LDC_LLDC_SIDS, "R/UI_and_SERVER/CLIMINET-SHINYApps/Données/Geo/REGIONs/DevLevels/LDC_LLDC_SIDS/CCMN_GeoRDATA_LDC_LLDC_SIDS_Reduced.RDS")

##### IncomeLevels #####
GeoRDATA_IncomeLevels <- GeoDATA %>% 
  #DATA MANIPULATIONs
  mutate(
    #Replace Values in IncomeLevel
    IncomeLevel = case_when(is.na(IncomeLevel) ~ "Other Countries/Territories", TRUE ~ IncomeLevel)) %>%
  #DATA MANIPULATIONs
  group_by(IncomeLevel) %>%
  #DATA MANIPULATIONs
  summarize(
    #New Variable => AREA
    AREA = sum(AREA, na.rm = TRUE),
    #New Variable => GEOMETRY+
    geometry = st_combine(geometry)) %>%
  #Rename Column(s)
  rename(NAME = 1) %>%
  #DATA MANIPULATIONs
  mutate(
    #Replace Values in NAME
    NAME = case_when(
      NAME == "HIGH" ~ "High-Income Countries",
      NAME == "UPPER-MIDDLE" ~ "Upper-Middle-Income Countries",
      NAME == "LOWER-MIDDLE" ~ "Lower-Middle-Income Countries",
      NAME == "LOW" ~ "Low-Income Countries",
      NAME == "Other Countries/Territories" ~ "Other Countries/Territories"),
    #New Variable => CODE
    CODE = case_when(
      NAME == "High-Income Countries" ~ 1503, 
      NAME == "Upper-Middle-Income Countries" ~ 1502, 
      NAME == "Lower-Middle-Income Countries" ~ 1501,
      NAME == "Low-Income Countries" ~ 1500),
    #New Variable => LabelR
    LabelR = paste0("<b>Income Level (2020): </b>", NAME),
    #New Variable => CLRCode
    CLRCode = c("#0F8554", "#7B3294", "#A885B7", "#999999", "#8DCE92"),
    #New Variable => SOURCE
    SOURCE = case_when(
      TRUE ~ "World Health OrGanization (WHO) - Countries and Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques and Global Administrative Areas (GADM)")) %>% 
  select(1, 4:6, 2, 7, 3) #Columns ORDER

CCMN_GeoRDATA_IncomeLevels <- readRDS("R/UI_and_SERVER/CLIMINET/Données/Geo/REGIONs/IncomeLevels/CCMN_GeoRDATA_IncomeLevels.RDS")
# GeoRDATA_IncomeLevels <- GeoRDATA_IncomeLevels %>% arrange(NAME)
GeoRDATA_IncomeLevels <- GeoRDATA_IncomeLevels[order(CCMN_GeoRDATA_IncomeLevels$NAME), ]
CCMN_GeoRDATA_IncomeLevels <- CCMN_GeoRDATA_IncomeLevels %>% arrange(NAME)
CCMN_GeoRDATA_IncomeLevels$geometry <- GeoRDATA_IncomeLevels$geometry
saveRDS(CCMN_GeoRDATA_IncomeLevels, "R/UI_and_SERVER/CLIMINET-SHINYApps/Données/Geo/REGIONs/IncomeLevels/CCMN_GeoRDATA_IncomeLevels_Reduced.RDS")
