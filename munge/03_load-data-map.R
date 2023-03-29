# MAP
# Source: https://github.com/ebp-group/Switzerland_Tilemap
# Tilemap CC-BY www.ebp.ch
shp <- read_sf("data/Switzerland_Tiles_CH1903LV03.shp")
# Remove the lakes
shp <- shp[!c(shp$Name %in% c("Bodensee", "Lac Léman")), ]
names(shp)[names(shp) == "Name"] <- "geoRegion"

# As SpatialPolygonsDataFrame
mapCH <- readShapePoly("data/Switzerland_Tiles_CH1903LV03.shp")
# Remove same parts as before
mapCH <- mapCH[!c(mapCH$Name %in% c("Bodensee", "Lac Léman")), ]
names(mapCH)[names(mapCH) == "Name"] <- "NAME"
mapCH$NAME <- as.character(mapCH$NAME)
row.names(mapCH) <- unique(mapCH$NAME)

rm(shp)