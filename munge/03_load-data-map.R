# MAP
# Source: https://github.com/ebp-group/Switzerland_Tilemap
# Tilemap CC-BY www.ebp.ch
# As SpatialPolygonsDataFrame
mapCH <- readShapePoly("data/Switzerland_Tiles_CH1903LV03.shp")
# Remove the lakes
mapCH <- mapCH[!c(mapCH$Name %in% c("Bodensee", "Lac Léman")), ]
names(mapCH)[names(mapCH) == "Name"] <- "geoRegion"
mapCH$NAME <- as.character(mapCH$geoRegion)
row.names(mapCH) <- unique(mapCH$NAME)

rm(shp)
