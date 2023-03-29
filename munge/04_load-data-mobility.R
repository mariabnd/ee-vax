date_range_full <- fixISOleadingzero(date_range_full)

# MOBILITY
data_mobi <- rbind(read.csv("./data/movement-range-data-2020-03-01--2020-12-31.csv"),
                   read.csv("./data/movement-range-2022-05-22.csv"))
# Correct names
data_mobi[data_mobi$polygon_name == "Lucerne", ]$polygon_name <- "Luzern"
data_mobi[data_mobi$polygon_name == "Sankt Gallen", ]$polygon_name <- "St. Gallen"
data_mobi[data_mobi$polygon_name == "Fribourg", ]$polygon_name <- "Freiburg"
data_mobi[data_mobi$polygon_name == "Graub-nden", ]$polygon_name <- "Graubünden"
data_mobi[data_mobi$polygon_name == "Neuch-tel", ]$polygon_name <- "Neuchâtel"             
data_mobi[data_mobi$polygon_name == "Z-rich", ]$polygon_name <- "Zürich"
data_mobi[data_mobi$polygon_name == "Gen-ve", ]$polygon_name <- "Genève"

# Add the canton labels
data_mobi <- merge(data_mobi, cantons_lookup,
                   by.x = "polygon_name", by.y = "geoRegion_label")
# ISO week
data_mobi$dateISO <-
  paste(isoWeekYear(as.Date(data_mobi$ds))$ISOYear,
        ifelse(nchar(as.numeric(isoWeekYear(as.Date(data_mobi$ds))$ISOWeek) + 1) == 1,
               # if one digit add leading zero
               paste0("0", as.numeric(isoWeekYear(as.Date(data_mobi$ds))$ISOWeek)),
               as.numeric(isoWeekYear(as.Date(data_mobi$ds))$ISOWeek)), sep = "-")
data_mobi$dateISO <- fixISOleadingzero(data_mobi$dateISO)
# Restrict to study period
data_mobi <- data_mobi[data_mobi$dateISO %in% date_range_full, ]
# Average over ISO week to obtain the weekly amount
data_mobi <- aggregate(all_day_bing_tiles_visited_relative_change ~
                         dateISO + geoRegion, data = data_mobi, FUN = mean)

# Copy Appenzeller Ausserhoden values to Appenzeller Innerhoden
data_mobi_non <- data_mobi[data_mobi$geoRegion == "AR", ]
data_mobi_non$geoRegion <- "AI"
data_mobi <- rbind(data_mobi_non, data_mobi)
rm(data_mobi_non)

# Ensure correct order
data_mobi <- data_mobi[order(data_mobi$geoRegion, data_mobi$dateISO), ]

# carry forward most recently observed value
data_mobi <- zoo::na.locf(data_mobi)

# Average
data_mobi$avg_relative_change <- aggregate(all_day_bing_tiles_visited_relative_change ~ dateISO,
          data = data_mobi, FUN = mean)$all_day_bing_tiles_visited_relative_change

# Normalise
data_mobi$movement_change <-
  (data_mobi$all_day_bing_tiles_visited_relative_change -
     min(data_mobi$all_day_bing_tiles_visited_relative_change)) / (
       max(data_mobi$all_day_bing_tiles_visited_relative_change) -
         min(data_mobi$all_day_bing_tiles_visited_relative_change))

data_mobi$avg_movement_change <- aggregate(movement_change ~ dateISO,
                                             data = data_mobi, FUN = mean)$movement_change

# Lower bound
data_mobi$movement_change <- ifelse(data_mobi$movement_change == 0,
                                    data_mobi$movement_change + 0.001,
                                    data_mobi$movement_change)

# Array with adjacencies
movement_mats <- array(data = NA,
                       dim = c(dim(map_nbOrder),
                               length(date_range_full)),
                       dimnames = list(unique(data_mobi$geoRegion),
                                       unique(data_mobi$geoRegion),
                                       date_range_full))

for (i in seq_len(dim(movement_mats)[3])){
  tmp <- do.call(rbind,
                 lapply(unique(data_mobi$geoRegion), function(x) {
                   data_mobi[data_mobi$dateISO == dimnames(movement_mats)[[3]][i] &
                               data_mobi$geoRegion == x, ]$movement_change *
                     map_nbOrder[rownames(map_nbOrder) == x, ]
                 }))
  tmp <- as.matrix(tmp)
  movement_mats[, , i] <- tmp
}

save(movement_mats,
     file = "output/matrices.Rdata")
