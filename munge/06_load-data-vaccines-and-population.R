data_vax <- read.csv(file = "data/COVID19VaccPersons_AKL10_w_v2.csv",
                     header = TRUE, stringsAsFactors = TRUE)

data_vax$dateISO <-
  paste0(substr(data_vax$date, 1, 4), "-", substr(data_vax$date, 5, 6))

data_vax <- data_vax[data_vax$type != "COVID19PartiallyVaccPersons", ]
data_vax <- data_vax[data_vax$age_group_type == "age_group_AKL10", ]
data_vax <- data_vax[!(data_vax$geoRegion %in% region_exclude), ]
data_vax <- data_vax[!(data_vax$altersklasse_covid19 %in% age_exclude), ]
# Restrict to study period
data_vax <- data_vax[data_vax$dateISO %in% date_range_full, ]
data_vax <- droplevels(data_vax)

# POPULATION
popCH <- data_vax[, names(x = data_vax) %in%
                         c("geoRegion", "altersklasse_covid19", "pop")]
popCH <- unique(x = popCH)

popCH <- dcast(data = popCH, formula = geoRegion ~ altersklasse_covid19,
               fun.aggregate = sum, value.var = "pop")
rownames(popCH) <- popCH$geoRegion
popCH <- popCH[, - (names(popCH) %in% "geoRegion")]
popCH <- as.array(as.matrix(popCH))

# VACCINES
tmp <- data_vax[ , - which(names(data_vax) %in% c("date", "dateISO"))]
# Set numerical variables to missing since we are interested in the
# combinations of different factors in the data set (for the dates
# where they are not present)
tmp[ , names(tmp) %in% c("entries", "pop", "sumTotal", "per100Persons",
                         "per100PersonsTotal", "freq", "prct")] <- NA
tmp <- unique(tmp)

# Pad missing dates if analysing date range earlier than 2021
missing <- date_range_full[!date_range_full %in% data_vax$dateISO]
missing <- merge(data.frame("dateISO" = missing,
                            "date" = gsub(pattern = "-", replacement = "", x = missing)),
                 tmp, by = NULL)

if(dim(missing)[1] > 0){
# Add population -- used in coverage calculation
for(i in rownames(popCH)){
  for(j in colnames(popCH)){
    missing[missing$geoRegion == i & missing$altersklasse_covid19 == j, ]$pop <-
      popCH[rownames(popCH) == i, colnames(popCH) == j]
  }
}

# Memory-efficient alternative to rbind to combine the two data sets
# i.e. complete the padding
data_vax <- as.data.frame(data.table::rbindlist(l = list(data_vax, missing), use.names = TRUE))

# When number of vaccines given is missing set to zero
data_vax[is.na(data_vax$entries), ]$entries <- 0
}

# Create age group and region strata
data_vax$groups <- paste(data_vax$geoRegion, data_vax$altersklasse_covid19,
                         sep = "-")

# Extract relevant doses
# This is done before the calculation of cumulative doses as else
# missing errors seem to occur
data_vax_1st <- data_vax[data_vax$type == "COVID19AtLeastOneDosePersons", ]
data_vax_2nd <- data_vax[data_vax$type == "COVID19FullyVaccPersons", ]
data_vax_3rd <- data_vax[data_vax$type == "COVID19FirstBoosterPersons", ]
## Remove other doses from factor
data_vax_1st <- droplevels(data_vax_1st)
data_vax_2nd <- droplevels(data_vax_2nd)
data_vax_3rd <- droplevels(data_vax_3rd)

# Ensure correct ordering
data_vax_1st <- data_vax_1st[order(data_vax_1st$groups, data_vax_1st$dateISO), ]
data_vax_2nd <- data_vax_2nd[order(data_vax_2nd$groups, data_vax_2nd$dateISO), ]
data_vax_3rd <- data_vax_3rd[order(data_vax_3rd$groups, data_vax_3rd$dateISO), ]

# Calculate cumulative doses given
data_vax_1st <- transform(data_vax_1st,
                          csum = ave(x = entries, dateISO, groups,
                                     FUN = cumsum))
data_vax_2nd <- transform(data_vax_2nd,
                      csum = ave(x = entries, dateISO, groups,
                                 FUN = cumsum))
data_vax_3rd <- transform(data_vax_3rd,
                      csum = ave(x = entries, dateISO, groups,
                                 FUN = cumsum))

# Calculate raw coverage
data_vax_1st$cov_1 <- data_vax_1st$csum / data_vax_1st$pop
data_vax_2nd$cov_1 <- data_vax_2nd$csum / data_vax_2nd$pop
data_vax_3rd$cov_1 <- data_vax_3rd$csum / data_vax_3rd$pop

# REGION AND AGE-SPECIFIC VACCINATION COVERAGE
# since aggregateCountsArray gives values that are too large
data_vax_2nd_r <- transform(
  aggregate(entries ~ dateISO + geoRegion,
            data = data_vax_2nd[, c("dateISO", "geoRegion", "entries")],
            FUN = sum),
  csum_r = ave(entries, geoRegion, FUN = cumsum))
data_vax_3rd_r <- transform(
  aggregate(entries ~ dateISO + geoRegion,
            data = data_vax_3rd[, c("dateISO", "geoRegion", "entries")],
            FUN = sum),
  csum_r = ave(entries, geoRegion, FUN = cumsum))
data_vax_2nd_a <- transform(
  aggregate(entries ~ dateISO + altersklasse_covid19,
            data = data_vax_2nd[, c("dateISO", "altersklasse_covid19",
                                    "entries")],
            FUN = sum),
  csum_a = ave(entries, altersklasse_covid19, FUN = cumsum))
data_vax_3rd_a <- transform(
  aggregate(entries ~ dateISO + altersklasse_covid19,
            data = data_vax_3rd[, c("dateISO", "altersklasse_covid19",
                                    "entries")],
            FUN = sum),
  csum_a = ave(entries, altersklasse_covid19, FUN = cumsum))

# Add the population to the data sets and calculate raw coverage
# Region
data_vax_2nd_r <- merge(data_vax_2nd_r,
                      data.frame(geoRegion = rownames(popCH),
                                 pop_r = rowSums(popCH)))
data_vax_3rd_r <- merge(data_vax_3rd_r,
                      data.frame(geoRegion = rownames(popCH),
                                 pop_r = rowSums(popCH)))
# Ensure correct ordering
data_vax_2nd_r <- data_vax_2nd_r[order(data_vax_2nd_r$geoRegion, data_vax_2nd_r$dateISO), ]
data_vax_3rd_r <- data_vax_3rd_r[order(data_vax_3rd_r$geoRegion, data_vax_3rd_r$dateISO), ]
data_vax_2nd_r$cov_1_r <- data_vax_2nd_r$csum_r / data_vax_2nd_r$pop_r
data_vax_3rd_r$cov_1_r <- data_vax_3rd_r$csum_r / data_vax_3rd_r$pop_r

# Age group
data_vax_2nd_a <- merge(data_vax_2nd_a,
                      data.frame(altersklasse_covid19 = colnames(popCH),
                                 pop_a = colSums(popCH)))
data_vax_3rd_a <- merge(data_vax_3rd_a,
                      data.frame(altersklasse_covid19 = colnames(popCH),
                                 pop_a = colSums(popCH)))
# Ensure correct ordering
data_vax_2nd_a <- data_vax_2nd_a[order(data_vax_2nd_a$altersklasse_covid19,
                                       data_vax_2nd_a$dateISO), ]
data_vax_3rd_a <- data_vax_3rd_a[order(data_vax_3rd_a$altersklasse_covid19,
                                       data_vax_3rd_a$dateISO), ]
data_vax_2nd_a$cov_1_a <- data_vax_2nd_a$csum_a / data_vax_2nd_a$pop_a
data_vax_3rd_a$cov_1_a <- data_vax_3rd_a$csum_a / data_vax_3rd_a$pop_a

save(data_vax_2nd, data_vax_3rd,
     data_vax_2nd_r, data_vax_3rd_r,
     data_vax_2nd_a, data_vax_3rd_a,
     file = "output/data-for-calculation-of-coverage.Rdata")

# Apply waning
data_vax_2nd <- apply_waning(data_set = data_vax_2nd, FUN = waning)
data_vax_3rd <- apply_waning(data_set = data_vax_3rd, FUN = waning)

data_vax_2nd_r <- apply_waning(data_set = data_vax_2nd_r, FUN = waning,
                               name = c("csum_waned_r", "cov_1_waned_r"))
data_vax_3rd_r <- apply_waning(data_set = data_vax_3rd_r, FUN = waning,
                               name = c("csum_waned_r", "cov_1_waned_r"))

data_vax_2nd_a <- apply_waning(data_set = data_vax_2nd_a, FUN = waning,
                               name = c("csum_waned_a", "cov_1_waned_a"))
data_vax_3rd_a <- apply_waning(data_set = data_vax_3rd_a, FUN = waning,
                               name = c("csum_waned_a", "cov_1_waned_a"))

rm(tmp, missing)