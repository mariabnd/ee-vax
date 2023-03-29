# Load the data for processing
load(file = "output/vaccination-coverage.Rdata")

for_fill <- vax_cov_data[, c("dateISO", "altersklasse_covid19", "geoRegion", "groups")]

vax_cov_data_alt_avg <- merge(for_fill[, - which(names(for_fill) == "geoRegion")],
                              aggregate(vax_cov ~ dateISO + altersklasse_covid19,
                                        vax_cov_data, FUN = mean))

vax_cov_data_alt_max <- merge(for_fill[, - which(names(for_fill) == "altersklasse_covid19")],
                              aggregate(vax_cov ~ dateISO + geoRegion, vax_cov_data, FUN = max))
# Remove higher level grouping
vax_cov_data_alt_avg$altersklasse_covid19 <- NULL
vax_cov_data_alt_max$geoRegion <- NULL

vax_cov_data_alt_max$dateISO <- factor(
  vax_cov_data_alt_max$dateISO,
  levels = sort(unique(vax_cov_data_alt_max$dateISO), decreasing = TRUE))

vax_cov_data_alt_avg$dateISO <- factor(
  vax_cov_data_alt_avg$dateISO,
  levels = sort(unique(vax_cov_data_alt_avg$dateISO), decreasing = TRUE))

vax_cov_covariate_alt_avg <- xtabs(data = vax_cov_data_alt_avg,
                                   formula = vax_cov ~ dateISO + groups)

vax_cov_covariate_alt_max <- xtabs(data = vax_cov_data_alt_max,
                                   formula = vax_cov ~ dateISO + groups)

save(vax_cov_covariate_alt_avg,
     vax_cov_covariate_alt_max,
     file = "output/coverage-scen.Rdata")

rm(for_fill)
