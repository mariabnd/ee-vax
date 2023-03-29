# Merge then sum to ensure no mismatch in group membership
# Unique names so can be summed
data_vax_2nd$vax_cov_2 <- data_vax_2nd$cov_1_waned
data_vax_3rd$vax_cov_3 <- data_vax_3rd$cov_1_waned

vax_cov_data <- merge(
  data_vax_2nd[, which(names(data_vax_2nd) %in%
                         c("dateISO", "geoRegion", "altersklasse_covid19",
                                                  "groups", "vax_cov_2"))],
  data_vax_3rd[, which(names(data_vax_3rd) %in%
                         c("dateISO", "geoRegion", "altersklasse_covid19",
                                                  "groups", "vax_cov_3"))],
  all = TRUE)
vax_cov_data$vax_cov <- vax_cov_data$vax_cov_2 + vax_cov_data$vax_cov_3

vax_cov_data$vax_cov <- ifelse(vax_cov_data$vax_cov > 1, 1, vax_cov_data$vax_cov)
vax_cov_data$vax_cov_2 <- NULL
vax_cov_data$vax_cov_3 <- NULL

# Region-specific coverage ---------------------------------------------
# Merge then sum to ensure no mismatch in group membership
# Unique names so can be summed
data_vax_2nd_r$vax_cov_2 <- data_vax_2nd_r$cov_1_waned_r
data_vax_3rd_r$vax_cov_3 <- data_vax_3rd_r$cov_1_waned_r

vax_cov_data_r <- merge(
  data_vax_2nd_r[, which(names(data_vax_2nd_r) %in% c("dateISO", "geoRegion", "vax_cov_2"))],
  data_vax_3rd_r[, which(names(data_vax_3rd_r) %in% c("dateISO", "geoRegion", "vax_cov_3"))],
  all = TRUE)
vax_cov_data_r$vax_cov_r <- vax_cov_data_r$vax_cov_2 + vax_cov_data_r$vax_cov_3
vax_cov_data_r$vax_cov_r <- ifelse(vax_cov_data_r$vax_cov_r > 1, 1,
                                   vax_cov_data_r$vax_cov_r)
vax_cov_data_r$vax_cov_2 <- vax_cov_data_r$vax_cov_3 <- NULL

# Age ----------------------------------------------
# Merge then sum to ensure no mismatch in group membership
# Unique names so can be summed
data_vax_2nd_a$vax_cov_2 <- data_vax_2nd_a$cov_1_waned_a
data_vax_3rd_a$vax_cov_3 <- data_vax_3rd_a$cov_1_waned_a

vax_cov_data_a <- merge(
  data_vax_2nd_a[, which(names(data_vax_2nd_a) %in% c("dateISO", "altersklasse_covid19", "vax_cov_2"))],
  data_vax_3rd_a[, which(names(data_vax_3rd_a) %in% c("dateISO", "altersklasse_covid19", "vax_cov_3"))],
  all = TRUE)
vax_cov_data_a$vax_cov_a <- vax_cov_data_a$vax_cov_2 + vax_cov_data_a$vax_cov_3
vax_cov_data_a$vax_cov_a <- ifelse(vax_cov_data_a$vax_cov_a > 1, 1,
                                   vax_cov_data_a$vax_cov_a)
vax_cov_data_a$vax_cov_2 <- vax_cov_data_a$vax_cov_3 <- NULL

# Create covariates
vax_cov_covariate <- xtabs(data = vax_cov_data, formula = vax_cov ~ dateISO + groups)
vax_cov_covariate_r <- xtabs(data = vax_cov_data_r,
                             formula = vax_cov_r ~ dateISO + geoRegion)
vax_cov_covariate_a <- xtabs(data = vax_cov_data_a,
                             formula = vax_cov_a ~ dateISO + altersklasse_covid19)

# Combine to one data set for other users
vax_cov_data <- merge(vax_cov_data, vax_cov_data_r)
vax_cov_data <- merge(vax_cov_data, vax_cov_data_a)

save(vax_cov_data,
     file = "output/vaccination-coverage.Rdata")

save(vax_cov_covariate,
     vax_cov_covariate_r,
     vax_cov_covariate_a,
     file = "output/coverage-covariates.Rdata")