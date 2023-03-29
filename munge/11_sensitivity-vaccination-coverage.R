# Load the data for processing
load(file = "output/data-for-calculation-of-coverage.Rdata")
# Delete the raw input
file.remove(file = "output/data-for-calculation-of-coverage.Rdata")

# Apply waning
data_vax_2nd_sens1 <- apply_waning(data_set = data_vax_2nd, FUN = waning_sens1)
data_vax_3rd_sens1 <- apply_waning(data_set = data_vax_3rd, FUN = waning_sens1)

data_vax_2nd_sens1_r <- apply_waning(data_set = data_vax_2nd_r, FUN = waning_sens1,
                                     name = c("csum_waned_r", "cov_1_waned_r"))
data_vax_3rd_sens1_r <- apply_waning(data_set = data_vax_3rd_r, FUN = waning_sens1,
                                     name = c("csum_waned_r", "cov_1_waned_r"))

data_vax_2nd_sens1_a <- apply_waning(data_set = data_vax_2nd_a, FUN = waning_sens1,
                                     name = c("csum_waned_a", "cov_1_waned_a"))
data_vax_3rd_sens1_a <- apply_waning(data_set = data_vax_3rd_a, FUN = waning_sens1,
                                     name = c("csum_waned_a", "cov_1_waned_a"))

# Merge then sum to ensure no mismatch in group membership
# Unique names so can be summed
data_vax_2nd_sens1$vax_cov_2 <- data_vax_2nd_sens1$cov_1_waned
data_vax_3rd_sens1$vax_cov_3 <- data_vax_3rd_sens1$cov_1_waned

vax_cov_data_sens1 <- merge(
  data_vax_2nd_sens1[, which(names(data_vax_2nd_sens1) %in%
                               c("dateISO", "geoRegion", "altersklasse_covid19",
                                 "groups", "vax_cov_2"))],
  data_vax_3rd_sens1[, which(names(data_vax_3rd_sens1) %in%
                               c("dateISO", "geoRegion", "altersklasse_covid19",
                                 "groups", "vax_cov_3"))],
  all = TRUE)
vax_cov_data_sens1$vax_cov <- vax_cov_data_sens1$vax_cov_2 +
  vax_cov_data_sens1$vax_cov_3

vax_cov_data_sens1$vax_cov <- ifelse(vax_cov_data_sens1$vax_cov > 1, 1,
                                     vax_cov_data_sens1$vax_cov)
vax_cov_data_sens1$vax_cov_2 <- NULL
vax_cov_data_sens1$vax_cov_3 <- NULL

## Region
data_vax_2nd_sens1_r$vax_cov_2 <- data_vax_2nd_sens1_r$cov_1_waned_r
data_vax_3rd_sens1_r$vax_cov_3 <- data_vax_3rd_sens1_r$cov_1_waned_r

vax_cov_data_sens1_r <- merge(
  data_vax_2nd_sens1_r[, which(names(data_vax_2nd_sens1_r) %in% c("dateISO", "geoRegion", "vax_cov_2"))],
  data_vax_3rd_sens1_r[, which(names(data_vax_3rd_sens1_r) %in% c("dateISO", "geoRegion", "vax_cov_3"))],
  all = TRUE)
vax_cov_data_sens1_r$vax_cov_r <- vax_cov_data_sens1_r$vax_cov_2 +
  vax_cov_data_sens1_r$vax_cov_3
vax_cov_data_sens1_r$vax_cov_r <- ifelse(vax_cov_data_sens1_r$vax_cov_r > 1, 1,
                                         vax_cov_data_sens1_r$vax_cov_r)
vax_cov_data_sens1_r$vax_cov_2 <- vax_cov_data_sens1_r$vax_cov_3 <- NULL

## Age
data_vax_2nd_sens1_a$vax_cov_2 <- data_vax_2nd_sens1_a$cov_1_waned_a
data_vax_3rd_sens1_a$vax_cov_3 <- data_vax_3rd_sens1_a$cov_1_waned_a

vax_cov_data_sens1_a <- merge(
  data_vax_2nd_sens1_a[, which(names(data_vax_2nd_sens1_a) %in% c("dateISO", "altersklasse_covid19", "vax_cov_2"))],
  data_vax_3rd_sens1_a[, which(names(data_vax_3rd_sens1_a) %in% c("dateISO", "altersklasse_covid19", "vax_cov_3"))],
  all = TRUE)
vax_cov_data_sens1_a$vax_cov_a <- vax_cov_data_sens1_a$vax_cov_2 + vax_cov_data_sens1_a$vax_cov_3
vax_cov_data_sens1_a$vax_cov_a <- ifelse(vax_cov_data_sens1_a$vax_cov_a > 1, 1,
                                         vax_cov_data_sens1_a$vax_cov_a)
vax_cov_data_sens1_a$vax_cov_2 <- vax_cov_data_sens1_a$vax_cov_3 <- NULL

# Create covariates
vax_cov_covariate_sens1 <- xtabs(data = vax_cov_data_sens1, formula = vax_cov ~ dateISO + groups)
vax_cov_covariate_sens1_r <- xtabs(data = vax_cov_data_sens1_r,
                                   formula = vax_cov_r ~ dateISO + geoRegion)
vax_cov_covariate_sens1_a <- xtabs(data = vax_cov_data_sens1_a,
                                   formula = vax_cov_a ~ dateISO + altersklasse_covid19)

data_vax_2nd_sens2 <- apply_waning(data_set = data_vax_2nd, FUN = waning_sens2)
data_vax_3rd_sens2 <- apply_waning(data_set = data_vax_3rd, FUN = waning_sens2)

data_vax_2nd_sens2_r <- apply_waning(data_set = data_vax_2nd_r, FUN = waning_sens2,
                                     name = c("csum_waned_r", "cov_1_waned_r"))
data_vax_3rd_sens2_r <- apply_waning(data_set = data_vax_3rd_r, FUN = waning_sens2,
                                     name = c("csum_waned_r", "cov_1_waned_r"))

data_vax_2nd_sens2_a <- apply_waning(data_set = data_vax_2nd_a, FUN = waning_sens2,
                                     name = c("csum_waned_a", "cov_1_waned_a"))
data_vax_3rd_sens2_a <- apply_waning(data_set = data_vax_3rd_a, FUN = waning_sens2,
                                     name = c("csum_waned_a", "cov_1_waned_a"))

# Merge then sum to ensure no mismatch in group membership
# Unique names so can be summed
data_vax_2nd_sens2$vax_cov_2 <- data_vax_2nd_sens2$cov_1_waned
data_vax_3rd_sens2$vax_cov_3 <- data_vax_3rd_sens2$cov_1_waned

vax_cov_data_sens2 <- merge(
  data_vax_2nd_sens2[, which(names(data_vax_2nd_sens2) %in%
                               c("dateISO", "geoRegion", "altersklasse_covid19",
                                 "groups", "vax_cov_2"))],
  data_vax_3rd_sens2[, which(names(data_vax_3rd_sens2) %in%
                               c("dateISO", "geoRegion", "altersklasse_covid19",
                                 "groups", "vax_cov_3"))],
  all = TRUE)
vax_cov_data_sens2$vax_cov <- vax_cov_data_sens2$vax_cov_2 +
  vax_cov_data_sens2$vax_cov_3

vax_cov_data_sens2$vax_cov <- ifelse(vax_cov_data_sens2$vax_cov > 1, 1,
                                     vax_cov_data_sens2$vax_cov)
vax_cov_data_sens2$vax_cov_2 <- NULL
vax_cov_data_sens2$vax_cov_3 <- NULL

## Region
data_vax_2nd_sens2_r$vax_cov_2 <- data_vax_2nd_sens2_r$cov_1_waned_r
data_vax_3rd_sens2_r$vax_cov_3 <- data_vax_3rd_sens2_r$cov_1_waned_r

vax_cov_data_sens2_r <- merge(
  data_vax_2nd_sens2_r[, which(names(data_vax_2nd_sens2_r) %in% c("dateISO", "geoRegion", "vax_cov_2"))],
  data_vax_3rd_sens2_r[, which(names(data_vax_3rd_sens2_r) %in% c("dateISO", "geoRegion", "vax_cov_3"))],
  all = TRUE)
vax_cov_data_sens2_r$vax_cov_r <- vax_cov_data_sens2_r$vax_cov_2 +
  vax_cov_data_sens2_r$vax_cov_3
vax_cov_data_sens2_r$vax_cov_r <- ifelse(vax_cov_data_sens2_r$vax_cov_r > 1, 1,
                                         vax_cov_data_sens2_r$vax_cov_r)
vax_cov_data_sens2_r$vax_cov_2 <- vax_cov_data_sens2_r$vax_cov_3 <- NULL

## Age
data_vax_2nd_sens2_a$vax_cov_2 <- data_vax_2nd_sens2_a$cov_1_waned_a
data_vax_3rd_sens2_a$vax_cov_3 <- data_vax_3rd_sens2_a$cov_1_waned_a

vax_cov_data_sens2_a <- merge(
  data_vax_2nd_sens2_a[, which(names(data_vax_2nd_sens2_a) %in% c("dateISO", "altersklasse_covid19", "vax_cov_2"))],
  data_vax_3rd_sens2_a[, which(names(data_vax_3rd_sens2_a) %in% c("dateISO", "altersklasse_covid19", "vax_cov_3"))],
  all = TRUE)
vax_cov_data_sens2_a$vax_cov_a <- vax_cov_data_sens2_a$vax_cov_2 + vax_cov_data_sens2_a$vax_cov_3
vax_cov_data_sens2_a$vax_cov_a <- ifelse(vax_cov_data_sens2_a$vax_cov_a > 1, 1,
                                         vax_cov_data_sens2_a$vax_cov_a)
vax_cov_data_sens2_a$vax_cov_2 <- vax_cov_data_sens2_a$vax_cov_3 <- NULL

# Create covariates
vax_cov_covariate_sens2 <- xtabs(data = vax_cov_data_sens2, formula = vax_cov ~ dateISO + groups)
vax_cov_covariate_sens2_r <- xtabs(data = vax_cov_data_sens2_r,
                                   formula = vax_cov_r ~ dateISO + geoRegion)
vax_cov_covariate_sens2_a <- xtabs(data = vax_cov_data_sens2_a,
                                   formula = vax_cov_a ~ dateISO + altersklasse_covid19)

rm(vax_cov_data_r, data_vax_2nd_r, data_vax_3rd_r)
rm(vax_cov_data_a, data_vax_2nd_a, data_vax_3rd_a)

save(vax_cov_covariate_sens1,
     vax_cov_covariate_sens2,
     vax_cov_covariate_sens1_r,
     vax_cov_covariate_sens2_r,
     vax_cov_covariate_sens1_a,
     vax_cov_covariate_sens2_a,
     file = "output/coverage-sens.Rdata")
