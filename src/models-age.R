# Load coverage
load(file = "output/coverage-covariates.Rdata")

input_list <- list(unvax = log((1 - vax_cov_covariate_a)),
                   pop_offset = population(covidCH_sts_a) / rowSums(population(covidCH_sts_a)),
                   linear_time = seq_len(dim(vax_cov_covariate_a)[1]) -
                     median(seq_len(dim(vax_cov_covariate_a)[1])))
fam <- "NegBin1"

qAGEGROUPS <- paste0("`", AGEGROUPS, "`")
# Age group indicators
AGE <- sapply(AGEGROUPS, function (g) {
  index <- which(stratum(covidCH_sts_a) == g)
  res <- col(covidCH_sts_a)
  res[] <- res %in% index
  res
}, simplify = FALSE, USE.NAMES = TRUE)

# Model specifications
ee_form_fixed_without  <- addSeason2formula(reformulate(
  c(qAGEGROUPS, "linear_time"),
  intercept = FALSE))
ee_form_fixed_with <- update(old = ee_form_fixed_without,
                             new = ~ . + unvax)

# NB in control arguments has to be input_list$pop_offset
# as else error occurs:
# Error: object 'pop_offset' not found
ee_control_constant <- list("neither" = list(
  end = list(f = ee_form_fixed_without, offset = input_list$pop_offset),
  ne = list(f = ee_form_fixed_without,
            weights = Cgrouped),
  data = c(input_list, AGE),
  family = fam, verbose = FALSE),
  "endemic" = list(
    end = list(f = ee_form_fixed_with, offset = input_list$pop_offset),
    ne = list(f = ee_form_fixed_without,
              weights = Cgrouped),
    data = c(input_list, AGE),
    family = fam, verbose = FALSE),
  "epidemic" = list(
    end = list(f = ee_form_fixed_without, offset = input_list$pop_offset),
    ne = list(f = ee_form_fixed_with,
              weights = Cgrouped),
    data = c(input_list, AGE),
    family = fam, verbose = FALSE),
  "both" = list(
    end = list(f = ee_form_fixed_with, offset = input_list$pop_offset),
    ne = list(f = ee_form_fixed_with,
              weights = Cgrouped),
    data = c(input_list, AGE),
    family = fam, verbose = FALSE))

ee_control_varying <- list("neither" = list(
  end = list(f = ee_form_fixed_without, offset = input_list$pop_offset),
  ne = list(f = ee_form_fixed_without,
            weights = contact_mats),
  data = c(input_list, AGE),
  family = fam, verbose = FALSE),
  "endemic" = list(
    end = list(f = ee_form_fixed_with, offset = input_list$pop_offset),
    ne = list(f = ee_form_fixed_without,
              weights = contact_mats),
    data = c(input_list, AGE),
    family = fam, verbose = FALSE),
  "epidemic" = list(
    end = list(f = ee_form_fixed_without, offset = input_list$pop_offset),
    ne = list(f = ee_form_fixed_with,
              weights = contact_mats),
    data = c(input_list, AGE),
    family = fam, verbose = FALSE),
  "both" = list(
    end = list(f = ee_form_fixed_with, offset = input_list$pop_offset),
    ne = list(f = ee_form_fixed_with,
              weights = contact_mats),
    data = c(input_list, AGE),
    family = fam, verbose = FALSE))

# Fit the models
ee_model_constant <- list(
  "neither" = hhh4(stsObj = covidCH_sts_a,
                   control = ee_control_constant$neither),
  "endemic" = hhh4(stsObj = covidCH_sts_a,
                   control = ee_control_constant$endemic),
  "epidemic" = hhh4(stsObj = covidCH_sts_a,
                    control = ee_control_constant$epidemic),
  "both" = hhh4(stsObj = covidCH_sts_a,
                control = ee_control_constant$both))

ee_model_varying <- list(
  "neither" = hhh4(stsObj = covidCH_sts_a,
                   control = ee_control_varying$neither),
  "endemic" = hhh4(stsObj = covidCH_sts_a,
                   control = ee_control_varying$endemic),
  "epidemic" = hhh4(stsObj = covidCH_sts_a,
                    control = ee_control_varying$epidemic),
  "both" = hhh4(stsObj = covidCH_sts_a,
                control = ee_control_varying$both))

if(TRUE){
# Convergence issue with age group in endemic component for
# models with constant transmission weights
ee_model_constant$epidemic <- update(
  ee_model_constant$epidemic,
  end = list(f = update(formula(ee_model_constant$epidemic)$end, ~ .
                        - `10 - 19` - `20 - 29` - `30 - 39` - `40 - 49`
                        - `50 - 59` - `60 - 69` - `70 - 79` - `80+`)))
ee_model_constant$both <- update(
  ee_model_constant$both,
  end = list(f = update(formula(ee_model_constant$both)$end, ~ .
                        - `10 - 19` - `20 - 29` - `30 - 39` - `40 - 49`
                        - `50 - 59` - `60 - 69` - `70 - 79` - `80+`)))
ee_model_varying$neither <- update(
  ee_model_varying$neither,
  end = list(f = update(formula(ee_model_varying$neither)$end, ~ .
                        - `10 - 19` - `20 - 29` - `30 - 39` - `40 - 49`
                        - `50 - 59` - `60 - 69` - `70 - 79` - `80+`)))
ee_model_varying$endemic <- update(
  ee_model_varying$endemic,
  end = list(f = update(formula(ee_model_varying$endemic)$end, ~ .
                        - `10 - 19` - `20 - 29` - `30 - 39` - `40 - 49`
                        - `50 - 59` - `60 - 69` - `70 - 79` - `80+`)))
ee_model_varying$epidemic <- update(
  ee_model_varying$epidemic,
  end = list(f = update(formula(ee_model_varying$epidemic)$end, ~ .
                        - `10 - 19` - `20 - 29` - `30 - 39` - `40 - 49`
                        - `50 - 59` - `60 - 69` - `70 - 79` - `80+`)))
ee_model_varying$both <- update(
  ee_model_varying$both,
  end = list(f = update(formula(ee_model_varying$both)$end, ~ .
                        - `10 - 19` - `20 - 29` - `30 - 39` - `40 - 49`
                        - `50 - 59` - `60 - 69` - `70 - 79` - `80+`)))
}

save(list = ls()[grepl("ee_model", ls()) | grepl("input_list", ls())],
     file = "output/models-age.Rdata")
