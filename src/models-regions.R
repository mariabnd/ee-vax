# Regional effects
load(file = "output/coverage-covariates.Rdata")

input_list <- list(unvax = log((1 - vax_cov_covariate_r)),
                   pop_offset = population(covidCH_sts_r) / rowSums(population(covidCH_sts_r)),
                   pop_gravity = log(population(covidCH_sts_r)),
                   linear_time = seq_len(dim(vax_cov_covariate_r)[1]) -
                     median(seq_len(dim(vax_cov_covariate_r)[1])))
fam <- "NegBin1"

# Model specifications
ee_form_fixed_without <- addSeason2formula(reformulate(
  c("fe(1, unitSpecific = FALSE)"),
  intercept = FALSE))
ee_form_fixed_with <- update(old = ee_form_fixed_without,
       new = ~ . + unvax)

# NB in control arguments has to be input_list$pop_offset
# as else error occurs:
# Error: object 'pop_offset' not found
ee_control_constant <- list("neither" = list(
  end = list(f = ee_form_fixed_without, offset = input_list$pop_offset),
  ne = list(f = update(old = ee_form_fixed_without,
                       new = ~ . + pop_gravity), # Add gravity model (log population)
            weights = 1 / map_nbOrder),
  data = input_list,
  family = fam, verbose = FALSE),
  "endemic" = list(
    end = list(f = ee_form_fixed_with, offset = input_list$pop_offset),
    ne = list(f = update(old = ee_form_fixed_without,
                         new = ~ . + pop_gravity), # Add gravity model (log population)
              weights = 1 / map_nbOrder),
    data = input_list,
    family = fam, verbose = FALSE),
  "epidemic" = list(
    end = list(f = ee_form_fixed_without, offset = input_list$pop_offset),
    ne = list(f = update(old = ee_form_fixed_with,
                         new = ~ . + pop_gravity), # Add gravity model (log population)
              weights = 1 / map_nbOrder),
    data = input_list,
    family = fam, verbose = FALSE),
  "both" = list(
    end = list(f = ee_form_fixed_with, offset = input_list$pop_offset),
    ne = list(f = update(old = ee_form_fixed_with,
                         new = ~ . + pop_gravity), # Add gravity model (log population)
              weights = 1 / map_nbOrder),
    data = input_list,
    family = fam, verbose = FALSE))

ee_control_varying <- list("neither" = list(
  end = list(f = ee_form_fixed_without, offset = input_list$pop_offset),
  ne = list(f = update(old = ee_form_fixed_without,
                       new = ~ . + pop_gravity), # Add gravity model (log population)
            weights = movement_mats),
  data = input_list,
  family = fam, verbose = FALSE),
  "endemic" = list(
  end = list(f = ee_form_fixed_with, offset = input_list$pop_offset),
  ne = list(f = update(old = ee_form_fixed_without,
                       new = ~ . + pop_gravity), # Add gravity model (log population)
            weights = movement_mats),
  data = input_list,
  family = fam, verbose = FALSE),
  "epidemic" = list(
  end = list(f = ee_form_fixed_without, offset = input_list$pop_offset),
  ne = list(f = update(old = ee_form_fixed_with,
                       new = ~ . + pop_gravity), # Add gravity model (log population)
            weights = movement_mats),
  data = input_list,
  family = fam, verbose = FALSE),
  "both" = list(
  end = list(f = ee_form_fixed_with, offset = input_list$pop_offset),
  ne = list(f = update(old = ee_form_fixed_with,
                       new = ~ . + pop_gravity), # Add gravity model (log population)
            weights = movement_mats),
  data = input_list,
  family = fam, verbose = FALSE))

# Fit the models
ee_model_constant <- list(
  "neither" = hhh4(stsObj = covidCH_sts_r,
                   control = ee_control_constant$neither),
  "endemic" = hhh4(stsObj = covidCH_sts_r,
                   control = ee_control_constant$endemic),
  "epidemic" = hhh4(stsObj = covidCH_sts_r,
                    control = ee_control_constant$epidemic),
  "both" = hhh4(stsObj = covidCH_sts_r,
                control = ee_control_constant$both))

ee_model_varying <- list(
  "neither" = hhh4(stsObj = covidCH_sts_r,
                   control = ee_control_varying$neither),
  "endemic" = hhh4(stsObj = covidCH_sts_r,
                   control = ee_control_varying$endemic),
  "epidemic" = hhh4(stsObj = covidCH_sts_r,
                    control = ee_control_varying$epidemic),
  "both" = hhh4(stsObj = covidCH_sts_r,
                control = ee_control_varying$both))

ee_model_constant$neither <- update(
  ee_model_constant$neither,
  end = list(f = update(formula(ee_model_constant$neither)$end, ~ . 
                        - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)),
  ne = list(f = update(formula(ee_model_constant$neither)$ne, ~ .
                       - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)))
ee_model_constant$endemic <- update(
  ee_model_constant$endemic,
  end = list(f = update(formula(ee_model_constant$endemic)$end, ~ . 
                        - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)),
  ne = list(f = update(formula(ee_model_constant$endemic)$ne, ~ .
                       - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)))
ee_model_constant$epidemic <- update(
  ee_model_constant$epidemic,
  end = list(f = update(formula(ee_model_constant$epidemic)$end, ~ .
                        - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)),
  ne = list(f = update(formula(ee_model_constant$epidemic)$ne, ~ .
                       - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)))
ee_model_constant$both <- update(
  ee_model_constant$both,
  end = list(f = update(formula(ee_model_constant$both)$end, ~ .
                        - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)),
  ne = list(f = update(formula(ee_model_constant$both)$ne, ~ .
                       - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)))

ee_model_varying$neither <- update(
  ee_model_varying$neither,
  end = list(f = update(formula(ee_model_varying$neither)$end, ~ .
                        - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)),
  ne = list(f = update(formula(ee_model_varying$neither)$ne, ~ .
                       - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)))
ee_model_varying$endemic <- update(
  ee_model_varying$endemic,
  end = list(f = update(formula(ee_model_varying$endemic)$end, ~ .
                        - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)),
  ne = list(f = update(formula(ee_model_varying$endemic)$ne, ~ .
                       - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)))
ee_model_varying$epidemic <- update(
  ee_model_varying$epidemic,
  end = list(f = update(formula(ee_model_varying$epidemic)$end, ~ .
                        - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)),
  ne = list(f = update(formula(ee_model_varying$epidemic)$ne, ~ .
                       - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)))
ee_model_varying$both <- update(
  ee_model_varying$both,
  end = list(f = update(formula(ee_model_varying$both)$end, ~ .
                        - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)),
  ne = list(f = update(formula(ee_model_varying$both)$ne, ~ .
                       - fe(1, unitSpecific = FALSE) + ri(type = "iid") - 1 + linear_time)))

save(list = ls()[grepl("ee_model", ls()) | grepl("input_list", ls())],
     file = "output/models-regions.Rdata")

# NB we are using update rather than replacing
#c("fe(1, unitSpecific = FALSE)"),
#with
#c("ri(type = 'iid')", "linear_time"),

# since it leads to a different expression for f
#namely
#~ri(type = "iid") + linear_time - 1 + sin(2 * pi * t/52) + cos(2 * 
#pi * t/52)
#'log Lik.' -7136 (df=NA)
#rather than
#ee_model_constant$neither$control$end$f
#~sin(2 * pi * t/52) + cos(2 * pi * t/52) + ri(type = "iid") + 
#  linear_time - 1
# 'log Lik.' -7198 (df=NA)
# which is the model we use