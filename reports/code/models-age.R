## ----"main-setup", include = FALSE--------------------------------------------
setwd("..")
# Check if in reports (relevant for the code extracted from the .Rnw files
# so they can be run by other researchers)
if(substring(getwd(), nchar(getwd()) - nchar("reports") + 1) == "reports"){
  setwd("..")
}
knitr::opts_knit$set(root.dir = getwd()) # Move up to main project folder
library("ProjectTemplate")
load.project()


## -----------------------------------------------------------------------------
ggplot(data = aggregate(value ~ dateISO + altersklasse_covid19,
                        data = melt(case_counts), FUN = sum),
       mapping = aes(x = dateISO, y = value, group = 1)) +
  geom_col() +
  facet_wrap(altersklasse_covid19 ~ ., scales = "free_y") +
  scale_x_discrete(labels = skip_labels(aggregate(value ~ dateISO + altersklasse_covid19,
                        data = melt(case_counts), FUN = sum)$dateISO)) +
  labs(y = "Cases", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


## ----out.width = "60%"--------------------------------------------------------
tmp <- aggregate(value ~ dateISO + altersklasse_covid19,
                        data = melt(case_counts), FUN = sum)
ggplot(data = tmp, aes_string(y = names(tmp)[1],
                              x = names(tmp)[2],
                              fill = names(tmp)[3])) + 
  geom_tile() +
  scale_fill_viridis_c(direction = - 1) +
  labs(x = "Unit", y = "Week", title = "Cases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #scale_x_discrete(labels = skip_labels(tmp$groups)) +
  scale_y_discrete(labels = skip_labels(tmp$dateISO))


## -----------------------------------------------------------------------------
# For projection later - start at first time point average
# vaccination coverage is 25%
start <- which.max(apply(vax_cov_covariate_a, 1, mean) > 0.25)
ggplot(data = melt(vax_cov_covariate_a),
       mapping = aes(y = value, x = dateISO,
                     group = altersklasse_covid19)) +
  geom_line() +
  facet_wrap(altersklasse_covid19 ~ ., scales = "free_y") +
  scale_x_discrete(labels = skip_labels(melt(vax_cov_covariate_a)$dateISO)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Coverage (second dose and booster with waning taken into account)", x = "Week") +
  ylim(0, 1)


## ----out.width = "60%"--------------------------------------------------------
tmp <- melt(vax_cov_covariate_a)
ggplot(data = tmp, aes_string(y = names(tmp)[1],
                              x = names(tmp)[2],
                              fill = names(tmp)[3])) + 
  geom_tile() +
  scale_fill_viridis_c(direction = - 1) +
  labs(x = "Unit", y = "Week", title = "Vaccination coverage", colour = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #scale_x_discrete(labels = skip_labels(tmp$groups)) +
  scale_y_discrete(labels = skip_labels(tmp$dateISO)) +
  expand_limits(fill = c(0, 1)) +
  geom_abline(aes(intercept = start, slope = 0,
                  colour = "25% avg coverage"), lty = 3,
              show.legend = TRUE)


## -----------------------------------------------------------------------------
ggplot(data = melt(log(1 - vax_cov_covariate_a)),
       mapping = aes(y = value, x = dateISO,
                     group = altersklasse_covid19)) +
  geom_line() +
  facet_wrap(altersklasse_covid19 ~ ., scales = "free_y") +
  scale_x_discrete(labels = skip_labels(melt(vax_cov_covariate_a)$dateISO)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "log(1 - vaccination coverage)", x = "Week")


## ----out.width = "60%"--------------------------------------------------------
tmp <- melt(log(1 - vax_cov_covariate_a))
ggplot(data = tmp, aes_string(y = names(tmp)[1],
                              x = names(tmp)[2],
                              fill = names(tmp)[3])) + 
  geom_tile() +
  scale_fill_viridis_c(direction = - 1) +
  labs(x = "Unit", y = "Week", title = "log(1 - vaccination coverage)", colour = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_discrete(labels = skip_labels(tmp$dateISO))  +
  geom_abline(aes(intercept = start, slope = 0,
                  colour = "25% avg coverage"), lty = 3,
              show.legend = TRUE) +
  geom_abline(aes(intercept = start, slope = 0,
                  colour = "25% avg coverage"), lty = 3, colour = "grey",
              show.legend = FALSE)


## -----------------------------------------------------------------------------
source(paste0(getwd(), "/src/models-age.R"))
load(file = paste0(getwd(), "/output/models-age.Rdata"))

loglikes_constant <- sapply(ee_model_constant, logLik)
invisible(capture.output(scores_constant <- sapply(ee_model_constant, function(x){
  scores(oneStepAhead(x, tp = tp, type = "rolling", which.start = "final"),
                      which = "dss", quiet = TRUE)
})))
loglikes_varying <- sapply(ee_model_varying, logLik)
invisible(capture.output(scores_varying <- sapply(ee_model_varying, function(x){
  scores(oneStepAhead(x, tp = tp, type = "rolling", which.start = "final"),
                      which = "dss", quiet = TRUE)
})))


## ----results = "asis"---------------------------------------------------------
res_table <- model_fit_info(ee_model_constant)

rownames(res_table) <- gsub("ne.unvax", "$\\\\beta^{(\\\\phi)}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.unvax", "$\\\\beta^{(\\\\nu)}$",
                            rownames(res_table))
rownames(res_table) <- gsub("overdisp", "$\\\\psi$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.10 - 19", "$\\\\alpha^{\\\\phi}_{10-19}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.20 - 29", "$\\\\alpha^{\\\\phi}_{20-29}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.30 - 39", "$\\\\alpha^{\\\\phi}_{30-39}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.40 - 49", "$\\\\alpha^{\\\\phi}_{40-49}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.50 - 59", "$\\\\alpha^{\\\\phi}_{50-59}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.60 - 69", "$\\\\alpha^{\\\\phi}_{60-69}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.70 - 79", "$\\\\alpha^{\\\\phi}_{70-79}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.80\\+", "$\\\\alpha^{\\\\phi}_{80+}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.pop_gravity", "$\\\\gamma^{\\\\phi}_{\\\\text{gravity}}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.linear_time", "$\\\\gamma^{\\\\nu}_{\\\\text{time}}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.linear_time", "$\\\\gamma^{\\\\phi}_{\\\\text{time}}$",
                            rownames(res_table))

rownames(res_table) <- gsub("end.ri\\(iid\\)", "$\\\\alpha^{\\\\nu}_{\\\\text{region}}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.ri\\(iid\\)", "$\\\\alpha^{\\\\phi}_{\\\\text{region}}$",
                            rownames(res_table))

rownames(res_table) <- gsub("end.sin\\(2 \\* pi \\* t/52\\)", "$\\\\gamma^{\\\\nu}_{\\\\sin(2\\\\pi t/52)}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.cos\\(2 \\* pi \\* t/52\\)", "$\\\\gamma^{\\\\nu}_{\\\\cos(2\\\\pi t/52)}$",
                            rownames(res_table))

rownames(res_table) <- gsub("ne.sin\\(2 \\* pi \\* t/52\\)", "$\\\\gamma^{\\\\phi}_{\\\\sin(2\\\\pi t/52)}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.cos\\(2 \\* pi \\* t/52\\)", "$\\\\gamma^{\\\\phi}_{\\\\cos(2\\\\pi t/52)}$",
                            rownames(res_table))

rownames(res_table) <- gsub("end.10 - 19", "$\\\\alpha^{\\\\nu}_{10-19}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.20 - 29", "$\\\\alpha^{\\\\nu}_{20-29}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.30 - 39", "$\\\\alpha^{\\\\nu}_{30-39}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.40 - 49", "$\\\\alpha^{\\\\nu}_{40-49}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.50 - 59", "$\\\\alpha^{\\\\nu}_{50-59}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.60 - 69", "$\\\\alpha^{\\\\nu}_{60-69}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.70 - 79", "$\\\\alpha^{\\\\nu}_{70-79}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.80\\+", "$\\\\alpha^{\\\\nu}_{80+}$",
                            rownames(res_table))

kable(res_table,
          booktabs = options()$booktabs,
          linesep = options()$linesep,
          escape = FALSE,
          align = "lrrrr")


## -----------------------------------------------------------------------------
ggplot(data = rbind(get_waves_data(ee_model_constant, "end"),
                    get_waves_data(ee_model_constant, "ne")),
       aes(x = time, y = est, ymin = `df[, "ymin"]`,
           ymax = `df[, "ymax"]`,
           group = variable, colour = variable, fill = variable)) +
  #geom_ribbon(alpha = 0.4) +
  geom_line() + facet_wrap(. ~ type, scales = "free")


## -----------------------------------------------------------------------------
plot_model(ee_model_constant)


## ----time-varying-contact-mats, fig.height = 8, fig.width = 20----------------
#expanded <- FALSE
assign("expanded", FALSE, .GlobalEnv)
source(paste0(getwd(), "/src/plot-time-varying-contact-matrices.R"))
wrap_plots(out[which(skip_labels(seq_len(length(out))) != "")],
           guides = "collect", tag_level = "keep",
           ncol = 5)


## ----results = "asis"---------------------------------------------------------
res_table <- model_fit_info(ee_model_varying)

rownames(res_table) <- gsub("ne.unvax", "$\\\\beta^{(\\\\phi)}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.unvax", "$\\\\beta^{(\\\\nu)}$",
                            rownames(res_table))
rownames(res_table) <- gsub("overdisp", "$\\\\psi$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.10 - 19", "$\\\\alpha^{\\\\phi}_{10-19}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.20 - 29", "$\\\\alpha^{\\\\phi}_{20-29}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.30 - 39", "$\\\\alpha^{\\\\phi}_{30-39}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.40 - 49", "$\\\\alpha^{\\\\phi}_{40-49}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.50 - 59", "$\\\\alpha^{\\\\phi}_{50-59}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.60 - 69", "$\\\\alpha^{\\\\phi}_{60-69}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.70 - 79", "$\\\\alpha^{\\\\phi}_{70-79}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.80\\+", "$\\\\alpha^{\\\\phi}_{80+}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.pop_gravity", "$\\\\gamma^{\\\\phi}_{\\\\text{gravity}}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.linear_time", "$\\\\gamma^{\\\\nu}_{\\\\text{time}}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.linear_time", "$\\\\gamma^{\\\\phi}_{\\\\text{time}}$",
                            rownames(res_table))

rownames(res_table) <- gsub("end.ri\\(iid\\)", "$\\\\alpha^{\\\\nu}_{\\\\text{region}}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.ri\\(iid\\)", "$\\\\alpha^{\\\\phi}_{\\\\text{region}}$",
                            rownames(res_table))

rownames(res_table) <- gsub("end.sin\\(2 \\* pi \\* t/52\\)", "$\\\\gamma^{\\\\nu}_{\\\\sin(2\\\\pi t/52)}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.cos\\(2 \\* pi \\* t/52\\)", "$\\\\gamma^{\\\\nu}_{\\\\cos(2\\\\pi t/52)}$",
                            rownames(res_table))

rownames(res_table) <- gsub("ne.sin\\(2 \\* pi \\* t/52\\)", "$\\\\gamma^{\\\\phi}_{\\\\sin(2\\\\pi t/52)}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.cos\\(2 \\* pi \\* t/52\\)", "$\\\\gamma^{\\\\phi}_{\\\\cos(2\\\\pi t/52)}$",
                            rownames(res_table))



rownames(res_table) <- gsub("end.10 - 19", "$\\\\alpha^{\\\\nu}_{10-19}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.20 - 29", "$\\\\alpha^{\\\\nu}_{20-29}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.30 - 39", "$\\\\alpha^{\\\\nu}_{30-39}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.40 - 49", "$\\\\alpha^{\\\\nu}_{40-49}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.50 - 59", "$\\\\alpha^{\\\\nu}_{50-59}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.60 - 69", "$\\\\alpha^{\\\\nu}_{60-69}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.70 - 79", "$\\\\alpha^{\\\\nu}_{70-79}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.80\\+", "$\\\\alpha^{\\\\nu}_{80+}$",
                            rownames(res_table))

kable(res_table,
          booktabs = options()$booktabs,
          linesep = options()$linesep,
          escape = FALSE,
          align = "lrrrr")


## -----------------------------------------------------------------------------
ggplot(data = rbind(get_waves_data(ee_model_varying, "end"),
                    get_waves_data(ee_model_varying, "ne")),
       aes(x = time, y = est, ymin = `df[, "ymin"]`,
           ymax = `df[, "ymax"]`,
           group = variable, colour = variable, fill = variable)) +
  #geom_ribbon(alpha = 0.4) +
  geom_line() + facet_wrap(. ~ type, scales = "free")


## -----------------------------------------------------------------------------
plot_model(ee_model_varying)


## -----------------------------------------------------------------------------
before_2nd <- ggplot(data = aggregate(entries ~ dateISO + altersklasse_covid19, data_vax_2nd, sum),
       aes(x = dateISO, y = entries, group = altersklasse_covid19,
           fill = altersklasse_covid19)) + geom_area(position = "stack")
before_3rd <- ggplot(data = aggregate(entries ~ dateISO + altersklasse_covid19, data_vax_3rd, sum),
       aes(x = dateISO, y = entries, group = altersklasse_covid19,
           fill = altersklasse_covid19)) + geom_area(position = "stack")
# New distribution -- full immunisation
data_vax_2nd <- redistribute_vaccines(data_vax_2nd)
# New distribution -- booster
data_vax_3rd <- redistribute_vaccines(data_vax_3rd)

after_2nd <- ggplot(data = aggregate(entries ~ dateISO + altersklasse_covid19, data_vax_2nd, sum),
                 aes(x = dateISO, y = entries, group = altersklasse_covid19,
                     fill = altersklasse_covid19)) + geom_area(position = "stack")
after_3rd <- ggplot(data = aggregate(entries ~ dateISO + altersklasse_covid19, data_vax_3rd, sum),
                 aes(x = dateISO, y = entries, group = altersklasse_covid19,
                     fill = altersklasse_covid19)) + geom_area(position = "stack")

(before_2nd + labs(title = "Original distribution (full immunisation)", y = "Vaccines", x = "Week", fill = "Age group") +
    before_3rd + labs(title = "Original distribution (booster)", y = "Vaccines", x = "Week", fill = "Age group")) /
  (after_2nd + labs(title = "After redistributing (full immunisation)", y = "Vaccines", x = "Week", fill = "Age group") +
     after_3rd + labs(title = "After redistributing (booster)", y = "Vaccines", x = "Week", fill = "Age group")) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


## -----------------------------------------------------------------------------
# Calculate cumulative doses given
# AGE-SPECIFIC VACCINATION COVERAGE
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

data_vax_2nd_a$cov_1_a <- data_vax_3rd_a$csum_a / data_vax_3rd_a$pop_a
data_vax_3rd_a$cov_1_a <- data_vax_3rd_a$csum_a / data_vax_3rd_a$pop_a

# Apply waning
data_vax_2nd_a <- apply_waning(data_set = data_vax_2nd_a, FUN = waning,
                               name = c("csum_waned_a", "cov_1_waned_a"))
data_vax_3rd_a <- apply_waning(data_set = data_vax_3rd_a, FUN = waning,
                               name = c("csum_waned_a", "cov_1_waned_a"))

# Create the coverage based on the redistribution of the vaccinations
# Merge then sum to ensure no mismatch in group membership
# Unique names so can be summed
data_vax_2nd$vax_cov_2 <- data_vax_2nd$cov_1_waned
data_vax_3rd$vax_cov_3 <- data_vax_3rd$cov_1_waned

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
vax_cov_covariate_a <- xtabs(data = vax_cov_data_a,
                             formula = vax_cov_a ~ dateISO + altersklasse_covid19)


## -----------------------------------------------------------------------------
ggplot(data = melt(vax_cov_covariate_a),
       mapping = aes(y = value, x = dateISO,
                     group = altersklasse_covid19)) +
  geom_line() +
  facet_wrap(altersklasse_covid19 ~ ., scales = "free_y") +
  scale_x_discrete(labels = skip_labels(melt(vax_cov_covariate_a)$dateISO)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Coverage (second dose and booster with waning taken into account)") +
  ylim(0, 1)


## ----out.width = "60%"--------------------------------------------------------
tmp <- melt(vax_cov_covariate_a)
ggplot(data = tmp, aes_string(y = names(tmp)[1],
                              x = names(tmp)[2],
                              fill = names(tmp)[3])) +
  geom_tile() +
  scale_fill_viridis_c(direction = - 1) +
  labs(x = "Unit", y = "Week", title = "Vaccination coverage", colour = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #scale_x_discrete(labels = skip_labels(tmp$groups)) +
  scale_y_discrete(labels = skip_labels(tmp$dateISO)) +
  expand_limits(fill = c(0, 1)) +
  geom_abline(aes(intercept = start, slope = 0,
                  colour = "25% avg coverage (of original)"), lty = 3,
              show.legend = TRUE)


## -----------------------------------------------------------------------------
ee_scen_varying <- ee_model_varying

ee_scen_varying$neither$control$data$unvax <- log(1 - vax_cov_covariate_a)
ee_scen_varying$endemic$control$data$unvax <- log(1 - vax_cov_covariate_a)
ee_scen_varying$epidemic$control$data$unvax <- log(1 - vax_cov_covariate_a)
ee_scen_varying$both$control$data$unvax <- log(1 - vax_cov_covariate_a)

ee_pred_orig <- lapply(ee_model_varying, function(x){
  make_prediction_dates_ISO(predictive_moments(x,
                   t_condition = start,
                   lgt = length(date_range_full) - start))
})
ee_pred_scen <- lapply(ee_scen_varying, function(x){
  make_prediction_dates_ISO(predictive_moments(x,
                   t_condition = start,
                   lgt = length(date_range_full) - start))
})

save(list = ls()[grepl("ee_pred", ls()) | grepl("input_list", ls())],
     file = "output/prediction-age.Rdata")


## -----------------------------------------------------------------------------
predicted_vals <- data.frame("true" = colSums(ee_pred_orig$neither$realizations_matrix),
"endemic_orig" = colSums(ee_pred_orig$endemic$mu_matrix),
"epidemic_orig" = colSums(ee_pred_orig$epidemic$mu_matrix),
"both_orig" = colSums(ee_pred_orig$both$mu_matrix),
"endemic_scen" = colSums(ee_pred_scen$endemic$mu_matrix),
"epidemic_scen" = colSums(ee_pred_scen$epidemic$mu_matrix),
"both_scen" = colSums(ee_pred_scen$both$mu_matrix))


## ----results = "asis"---------------------------------------------------------
save <- colnames(predicted_vals)
colnames(predicted_vals) <- c("True", "Endemic", "Epidemic", "Both",
                    "Endemic", "Epidemic", "Both")
add_header_above(
  kable_input = kable(predicted_vals,
                      booktabs = options()$booktabs,
      linesep = options()$linesep),
  header = c("", "", "Original coverage" = 3, "Alternative coverage" = 3))
colnames(predicted_vals) <- save
rm(save)


## -----------------------------------------------------------------------------
ratio_df <- extract_quantiles(
  add_prediction_uncertainty(alt = ee_scen_varying,
                             orig = ee_model_varying,
                             start = start))
save(ratio_df,
     file = "output/ratio-age.Rdata")


## ----predicted-ratio-age-groups-----------------------------------------------
plot_prediction(alt = ee_pred_scen,
                orig = ee_pred_orig,
                uncertainty = TRUE,
                ratio_df = ratio_df,
                mod_opt = "endemic")

