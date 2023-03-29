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
ggplot(data = aggregate(value ~ dateISO + geoRegion,
                        data = melt(case_counts), FUN = sum),
       mapping = aes(x = dateISO, y = value, group = geoRegion)) +
  geom_col() +
  facet_wrap(geoRegion ~ ., scales = "free_y") +
  scale_x_discrete(labels = skip_labels(aggregate(value ~ dateISO + geoRegion,
                        data = melt(case_counts), FUN = sum)$dateISO)) +
  labs(y = "Cases", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


## ----out.width = "60%"--------------------------------------------------------
tmp <- aggregate(value ~ dateISO + geoRegion,
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
tmp <- merge(aggregate(value ~ dateISO + geoRegion,
                        data = melt(case_counts), FUN = sum),
rowSums(popCH))
tmp$cases_per_pop <- tmp$value / tmp$y
tmp$cases_per_100k <- tmp$cases_per_pop * 100000

# GEOM_COL/GEOM_BAR(STAT = "IDENTITY") GIVE INCORRECT VALUES
#ggplot(data = tmp[tmp$geoRegion == "AG", ],
#       mapping = aes(x = dateISO, y = cases_per_100k)) +
#  geom_line(colour = "blue") +
#  geom_col(fill = "yellow", alpha = 0.3) +
#  geom_abline(intercept = range(tmp$cases_per_100k)[2], col = 2)

ggplot(data = tmp,
       mapping = aes(x = dateISO, y = cases_per_100k,
                     group = geoRegion)) +
  geom_line() +
  facet_wrap(geoRegion ~ ., scales = "free_y") +
  scale_x_discrete(labels = skip_labels(aggregate(value ~ dateISO + geoRegion,
                        data = melt(case_counts), FUN = sum)$dateISO)) +
  labs(y = "Cases per 100,000 population", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


## -----------------------------------------------------------------------------
ggplot(data = melt(vax_cov_covariate_r),
       mapping = aes(y = value, x = dateISO,
                     group = geoRegion)) +
  geom_line() +
  facet_wrap(geoRegion ~ ., scales = "free_y") +
  scale_x_discrete(labels = skip_labels(melt(vax_cov_covariate_r)$dateISO)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Coverage (second dose and booster with waning taken into account)", x = "Week") +
  ylim(0, 1)


## ----out.width = "60%"--------------------------------------------------------
# For projection later - start at first time point average
# vaccination coverage is 25%
start <- which.max(apply(vax_cov_covariate_r, 1, mean) > 0.25)
tmp <- melt(vax_cov_covariate_r)
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
ggplot(data = melt(log(1 - vax_cov_covariate_r)),
       mapping = aes(y = value, x = dateISO,
                     group = geoRegion)) +
  geom_line() +
  facet_wrap(geoRegion ~ ., scales = "free_y") +
  scale_x_discrete(labels = skip_labels(melt(vax_cov_covariate_r)$dateISO)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "log(1 - vaccination coverage)", x = "Week") +
  ylim(- 1, 0)


## ----out.width = "60%"--------------------------------------------------------
tmp <- melt(log(1 - vax_cov_covariate_r))
ggplot(data = tmp, aes_string(y = names(tmp)[1],
                              x = names(tmp)[2],
                              fill = names(tmp)[3])) + 
  geom_tile() +
  scale_fill_viridis_c(direction = - 1) +
  labs(x = "Unit", y = "Week", title = "log(1 - vaccination coverage)", colour = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_discrete(labels = skip_labels(tmp$dateISO)) +
  geom_abline(aes(intercept = start, slope = 0,
                  colour = "25% avg coverage"), lty = 3,
              show.legend = TRUE) +
  geom_abline(aes(intercept = start, slope = 0,
                  colour = "25% avg coverage"), lty = 3, colour = "grey",
              show.legend = FALSE)


## -----------------------------------------------------------------------------
source(paste0(getwd(), "/src/models-regions.R"))
load(file = paste0(getwd(), "/output/models-regions.Rdata"))

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

kable(res_table,
          booktabs = options()$booktabs,
          linesep = options()$linesep,
          escape = FALSE,
          align = "lrrrr")


## -----------------------------------------------------------------------------
plot(ee_model_constant$endemic, type = "ri", component = "end", labels = TRUE,
      main = "Endemic component",
     col.regions = hcl.colors(100, "viridis", rev = TRUE),
     par.settings = list(axis.line = list(col = "transparent")))


## -----------------------------------------------------------------------------
plot(ee_model_constant$endemic, type = "ri", component = "ne", labels = TRUE,
      main = "Epidemic component",
     col.regions = hcl.colors(100, "viridis", rev = TRUE),
     par.settings = list(axis.line = list(col = "transparent")))


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


## ----time-varying-mats-models-region, fig.height = 8, fig.width = 20----------
if(isFALSE(all(all.equal(ee_model_varying$neither$control$ne$weights,
ee_model_varying$endemic$control$ne$weights),
all.equal(ee_model_varying$endemic$control$ne$weights,
          ee_model_varying$epidemic$control$ne$weights),
all.equal(ee_model_varying$epidemic$control$ne$weights,
ee_model_varying$both$control$ne$weights)))){
  stop("Time-varying weights are not the same for the models")
}

#expanded <- FALSE
assign("expanded", FALSE, .GlobalEnv)
source(paste0(getwd(), "/src/plot-time-varying-adjacency-matrices.R"))
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

kable(res_table,
          booktabs = options()$booktabs,
          linesep = options()$linesep,
          escape = FALSE,
          align = "lrrrr")


## -----------------------------------------------------------------------------
plot(ee_model_varying$endemic, type = "ri", component = "end", labels = TRUE,
      main = "Endemic component",
     col.regions = hcl.colors(100, "viridis", rev = TRUE),
     par.settings = list(axis.line = list(col = "transparent")))


## -----------------------------------------------------------------------------
plot(ee_model_varying$endemic, type = "ri", component = "ne", labels = TRUE,
      main = "Epidemic component",
     col.regions = hcl.colors(100, "viridis", rev = TRUE),
     par.settings = list(axis.line = list(col = "transparent")))


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


## ----max-regional-coverage, eval = FALSE--------------------------------------
## max_regions <- levels(melt(vax_cov_covariate_r)$geoRegion)[aggregate(value ~ dateISO, melt(vax_cov_covariate_r), which.max)$value]
## farve <- ifelse(seq_len(length(unique(melt(vax_cov_covariate_r)$dateISO))) > start, "e", "f")
## 
## (plot_for_understanding_prediction <-
##     ggplot(data = aggregate(value ~ dateISO, melt(vax_cov_covariate_r), max), aes(x = dateISO, y = value, colour = as.character(dateISO) > date_range_full[start])) +
##     geom_text(label = max_regions, check_overlap = TRUE, show.legend = FALSE) +
##     labs(x = "Week", y = "Maximum vaccination coverage", colour = "Prediction window") +
##     ylim(0, 1) +
##     geom_vline(xintercept = start) +
##     # Manual legend
##     geom_point(label = max_regions, alpha = 0) +
##     guides(colour = guide_legend("Prediction window", override.aes = list(size = 3, alpha = 1))) +
##     scale_colour_manual(values = c("FALSE" = viridis(9)[8],
##                                  "TRUE" = viridis(9)[4])) +
##     scale_x_discrete(labels = skip_labels(aggregate(value ~ dateISO, melt(vax_cov_covariate_r), max)$dateISO)))


## -----------------------------------------------------------------------------
# [] ensures it goes back to matrix
vax_cov_covariate_r[] <- apply(vax_cov_covariate_r, 1, max)
ggplot(data = melt(vax_cov_covariate_r),
       mapping = aes(y = value, x = dateISO,
                     group = geoRegion)) +
  geom_line() +
  facet_wrap(geoRegion ~ ., scales = "free_y") +
  scale_x_discrete(labels = skip_labels(melt(vax_cov_covariate_r)$dateISO)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Coverage (second dose and booster with waning taken into account)") +
  ylim(0, 1)


## ----out.width = "60%"--------------------------------------------------------
tmp <- melt(vax_cov_covariate_r)
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

ee_scen_varying$neither$control$data$unvax <- log(1 - vax_cov_covariate_r)
ee_scen_varying$endemic$control$data$unvax <- log(1 - vax_cov_covariate_r)
ee_scen_varying$epidemic$control$data$unvax <- log(1 - vax_cov_covariate_r)
ee_scen_varying$both$control$data$unvax <- log(1 - vax_cov_covariate_r)

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
     file = "output/prediction-regions.Rdata")


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
     file = "output/ratio-regions.Rdata")


## ----predicted-ratio-regions--------------------------------------------------
plot_prediction(alt = ee_pred_scen,
                orig = ee_pred_orig,
                uncertainty = TRUE,
                ratio_df = ratio_df,
                mod_opt = "endemic")

