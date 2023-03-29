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
par(las = 1)# LH pref


## ----fig.height=3, fig.width=5------------------------------------------------
x <- seq(0.1, 0.9, by = 0.1)
plot(x = x, y = x, col = viridis(9, direction = - 1),
     xlab = "",
     ylab = "Vaccination coverage\n x",
     cex.axis = 0.7, cex = 0.5, cex.lab = 0.5,
     pch = 19)


## ----fig.height=3, fig.width=5------------------------------------------------
f <- function(x){1 - x}
plot(x = x, y = f(x), col = viridis(9, direction = - 1),
     ylab = "Unvaccinated\n f(x) = 1 - x",
     xlab = "Vaccination coverage (x)",
     cex.axis = 0.7, cex = 0.5, cex.lab = 0.5,
     pch = 19)


## ----fig.height=3, fig.width=5------------------------------------------------
g <- function(x){log(x)}
plot(x = x, y = g(f(x)), col = viridis(9, direction = - 1),
     ylab = "Log-transform\n g(f(x)) = log(1 - x)",
     xlab = "Vaccination coverage (x)",
     cex.axis = 0.7, cex = 0.5, cex.lab = 0.5,
     pch = 19)


## ----alternative-waning-function, fig.height = 3, fig.width = 10--------------
par(mfrow = c(1, 3))
plot(waning(0 : 55, val = 1),
     type = "b", xlab = "p (weeks passed since vaccine given)",
     main = "Original", ylab = expression(u(p)), xaxt = "n")
lines(c(26, 39), c(1, 0), col = 2)
axis(1, at = c(0, 2, 26, 39, 52), labels = c(0, 2, 26, 39, 52))
plot(waning_sens1(0 : 55, val = 1),
     type = "b", xlab = "p (weeks passed since vaccine given)",
     main = "Sensitivity analysis 1 (h = 0.5)", ylab = expression(u["alt"](p)), xaxt = "n")
lines(c(26, 39), c(1, 0), col = 2)
axis(1, at = c(0, 2, 26, 39, 52), labels = c(0, 2, 26, 39, 52))
plot(waning_sens2(0 : 55, val = 1),
     type = "b", xlab = "p (weeks passed since vaccine given)",
     main = "Sensitivity analysis 2 (h = 0.3)", ylab = expression(u["alt"](p)), xaxt = "n")
lines(c(26, 39), c(1, 0), col = 2)
axis(1, at = c(0, 2, 26, 39, 52), labels = c(0, 2, 26, 39, 52))


## ----coverage-waned-alternative, fig.height = 7, fig.width = 16---------------
load(paste0(getwd(), "/output/coverage-sens.Rdata"))
plot_data_sens1_a <- melt(vax_cov_covariate_sens1_a)
plot_data_sens1_r <- melt(vax_cov_covariate_sens1_r)
plot_data_sens2_a <- melt(vax_cov_covariate_sens2_a)
plot_data_sens2_r <- melt(vax_cov_covariate_sens2_r)

ggplot(data = plot_data_sens1_a,
       mapping = aes(x = dateISO,
                     y = value,
                     group = altersklasse_covid19,
                     colour = altersklasse_covid19)) +#,
  #                        shape = geoRegion)) +
  labs(x = "Week", y = expression(x[at]),
       title = "Vaccination coverage (sensitivity analysis 1)") +
  ylim(0, 1) +
  geom_line() +
  #geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  #guides(colour = guide_legend(ncol = 13)) +
  scale_x_discrete(labels = skip_labels(plot_data_sens1_a$dateISO)) +
  #scale_colour_manual(values = viridis(26),
  #                     name = "Canton") +
  # scale_shape_manual(values = rep(c(15 : 19), each = 6),
  #                    name = "Canton")) +
  ggplot(data = plot_data_sens2_a,
         mapping = aes(x = dateISO,
                       y = value,
                       group = altersklasse_covid19,
                       colour = altersklasse_covid19)) +#,
  #shape = geoRegion)) +
  labs(x = "Week", y = expression(x[at]),
       title = "Vaccination coverage (sensitivity analysis 2)",
       colour = "Age group") +
  ylim(0, 1) +
  geom_line() +
  #geom_point() +
  #facet_wrap(altersklasse_covid19 ~ .) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  #guides(colour = guide_legend(ncol = 13)) +
  scale_x_discrete(labels = skip_labels(plot_data_sens2_a$dateISO)) +
  #  scale_colour_manual(values = viridis(26),
  #                       name = "Canton") +
  #   scale_shape_manual(values = rep(c(15 : 19), each = 6),
  #                      name = "Canton")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") +
# Larger text
    theme(text = element_text(size = theme_get()$text$size + 1),
axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
legend.text = element_text(size = theme_get()$legend.text$size + 0.3))

ggplot(data = plot_data_sens1_r,
       mapping = aes(x = dateISO,
                     y = value,
                     group = geoRegion,
                     colour = geoRegion,
                     shape = geoRegion)) +
  labs(x = "Week", y = expression(x[rt]),
       title = "Vaccination coverage (sensitivity analysis 1)") +
  ylim(0, 1) +
  geom_line() +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(colour = guide_legend(ncol = 13)) +
  scale_x_discrete(labels = skip_labels(plot_data_sens1_r$dateISO)) +
  scale_colour_manual(values = viridis(26),
                      name = "Canton") +
  scale_shape_manual(values = rep(c(15 : 19), each = 6),
                     name = "Canton") +
  ggplot(data = plot_data_sens2_r,
         mapping = aes(x = dateISO,
                       y = value,
                       group = geoRegion,
                       colour = geoRegion,
                       shape = geoRegion)) +
  labs(x = "Week", y = expression(x[rt]),
       title = "Vaccination coverage (sensitivity analysis 2)") +
  ylim(0, 1) +
  geom_line() +
  geom_point() +
  #facet_wrap(altersklasse_covid19 ~ .) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(colour = guide_legend(ncol = 13)) +
  scale_x_discrete(labels = skip_labels(plot_data_sens2_a$dateISO)) +
  scale_colour_manual(values = viridis(26),
                      name = "Canton") +
  scale_shape_manual(values = rep(c(15 : 19), each = 6),
                     name = "Canton") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")  +
# Larger text
    theme(text = element_text(size = theme_get()$text$size + 1),
axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
legend.text = element_text(size = theme_get()$legend.text$size + 0.3))


## ----coverage-covariate-plots-alternative, fig.width = 12, fig.height = 7, eval = FALSE----
## ggplot(data = plot_data_sens1_a,
##        mapping = aes(x = dateISO,
##                      y = log(1 - value),
##                      group = altersklasse_covid19,
##                      colour = altersklasse_covid19)) +#,
##   #                        shape = geoRegion)) +
##   labs(x = "Week", y = expression(log ~ "(1 - " ~ x[at] ~ ")")),
##        title = "Vaccination coverage (sensitivity analysis 1)") +
##   #ylim(0, 1) +
##   geom_line() +
##   #geom_point()+
##   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
##   #guides(colour = guide_legend(ncol = 13)) +
##   scale_x_discrete(labels = skip_labels(plot_data_sens1_a$dateISO)) +
##   #scale_colour_manual(values = viridis(26),
##   #                     name = "Canton") +
##   # scale_shape_manual(values = rep(c(15 : 19), each = 6),
##   #                    name = "Canton")) +
##   ggplot(data = plot_data_sens2_a,
##          mapping = aes(x = dateISO,
##                        y = log(1 - value),
##                        group = altersklasse_covid19,
##                        colour = altersklasse_covid19)) +#,
##   #shape = geoRegion)) +
##   labs(x = "Week", y = expression(log ~ "(1 - " ~ x[at] ~ ")")),
##        title = "Vaccination coverage (sensitivity analysis 2)",
##        colour = "Age group") +
##   ylim(0, 1) +
##   geom_line() +
##   #geom_point() +
##   #facet_wrap(altersklasse_covid19 ~ .) +
##   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
##   #guides(colour = guide_legend(ncol = 13)) +
##   scale_x_discrete(labels = skip_labels(plot_data_sens2_a$dateISO)) +
##   #  scale_colour_manual(values = viridis(26),
##   #                       name = "Canton") +
##   #   scale_shape_manual(values = rep(c(15 : 19), each = 6),
##   #                      name = "Canton")) +
##   plot_layout(guides = "collect") &
##   theme(legend.position = "bottom") +
## # Larger text
##     theme(text = element_text(size = theme_get()$text$size + 1),
## axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
## axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
## legend.text = element_text(size = theme_get()$legend.text$size + 0.3))
## 
## ggplot(data = plot_data_sens1_r,
##        mapping = aes(x = dateISO,
##                      y = log(1 - value),
##                      group = geoRegion,
##                      colour = geoRegion,
##                      shape = geoRegion)) +
##   labs(x = "Week", y = expression(log ~ "(1 - " ~ x[rt] ~ ")")),
##        title = "Vaccination coverage (sensitivity analysis 1)") +
##   ylim(0, 1) +
##   geom_line() +
##   geom_point()+
##   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
##   guides(colour = guide_legend(ncol = 13)) +
##   scale_x_discrete(labels = skip_labels(plot_data_sens1_r$dateISO)) +
##   scale_colour_manual(values = viridis(26),
##                       name = "Canton") +
##   scale_shape_manual(values = rep(c(15 : 19), each = 6),
##                      name = "Canton") +
##   ggplot(data = plot_data_sens2_r,
##          mapping = aes(x = dateISO,
##                        y = log(1 - value),
##                        group = geoRegion,
##                        colour = geoRegion,
##                        shape = geoRegion)) +
##   labs(x = "Week", y = expression(log ~ "(1 - " ~ x[rt] ~ ")")),
##        title = "Vaccination coverage (sensitivity analysis 2)") +
##   ylim(0, 1) +
##   geom_line() +
##   geom_point() +
##   #facet_wrap(altersklasse_covid19 ~ .) +
##   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
##   guides(colour = guide_legend(ncol = 13)) +
##   scale_x_discrete(labels = skip_labels(plot_data_sens2_a$dateISO)) +
##   scale_colour_manual(values = viridis(26),
##                       name = "Canton") +
##   scale_shape_manual(values = rep(c(15 : 19), each = 6),
##                      name = "Canton") +
##   plot_layout(guides = "collect") &
##   theme(legend.position = "bottom")  +
## # Larger text
##     theme(text = element_text(size = theme_get()$text$size + 1),
## axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
## axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
## legend.text = element_text(size = theme_get()$legend.text$size + 0.3))


## -----------------------------------------------------------------------------
load(file = paste0(getwd(), "/output/models-regions.Rdata"))
region_ee_model_constant <- ee_model_constant
region_ee_model_varying <- ee_model_varying
load(file = paste0(getwd(), "/output/models-age.Rdata"))
age_ee_model_constant <- ee_model_constant
age_ee_model_varying <- ee_model_varying


## -----------------------------------------------------------------------------
region_ee_model_constant$endemic$control$data$unvax <-
  region_ee_model_constant$epidemic$control$data$unvax <-
  region_ee_model_constant$both$control$data$unvax <-
  log(1 - vax_cov_covariate_sens1_r)

region_ee_model_constant$endemic <-
  hhh4(region_ee_model_constant$endemic$stsObj,
       region_ee_model_constant$endemic$control)
region_ee_model_constant$epidemic <-
  hhh4(region_ee_model_constant$epidemic$stsObj,                                   region_ee_model_constant$epidemic$control)
region_ee_model_constant$both <-
  hhh4(region_ee_model_constant$both$stsObj,
       region_ee_model_constant$both$control)

region_ee_model_varying$endemic$control$data$unvax <-
  region_ee_model_varying$epidemic$control$data$unvax <-
  region_ee_model_varying$both$control$data$unvax <-
  log(1 - vax_cov_covariate_sens1_r)

region_ee_model_varying$endemic <- 
  hhh4(region_ee_model_varying$endemic$stsObj,
       region_ee_model_varying$endemic$control)
region_ee_model_varying$epidemic <- 
  hhh4(region_ee_model_varying$epidemic$stsObj,
       region_ee_model_varying$epidemic$control)
region_ee_model_varying$both <- 
  hhh4(region_ee_model_varying$both$stsObj,
       region_ee_model_varying$both$control)

age_ee_model_constant$endemic$control$data$unvax <-
  age_ee_model_constant$epidemic$control$data$unvax <-
  age_ee_model_constant$both$control$data$unvax <-
  log(1 - vax_cov_covariate_sens1_a)

age_ee_model_constant$endemic <- 
  hhh4(age_ee_model_constant$endemic$stsObj,
       age_ee_model_constant$endemic$control)
age_ee_model_constant$epidemic <- 
  hhh4(age_ee_model_constant$epidemic$stsObj,
       age_ee_model_constant$epidemic$control)
age_ee_model_constant$both <- 
  hhh4(age_ee_model_constant$both$stsObj,
       age_ee_model_constant$both$control)

age_ee_model_varying$endemic$control$data$unvax <-
  age_ee_model_varying$epidemic$control$data$unvax <-
  age_ee_model_varying$both$control$data$unvax <-
  log(1 - vax_cov_covariate_sens1_a)

age_ee_model_varying$endemic <- 
  hhh4(age_ee_model_varying$endemic$stsObj,
       age_ee_model_varying$endemic$control)
age_ee_model_varying$epidemic <- 
  hhh4(age_ee_model_varying$epidemic$stsObj,
       age_ee_model_varying$epidemic$control)
age_ee_model_varying$both <- 
  hhh4(age_ee_model_varying$both$stsObj,
       age_ee_model_varying$both$control)


## -----------------------------------------------------------------------------
our_models_df <- rbind(
  c(model_fit_info(region_ee_model_constant, FALSE)["end.unvax", "est-endemic"], model_fit_info(region_ee_model_constant, FALSE)["end.unvax", "se-endemic"], "Endemic", "Region", "1R", "Endemic"),
  c(model_fit_info(region_ee_model_constant, FALSE)["ne.unvax", "est-epidemic"], model_fit_info(region_ee_model_constant, FALSE)["ne.unvax", "se-epidemic"], "Epidemic", "Region", "1R", "Epidemic"),
  c(model_fit_info(region_ee_model_constant, FALSE)["end.unvax", "est-both"], model_fit_info(region_ee_model_constant, FALSE)["end.unvax", "se-both"], "Endemic", "Region", "1R", "Both"),
  c(model_fit_info(region_ee_model_constant, FALSE)["ne.unvax", "est-both"], model_fit_info(region_ee_model_constant, FALSE)["ne.unvax", "se-both"], "Endemic", "Region", "1R", "Both"),
  c(model_fit_info(region_ee_model_varying, FALSE)["end.unvax", "est-endemic"], model_fit_info(region_ee_model_varying, FALSE)["end.unvax", "se-endemic"], "Endemic", "Region", "2R", "Endemic"),
  c(model_fit_info(region_ee_model_varying, FALSE)["ne.unvax", "est-epidemic"], model_fit_info(region_ee_model_varying, FALSE)["ne.unvax", "se-epidemic"], "Epidemic", "Region", "2R", "Epidemic"),
  c(model_fit_info(region_ee_model_varying, FALSE)["end.unvax", "est-both"], model_fit_info(region_ee_model_varying, FALSE)["end.unvax", "se-both"], "Endemic", "Region", "2R", "Both"),
  c(model_fit_info(region_ee_model_varying, FALSE)["ne.unvax", "est-both"], model_fit_info(region_ee_model_varying, FALSE)["ne.unvax", "se-both"], "Endemic", "Region", "2R", "Both"),
  c(model_fit_info(age_ee_model_constant, FALSE)["end.unvax", "est-endemic"], model_fit_info(age_ee_model_constant, FALSE)["end.unvax", "se-endemic"], "Endemic", "Age", "1A", "Endemic"),
  c(model_fit_info(age_ee_model_constant, FALSE)["ne.unvax", "est-epidemic"], model_fit_info(age_ee_model_constant, FALSE)["ne.unvax", "se-epidemic"], "Epidemic", "Age", "1A", "Epidemic"),
  c(model_fit_info(age_ee_model_constant, FALSE)["end.unvax", "est-both"], model_fit_info(age_ee_model_constant, FALSE)["end.unvax", "se-both"], "Endemic", "Age", "1A", "Both"),
  c(model_fit_info(age_ee_model_constant, FALSE)["ne.unvax", "est-both"], model_fit_info(age_ee_model_constant, FALSE)["ne.unvax", "se-both"], "Endemic", "Age", "1A", "Both"),
  c(model_fit_info(age_ee_model_varying, FALSE)["end.unvax", "est-endemic"], model_fit_info(age_ee_model_varying, FALSE)["end.unvax", "se-endemic"], "Endemic", "Age", "2A", "Endemic"),
  c(model_fit_info(age_ee_model_varying, FALSE)["ne.unvax", "est-epidemic"], model_fit_info(age_ee_model_varying, FALSE)["ne.unvax", "se-epidemic"], "Epidemic", "Age", "2A", "Epidemic"),
  c(model_fit_info(age_ee_model_varying, FALSE)["end.unvax", "est-both"], model_fit_info(age_ee_model_varying, FALSE)["end.unvax", "se-both"], "Endemic", "Age", "2A", "Both"),
  c(model_fit_info(age_ee_model_varying, FALSE)["ne.unvax", "est-both"], model_fit_info(age_ee_model_varying, FALSE)["ne.unvax", "se-both"], "Endemic", "Age", "2A", "Both"))
our_models_df <- as.data.frame(our_models_df)
names(our_models_df) <- c("est", "se", "Component", "unit", "weights", "inclusion")
our_models_df$est <- as.numeric(our_models_df$est)
our_models_df$se <- as.numeric(our_models_df$se)
our_models_df$Disease <- "COVID-19"
our_models_df$placement <- seq(2, 2 + dim(our_models_df)[1] / 10,		  length.out = dim(our_models_df)[1])
our_models_df$model <- our_models_df$weight
our_models_df_sens_1 <- our_models_df; rm(our_models_df)


## -----------------------------------------------------------------------------
load(file = paste0(getwd(), "/output/models-regions.Rdata"))
region_ee_model_constant <- ee_model_constant
region_ee_model_varying <- ee_model_varying
load(file = paste0(getwd(), "/output/models-age.Rdata"))
age_ee_model_constant <- ee_model_constant
age_ee_model_varying <- ee_model_varying


## -----------------------------------------------------------------------------
region_ee_model_constant$endemic$control$data$unvax <-
  region_ee_model_constant$epidemic$control$data$unvax <-
  region_ee_model_constant$both$control$data$unvax <-
  log(1 - vax_cov_covariate_sens2_r)

region_ee_model_constant$endemic <- 
  hhh4(region_ee_model_constant$endemic$stsObj,
       region_ee_model_constant$endemic$control)
region_ee_model_constant$epidemic <- 
  hhh4(region_ee_model_constant$epidemic$stsObj,
       region_ee_model_constant$epidemic$control)
region_ee_model_constant$both <- 
  hhh4(region_ee_model_constant$both$stsObj,
       region_ee_model_constant$both$control)

region_ee_model_varying$endemic$control$data$unvax <-
  region_ee_model_varying$epidemic$control$data$unvax <-
  region_ee_model_varying$both$control$data$unvax <-
  log(1 - vax_cov_covariate_sens2_r)

region_ee_model_varying$endemic <-
  hhh4(region_ee_model_varying$endemic$stsObj,
       region_ee_model_varying$endemic$control)
region_ee_model_varying$epidemic <-
  hhh4(region_ee_model_varying$epidemic$stsObj,
       region_ee_model_varying$epidemic$control)
region_ee_model_varying$both <-
  hhh4(region_ee_model_varying$both$stsObj,
       region_ee_model_varying$both$control)

age_ee_model_constant$endemic$control$data$unvax <-
  age_ee_model_constant$epidemic$control$data$unvax <-
  age_ee_model_constant$both$control$data$unvax <-
  log(1 - vax_cov_covariate_sens2_a)

age_ee_model_constant$endemic <-
  hhh4(age_ee_model_constant$endemic$stsObj,
       age_ee_model_constant$endemic$control)
age_ee_model_constant$epidemic <-
  hhh4(age_ee_model_constant$epidemic$stsObj,
       age_ee_model_constant$epidemic$control)
age_ee_model_constant$both <-
  hhh4(age_ee_model_constant$both$stsObj,
       age_ee_model_constant$both$control)

age_ee_model_varying$endemic$control$data$unvax <-
  age_ee_model_varying$epidemic$control$data$unvax <-
  age_ee_model_varying$both$control$data$unvax <-
  log(1 - vax_cov_covariate_sens2_a)

age_ee_model_varying$endemic <-
  hhh4(age_ee_model_varying$endemic$stsObj,
                                     age_ee_model_varying$endemic$control)
age_ee_model_varying$epidemic <-
  hhh4(age_ee_model_varying$epidemic$stsObj,
                                      age_ee_model_varying$epidemic$control)
age_ee_model_varying$both <-
  hhh4(age_ee_model_varying$both$stsObj,
                                  age_ee_model_varying$both$control)


## -----------------------------------------------------------------------------
our_models_df <- rbind(
  c(model_fit_info(region_ee_model_constant, FALSE)["end.unvax", "est-endemic"], model_fit_info(region_ee_model_constant, FALSE)["end.unvax", "se-endemic"], "Endemic", "Region", "1R", "Endemic"),
  c(model_fit_info(region_ee_model_constant, FALSE)["ne.unvax", "est-epidemic"], model_fit_info(region_ee_model_constant, FALSE)["ne.unvax", "se-epidemic"], "Epidemic", "Region", "1R", "Epidemic"),
  c(model_fit_info(region_ee_model_constant, FALSE)["end.unvax", "est-both"], model_fit_info(region_ee_model_constant, FALSE)["end.unvax", "se-both"], "Endemic", "Region", "1R", "Both"),
  c(model_fit_info(region_ee_model_constant, FALSE)["ne.unvax", "est-both"], model_fit_info(region_ee_model_constant, FALSE)["ne.unvax", "se-both"], "Endemic", "Region", "1R", "Both"),
  c(model_fit_info(region_ee_model_varying, FALSE)["end.unvax", "est-endemic"], model_fit_info(region_ee_model_varying, FALSE)["end.unvax", "se-endemic"], "Endemic", "Region", "2R", "Endemic"),
  c(model_fit_info(region_ee_model_varying, FALSE)["ne.unvax", "est-epidemic"], model_fit_info(region_ee_model_varying, FALSE)["ne.unvax", "se-epidemic"], "Epidemic", "Region", "2R", "Epidemic"),
  c(model_fit_info(region_ee_model_varying, FALSE)["end.unvax", "est-both"], model_fit_info(region_ee_model_varying, FALSE)["end.unvax", "se-both"], "Endemic", "Region", "2R", "Both"),
  c(model_fit_info(region_ee_model_varying, FALSE)["ne.unvax", "est-both"], model_fit_info(region_ee_model_varying, FALSE)["ne.unvax", "se-both"], "Endemic", "Region", "2R", "Both"),
  c(model_fit_info(age_ee_model_constant, FALSE)["end.unvax", "est-endemic"], model_fit_info(age_ee_model_constant, FALSE)["end.unvax", "se-endemic"], "Endemic", "Age", "1A", "Endemic"),
  c(model_fit_info(age_ee_model_constant, FALSE)["ne.unvax", "est-epidemic"], model_fit_info(age_ee_model_constant, FALSE)["ne.unvax", "se-epidemic"], "Epidemic", "Age", "1A", "Epidemic"),
  c(model_fit_info(age_ee_model_constant, FALSE)["end.unvax", "est-both"], model_fit_info(age_ee_model_constant, FALSE)["end.unvax", "se-both"], "Endemic", "Age", "1A", "Both"),
  c(model_fit_info(age_ee_model_constant, FALSE)["ne.unvax", "est-both"], model_fit_info(age_ee_model_constant, FALSE)["ne.unvax", "se-both"], "Endemic", "Age", "1A", "Both"),
  c(model_fit_info(age_ee_model_varying, FALSE)["end.unvax", "est-endemic"], model_fit_info(age_ee_model_varying, FALSE)["end.unvax", "se-endemic"], "Endemic", "Age", "2A", "Endemic"),
  c(model_fit_info(age_ee_model_varying, FALSE)["ne.unvax", "est-epidemic"], model_fit_info(age_ee_model_varying, FALSE)["ne.unvax", "se-epidemic"], "Epidemic", "Age", "2A", "Epidemic"),
  c(model_fit_info(age_ee_model_varying, FALSE)["end.unvax", "est-both"], model_fit_info(age_ee_model_varying, FALSE)["end.unvax", "se-both"], "Endemic", "Age", "2A", "Both"),
  c(model_fit_info(age_ee_model_varying, FALSE)["ne.unvax", "est-both"], model_fit_info(age_ee_model_varying, FALSE)["ne.unvax", "se-both"], "Endemic", "Age", "2A", "Both"))
our_models_df <- as.data.frame(our_models_df)
names(our_models_df) <- c("est", "se", "Component", "unit", "weights", "inclusion")
our_models_df$est <- as.numeric(our_models_df$est)
our_models_df$se <- as.numeric(our_models_df$se)
our_models_df$Disease <- "COVID-19"
our_models_df$placement <- seq(2, 2 + dim(our_models_df)[1] / 10,		  length.out = dim(our_models_df)[1])
our_models_df$model <- our_models_df$weight
our_models_df_sens_2 <- our_models_df; rm(our_models_df)


## -----------------------------------------------------------------------------
our_models_df_sens_1$placement <- our_models_df_sens_1$placement + 3
our_models_df_sens_2$placement <- our_models_df_sens_2$placement + (3 * 2)
load(file = paste0(getwd(), "/output/model_comp.Rdata"))

our_models_df_sens_1$disease <- "COVID-19 (sensitivity analysis 1)"
our_models_df_sens_2$disease <- "COVID-19 (sensitivity analysis 2)"

plot_df <- merge(merge(merge(model_comp_df, our_models_df, all = TRUE),
                 our_models_df_sens_1, all = TRUE),
                 our_models_df_sens_2, all = TRUE)

ggplot(data = plot_df,
       aes(x = placement, y = est, colour = Disease,
           ymin = est - se, ymax = est + se, shape = Component)) +
  scale_colour_manual(values = c("Measles" = viridis(9)[8],
                                 "COVID-19" = viridis(9)[4],
                                 "COVID-19 (sensitivity analysis 1)" = viridis(9)[5],
                                 "COVID-19 (sensitivity analysis 2)" = viridis(9)[6])) +
  geom_pointrange() +
  scale_x_discrete(name = "Estimate sourced from the literature (measles) and estimated in current analysis\n with labels defined by time-varying transmission weight scheme (COVID-19)",
                   limits = c(0.05, 0.45, 1.15, 1.7,
                              mean(our_models_df$placement[1 : 4]),
                              mean(our_models_df$placement[5 : 8]),
                              mean(our_models_df$placement[9 : 12]),
                              mean(our_models_df$placement[13 : 16]),
                              mean(our_models_df$placement[17 : 20]),
                              mean(our_models_df_sens_1$placement[1 : 4]),
                              mean(our_models_df_sens_1$placement[5 : 8]),
                              mean(our_models_df_sens_1$placement[9 : 12]),
                              mean(our_models_df_sens_1$placement[13 : 16]),
                              mean(our_models_df_sens_1$placement[17 : 20]),
                              mean(our_models_df_sens_2$placement[1 : 4]),
                              mean(our_models_df_sens_2$placement[5 : 8]),
                              mean(our_models_df_sens_2$placement[9 : 12]),
                              mean(our_models_df_sens_2$placement[13 : 16]),
                              mean(our_models_df_sens_2$placement[17 : 20])),
                   labels = c("Herzog et al.", "Robert et al.",
                              "Nguyen et al.", "Lu and Meyer",
                              "1R", "2R", "1A", "2A",
                              "1R", "2R", "1A", "2A",
                              "1R", "2R", "1A", "2A")) +
  labs(y = expression(hat(beta))) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)),
         shape = guide_legend(override.aes = list(linetype = 0))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_vline(xintercept = 0.2, colour = "grey") +
  geom_vline(xintercept = 0.7, colour = "grey") +
  geom_vline(xintercept = 1.6, colour = "grey") +
  geom_vline(xintercept = 1.8, colour = "grey") +
  geom_vline(xintercept = 4.25, colour = "grey") +
  geom_vline(xintercept = 7.25, colour = "grey") +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title.position = "top"),
         shape = guide_legend(title.position = "top"))


## -----------------------------------------------------------------------------
# For projection later - start at first time point average
# vaccination coverage is 25%
start <- which.max(apply(vax_cov_covariate_a, 1, mean) > 0.25)
load(file = paste0(getwd(), "/output/models-age.Rdata"))

# [] ensures it goes back to matrix
vax_cov_covariate_a[] <- apply(vax_cov_covariate_a, 1, mean)
ggplot(data = melt(vax_cov_covariate_a),
       mapping = aes(y = value, x = dateISO,
                     group = altersklasse_covid19)) +
  geom_line() +
  facet_wrap(altersklasse_covid19 ~ ., scales = "free_y") +
  scale_x_discrete(labels = skip_labels(melt(vax_cov_covariate_a)$dateISO)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Coverage (second dose and booster with waning taken into account)") +
  ylim(0, 1)


## ----out.width="60%"----------------------------------------------------------
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

