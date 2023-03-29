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
# Ensure plots are square
knitr::opts_chunk$set(fig.width = 5, fig.height = 5)
load("output/matrices.Rdata")


## ----results = "asis"---------------------------------------------------------
kable(
  cantons_lookup[, c("geoRegion", "geoRegion_label")],
  col.names = c("Abbreviation", "Region"),
  linesep = getOption("linesep"),
  booktabs = getOption("booktabs"),
  row.names = FALSE)


## ----population, results = "asis"---------------------------------------------
kable(popCH,
      linesep = getOption("linesep"),
      booktabs = getOption("booktabs"))


## ----contacts-time-varying----------------------------------------------------
#expanded <- FALSE
assign("expanded", FALSE, .GlobalEnv)
source(paste0(getwd(), "/src/plot-time-varying-contact-matrices.R"))
out


## ----time-varying-contact-mats, fig.height = 16, fig.width = 10---------------
#expanded <- FALSE
assign("expanded", FALSE, .GlobalEnv)
source(paste0(getwd(), "/src/plot-time-varying-contact-matrices.R"))
wrap_plots(out,
           guides = "collect", tag_level = "keep",
           ncol = 6)


## ----adjacency-time-varying---------------------------------------------------
#expanded <- FALSE
assign("expanded", FALSE, .GlobalEnv)
source(paste0(getwd(), "/src/plot-time-varying-adjacency-matrices.R"))
out


## ----time-varying-movement-mats, fig.height = 16, fig.width = 10--------------
#expanded <- FALSE
assign("expanded", FALSE, .GlobalEnv)
source(paste0(getwd(), "/src/plot-time-varying-adjacency-matrices.R"))
wrap_plots(out,
           guides = "collect", tag_level = "keep",
           ncol = 6)


## ----coverage, out.height = "70%", fig.height = 7, fig.width = 16-------------
#pdf("plot.pdf", height = 6, width = 8)
(ggplot(data = data_vax_1st,
        mapping = aes(x = dateISO,
                      y = cov_1,
                      group = groups,
                      colour = geoRegion,
                      shape = geoRegion)) +
   labs(x = "Week", y = expression(x[art]^"(1)"),
        title = "Partial immunity (first dose)") +
   ylim(0, 1) +
   geom_line() +
   geom_point() +
   facet_wrap(altersklasse_covid19 ~ .) +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
         legend.position = "bottom") +
   scale_x_discrete(labels = skip_labels(data_vax_1st$dateISO)) +
   scale_colour_manual(values = viridis(26),
                       name = "Canton") +
   scale_shape_manual(values = rep(c(15 : 19), each = 6),
                      name = "Canton")) +
  (ggplot(data = data_vax_2nd,
          mapping = aes(x = dateISO,
                        y = cov_1,
                        group = groups,
                        colour = geoRegion,
                        shape = geoRegion)) +
     labs(x = "Week", y = expression(x[art]^"(2)"),
          title = "Full immunity (second dose) before waning applied") +
     ylim(0, 1) +
     geom_line() +
     geom_point() +
     facet_wrap(altersklasse_covid19 ~ .) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
           legend.position = "bottom") +
     scale_x_discrete(labels = skip_labels(data_vax_2nd$dateISO)) +
     scale_colour_manual(values = viridis(26),
                         name = "Canton") +
     scale_shape_manual(values = rep(c(15 : 19), each = 6),
                        name = "Canton")) +
  (ggplot(data = data_vax_3rd,
          mapping = aes(x = dateISO,
                        y = cov_1,
                        group = groups,
                        colour = geoRegion,
                        shape = geoRegion)) +
     labs(x = "Week", y = expression(x[art]^"(3)"),
          title = "Booster (third dose) before waning applied") +
     ylim(0, 1) +
     geom_line() +
     geom_point() +
     facet_wrap(altersklasse_covid19 ~ .) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
           legend.position = "bottom") +
     scale_x_discrete(labels = skip_labels(data_vax_3rd$dateISO)) +
     scale_colour_manual(values = viridis(26),
                         name = "Canton") +
     scale_shape_manual(values = rep(c(15 : 19), each = 6),
                        name = "Canton")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


## ----waning-function, eval = FALSE, include = FALSE---------------------------
## plot(waning(0 : 55, val = 1),
##      type = "b", xlab = "p (weeks passed since vaccine given)",
##      main = "Waning", ylab = expression(u(p)), xaxt = "n")
## axis(1, at = c(0, 2, 26, 39, 52), labels = c(0, 2, 26, 39, 52))


## ----eval = FALSE, coverage-waned, out.height = "70%", fig.height = 7, fig.width = 16----
## (ggplot(data = data_vax_2nd,
##         mapping = aes(x = dateISO,
##                       y = cov_1_waned,
##                       group = groups,
##                       colour = geoRegion,
##                       shape = geoRegion)) +
##     labs(x = "Week", y = expression(x[art]^"(2)" ~ "(waned)"),
##          #expression(w ~ "(t-s)" ~ v[art]^"(2)" ~ "/" ~ e[ar]),
##          title = "Full immunity (second dose) after waning applied") +
##     ylim(0, 1) +
##     geom_line() +
##     geom_point() +
##     facet_wrap(altersklasse_covid19 ~ .) +
##     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
##           legend.position = "bottom") +
##     scale_x_discrete(labels = skip_labels(data_vax_2nd$dateISO)) +
##     scale_colour_manual(values = viridis(26),
##                         name = "Canton") +
##     scale_shape_manual(values = rep(c(15 : 19), each = 6),
##                        name = "Canton")) +
##   (ggplot(data = data_vax_3rd,
##           mapping = aes(x = dateISO,
##                         y = cov_1_waned,
##                         group = groups,
##                         colour = geoRegion,
##                         shape = geoRegion)) +
##      labs(x = "Week", y = expression(x[art]^"(3)" ~ "(waned)"),
##           #expression(w ~ "(t-s)" ~ v[art]^"(3)" ~ "/" ~ e[ar]),
##           title = "Booster (third dose) after waning applied") +
##      ylim(0, 1) +
##      geom_line() +
##      geom_point() +
##      facet_wrap(altersklasse_covid19 ~ .) +
##      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
##            legend.position = "bottom") +
##      scale_x_discrete(labels = skip_labels(data_vax_3rd$dateISO)) +
##      scale_colour_manual(values = viridis(26),
##                          name = "Canton") +
##      scale_shape_manual(values = rep(c(15 : 19), each = 6),
##                         name = "Canton")) +
##   (ggplot(data = vax_cov_data,
##           mapping = aes(x = dateISO,
##                         y = vax_cov,
##                         group = groups,
##                         colour = geoRegion,
##                         shape = geoRegion)) +
##      labs(x = "Week", y = expression(x[art]),
##           title = "Vaccination coverage") +
##      ylim(0, 1) +
##      geom_line() +
##      geom_point() +
##      facet_wrap(altersklasse_covid19 ~ .) +
##      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
##            legend.position = "bottom") +
##      scale_x_discrete(labels = skip_labels(vax_cov_data$dateISO)) +
##      scale_colour_manual(values = viridis(26),
##                          name = "Canton") +
##      scale_shape_manual(values = rep(c(15 : 19), each = 6),
##                         name = "Canton")) +
##   plot_layout(guides = "collect") &
##   theme(legend.position = "bottom")


## ----cases-plot, fig.height = 18, fig.width = 5, eval = FALSE-----------------
## cex <- 1
## ggplot(data = aggregate(entries ~ dateISO + altersklasse_covid19,
##                         x = data_cases, FUN = sum),
##        mapping = aes(x = dateISO, y = entries, group = 1)) +
##   geom_col() +
##   facet_grid(altersklasse_covid19 ~ ., scales = "free_y") +
##   scale_x_discrete(labels = skip_labels(data_cases$dateISO)) +
##   labs(y = "Cases", x = "") +
##   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
##   theme(axis.title.x = element_text(size = rel(cex)),
##         axis.text.x = element_text(size = rel(cex)),
##         axis.title.y = element_text(size = rel(cex)),
##         axis.text.y = element_text(size = rel(cex)),
##         legend.text = element_text(size = rel(cex)),
##         legend.title = element_text(size = rel(cex)))
## 
## ggplot(data = aggregate(entries ~ dateISO + geoRegion, x = data_cases, FUN = sum),
##        mapping = aes(x = dateISO, y = entries)) +
##   geom_col() +
##   facet_grid(geoRegion ~ ., scales = "free_y") +
##   scale_x_discrete(labels = skip_labels(data_cases$dateISO)) +
##   labs(y = "Cases", x = "") +
##   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
##   theme(axis.title.x = element_text(size = rel(cex)),
##         axis.text.x = element_text(size = rel(cex)),
##         axis.title.y = element_text(size = rel(cex)),
##         axis.text.y = element_text(size = rel(cex)),
##         legend.text = element_text(size = rel(cex)),
##         legend.title = element_text(size = rel(cex)))
## 
## ggplot(data = data_cases,
##        mapping = aes(x = dateISO, y = entries, group = groups)) +
##   geom_col() +
##   facet_grid(geoRegion ~ altersklasse_covid19, scales = "free_y") +
##   scale_x_discrete(labels = skip_labels(data_cases$dateISO)) +
##   labs(y = "Cases", x = "") +
##   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
##   theme(axis.title.x = element_text(size = rel(cex)),
##         axis.text.x = element_text(size = rel(cex)),
##         axis.title.y = element_text(size = rel(cex)),
##         axis.text.y = element_text(size = rel(cex)),
##         legend.text = element_text(size = rel(cex)),
##         legend.title = element_text(size = rel(cex)))


## ----fig.height = 4, fig.width = 8--------------------------------------------
model_comp_df <- data.frame(
  rbind(c(0, "Herzog et al.", 1.52, 0.29,
          "Endemic", "Measles"),
        c(0.1, "Herzog et al.", 1.38, 0.23,
          "Epidemic", "Measles"),
        c(0.3, "Robert et al.", 0.37, (0.91 - (- 0.17)) / 3.92,
          "Endemic", "Measles"),
        c(0.5, "Robert et al.", 0.14, (0.24 - 0.03) / 3.92,
          "Epidemic", "Measles"),
        c(0.4, "Robert et al.", 0.48, (0.80 - 0.17) / 3.92,
          "Endemic", "Measles"),
        c(0.6, "Robert et al.", 0.19, (0.29 - 0.09) / 3.92,
          "Epidemic", "Measles"),
        c(0.8, "Nguyen et al.", 0.158, 0.020,
          "Endemic", "Measles"),
        c(0.9, "Nguyen et al.", 0.172, 0.022,
          "Endemic", "Measles"),
        c(1, "Nguyen et al.", 0.257, 0.024,
          "Endemic", "Measles"),
        c(1.1, "Nguyen et al.", 0.434, 0.167,
          "Endemic", "Measles"),
        c(1.2, "Nguyen et al.", 0.320, 0.036,
          "Epidemic", "Measles"),
        c(1.3, "Nguyen et al.", 0.300, 0.037,
          "Epidemic", "Measles"),
        c(1.4, "Nguyen et al.", 0.403, 0.054,
          "Epidemic", "Measles"),
        c(1.5, "Nguyen et al.", 0.063, 0.079,
          "Epidemic", "Measles"),
        c(1.7, "Lu and Meyer", 4.22669, 0.04823,
          "Endemic", "Measles")))

names(model_comp_df) <- c("placement", "model", "est", "se", "component", "disease")
model_comp_df$placement <- as.numeric(model_comp_df$placement)
model_comp_df$est <- as.numeric(model_comp_df$est)
model_comp_df$se <- as.numeric(model_comp_df$se)

p1 <- ggplot(data = model_comp_df, aes(x = placement, y = est, colour = component,
                      ymin = est - se, ymax = est + se)) +
  scale_colour_manual(values = c("Endemic" = viridis(9)[8],
                                   "Epidemic" = viridis(9)[4])) +
  geom_pointrange() +
  labs(y = "Vaccination coverage covariate effect estimate", x = "") +
  facet_wrap(. ~ model, scales = "free", ncol = 4) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
p2 <- ggplot(data = model_comp_df, aes(x = placement, y = est, colour = component,
                      ymin = est - se, ymax = est + se)) +
  scale_colour_manual(values = c("Endemic" = viridis(9)[8],
                                   "Epidemic" = viridis(9)[4])) +
  geom_pointrange() +
  scale_x_discrete(name = "Model",
                   limits = c(0.05, 0.45, 1.15, 1.7),
                   labels = c("Herzog et al.", "Robert et al.",
                              "Nguyen et al.", "Lu and Meyer")) +
  labs(y = "") +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
p1 + p2 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


## ----case-distribution-endemic, fig.width=10----------------------------------
load(file = "output/prediction-all.Rdata")

tmap_arrange(tm_shape(merge(mapCH, data.frame("geoRegion" = colnames(ee_pred_orig_varying_r$endemic$mu_matrix),
           "Original" = colMeans(ee_pred_orig_varying_r$endemic$mu_matrix)))) +
  tm_polygons("Original", palette = viridis(9, direction = - 1),
              breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000),
              title = "Predicted cases (mean)") +
  tm_text("geoRegion", col = "black") +
  tm_borders() +
  tm_layout(frame = FALSE, title = "Original",
            legend.position = c(0.75, 0.1),
            legend.text.size = 0.7,
              legend.title.size = 1),
tm_shape(merge(mapCH, data.frame("geoRegion" = colnames(ee_pred_scen_varying_r$endemic$mu_matrix),
           "Alternative" = colMeans(ee_pred_scen_varying_r$endemic$mu_matrix)))) +
  tm_polygons("Alternative", palette = viridis(9, direction = - 1),
              breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)) +
  tm_text("geoRegion", col = "black") +
  tm_borders() +
  tm_layout(frame = FALSE, title = "Alternative") +
    tm_layout(legend.show = FALSE))

ggplot(data = melt(ee_pred_orig_varying_a$endemic$mu_matrix),
       aes(x = Var1, y = value, fill = Var2, group = Var2)) +
  geom_area(stat = "identity", position = "fill") +
  labs(x = "Week", y = "Predicted proportion", fill = "Age group", title = "Original") +
  scale_fill_manual(values = viridis(9)) +
  scale_x_discrete(labels = skip_labels(melt(ee_pred_orig_varying_a$endemic$mu_matrix)$Var1)) +
  ggplot(data = melt(ee_pred_scen_varying_a$endemic$mu_matrix),
       aes(x = Var1, y = value, fill = Var2, group = Var2)) +
  geom_area(stat = "identity", position = "fill") +
  labs(x = "Week", y = "Predicted proportion", fill = "Age group", title = "Alternative") +
  scale_x_discrete(labels = skip_labels(melt(ee_pred_scen_varying_a$endemic$mu_matrix)$Var1)) +
  scale_fill_manual(values = viridis(9)) +
  plot_layout(guides = "collect")


## ----results = "asis"---------------------------------------------------------
predicted_vals_r <- data.frame("true" = colSums(ee_pred_orig_varying_r$neither$realizations_matrix),
"endemic_orig" = colSums(ee_pred_orig_varying_r$endemic$mu_matrix),
"epidemic_orig" = colSums(ee_pred_orig_varying_r$epidemic$mu_matrix),
"both_orig" = colSums(ee_pred_orig_varying_r$both$mu_matrix),
"endemic_scen" = colSums(ee_pred_scen_varying_r$endemic$mu_matrix),
"epidemic_scen" = colSums(ee_pred_scen_varying_r$epidemic$mu_matrix),
"both_scen" = colSums(ee_pred_scen_varying_r$both$mu_matrix))

save <- colnames(predicted_vals_r)
colnames(predicted_vals_r) <- c("True", "Endemic", "Epidemic", "Both",
                    "Endemic", "Epidemic", "Both")
add_header_above(
  kable_input = kable(predicted_vals_r,
                      booktabs = options()$booktabs,
      linesep = options()$linesep),
  header = c("", "", "Original coverage" = 3, "Alternative coverage" = 3))
colnames(predicted_vals_r) <- save
rm(save)


## ----results = "asis"---------------------------------------------------------
predicted_vals_a <- data.frame("true" = colSums(ee_pred_orig_varying_a$neither$realizations_matrix),
"endemic_orig" = colSums(ee_pred_orig_varying_a$endemic$mu_matrix),
"epidemic_orig" = colSums(ee_pred_orig_varying_a$epidemic$mu_matrix),
"both_orig" = colSums(ee_pred_orig_varying_a$both$mu_matrix),
"endemic_scen" = colSums(ee_pred_scen_varying_a$endemic$mu_matrix),
"epidemic_scen" = colSums(ee_pred_scen_varying_a$epidemic$mu_matrix),
"both_scen" = colSums(ee_pred_scen_varying_a$both$mu_matrix))

save <- colnames(predicted_vals_a)
colnames(predicted_vals_a) <- c("True", "Endemic", "Epidemic", "Both",
                    "Endemic", "Epidemic", "Both")
add_header_above(
  kable_input = kable(predicted_vals_a,
                      booktabs = options()$booktabs,
      linesep = options()$linesep),
  header = c("", "", "Original coverage" = 3, "Alternative coverage" = 3))
colnames(predicted_vals_a) <- save
rm(save)

