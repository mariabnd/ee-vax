## ----"main-setup", include = FALSE--------------------------------------------
setwd("..")
# Check if in reports (relevant for the code extracted from the .Rnw files
# so they can be run by other researchers)
if(substring(getwd(), nchar(getwd()) - nchar("reports") + 1) == "reports"){
  setwd("..")
}
knitr::opts_knit$set(root.dir = getwd()) # Move up to main project folder
knitr::opts_chunk$set(out.width = "0.9\\linewidth")
library("ProjectTemplate")
load.project()


## ----facebook-mobility, fig.height=3------------------------------------------
  ggplot(data = data_mobi,
       mapping = aes(x = dateISO, colour = geoRegion,
                     shape = geoRegion, group = geoRegion,
                     y = movement_change)) +
  labs(x = "Week", y = expression(m["rt"]),
       title = "") +
  geom_hline(yintercept = 0, lty = 2) +
  geom_line() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_discrete(labels = skip_labels(data_mobi$dateISO)) +
  scale_colour_manual(values = viridis(26),
                      name = "Region") +
  scale_shape_manual(values = rep(c(15 : 19), each = 6),
                     name = "Region") +
  guides(colour = guide_legend(ncol = 4)) +
  plot_layout(guides = "collect") +
ggplot(data = data_mobi,
       mapping = aes(x = dateISO, y = avg_movement_change, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "bottom") +
  guides(colour = guide_legend(title.position = "top", ncol = 2)) +
  scale_x_discrete(labels = skip_labels(data_mobi$dateISO)) +
  labs(y = expression(m[t]), x = "Week") +
  geom_hline(yintercept = 0, lty = 2)


## ----contacts-policy-and-trace-plot, fig.height = 4---------------------------
p <- ggplot(data = data_poli,
       mapping = aes(x = dateISO, group = 1)) +
  geom_line(aes(y = C1_change + 0.01, colour = "School (C1)")) +
  geom_line(aes(y = C2_change + 0.02, colour = "Work (C2)")) +
  geom_line(aes(y = C6_change - 0.01, colour = "Household (C6)")) +
  geom_line(aes(y = C4_change, colour = "Other (C4)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "bottom") +
  guides(colour = guide_legend(title.position = "top", ncol = 2)) +
  scale_x_discrete(labels = skip_labels(data_poli$dateISO)) +
  labs(y = "Normalised indicator", x = "Week",
       colour = "Location") +
  scale_colour_manual(values = c("School (C1)" = viridis(9)[9],
                                 "Work (C2)" = viridis(9)[4],
                                 "Household (C6)" = viridis(9)[6],
                                 "Other (C4)" = viridis(9)[8]))

tmp <- merge(
  x = data.frame("Week" = as.factor(dimnames(contact_mats)[3][[1]]),
                 "trace" = sapply(seq_len(dim(contact_mats)[3]),
                                  FUN = function(mat) {
                                    sum(diag(contact_mats[, , mat]))
                                  }),
                 "type" = "Contact",
                 "constant" = sum(diag(Cgrouped))),
  y = data.frame("Week" = as.factor(dimnames(movement_mats)[3][[1]]),
                 "trace" = sapply(seq_len(dim(movement_mats)[3]),
                                  FUN = function(mat) {
                                    sum(diag(movement_mats[, , mat]))
                                  }),
                 "type" = "Mobility",
                 "constant" = sum(diag(1 / map_nbOrder))),
  all = TRUE)
p + ggplot(data = tmp,
           aes(x = Week, group = type)) +
    geom_line(aes(y = constant, lty = "Time-constant")) +
    geom_line(aes(y = trace, lty = "Trace")) +
    facet_wrap(. ~ type, scales = "free_y") +
    scale_x_discrete(labels = skip_labels(tmp$Week)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    expand_limits(y = 0) +
    scale_linetype_manual(values = c(3, 1), labels = c("Time-constant", "Trace")) +
    guides(linetype = guide_legend(ncol = 1, title.position = "top")) +
    labs(y = "", lty = "Value") +
    theme(legend.position = "bottom") &
    plot_layout(widths = c(1, 2))


## ----"tile-map", out.width = "100%", fig.width = 7, fig.height = 7------------
# Plot the tile map
tm_shape(mapCH) +
  tm_text("geoRegion", col = "black", size = 1.5) +
  tm_borders() +
# Crop figure margins so figure larger in manuscript
  tm_layout(frame = FALSE)#, outer.margins = c(0, -1, 0, -1))


## ----"adjacency-matrix", out.width = "100%", fig.width = 6, fig.height = 6----
tmp <- melt(mobility_for_plot)
tmp$value <- as.factor(tmp$value)
# Relabel socialmixr for panel plot
ggplot(data = tmp, aes_string(x = names(tmp)[1], y = names(tmp)[2], 
                              fill = names(tmp)[3])) + 
  geom_tile() +
  labs(x = "Region", y = "Region", title = "",
       fill = "Neighbourhood\n Order") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_viridis_d(direction = - 1) +
  coord_fixed() +
# Larger text
    theme(text = element_text(size = theme_get()$text$size + 1),
axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
legend.text = element_text(size = theme_get()$legend.text$size + 0.3))
rm(tmp)


## ----"contact-matrix", out.width = "100%", fig.width = 6, fig.height = 6------
tmp <- melt(contact_for_plot)
# Relabel socialmixr for panel plot
ggplot(data = tmp, aes_string(x = names(tmp)[1], y = names(tmp)[2], 
                              fill = names(tmp)[3])) + 
  geom_tile() + 
  labs(x = "Age of contact", y = "Age of participant", title = "", fill = "Contacts") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_viridis_c(direction = - 1) + coord_fixed()  +
# Larger text
    theme(text = element_text(size = theme_get()$text$size + 2),
axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
legend.text = element_text(size = theme_get()$legend.text$size + 0.3))


## ----snapshot-regions, fig.height = 5, fig.width = 11, out.width = "100%"-----
#expanded <- FALSE
assign("expanded", FALSE, .GlobalEnv)
source(paste0(getwd(), "/src/plot-time-varying-adjacency-matrices.R"))

wrap_plots(out[which(skip_labels(seq_len(length(out)), amount = 6.5) != "")],
           guides = "collect", tag_level = "keep",
           ncol = 4) &
theme(plot.margin = margin(t = 0.1, l = 0, b = 0, r = 0, unit = "cm"))


## ----snapshot-age, fig.height = 5, fig.width = 11, out.width = "100%"---------
#expanded <- FALSE
assign("expanded", FALSE, .GlobalEnv)
source(paste0(getwd(), "/src/plot-time-varying-contact-matrices.R"))
wrap_plots(out[which(skip_labels(seq_len(length(out)), amount = 6.5) != "")],
           guides = "collect", tag_level = "keep",
           ncol = 4) &
theme(plot.margin = margin(t = 0.1, l = 0, b = 0, r = 0, unit = "cm"))


## ----fig.width = 7, fig.height = 4--------------------------------------------
par(las = 1)
plot(waning(0 : 55, val = 1),
     type = "b", xlab = "p (weeks passed since vaccine given)",
     ylab = expression(u(p)), xaxt = "n",
     pch = 19, cex = 0.9)
axis(1, at = seq(0, 55, 5), labels = seq(0, 55, 5))


## -----------------------------------------------------------------------------
tmp <- melt(vax_cov_covariate_r)
tmp$dateISO <- factor(tmp$dateISO, levels = sort(levels(tmp$dateISO), decreasing = FALSE))
p1 <- ggplot(data = tmp, aes_string(y = names(tmp)[1],
                              x = names(tmp)[2],
                              fill = names(tmp)[3])) + 
  geom_tile() +
  scale_fill_viridis_c(direction = - 1) +
  labs(x = "Region", y = "Week", title = "Vaccination coverage", fill = expression(x[rt])) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  #scale_x_discrete(labels = skip_labels(tmp$geoRegion)) +
  scale_y_discrete(labels = skip_labels(tmp$dateISO))

tmp$value_log <- log(1 - tmp$value)
p2 <- ggplot(data = tmp, aes_string(y = names(tmp)[1],
                              x = names(tmp)[2],
                              fill = names(tmp)[4])) +
  geom_tile() +
  scale_fill_viridis_c(direction = - 1) +
  labs(x = "Region", y = "Week", title = "Vaccination coverage", fill = expression(log ~ "(1 - " ~ x[rt] ~ ")")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  #scale_x_discrete(labels = skip_labels(tmp$geoRegion)) +
  scale_y_discrete(labels = skip_labels(tmp$dateISO))

tmp <- melt(vax_cov_covariate_a)
tmp$dateISO <- factor(tmp$dateISO, levels = sort(levels(tmp$dateISO), decreasing = FALSE))
p3 <- ggplot(data = tmp, aes_string(y = names(tmp)[1],
                              x = names(tmp)[2],
                              fill = names(tmp)[3])) + 
  geom_tile() +
  scale_fill_viridis_c(direction = - 1) +
  labs(x = "Age group", y = "Week", title = "Vaccination coverage", fill = expression(x[at])) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  #scale_x_discrete(labels = skip_labels(tmp$altersklasse_covid19)) +
  scale_y_discrete(labels = skip_labels(tmp$dateISO))

tmp$value_log <- log(1 - tmp$value)
p4 <- ggplot(data = tmp, aes_string(y = names(tmp)[1],
                              x = names(tmp)[2],
                              fill = names(tmp)[4])) +
  geom_tile() +
  scale_fill_viridis_c(direction = - 1) +
  labs(x = "Age group", y = "Week", title = "Vaccination coverage", fill = expression(log ~ "(1 - " ~ x[at] ~ ")")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  #scale_x_discrete(labels = skip_labels(tmp$altersklasse_covid19)) +
  scale_y_discrete(labels = skip_labels(tmp$dateISO))


## ----coverage-covariate-plots, fig.height = 3, fig.width = 8------------------
(p1 + p3) #/
#(p2 + p4)


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

names(model_comp_df) <- c("placement", "model", "est", "se", "Component", "Disease")
model_comp_df$placement <- as.numeric(model_comp_df$placement)
model_comp_df$est <- as.numeric(model_comp_df$est)
model_comp_df$se <- as.numeric(model_comp_df$se)


## -----------------------------------------------------------------------------
source(paste0(getwd(), "/src/models-regions.R"))
load(file = paste0(getwd(), "/output/models-regions.Rdata"))
region_ee_model_constant <- ee_model_constant
region_ee_model_varying <- ee_model_varying
source(paste0(getwd(), "/src/models-age.R"))
load(file = paste0(getwd(), "/output/models-age.Rdata"))
age_ee_model_constant <- ee_model_constant
age_ee_model_varying <- ee_model_varying


## ----results = "asis"---------------------------------------------------------
all_models <- c(region_ee_model_constant,
                region_ee_model_varying,
                age_ee_model_constant,
                age_ee_model_varying)
#DSS_vals
tp <- length(date_range_full) - 1
invisible(capture.output(DSS_vals <- sapply(all_models, function(x){
  scores(oneStepAhead(x, tp = tp, type = "rolling", which.start = "final"),
         which = "dss", quiet = TRUE)
})))

goodness_of_fit <- cbind(
  rep(c("Region", "Age"), c(8, 8)),
  rep(c("Time-constant", "Time-varying",
        "Time-constant", "Time-varying"),
      c(4, 4, 4, 4)),
  rep(c("neither", "endemic", "epidemic", "both")),
  DSS_vals)
goodness_of_fit <- as.data.frame(goodness_of_fit)
goodness_of_fit$DSS_vals <- as.numeric(goodness_of_fit$DSS_vals)
names(goodness_of_fit) <- c("Unit", "Transmission weights", "Model", "DSS")


## -----------------------------------------------------------------------------
goodness_of_fit <- cbind(goodness_of_fit[1 : 8, ], goodness_of_fit[9 : 16, c("DSS")])
names(goodness_of_fit)[4 : 5] <- c("Region", "Age")
goodness_of_fit <- goodness_of_fit[, - which(names(goodness_of_fit) == "Unit")]

bold_rows <- c(which.min(goodness_of_fit[goodness_of_fit$`Transmission weights` == "Time-varying", ]$Region),
               which.min(goodness_of_fit[goodness_of_fit$`Transmission weights` == "Time-varying", ]$Age)) + 4


## ----results = "asis"---------------------------------------------------------
dec <- 2#3
goodness_of_fit$Region <- as.character(round(goodness_of_fit$Region, dec))
goodness_of_fit$Age <- as.character(round(goodness_of_fit$Age, dec))

goodness_of_fit[bold_rows[1], which(names(goodness_of_fit) == "Region")] <-
  cell_spec(goodness_of_fit[bold_rows[1], which(names(goodness_of_fit) == "Region")], "latex", bold = TRUE)
goodness_of_fit[bold_rows[2], which(names(goodness_of_fit) == "Age")] <-
  cell_spec(goodness_of_fit[bold_rows[2], which(names(goodness_of_fit) == "Age")], "latex", bold = TRUE)

add_header_above(
kable(goodness_of_fit,
      linesep = "",
      booktabs = getOption("booktabs"),
      format = "latex",
      escape = FALSE,
      row.names = FALSE),
c("", "", "DSS" = 2))


## ----fig.height=3-------------------------------------------------------------
plot_model(region_ee_model_varying, "total")

plot_model(age_ee_model_varying, "total")


## ----results = "asis"---------------------------------------------------------
res_table <- model_fit_info(c(region_ee_model_varying,
                age_ee_model_varying), se_bracket = TRUE,
                include_SE = FALSE)
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

rownames(res_table) <- gsub("end.sin\\(2 \\* pi \\* t/52\\)", "$\\\\gamma^{\\\\nu}_{\\\\sin}$",
                            rownames(res_table))
rownames(res_table) <- gsub("end.cos\\(2 \\* pi \\* t/52\\)", "$\\\\gamma^{\\\\nu}_{\\\\cos}$",
                            rownames(res_table))

rownames(res_table) <- gsub("ne.sin\\(2 \\* pi \\* t/52\\)", "$\\\\gamma^{\\\\phi}_{\\\\sin}$",
                            rownames(res_table))
rownames(res_table) <- gsub("ne.cos\\(2 \\* pi \\* t/52\\)", "$\\\\gamma^{\\\\phi}_{\\\\cos}$",
                            rownames(res_table))

kbl <- column_spec(
    kable(res_table,
          booktabs = options()$booktabs,
          linesep = options()$linesep,
          escape = FALSE,
          align = "lrrrrrrrr"),
    column = c(3, 7),
    bold = TRUE, latex_column_spec = "r")

add_header_above(kbl,
  c("", "Region" = 4, "Age" = 4))


## ----fig.height = 3, fig.width = 8--------------------------------------------
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
save(model_comp_df, our_models_df, file = "output/model_comp.Rdata")

ggplot(data = merge(model_comp_df, our_models_df, all = TRUE),
       aes(x = placement, y = est, colour = Disease,
           ymin = est - se, ymax = est + se, shape = Component)) +
  scale_colour_manual(values = c("Measles" = viridis(9)[8],
                                 "COVID-19" = viridis(9)[4])) +
  geom_pointrange() +
  scale_x_discrete(name = "Estimate sourced from the literature (measles) and estimated in current analysis\n with labels defined by time-varying transmission weight scheme (COVID-19)",
                   limits = c(0.05, 0.45, 1.15, 1.7,
                              mean(our_models_df$placement[1 : 4]),
                              mean(our_models_df$placement[5 : 8]),
                              mean(our_models_df$placement[9 : 12]),
                              mean(our_models_df$placement[13 : 16]),
                              mean(our_models_df$placement[17 : 20])),
                   labels = c("Herzog et al.", "Robert et al.",
                              "Nguyen et al.", "Lu and Meyer",
                              "1R", "2R", "1A", "2A")) +
  labs(y = expression(hat(beta))) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)),
         shape = guide_legend(override.aes = list(linetype = 0))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_vline(xintercept = 0.2, colour = "grey") +
  geom_vline(xintercept = 0.7, colour = "grey") +
  geom_vline(xintercept = 1.6, colour = "grey") +
  geom_vline(xintercept = 1.8, colour = "grey")# +
#geom_hline(yintercept = 0, lty = 2)


## -----------------------------------------------------------------------------
plot(region_ee_model_varying$endemic, type = "ri", component = "end", labels = TRUE,
      main = "Endemic component",
     col.regions = hcl.colors(100, "viridis", rev = TRUE),
     par.settings = list(axis.line = list(col = "transparent")))


## -----------------------------------------------------------------------------
plot(region_ee_model_varying$endemic, type = "ri", component = "ne", labels = TRUE,
      main = "Epidemic component",
     col.regions = hcl.colors(100, "viridis", rev = TRUE),
     par.settings = list(axis.line = list(col = "transparent")))


## -----------------------------------------------------------------------------
best_models <- all_models[bold_rows]
region_model <- best_models[[1]]
age_model <- best_models[[2]]

# The predictions come from the individual .rnw files for
# models-region and models-age

load(file = "output/prediction-regions.Rdata")
ee_pred_orig_varying_r <- ee_pred_orig
ee_pred_scen_varying_r <- ee_pred_scen
load(file = "output/prediction-age.Rdata")
ee_pred_orig_varying_a <- ee_pred_orig
ee_pred_scen_varying_a <- ee_pred_scen

save(list = ls()[grepl("ee_pred", ls())],
     file = "output/prediction-all.Rdata")


## ----fig.height=8, fig.width=10-----------------------------------------------
load(file = "output/prediction-all.Rdata")
load(file = "output/ratio-regions.Rdata")
ratio_df_r <- ratio_df
plot_prediction(alt = ee_pred_scen_varying_r,
                orig = ee_pred_orig_varying_r,
                uncertainty = TRUE,
                ratio_df = ratio_df_r, unit = "total",
                mod_opt = "endemic")

load(file = "output/ratio-age.Rdata")
ratio_df_a <- ratio_df
plot_prediction(alt = ee_pred_scen_varying_a,
                orig = ee_pred_orig_varying_a,
                uncertainty = TRUE,
                ratio_df = ratio_df_a, unit = "total",
                mod_opt = "endemic")

