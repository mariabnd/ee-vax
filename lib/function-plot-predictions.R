#' Plot predictions
#' 
#' @description Given a list of model fits plot the models side by side
#' @param alt List of hhh4 predictions under the alternative scenario
#' @param orig List of hhh4 predictions under the original scenario
#' @param unit Unit to subset to, defaults to "all" (no subsetting)
#' @param mod_opt Which model to plot
#' @seealso model_fit_data
plot_prediction <- function(alt, orig, unit = "all",
                            uncertainty = FALSE,
                            ratio_df = NULL,
                            mod_opt = c("endemic", "epidemic", "both", "all")){
  mod_opt <- match.arg(mod_opt)
  if(class(alt) != "list"){
    stop("alt must be a list")
  }
  if(class(orig) != "list"){
    stop("orig must be a list")
  }
  if(!("predictive_moments_hhh4" %in% unique(sapply(alt, class)))){
    stop("alt must contain predictive moments of hhh4 models")
  }
  if(!("predictive_moments_hhh4" %in% unique(sapply(orig, class)))){
    stop("orig must contain predictive moments of hhh4 models")
  }
  true <- melt(orig$neither$realizations_matrix)
  colnames(true)[1 : 2] <- c("dateISO", "unit")
  alt <- alt[which(names(alt) != "neither")]
  orig <- orig[which(names(orig) != "neither")]
  
  extract_preds <- function(dat){
    dat <- lapply(dat, "[[", which(names(dat[[1]]) == "mu_matrix"))
    dat <- lapply(dat, "melt", 1)
    dat <- do.call(rbind, dat)
    dat$model <- gsub(".\\d", "", rownames(dat))
    colnames(dat)[1 : 2] <- c("dateISO", "unit")
    rownames(dat) <- NULL
    return(dat)
  }
  
  alt <- extract_preds(alt)
  orig <- extract_preds(orig)
  
  orig$name <- "original"
  alt$name <- "alternative"
  
  df <- merge(orig, alt, all = TRUE)
  df$model <- gsub("\\d", "", df$model)
  
  true <- do.call(rbind, list(data.frame(true, "model" = "endemic"),
                              data.frame(true, "model" = "epidemic"),
                              data.frame(true, "model" = "both")))
  
  if(!is.null(ratio_df)){
    plot_df <- Reduce(function(x, y) merge(x, y, all = TRUE), ratio_df)
    if(mod_opt != "all"){
      plot_df <- plot_df[plot_df$model == mod_opt, ]
      plot_df$model <- as.factor(plot_df$model)
    }
  } else {
    warning("If using uncertainty  = TRUE you must provide a data set with values")
  }
  if(mod_opt != "all"){
    df <- df[df$model == mod_opt, ]
    true <- true[true$model == mod_opt, ]
  }
  df$model <- as.factor(df$model)
  df$name <- as.factor(df$name)
  if(unit == "all"){
    for(i in unique(df$unit)){
      unit <- i
      p1 <- ggplot() +
        geom_line(data = df[df$unit == unit, ],
                  aes(x = dateISO, y = value, colour = name, group = unit)) +
        geom_point(data = true[true$unit == unit, ],
                   aes(x = dateISO, y = value, group = unit), size = 0.1) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              text = element_text(size = theme_get()$text$size + 1),
              axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
              axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
              legend.text = element_text(size = theme_get()$legend.text$size + 0.3)) +
        labs(y = paste0("Predicted cases (unit ", unit, ")"), x = "Week",
             colour = "Scenario") +
        scale_x_discrete(labels = skip_labels(df$dateISO)) +
        scale_colour_manual(values = c("original" = viridis(9)[8],
                                       "alternative" = viridis(9)[4])) +
        facet_grid(. ~ name)
      if(uncertainty){
        p2 <- ggplot(plot_df[plot_df$Var2 == unit, ],
                     aes(x = Var1, y = value,
                         group = Var2)) +
          geom_ribbon(aes(ymin = q10, ymax = q90, fill = "10th/90th percentile"), alpha = 0.3) +
          geom_ribbon(aes(ymin = q25, ymax = q75, fill = "25th/75th percentile"), alpha = 0.3) +
          geom_line() +
          geom_hline(yintercept = 1, lty = 3) +
          expand_limits(y = 0) +
          scale_x_discrete(labels = skip_labels(plot_df$Var1)) +
          labs(y = "Ratio between scenarios \n change in COVID-19 cases", x = "Week", fill = "Range") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                text = element_text(size = theme_get()$text$size + 1),
                axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
                axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
                legend.text = element_text(size = theme_get()$legend.text$size + 0.3))
      } else {
        p2 <- ggplot(data = df[df$unit == unit & df$name == "alternative", ],
                     aes(x = dateISO, y = df[df$unit == unit & df$name == "alternative", ]$value / df[df$unit == unit & df$name == "original", ]$value,
                         group = unit)) +
          geom_line() +
          labs(y = "Ratio between scenarios \n change in COVID-19 cases",
               x = "Week") +
          scale_x_discrete(labels = skip_labels(df$dateISO)) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                text = element_text(size = theme_get()$text$size + 1),
                axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
                axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
                legend.text = element_text(size = theme_get()$legend.text$size + 0.3)) +
          expand_limits(y = 0) +
          geom_hline(yintercept = 1, lty = 3)
      }
      p <- p1 / (p2 + scale_y_log10()) + 
        plot_layout(widths = c(2, 1))
      #p <- p + plot_layout(guides = "collect")
      print(p)
    }
  }
  if(unit == "total"){
    p1 <- ggplot() +
      geom_line(data = aggregate(value ~ dateISO + model + name, df, sum),
                aes(x = dateISO, y = value, colour = name, group = 1)) +
      geom_point(data = aggregate(value ~ dateISO, true, sum),
                 aes(x = dateISO, y = value), size = 0.1) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            text = element_text(size = theme_get()$text$size + 1),
            axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
            axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
            legend.text = element_text(size = theme_get()$legend.text$size + 0.3)) +
      labs(y = "Predicted cases (summed across units)", x = "Week",
           colour = "Scenario") +
      scale_x_discrete(labels = skip_labels(aggregate(value ~ dateISO + model + name, df, sum)$dateISO)) +
      scale_colour_manual(values = c("original" = viridis(9)[8],
                                     "alternative" = viridis(9)[4])) +
      facet_grid(. ~ name)
    if(uncertainty){
      p2 <-
        ggplot(
          merge(merge(merge(merge(aggregate(value ~ Var1 + model, plot_df, mean),
                                  aggregate(q10 ~ Var1 + model, plot_df, mean)),
                            aggregate(q90 ~ Var1 + model, plot_df, mean)),
                      aggregate(q25 ~ Var1 + model, plot_df, mean)),
                aggregate(q75 ~ Var1 + model, plot_df, mean)),
          aes(x = Var1, y = value,
              group = 1)) +
        geom_ribbon(aes(ymin = q10, ymax = q90, fill = "10th/90th percentile"), alpha = 0.3) +
        geom_ribbon(aes(ymin = q25, ymax = q75, fill = "25th/75th percentile"), alpha = 0.3) +
        geom_line() +
        geom_hline(yintercept = 1, lty = 3) +
        scale_x_discrete(labels = skip_labels(plot_df$Var1)) +
        labs(y = "Ratio between scenarios \n change in COVID-19 cases \n(averaged across units)", x = "Week", fill = "Range") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              text = element_text(size = theme_get()$text$size + 1),
              axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
              axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
              legend.text = element_text(size = theme_get()$legend.text$size + 0.3))
    } else {
      p2 <- ggplot(data = aggregate(value ~ dateISO + model + name, df, mean)[aggregate(value ~ dateISO + model + name, df, sum)$name == "alternative", ],
                   aes(x = dateISO,
                       y = aggregate(value ~ dateISO + model + name, df, mean)[aggregate(value ~ dateISO + model + name, df, sum)$name == "alternative", ]$value /
                         aggregate(value ~ dateISO + model + name, df, mean)[aggregate(value ~ dateISO + model + name, df, sum)$name == "original", ]$value,
                       group = 1)) +
        geom_line() +
        labs(y = "Ratio between scenarios \n change in COVID-19 cases \n(averaged across units)",
             x = "Week") +
        scale_x_discrete(labels = skip_labels(df$dateISO)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              text = element_text(size = theme_get()$text$size + 1),
              axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
              axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
              legend.text = element_text(size = theme_get()$legend.text$size + 0.3)) +
        expand_limits(y = 0) +
        geom_hline(yintercept = 1, lty = 3)
    }
    p <- p1 / (p2 + scale_y_log10()) + 
      plot_layout(widths = c(2, 1))
    #p <- p + plot_layout(guides = "collect")
    print(p)
  } else { # For specific unit = "something" call
    p1 <- ggplot() +
      geom_line(data = df[df$unit == unit, ],
                aes(x = dateISO, y = value, colour = name, group = unit)) +
      geom_point(data = true[true$unit == unit, ],
                 aes(x = dateISO, y = value, group = unit), size = 0.1) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            text = element_text(size = theme_get()$text$size + 1),
            axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
            axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
            legend.text = element_text(size = theme_get()$legend.text$size + 0.3)) +
      labs(y = paste0("Predicted cases (unit ", unit, ")"), x = "Week",
           colour = "Scenario") +
      scale_x_discrete(labels = skip_labels(df$dateISO)) +
      scale_colour_manual(values = c("original" = viridis(9)[8],
                                     "alternative" = viridis(9)[4])) +
      facet_grid(. ~ name)
    if(uncertainty){
      p2 <- ggplot(plot_df[plot_df$Var2 == unit, ],
                   aes(x = Var1, y = value,
                       group = Var2)) +
        geom_ribbon(aes(ymin = q10, ymax = q90, fill = "10th/90th percentile"), alpha = 0.3) +
        geom_ribbon(aes(ymin = q25, ymax = q75, fill = "25th/75th percentile"), alpha = 0.3) +
        geom_line() +
        geom_hline(yintercept = 1, lty = 3) +
        expand_limits(y = 0) +
        scale_x_discrete(labels = skip_labels(plot_df$Var1)) +
        labs(y = "Ratio between scenarios \n change in COVID-19 cases", x = "Week", fill = "Range") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              text = element_text(size = theme_get()$text$size + 1),
              axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
              axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
              legend.text = element_text(size = theme_get()$legend.text$size + 0.3))
    } else {
      p2 <- ggplot(data = df[df$unit == unit & df$name == "alternative", ],
                   aes(x = dateISO, y = df[df$unit == unit & df$name == "alternative", ]$value / df[df$unit == unit & df$name == "original", ]$value,
                       group = unit)) +
        geom_line() +
        labs(y = "Ratio between scenarios \n change in COVID-19 cases",
             x = "Week") +
        scale_x_discrete(labels = skip_labels(df$dateISO)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              text = element_text(size = theme_get()$text$size + 1),
              axis.text = element_text(size = theme_get()$axis.text$size + 0.3),
              axis.title = element_text(size = theme_get()$axis.text$size + 0.6),
              legend.text = element_text(size = theme_get()$legend.text$size + 0.3)) +
        expand_limits(y = 0) +
        geom_hline(yintercept = 1, lty = 3)
    }
    p <- p1 / (p2 + scale_y_log10()) + 
      plot_layout(widths = c(2, 1))
    #p <- p + plot_layout(guides = "collect")
    print(p)
  }
}
