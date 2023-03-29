#' Extract model fit as data frame
#' 
#' @description Given a list of model fits extract the observed cases as well as
#' the fitted values
#' @param mod_list List of hhh4 models
#' @returns a data frame
#' @seealso plot_model
model_fit_data <- function(mod_list){
  if(class(mod_list) != "list"){
    stop("mod_list must be a list")
  }
  if(unique(sapply(mod_list, class)) != "hhh4"){
    stop("mod_list must contain hhh4 models")
  }
  if(length(mod_list) != 4){
    stop("mod_list must be of length four 
         (function is specific to analyses for COVID-19 and may not work unless
         similar dimensions used)")
  }
  fit_df <- lapply(mod_list, function(mod){
    inference_dates <- rownames(mod$stsObj@observed[mod$control$subset, ])
    # Predicted mean
    mod$terms <- terms(mod)
    fitted <- meanHHH(theta = unname(mod$coefficients), model = mod$terms, 
                      subset = mod$control$subset, total.only = FALSE)
    rownames(fitted$endemic) <- inference_dates
    rownames(fitted$epidemic) <- inference_dates
    
    df <- rbind(data.frame(melt(fitted$endemic), type = "Endemic"),
                data.frame(melt(fitted$epidemic), type = "Epidemic"))
    
    tmp <- mod$stsObj@observed
    tmp <- melt(tmp)
    names(tmp)[names(tmp) == "value"] <- "observed"
    # Add observed values to data frame of fitted values
    df <- merge(df, tmp)
    
    return(as.data.frame(df))
  })
  
  fit_df[[1]]$model <- names(mod_list)[1]
  fit_df[[2]]$model <- names(mod_list)[2]
  fit_df[[3]]$model <- names(mod_list)[3]
  fit_df[[4]]$model <- names(mod_list)[4]
  
  df <- do.call(rbind, fit_df)
  df$type <- factor(df$type, levels = c("Epidemic", "Endemic"))
  return(df)
}

#' Plot model fit
#' 
#' @description Given a list of model fits plot the models side by side
#' @param mod_list List of hhh4 models
#' @param unit Unit to subset to, defaults to "all" (no subsetting)
#' @seealso model_fit_data
plot_model <- function(mod_list, unit = "all"){
  if(class(mod_list) != "list"){
    stop("mod_list must be a list")
  }
  if(unique(sapply(mod_list, class)) != "hhh4"){
    stop("mod_list must contain hhh4 models")
  }
  if(length(mod_list) != 4){
    stop("mod_list must be of length four 
         (function is specific to analyses for COVID-19 and may not work unless
         similar dimensions used)")
  }
  df <- model_fit_data(mod_list)
  if(unit == "all"){
    for(i in unique(df$Var2)){
      p <- ggplot(data = df[df$Var2 == i, ],
                  aes(x = Var1, fill = type,
                      group = type)) +
        geom_area(aes(y = value), position = "stack") +
        geom_point(aes(y = observed), size = 0.3, show.legend = FALSE) +
        facet_grid(Var2 ~ model) +
        labs(y = "Cases", x = "Week", fill = "Component") +
        scale_x_discrete(labels = skip_labels(df$Var1)) +
        # Rotate labels
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        # Ensure endemic at bottom
        scale_fill_manual(values = c("Endemic" = viridis(9)[8],
                                     "Epidemic" = viridis(9)[4]))
      print(p)
    }
  } else if(unit == "total"){
    p <- ggplot(merge(
      aggregate(value ~ Var1 + type + model, df, sum),
      aggregate(observed ~ Var1 + type + model, df, sum)),
      aes(x = Var1, fill = type,
          group = type)) +
      geom_area(aes(y = value), position = "stack") +
      geom_point(aes(y = observed), size = 0.3, show.legend = FALSE) +
      facet_grid(. ~ model) +
      labs(y = "Cases", x = "Week", fill = "Component") +
      scale_x_discrete(labels = skip_labels(df$Var1)) +
      # Rotate labels
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      # Ensure endemic at bottom
      scale_fill_manual(values = c("Endemic" = viridis(9)[8],
                                   "Epidemic" = viridis(9)[4]))
    print(p)
  } else {
    p <- ggplot(data = df[df$Var2 == unit, ],
                aes(x = Var1, fill = type,
                    group = type)) +
      geom_area(aes(y = value), position = "stack") +
      geom_point(aes(y = observed), size = 0.3, show.legend = FALSE) +
      facet_grid(Var2 ~ model) +
      labs(y = "Cases", x = "Week", fill = "Component") +
      scale_x_discrete(labels = skip_labels(df$Var1)) +
      # Rotate labels
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      # Ensure endemic at bottom
      scale_fill_manual(values = c("Endemic" = viridis(9)[8],
                                   "Epidemic" = viridis(9)[4]))
    print(p)
  }
}
