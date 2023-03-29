#' Add prediction uncertainty to models
#' @param orig
#' @param alt
#' @start Time point where the prediction window should start
#' @nsim Number of simulations to do
add_prediction_uncertainty <- function(orig, alt, start, nsim = 100){#5000){
  if(class(alt) != "list"){
    stop("alt must be a list")
  }
  if(class(orig) != "list"){
    stop("orig must be a list")
  }
  if(!("hhh4" %in% unique(sapply(alt, class)))){
    stop("alt must contain hhh4 models")
  }
  if(!("hhh4" %in% unique(sapply(orig, class)))){
    stop("orig must contain hhh4 models")
  }
  alt <- alt[which(names(alt) != "neither")]
  orig <- orig[which(names(orig) != "neither")]
  
  out <- lapply(seq_len(length(names(alt))), function(x){
    # sample coefficients
    coef_list <- lapply(seq_len(nsim), function(y){
      set.seed(y) # Reproducibility
      mvrnorm(n = 1, mu = orig[[x]]$coefficients, Sigma = orig[[x]]$cov)
    })
    preds <- lapply(seq_len(nsim), function(y){
      # Set sampled coefficients
      orig[[x]]$coefficients <- alt[[x]]$coefficients <- coef_list[[y]]
      # Apply JB fix
      orig[[x]]$terms <- alt[[x]]$terms <- NULL
      pred_orig <- make_prediction_dates_ISO(
        predictive_moments(orig[[x]],
                           t_condition = start,
                           lgt = length(date_range_full) - start))
      pred_alt <- make_prediction_dates_ISO(
        predictive_moments(alt[[x]],
                           t_condition = start,
                           lgt = length(date_range_full) - start))
      return(list("pred_orig" = pred_orig,
                  "pred_alt" = pred_alt))
    })
    return(list("nsim" = nsim,
                "coef_list" = coef_list,
                "predictions" = preds))
  })
  names(out) <- names(alt)
  return(out)
}

#' Add prediction uncertainty to models
#' @param pred_list List of predictions with uncertainty created by
#' `add_prediction_uncertainty`
#' @param quantiles Vector of desired quantiles
#' @seealso add_prediction_uncertainty
extract_quantiles <- function(pred_list,
                              quantiles = c(10, 90, 25, 75)){
  out <- lapply(names(pred_list),
                function(x){
                  model_preds <- pred_list[[x]]$predictions
                  alt <- lapply(model_preds, "[[", which(names(model_preds[[1]]) == "pred_alt"))
                  orig <- lapply(model_preds, "[[", which(names(model_preds[[1]]) == "pred_orig"))
                  ratio <- make_predictions_df(alt, orig)$pred_ratio
                  ratio <- melt(do.call(rbind, ratio))
                  # Calculate the quantiles for the ratios
                  quant_dfs <- lapply(
                    quantiles,
                    function(y){
                      df <- aggregate(value ~ Var1 + Var2, ratio,
                                      function(z) quantile(z, probs = y / 100))
                      colnames(df)[dim(df)[2]] <- paste0("q", y)
                      df$model <- x
                      return(df)
                    })
                  # and the mean
                  df <- aggregate(value ~ Var1 + Var2, ratio, mean)
                  df$model <- x
                  
                  vals <- Reduce(function(x, y) merge(x, y, all = TRUE), quant_dfs)
                  vals <- merge(df, vals, all = TRUE)
                  return(vals)
                })
  names(out) <- names(pred_list)
  return(out)
}
