#' Ensure consistency of date labels used in predictive_moments outputs
#' 
#' @description Replaces the default rownames in the predictive moments object
#' such that they are the same as the original case counts
#' @param obj A predictive_moments object
#' @returns The same predictive_moments object but with dates rather than
#' "t=" row labels on the mu_matrix and var_matrix
make_prediction_dates_ISO <- function(obj){
  #if(class(obj) != "predictive_moments_hhh4"){
  #  stop("obj must be a predictive_moments object")
  #}
  rownames(obj$mu_matrix) <- rownames(obj$realizations_matrix)
  rownames(obj$var_matrix) <- rownames(obj$realizations_matrix)
  return(obj)
}

#' Compare two sets of predictions
#' 
#' @description Given a list of predictive_moments objects the function returns
#' a data frame with the predicted counts
#' @param alt A list of predictive_moments objects under the alternative
#' scenario considered
#' @param orig A list of predictive_moments objects under the original scenario
#' considered (the one to be used as comparison)
#' @returns The ratio and differences between predictions as lists and data
#' frames
make_predictions_df <- function(alt, orig){
  if(class(alt) != "list"){
    stop("alt must be a list")
  }
  if(class(orig) != "list"){
    stop("orig must be a list")
  }
  #if(!("predictive_moments_hhh4" %in% unique(sapply(alt, class)))){
  #  stop("alt must contain predictive moments of hhh4 models")
  #}
  #if(!("predictive_moments_hhh4" %in% unique(sapply(orig, class)))){
  #  stop("orig must contain predictive moments of hhh4 models")
  #}
  if(!is.null(names(alt))){
    alt <- alt[which(names(alt) != "neither")]
    orig <- orig[which(names(orig) != "neither")]
  }
  
  pred_ratio <- Map("/", lapply(alt, function(x){x$mu_matrix}),
                    lapply(orig, function(x){x$mu_matrix}))
  pred_ratio_df <- as.data.frame(
    cbind(do.call(rbind, pred_ratio),
          rep(names(pred_ratio), vapply(pred_ratio, nrow, numeric(1)))))
  colnames(pred_ratio_df)[dim(pred_ratio_df)[2]] <- "model"
  pred_ratio_df$date <- substr(gsub("\\.", "-",
                                    gsub("X", "", rownames(pred_ratio_df))),
                               1, 7)
  pred_diff <- Map("-", lapply(alt, function(x){x$mu_matrix}),
                    lapply(orig, function(x){x$mu_matrix}))
  pred_diff_df <- as.data.frame(
    cbind(do.call(rbind, pred_diff), 
          rep(names(pred_diff), vapply(pred_diff, nrow, numeric(1)))))
  colnames(pred_diff_df)[dim(pred_diff_df)[2]] <- "model"
  pred_diff_df$date <- substr(gsub("\\.", "-",
                                    gsub("X", "", rownames(pred_diff_df))),
                               1, 7)
  return(list("pred_ratio" = pred_ratio,
              "pred_ratio_df" = pred_ratio_df,
              "pred_diff" = pred_diff,
              "pred_diff_df" = pred_diff_df))
}
