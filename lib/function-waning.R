#' Waning function
#' 
#' @description `waning` (as well as sensitivity options `waning_sens1` and
#' `waning_sens2`) is the implementation of the waning functions outlined in
#' the study protocol
#' @param t Number of weeks passed since vaccine administered
#' @param val Value to adjust waning by
#' @seealso apply_waning
#' @returns The waned values
#' @example
#' # Unadjusted waning
#' waning(0 : 55, val = 1)
#' # 0.0000 0.5000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000
#' # 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000
#' # 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 0.9231 0.8462 0.7692 0.6923
#' # 0.6154 0.5385 0.4615 0.3846 0.3077 0.2308 0.1538 0.0769 0.0000 0.0000
#' # 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
#' # 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
#' # Plot from protocol
#' plot(waning(0 : 55, val = 1),
#' type = "b", xlab = "p (weeks passed since vaccine given)",
#' main = "Waning", ylab = expression(u(p)), xaxt = "n")
#' axis(1, at = c(0, 2, 26, 39, 52), labels = c(0, 2, 26, 39, 52))
#' # Plot from sensitivity analysis supporting information
#' par(mfrow = c(1, 3))
#' plot(waning(0 : 55, val = 1),
#' type = "b", xlab = "p (weeks passed since vaccine given)",
#' main = "Original", ylab = expression(u(p)), xaxt = "n")
#' lines(c(26, 39), c(1, 0), col = 2)
#' axis(1, at = c(0, 2, 26, 39, 52), labels = c(0, 2, 26, 39, 52))
#' plot(waning_sens1(0 : 55, val = 1),
#' type = "b", xlab = "p (weeks passed since vaccine given)",
#' main = "Sensitivity analysis 1 (h = 0.5)", ylab = expression(u["alt"](p)),
#' xaxt = "n")
#' lines(c(26, 39), c(1, 0), col = 2)
#' axis(1, at = c(0, 2, 26, 39, 52), labels = c(0, 2, 26, 39, 52))
#' plot(waning_sens2(0 : 55, val = 1),
#' type = "b", xlab = "p (weeks passed since vaccine given)",
#' main = "Sensitivity analysis 2 (h = 0.3)", ylab = expression(u["alt"](p)),
#' xaxt = "n")
#' lines(c(26, 39), c(1, 0), col = 2)
#' axis(1, at = c(0, 2, 26, 39, 52), labels = c(0, 2, 26, 39, 52))
waning <- function(t, val){
  # Calculate downwards slope for waning time
  slope <- coef(lm(c(1, 0) ~ c(25, 38)))[[2]]
  adj <- ifelse(t <= 0, 0,
                ifelse(t == 1, 0.5,
                       ifelse(t > 1 & t <= 25, 1,
                              ifelse(t > 1 & t > 25 & t < 39, - 1 * (38 - t) * slope,
                                     ifelse(t > 1 & t >= 39, 0, 0)))))
  adj * val
}
#' @rdname waning
waning_sens1 <- function(t, val){
  adj <- ifelse(t <= 0, 0,
                ifelse(t == 1, 0.5,
                       1 / (1 + exp(0.5 * (t - 32.5)))))
  adj * val
}
#' @rdname waning
waning_sens2 <- function(t, val){
  adj <- ifelse(t <= 0, 0,
                ifelse(t == 1, 0.5,
                       1 / (1 + exp(0.3 * (t - 32.5)))))
  adj * val
}

#' Apply the waning funtion
#' 
#' @description Applies a given waning function to a data set using a triangle
#' matrix sum
#' @param data_set data set which will be waned. Must contain groups, geoRegion
#' or altersklasse_covid19 as one of the variables (denotes the units), entries
#' (the case counts), and pop (the population size of the unit)
#' @param FUN waning function of choice
#' @param name desired names of variables added to data set (cumulative cases
#' and waned coverage). Defaults to c("csum_waned", "cov_1_waned"))
#' @seealso waning
apply_waning <- function(data_set, FUN,
                         name = c("csum_waned", "cov_1_waned")){
  if(length(name) != 2){
    stop("name must have two entries")
  }
  if(!("entries" %in% names(data_set))){
    stop("data set must contain variable entries")
  }
  if(!any(grepl(pattern = "pop*", x = names(data_set)))){
    stop("data set must contain variable starting with pop")
  }
  if(!any(c("groups", "geoRegion", "altersklasse_covid19") %in% names(data_set))){
    stop("data set must contain unit variable (groups, geoRegion or altersklasse_covid19)")
  }
  # Create empty variable for populating
  data_set$tmp_csum_waned <- NA
  # Check variable names
  check <- isFALSE("groups" %in% names(data_set))
  if(check){
    if("geoRegion" %in% names(data_set)){
      data_set$groups <- data_set$geoRegion
      save <- data_set$pop
      data_set$pop <- data_set$pop_r
    }
    if("altersklasse_covid19" %in% names(data_set)){
      data_set$groups <- data_set$altersklasse_covid19
      save <- data_set$pop
      data_set$pop <- data_set$pop_a
    }
  }
  # Calculate the waned sum for each age and region group
  for (g in unique(data_set$groups)){
    # Extract the data for the group
    df <- data_set[data_set$groups == g, ]
    dm <- dim(df)
    # Create an empty (triangle) matrix to be populated with the
    # waned values of entries
    mat <- matrix(rep(NA, times = dm[1] * dm[1]), ncol = dm[1])
    colnames(mat) <- df$dateISO
    for (i in 1 : dm[1]){
      # extract the values at each date
      # calculate the waning at that date
      mat[i, ] <- FUN(1 : dm[1] - i, df$entries[i])
    }
    # sum across the date to obtain the cumulative sum of waned
    data_set[data_set$groups == g, ]$tmp_csum_waned <- colSums(mat)
  }
  data_set$tmp_cov_1_waned <- data_set$tmp_csum_waned / data_set$pop
  names(data_set)[which(names(data_set) == "tmp_csum_waned")] <- name[1]
  names(data_set)[which(names(data_set) == "tmp_cov_1_waned")] <- name[2]
  # Clean up for later merge
  if(check){
    data_set$groups <- NULL
    # Reset
    if("geoRegion" %in% names(data_set)){
      data_set$pop <- save
    }
    if("altersklasse_covid19" %in% names(data_set)){
      data_set$pop <- save
    }
  }
  return(data_set)
}
