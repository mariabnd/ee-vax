#' Extract sine and cosine waves from hhh4 model with seasonal effects
#' 
#' @param mod_list A list of hhh4 models to extract information from
#' @param inpt Can take values "end" (endemic) or "ne" (epidemic)
#' @seealso model_fit_info
#' @example
#' # Using the examples from hhh4
#' data("measlesWeserEms")
#' mod <- hhh4(measlesWeserEms,
#' list(
#' end = list(f = addSeason2formula(~1 + t, period = 52),
#' offset = population(measlesWeserEms)),
#' ne = list(f = addSeason2formula(f = ~ 1),
#'                    weights = neighbourhood(measlesWeserEms) == 1),
#' family = "NegBin1"
#' ))
#' get_waves_data(list("model" = mod, "model" = mod,
#'                     "model" = mod, "model" = mod))
#' variable     est df[, "ymin"] df[, "ymax"] time    type
#' 1      model  1.2397      -2.7473       1.4485    1 Endemic
#' 2      model  1.3705      -2.6135       1.6023    2 Endemic
#' 3      model  1.4814      -2.4416       1.7326    3 Endemic
#' 4      model  1.5706      -2.2340       1.8377    4 Endemic
#' ...
get_waves_data <- function(mod_list, inpt = c("end", "ne")){
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
  inpt <- match.arg(inpt)
  tab <- rbind(model_fit_info(mod_list, se = FALSE)[
    grep("sin", rownames(model_fit_info(mod_list))), ],
    model_fit_info(mod_list, se = FALSE)[
      grep("cos", rownames(model_fit_info(mod_list))), ])
  # NB assumes S = 1 in addSeason2formula (a single seasonal term)
  t <- 1 : as.numeric(substr(rownames(tab)[1],
                             regexpr("*/", rownames(tab)[1]) + 1,
                             regexpr(")", rownames(tab)[1]) - 1))
  tmp <- tab[grep(inpt, rownames(tab)), grep("est", colnames(tab))]
  df <- data.frame(rep(NA, length(t)))
  for (i in 1 : 4){
    df[, i] <- tmp[1, i] * sin(2 * pi * t / 52) +
      tmp[2, i] * cos(2 * pi * t / 52)
    
  }
  names(df) <- gsub("est-", "", colnames(tmp))
  df <- melt(df)
  names(df)[names(df) == "value"] <- "est"
  save <- df
  
  tmp <- tab[grep("ne", rownames(tab)), grep("est", colnames(tab))] -
    tab[grep("ne", rownames(tab)), grep("se", colnames(tab))]
  
  df <- data.frame(rep(NA, length(t)))
  for (i in 1 : 4){
    df[, i] <- tmp[1, i] * sin(2 * pi * t / 52) +
      tmp[2, i] * cos(2 * pi * t / 52)
    
  }
  df <- melt(df)
  names(df)[names(df) == "value"] <- "ymin"
  save <- cbind(save, df[, "ymin"])
  
  tmp <- tab[grep(inpt, rownames(tab)), grep("est", colnames(tab))] +
    tab[grep(inpt, rownames(tab)), grep("se", colnames(tab))]
  
  df <- data.frame(rep(NA, length(t)))
  for (i in 1 : 4){
    df[, i] <- tmp[1, i] * sin(2 * pi * t / 52) +
      tmp[2, i] * cos(2 * pi * t / 52)
    
  }
  df <- melt(df)
  names(df)[names(df) == "value"] <- "ymax"
  save <- cbind(save, df[, "ymax"])
  save$time <- rep(t, times = 4)
  
  if(inpt == "end"){
    save$type <- "Endemic"
  }
  if(inpt == "ne"){
    save$type <- "Epidemic"
  }
  return(as.data.frame(save))
}
