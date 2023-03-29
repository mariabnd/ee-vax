#' Format coefficients to be included in table in document
#'
#' @param mod A hhh4 model
#' @param unvax_focus A Boolean which determines whether to extract only the
#' coefficients related to vaccination coverage (TRUE) or all model coefficients
#' (FALSE). Defaults to TRUE
#' @param include_SE A Boolean which determines whether to annotate the standard
#' error by SE. Defaults to TRUE
#' @returns The estimates and standard errors of the model given in the format
#' X (SE Y)
#' @example 
#' # Using the examples from hhh4
#' data("measlesWeserEms")
#' coef_for_table(hhh4(measlesWeserEms,
#' list(
#' end = list(f = addSeason2formula(~1 + t, period = 52),
#' offset = population(measlesWeserEms)),
#' ne = list(f = ~1, weights = neighbourhood(measlesWeserEms) == 1),
#' family = "NegBin1"
#' )), unvax_focus = FALSE)
#'                ne.1                  end.1                  end.t 
#' "-1.456 (SE 0.179)"     "0.627 (SE 0.345)"     "0.006 (SE 0.006)" 
#' end.sin(2 * pi * t/52) end.cos(2 * pi * t/52)               overdisp 
#' "0.82 (SE 0.269)"      "0.85 (SE 0.196)"    "11.361 (SE 0.995)"
coef_for_table <- function(mod, unvax_focus = TRUE, include_SE = TRUE){
  if(class(mod) != "hhh4"){
    stop("Model is not an endemic-epidemic model")
  }
  if(!is.logical(unvax_focus)){
    stop("unvax_focus must be TRUE/FALSE")
  }
  coef <- coefficients(mod, se = TRUE)
  if(unvax_focus){
  extract_est <- coef[grepl(pattern = "vax",
                            x = rownames(coef)), "Estimate"]
  extract_se <- coef[grepl(pattern = "vax",
                           x = rownames(coef)), "Std. Error"]
  }
  if(!unvax_focus){
    extract_est <- coef[, "Estimate"]
    extract_se <- coef[, "Std. Error"]
    }
  df <- data.frame("estimate" = extract_est,
                   "std error" = extract_se)
  if(include_SE){
  out <- apply(df, 1,
               function(x){
                 paste(round(x[1], options()$digits), " (SE ",
                       round(x[2], options()$digits), ")", sep = "")
               })
  } else {
    out <- apply(df, 1,
                 function(x){
                   paste(round(x[1], options()$digits), " (",#" (SE ",
                         round(x[2], options()$digits), ")", sep = "")
                 })
  }
  return(out)
}
