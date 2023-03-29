#' Corrected Akaike information criterion
#' 
#' @param mod A hhh4 model
#' @returns The corrected AIC value
#' @seealso HQIC
#' @example 
#' # Using the examples from hhh4
#' data("measlesWeserEms")
#' AICc(hhh4(measlesWeserEms,
#' list(
#' end = list(f = addSeason2formula(~1 + t, period = 52),
#' offset = population(measlesWeserEms)),
#' ne = list(f = ~1, weights = neighbourhood(measlesWeserEms) == 1),
#' family = "NegBin1"
#' )))
#' # 2366
AICc <- function(mod){
  if(class(mod) != "hhh4"){
    stop("Model is not an endemic-epidemic model")
  }
  K <- attributes(logLik(mod))$df
  n <- attributes(logLik(mod))$nobs
  correction <- (2 * K * (K + 1)) / (n - K + 1)
  return(AIC(mod) + correction)
}
# Perhaps not useful since nObs for our stsObjs seems to be large enough




