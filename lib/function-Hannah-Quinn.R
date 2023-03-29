#' Hannah-Quinn information criterion
#' 
#' @param mod A hhh4 model
#' @returns The HQIC value
#' @seealso AICc
#' # Using the examples from hhh4
#' data("measlesWeserEms")
#' HQIC(hhh4(measlesWeserEms,
#' list(
#' end = list(f = addSeason2formula(~1 + t, period = 52),
#' offset = population(measlesWeserEms)),
#' ne = list(f = ~1, weights = neighbourhood(measlesWeserEms) == 1),
#' family = "NegBin1"
#' )))
#' # -1153
HQIC <- function(mod){
  if(class(mod) != "hhh4"){
    stop("Model is not an endemic-epidemic model")
  }
  K <- attributes(logLik(mod))$df
  n <- attributes(logLik(mod))$nobs
  HQIC <- mod$loglikelihood + 2 * K * log(log(n))
  return(HQIC)
}
# Perhaps not useful for our work