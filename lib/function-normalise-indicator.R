#' Normalise policy indicators
#' 
#' @param x Policy indicator level values
#' @param rev Boolean denoting whether the policy indicator should be
#' reversed such that more policy in place leads to a lower value (used
#' to weight contact matrices). Defaults to TRUE
#' @returns Normalised policy indicator values
#' @example
#' set.seed(20221015)
#' policy <- sample(x = c(1 : 4), size = 25, replace = TRUE)
#' policy_norm(policy)
#' # 0.667 1.000 1.000 0.333 0.333 1.000 0.667 0.667 0.667 0.333 0.667 1.000 0.000 0.667
#' # 0.667 0.000 0.333 0.333 1.000 0.333 0.333 0.667 1.000 0.667 1.000
policy_norm <- function(x, max, rev = TRUE){
  if(!is.logical(rev)){
    stop("rev must be TRUE/FALSE")
  }
  if(rev){
    out <- 1 - (x / max)
  } else {
    out <- x / max
  }
  return(out)
}
