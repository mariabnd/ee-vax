#' Function that ensures all ISO week dates have the same number of characters
#' 
#' @param x The ISO week date that may need a leading zero
#' @returns The ISO week date with a zero amended to the week
#' @example
#' fixISOleadingzero("2021-9")
#' # "2021-09"
fixISOleadingzero <- function(x) {
  # Labels for weeks 1 to 10
  x[which(nchar(x) == 6)] <-
    gsub(x = x[which(nchar(x) == 6)],
         pattern = "-", replacement = "-0")
  return(x)
}
# This is placed here because globals loads before rest of lib folder
date_range_full <- fixISOleadingzero(date_range_full)