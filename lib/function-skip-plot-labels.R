# Source: https://github.com/mariabnd/miscellaneous/blob/master/R/skip_labels.R
skip_labels <- function(x, amount = 4L){
  x <- as.character(x) # In case x is factor
  x <- sort(unique(x))
  x[!(x %in% x[seq.int(1L, length(x), amount)])] <- ""
  return(x)
}
