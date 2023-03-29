#' Convert Date object to written date
#' 
#' @description Given a date input the function returns the date as it would
#' be written in a text
#' @param date A Date object
#' @returns A character string with the date formatted as Xst/nd/rd/th Month
#' Year
#' @examples
#' day <- as.Date("1993-01-27")
#' date_to_text(day)
#' # "27th January 1993"
date_to_text <- function(date){
  if(class(date) != "Date"){
    date <- as.Date(date)
    warning("Input was converted using as.Date()")
  }
  day <-
    ifelse(
      as.numeric(format(date, "%d")) == 1, "1st",
      ifelse(
        as.numeric(format(date, "%d")) == 2, "2nd",
        ifelse(
          as.numeric(format(date, "%d")) == 3, "3rd",
          paste0(format(date, "%d"), "th"))))
  res <- paste(day, format(date, "%B %Y"), sep = " ")
  return(res)
}
