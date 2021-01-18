#' numberOfDays
#' @param date
#' 
#' @return number of dates (integer)
#' 
#' @example 
#' @export
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

