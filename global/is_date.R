#' check if a given data is a date 
#' @param mydate
#' 
#' @return
#' @export
#' @example 
IsDate <- function(mydate) {
  tryCatch(!is.na(base::as.Date(mydate, format = "%Y-%m-%d",tryFormats = c("%Y/%m/%d","%d-%m-%Y","%m-%d-%Y"))),  
           error = function(err) {FALSE})  
}

