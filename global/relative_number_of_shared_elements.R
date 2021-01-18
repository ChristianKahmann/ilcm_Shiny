#' relative_number_of_shared_elements
#' @param a
#' @param b
#' 
#' @return length of shared/a
#' 
#' @export
#' @example 
relative_number_of_shared_elements<-function(a,b){
  shared<-intersect(a,b)
  return(length(shared)/length(a))
} 