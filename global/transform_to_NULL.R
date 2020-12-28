#' transform to NULL
#' @param choices
#' 
#' @return NULL (if there are no choices) or choices 
#' @export
#' @example 
transform_to_NULL<-function(choices){
  if(length(choices)==0){
    return(NULL)
  }
  else{
    return(choices)
  }
}