#' get_names_from_list
#' @param l (list of categories)
#' 
#' @return names (of categories)
get_names_from_list<-function(l){
  names<-names(l)
  for(i in 1:length(names)){
    try({
      names<-c(names,names(l[[i]]$sublist))
    })
  }
  return(names)
}