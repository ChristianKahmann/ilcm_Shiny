#' parse dataframe to dictionary
#' @param DF
#' 
#' @return 
#' @export
#' @example 
DF_to_Dict<-function(DF){
  Dict<-list()
  for(i in 1:dim(DF)[2]){
   Dict[[colnames(DF)[i]]]<-DF[,i] 
  }
  Dict<-quanteda::dictionary(x = Dict,tolower=T)
}

#' parse dictionary to data frame
#' @param Dict
#' 
#' @return DF (dataframe)
#' 
#' @export
#' @example 
Dict_to_DF<-function(Dict){
  DF<-t(plyr::ldply(Dict,rbind))
  colnames(DF)<-DF[1,]
  DF<-DF[-1,,drop=FALSE]
  DF[which(is.na(DF))]<-""
  DF<-data.frame(DF,stringsAsFactors = FALSE)
  return(DF)
}