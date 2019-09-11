

kwic2<-function(Tokens,target,k,n,indices=NULL){
  ergeb<-matrix(c(0),0,3)
  colnames(ergeb)<-c("before","target","after")
  
  if(is.null(indices)){
    indices<-1
  }
  inds<-which(tolower(Tokens[,2])==target)
  if(length(inds)>0){
    for(l in 1:min(length(inds),n)){
      ind<-inds[l]
      row<-cbind(paste(Tokens[max(1,(ind-k)):(ind-1),2],collapse = " "),(target),paste(Tokens[(ind+1):min((ind+k),dim(Tokens)[1]),2],collapse = " "))
      ergeb<-rbind(ergeb,row)
    }
  }
  return(ergeb) 
}




myLabelFormat = function(...,dates=FALSE){ 
  if(dates){ 
    function(type = "numeric", cuts){ 
      as.Date(cuts, origin="1970-01-01")
    } 
  }else{
    labelFormat(...)
  }
}
