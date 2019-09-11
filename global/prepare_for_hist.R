

make_ready_for_hist<-function(counts,breaks){
  result<-list()
  erg<-list()
  for(i in 1:dim(counts)[1]){
    erg[[i]]<-rep(counts[i,1],counts[i,2])
  }
  erg<-as.vector(unlist(erg))
  result[[1]]<-hist(erg,breaks = breaks,plot=FALSE)$breaks
  result[[1]]<-result[[1]][1:(length(result[[1]])-1)]
  result[[2]]<-hist(erg,breaks = breaks,plot=FALSE)$counts
  result<-do.call(rbind,result)
  return(result)
}