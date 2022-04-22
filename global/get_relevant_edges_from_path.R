get_relevant_edge_ids_for_paths<-function(path,edges){
  nodes<-names(path)
  rel_edges<-NULL
  for(i in 1:(length(nodes)-1)){
    edge_searched<-as.numeric(c(nodes[i],nodes[i+1]))
    rel_edges<-c(rel_edges,which(unlist(lapply(1:nrow(edges),FUN = function(x){
      all(c(edges[x,1],edges[x,2])%in%edge_searched)
    })
    )))
  }
  return(rel_edges)
}