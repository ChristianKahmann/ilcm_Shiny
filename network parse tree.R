library(visNetwork)

x<-annotation[1:12,]

nodes<-data.frame(id=x$token_id,label=x$token,shape="box",group=x$upos)
edges<-data.frame(from=x$token_id,to=x$head_token_id,label=x$dep_rel,shadow=TRUE)

visNetwork(nodes,edges)%>%
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout() 