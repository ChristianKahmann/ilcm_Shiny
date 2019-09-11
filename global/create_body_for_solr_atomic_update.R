
create_body_solr_update_set<-function(ids,field_name,values){
  body<-"["
  body<-paste0(body,'{"id":"',ids[1],'","',field_name,'":{"set":"',values[1],'"}}')
  if(length(ids)>1){
    body_intermediate<-paste0(',{"id":"',ids[2:length(ids)],'","',field_name,'":{"set":"',values[2:length(ids)],'"}}',collapse="")
    body<-paste0(body,body_intermediate)
  }
  body<-paste0(body,"]") 
}


create_body_solr_update_add<-function(ids,field_name,values){
  body<-"["
  body<-paste0(body,'{"id":"',ids[1],'","',field_name,'":{"add":"',values[1],'"}}')
  if(length(ids)>1){
    body_intermediate<-paste0(',{"id":"',ids[2:length(ids)],'","',field_name,'":{"add":"',values[2:length(ids)],'"}}',collapse="")
    body<-paste0(body,body_intermediate)
  }
  body<-paste0(body,"]") 
}

create_body_solr_update_remove<-function(ids,field_name,values){
  body<-"["
  body<-paste0(body,'{"id":"',ids[1],'","',field_name,'":{"remove":"',values[1],'"}}')
  if(length(ids)>1){
    body_intermediate<-paste0(',{"id":"',ids[2:length(ids)],'","',field_name,'":{"remove":"',values[2:length(ids)],'"}}',collapse="")
    body<-paste0(body,body_intermediate)
  }
  body<-paste0(body,"]") 
}
