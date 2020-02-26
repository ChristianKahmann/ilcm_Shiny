get_metadata_from_db<-function(dataset=NULL,doc_ids=NULL,host=NULL,port=NULL){
  if(length(dataset>1)){
    if(length(dataset)!=length(doc_ids)){
      stop("number of given datasets and document ids differs")
    }
  }
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  meta<-NULL
  try({
    
    if(length(doc_ids)>1){
      rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8mb4"')
      for(i in 1:length(doc_ids)){
        if(length(dataset)==1){
          meta<-rbind(meta,RMariaDB::dbGetQuery(mydb, paste("select * from documents where dataset='",dataset,"' and id_doc=",doc_ids[i],";",sep="")))
        }
        else{
          meta<-rbind(meta, RMariaDB::dbGetQuery(mydb, paste("select * from documents where dataset='",dataset[i],"' and id_doc=",doc_ids[i],";",sep="")))
        }
      }
    }
    else{
      rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8mb4"')
      meta<-rbind(meta,RMariaDB::dbGetQuery(mydb, paste("select * from documents where dataset='",dataset,"' and id_doc=",doc_ids,";",sep="")))
    }
  })
  RMariaDB::dbDisconnect(mydb)
  return(meta)
}