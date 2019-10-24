get_token_from_db<-function(dataset=NULL,doc_ids=NULL,sentence_ids=NULL,host=NULL,port=NULL){
  if(!is.null(sentence_ids)){
    if(length(sentence_ids)!=length(doc_ids)){
      stop("number of given document ids and sentence ids differs")
    }
  }
  if(length(dataset>1)){
    if(length(dataset)!=length(doc_ids)){
      stop("number of given datasets and document ids differs")
    }
  }
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  token<-NULL
  try({
    if(is.null(sentence_ids)){
      if(length(doc_ids)>1){
        token<-list()
        rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8mb4"')
        for(i in 1:length(doc_ids)){
          if(length(dataset)==1){
            token<-RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",dataset,"' and id=",doc_ids[i],";",sep=""))
          }
          else{
            token<-RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",dataset[i],"' and id=",doc_ids[i],";",sep=""))
          }
        }
      }
      else{
        rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8mb4"')
        token<-RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",dataset,"' and id=",doc_ids,";",sep=""))
      }
    }
    else{
      if(length(doc_ids)>1){
        token<-list()
        rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8mb4"')
        for(i in 1:length(doc_ids)){
          if(length(dataset)==1){
            token<-RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",dataset,"' and id=",doc_ids[i]," and sid=",sentence_ids[i],";",sep=""))
          }
          else{
            token<-RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",dataset[i],"' and id=",doc_ids[i]," and sid=",sentence_ids[i],";",sep=""))
          }
        }
      }
      else{
        rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8mb4"')
        token<-RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",dataset,"' and id=",doc_ids," and sid=",sentence_ids,";",sep=""))
      }
    }
  })
  RMariaDB::dbDisconnect(mydb)
  return(token)
}