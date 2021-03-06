#' save collection to database
#' @param info
#' 
#' @return 
#' @export
#' @example 
save_collection_to_db<-function(info){
  source("config_file.R")
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  creation_query<-info[[4]]
  if(nchar(creation_query)>5000){
    creation_query<-substr(creation_query,1,5000)
  }
  vals<-c(info[[5]],as.character(info[[2]][1,1]),as.character(Sys.time()),creation_query,length(info[[1]][,1]))
  vals[[4]]<-stringr::str_replace_all(string = vals[[4]],pattern = '\\\"',replacement = '')
  query<-paste0('Insert into Collections Values(DEFAULT, "',paste0(vals,collapse='", "'),'");')
  query<-stringr::str_replace_all(string = query,pattern = '\"NA\"',replacement = "NULL")
  RMariaDB::dbBegin(conn = mydb)
  rs <- RMariaDB::dbSendQuery(mydb, query)
  RMariaDB::dbCommit(mydb)
  RMariaDB::dbDisconnect(mydb)
}

#' delete collection from database
#' @param name
#' 
#' @return 
#' @export
#' @example 
delete_collection_from_db<-function(name){
  source("config_file.R")
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  query<-paste0('SET SQL_SAFE_UPDATES = 0;')
  rs <- RMariaDB::dbSendStatement(mydb, query)
  query<-paste0('Delete from Collections where name="',name,'";')
  rs <- delete_from_MariaDB(mydb, query)
  RMariaDB::dbDisconnect(mydb)
}