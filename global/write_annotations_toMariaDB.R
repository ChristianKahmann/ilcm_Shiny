write_to_MariaDB<-function(mydb,query){
  #RMariaDB::dbBegin(conn = mydb)
  rs<-RMariaDB::dbSendQuery(conn = mydb,statement = query)
  RMariaDB::dbClearResult(res = rs)
  #RMariaDB::dbCommit(mydb)
}

delete_from_MariaDB<-function(mydb,query){
  #RMariaDB::dbBegin(conn = mydb)
  rs<-RMariaDB::dbSendStatement(conn = mydb,statement = query)
  RMariaDB::dbClearResult(res = rs)
  #RMariaDB::dbCommit(mydb)
}