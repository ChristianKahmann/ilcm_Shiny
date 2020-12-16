DB_get_database_connection <- function(){
  database <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user=DB_USER, password=DB_PASS, dbname=DATABASE, host=host,port=db_port)
  RMariaDB::dbSendStatement(database, 'set character set "utf8"')
  return(database)
}

DB_get_documents_by_id <-function(document_ids){
  connection <- DB_get_database_connection()
  query <- glue_sql("SELECT * FROM documents WHERE id IN ({doc_ids*})", doc_ids = unique(document_ids), .con = connection)
  documents <- RMariaDB::dbGetQuery(connection, query)
  RMariaDB::dbDisconnect(connection)
  return(documents)
}

DB_get_document_by_id <-function(id){
  connection <- DB_get_database_connection()
  query <- paste0("SELECT * FROM documents WHERE id = '", id, "';")
  document <- RMariaDB::dbGetQuery(connection, query)
  RMariaDB::dbDisconnect(connection)
  return(document)
}

DB_get_document_by_id_and_dataset <-function(document_id, dataset){
  connection <- DB_get_database_connection()
  query <- sprintf("SELECT * FROM documents WHERE id_doc = %s AND dataset = '%s'", document_id, dataset)
  document <- RMariaDB::dbGetQuery(connection, query)
  RMariaDB::dbDisconnect(connection)
  return(document)
}

DB_get_annotations_by_global_document_ids_with_anno_scheme <- function(global_document_ids, anno_set){
  connection <- DB_get_database_connection()
  query <- glue_sql("SELECT * FROM Annotations WHERE global_doc_id IN ({doc_ids*}) AND Anno_set = {anno_set};", doc_ids = unique(global_document_ids), anno_set = anno_set, .con = connection)
  annotations <- RMariaDB::dbGetQuery(connection, query)
  RMariaDB::dbDisconnect(connection)
  return(annotations)
}

DB_get_annotations_by_global_document_ids <- function(global_document_ids, anno_set){
  connection <- DB_get_database_connection()
  query <- glue_sql("SELECT * FROM Annotations WHERE global_doc_id IN ({doc_ids*});", doc_ids = unique(global_document_ids), .con = connection)
  annotations <- RMariaDB::dbGetQuery(connection, query)
  RMariaDB::dbDisconnect(connection)
  return(annotations)
}

DB_get_used_annotation_schemes <- function(collection){
  connection <- DB_get_database_connection()
  query <- sprintf("SELECT DISTINCT Anno_set FROM Annotations WHERE collection = '%s';", collection)
  anno_sets <- RMariaDB::dbGetQuery(connection, query)
  RMariaDB::dbDisconnect(connection)
  return(anno_sets)
}

DB_get_collection_names <- function(){
  connection <- DB_get_database_connection()
  result <- RMariaDB::dbGetQuery(connection, "SELECT DISTINCT name FROM Collections;")
  RMariaDB::dbDisconnect(connection)
  return(result$name)
}

DB_get_collections <- function(collection){
  connection <- DB_get_database_connection()
  query <- paste0("SELECT DISTINCT id, name, created, `number of documents` FROM Collections WHERE name = '", collection, "';")
  result <- RMariaDB::dbGetQuery(connection, query)
  RMariaDB::dbDisconnect(connection)
  return(result)
}

DB_get_datasets <- function(){
  connection <- DB_get_database_connection()
  query <- "SELECT DISTINCT dataset FROM ilcm.metadata_names;"
  result <- RMariaDB::dbGetQuery(connection, query)
  RMariaDB::dbDisconnect(connection)
  return(result)
}

DB_import_annotations <- function(annotations){
  connection <- DB_get_database_connection()
  RMariaDB::dbBegin(conn = connection)
  # ensure no single quotes are used
  annotations<-apply(X = annotations,MARGIN = 2,function(x){
    return(stringr::str_replace_all(string = x,pattern = "'",replacement = "''"))
  })
  values <- paste0(apply(annotations, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  query <- paste0("INSERT INTO Annotations VALUES ", values, ";")
  RMariaDB::dbSendQuery(connection, query)
  RMariaDB::dbCommit(connection)
  RMariaDB::dbDisconnect(connection)
}

DB_delete_dataset <- function(dataset){
  connection <- DB_get_database_connection()
  RMariaDB::dbBegin(conn = connection)
  RMariaDB::dbSendStatement(connection, 'SET SQL_SAFE_UPDATES = 0;')
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.token where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.documents where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.metadata_names where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.meta_date where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.meta_token where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.meta_mde1 where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.meta_mde2 where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.meta_mde3 where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.meta_mde4 where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.meta_mde5 where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.meta_mde6 where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.meta_mde7 where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.meta_mde8 where dataset='", dataset,"';"))
  RMariaDB::dbSendStatement(connection, paste0("delete FROM ilcm.meta_mde9 where dataset='", dataset,"';"))
  RMariaDB::dbCommit(connection)
  RMariaDB::dbDisconnect(connection)
}

#DB_delete_dataset("refi-import")





