source("global/text_functions.R")
source("global/log_to_file.R")
source("global/rbind_huge_sparse_Matrix.R")
source("config_file.R")
source("global/functions_used_in_scripts.R")

#process
error<-try(expr = {
  library(Matrix)
  library(dplyr)
  library(spacyr)
  #load parameters
  load("collections/tmp/tmp.RData")
  
  #load collection 
  log_to_file(message = "<b>Step 1/9: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  
  
  #load data from database
  log_to_file(message = "<b>Step 2/9: Loading data from database</b>",file = logfile)
  db_data<-get_token_meta_and_language_from_db(get_meta = F,get_language = T,get_global_doc_ids = F)
  #token<-db_data$token[,c("id","word")]
  #colnames(token)<-c("doc_id","token")
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  
  #sanity check
  log_to_file(message = "<b>Step 3/9: Sanity check</b>",file = logfile)
  #token object not empty
  log_to_file(message = "&emsp; token object not empty?",logfile)
  if(dim(db_data$token)[1]>1){
    log_to_file(message = "&emsp; ✔",logfile)
  }
  else{
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No documents were found in the database for the specified collection.</b>",logfile)
    stop("Token empty")
  }
   log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished sanity checks",file = logfile)
  
  
  #preparing parameters
  log_to_file(message = "<b>Step 4/9: Preparing input parameters</b>",file = logfile)
  prepare_input_parameters()
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  #preparing token object
  log_to_file(message = "<b>Step 5/9: Preparing token object</b>",file = logfile)
  db_data$token<-prepare_token_object(token = db_data$token)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing token object",file = logfile)
  
  
  #calculating dtm
  log_to_file(message = "<b>Step 6/9: Calculating DTM</b>",file = logfile)
  dtm<-calculate_dtm()
  log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished pre-processing with",dim(dtm)[1], "documents and ",dim(dtm)[2], "features"),file = logfile)
  
  
  
  #calculating co-occurrences
  log_to_file(message = "<b>Step 7/9: Calculating Co-occurrences</b>",file = logfile)
  db_data$token<-db_data$token[,c("doc_id","token")]
  calculate_cooccurrences_all_measures()
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished calculating co-occurrences",file = logfile)
 
  #Saving results
  log_to_file(message = "<b>Step 8/9: Saving results</b>",file = logfile)
  path0<-paste0("collections/results/cooccurrence-analysis/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  dir.create(path0)

  save(terms,info,token,file=paste0(path0,"data_Coocs.RData"))
  save(coocs_matrix_dice,file=paste0(path0,"dice.RData"))
  save(coocs_matrix_count,file=paste0(path0,"count.RData"))
  save(coocs_matrix_mi,file=paste0(path0,"mi.RData"))
  save(coocs_matrix_log,file=paste0(path0,"log.RData"))
  save(info,file=paste0(path0,"info.RData"))
  save(parameters,file=paste0(path0,"parameters.RData"))
  save(dtm,file=paste0(path0,"dtm.RData"))
  log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
  
  #Wrinting metadata to database Task column
  log_to_file(message = "<b>Step 9/9: Writing task parameter to database</b>",file = logfile)
  write_metadata_to_database(parameters)
  log_to_file(message = " <b style='color:green'> ✔ </b>  Finished writing task parameter",logfile)
  
  log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Cooccurrences </b>",logfile)
  
  
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))

}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}




