source("global/text_functions.R")
source("global/log_to_file.R")
source("global/rbind_huge_sparse_Matrix.R")
source("config_file.R")
source("global/functions_used_in_scripts.R")

error<-try(expr = {
  library(Matrix)
  library(dplyr)
  library(spacyr)
  #load parameters
  load("collections/tmp/tmp.RData")
  parameters_original<-parameters
  
  #load collection 
  log_to_file(message = "<b>Step 1/8: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  
  
  #load data from database
  log_to_file(message = "<b>Step 2/8: Loading data from database</b>",file = logfile)
  db_data<-get_token_meta_and_language_from_db(host=host,port=db_port,id=info[[1]],dataset=info[[2]])
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  
  #sanity check
  log_to_file(message = "<b>Step 3/8: Sanity check</b>",file = logfile)
  #token object not empty
  log_to_file(message = "&emsp; token object not empty?",logfile)
  if(dim(db_data$token)[1]>1){
    log_to_file(message = "&emsp; ✔",logfile)
  }
  else{
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No documents were found in the database for the specified collection.</b>",logfile)
    stop("Token empty")
  }
  #specified language has quanteda stopwordlist?
  if(parameters$remove_stopwords==T){
    log_to_file(message = "&emsp; stopwords available for specified language?",logfile)
    if(!(db_data$language%in%stopwords::stopwords_getlanguages(source="stopwords-iso"))){
      log_to_file(message = "&emsp;<b style='color:red'>&#10008; The specified language is not included in stop word list. You can maybe specify a stopwordlist using the blacklist functionality.</b>",file = logfile)
      stop("Stopwords not available for found language")
    }
    else{
      log_to_file(message = "&emsp; ✔",logfile)
    }
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished sanity checks",file = logfile)
  
  
  
  #preparing parameters
  log_to_file(message = "<b>Step 4/8: Preparing input parameters</b>",file = logfile)
  parameters<-prepare_input_parameters(parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  #preparing token object
  log_to_file(message = "<b>Step 5/8: Preparing token object</b>",file = logfile)
  db_data$token<-prepare_token_object(token = db_data$token,parameters=parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing token object",file = logfile)
  
  
  #calculating dtm
  log_to_file(message = "<b>Step 6/8: Calculating DTM</b>",file = logfile)
  dtm<-calculate_dtm(token = db_data$token,parameters = parameters,lang = db_data$language)
  log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished pre-processing with",dim(dtm)[1], "documents and ",dim(dtm)[2], "features"),file = logfile)
  
  
  #calculating frequencies
  log_to_file(message = "<b>Step 7/8: Calculating frequencies</b>",file = logfile)
  frequencies<-calculate_diachron_frequencies(dtm=dtm,meta=db_data$meta)
  doc_freqs_year<-frequencies$doc_freqs_year
  doc_freqs_month<-frequencies$doc_freqs_month
  doc_freqs_week<-frequencies$doc_freqs_week
  doc_freqs_day<-frequencies$doc_freqs_day
  freqs_year<-frequencies$freqs_year
  freqs_month <-frequencies$freqs_month
  freqs_week<-frequencies$freqs_week
  freqs_day<-frequencies$freqs_day
  rel_doc_freqs_year<-frequencies$rel_doc_freqs_year
  rel_doc_freqs_month<-frequencies$rel_doc_freqs_month
  rel_doc_freqs_week<-frequencies$rel_doc_freqs_week
  rel_doc_freqs_day<-frequencies$rel_doc_freqs_day
  rel_freqs_year<-frequencies$rel_freqs_year
  rel_freqs_month<-frequencies$rel_freqs_month
  rel_freqs_week<-frequencies$rel_freqs_week
  rel_freqs_day<-frequencies$rel_freqs_day
  
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished calculating frequencies",file = logfile)
  

  
  
  #Saving results
  log_to_file(message = "<b>Step 8/8: Saving results</b>",file = logfile)
  vocab<-cbind(colnames(dtm),colSums(dtm))
  vocab<-vocab[order(as.numeric(vocab[,2]),decreasing = T),]
  path<-paste(parameters$id,parameters$collection,sep = "_")
  path0<-paste0("collections/results/frequency-extraction/",path,"/")
  dir.create(path0)
  save(freqs_day,freqs_week,freqs_month,freqs_year,doc_freqs_day,doc_freqs_week,doc_freqs_month,doc_freqs_year,
       rel_freqs_day,rel_freqs_week,rel_freqs_month,rel_freqs_year,rel_doc_freqs_day,rel_doc_freqs_week,rel_doc_freqs_month,rel_doc_freqs_year,file = paste0(path0,"frequencies.RData"))
  save(vocab,file=paste0(path0,"vocab.RData"))
  save(dtm,file=paste0(path0,"dtm.RData"))
  save(info,file=paste0(path0,"info.RData"))
  parameters<-parameters_original
  save(parameters,file=paste0(path0,"parameters.RData"))
  log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)

  
  log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Term Frequency Extraction </b>",logfile)
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  
}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}



