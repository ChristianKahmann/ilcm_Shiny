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
  
  #load collection 
  log_to_file(message = "<b>Step 1/11: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  
  
  #load data from database
  log_to_file(message = "<b>Step 2/11: Loading data from database</b>",file = logfile)
  db_data<-get_token_meta_and_language_from_db()
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  
  #sanity check
  log_to_file(message = "<b>Step 3/11: Sanity check</b>",file = logfile)
  #enough dates specified
  log_to_file(message = "&emsp; enough points in time avaibale?",logfile)
  if(length(unique(db_data$meta[,2]))>parameters$va_history){
    log_to_file(message = "&emsp; ✔",logfile)
  }
  else{
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; The specified memory size is larger than the points in time avaiable. Please check your settings.</b>",logfile)
    stop("Memorysize larger, than the datapoints avaiable.")
  }
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
    log_to_file(message = "&emsp; stopwords available for specified language",logfile)
    if(!(db_data$language%in%stopwords::stopwords_getlanguages(source="stopwords-iso"))){
      log_to_file(message = "&emsp;<b style='color:red'>&#10008; The specified language is not included in stop word list. You can maybe specify a stopwordlist using the blacklist functionality.</b>",file = logfile)
      stop("Stopwords not avaiable for found language")
    }
    else{
      log_to_file(message = "&emsp; ✔",logfile)
    }
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished sanity checks",file = logfile)
  
  
  
  #preparing parameters
  log_to_file(message = "<b>Step 4/11: Preparing input parameters</b>",file = logfile)
  prepare_input_parameters()
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  #preparing token object
  log_to_file(message = "<b>Step 5/11: Preparing token object</b>",file = logfile)
  db_data$token<-prepare_token_object(token = db_data$token)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing token object",file = logfile)
  
  
  #calculating dtm
  log_to_file(message = "<b>Step 6/11: Calculating DTM</b>",file = logfile)
  dtm<-calculate_dtm()
  log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished pre-processing with",dim(dtm)[1], "documents and ",dim(dtm)[2], "features"),file = logfile)
  
  
  #calculate co-occurrence statistics
  log_to_file(message = "<b>Step 7/11: Calculating diachronic co-occurrence statistics</b>",file = logfile)
  calculate_diachronic_cooccurrences()
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished calculating co-occurrence slices",logfile)
  
  
  #calculate volatility
  log_to_file(message = "<b>Step 8/11: Calculating volatility statistics</b>",file = logfile)
  if(parameters$whitelist_only_results==TRUE){
    log_to_file(message = paste0("&emsp; Creating results only for words in whitelist and keep_custom input"),logfile)
    voldata<-tmca.contextvolatility::calculate_context_volatility(memory = parameters$va_history,Coocs_TimeSlices = diachron_Coocs,global = global_Coocs,un_dates=un_dates,
                                                                  terms = as.vector(stringr::str_split(string = parameters$keep_custom,pattern = ",",simplify = T)),
                                                                  measure =parameters$va_method,wf = parameters$va_weightfactor,logfile=logfile ) 
  }
  else{
    voldata<-tmca.contextvolatility::calculate_context_volatility(memory = parameters$va_history,Coocs_TimeSlices = diachron_Coocs,global = global_Coocs,un_dates=un_dates,
                                                                  terms = terms_to_use,
                                                                  measure =parameters$va_method,wf = "linear",logfile=logfile )
    if(length(empty_terms)>0){
      empty<-matrix(rep(0,(dim(voldata)[2]*length(empty_terms))),byrow = T,nrow = length(empty_terms))
      rownames(empty)<-colnames(diachron_Coocs[[1]])[empty_terms]
      voldata<-rbind(voldata,empty)
    }
  }
  log_to_file(message = " <b style='color:green'> ✔ </b>   Finished calculating volatility statistics",logfile)
  
  
  un_dates<-un_dates[(as.numeric(parameters$va_history)+1):length(un_dates)]
  diachron_Coocs<-diachron_Coocs[(as.numeric(parameters$va_history)+1):length(diachron_Coocs)]
  freq<-word_Frequencies[,(as.numeric(parameters$va_history)+1):dim(word_Frequencies)[2]]
  
  
  
  #extract NER- and POS Tags
  log_to_file(message = "<b>Step 9/11: Extracting NER- and POS-tags</b>",file = logfile)
  get_ner_and_pos_tags(token=db_data$token)
  log_to_file(message = " <b style='color:green'> ✔ </b>   Finished creating NER- and POS-tags",logfile)
  
  
  #Saving results
  log_to_file(message = "<b>Step 10/11: Saving results</b>",file = logfile)
  path<-paste(parameters$id,parameters$collection,sep = "_")
  path0<-paste0("collections/results/volatility-analysis/",path,"/")
  dir.create(path0)
  save(diachron_Coocs,file=paste0(path0,"CoocYears.RData"))
  save(ner_tags,pos_tags,file=paste0(path0,"tags.RData"))
  save(freq,file = paste0(path0,"freq.RData"))
  save(voldata,file = paste0(path0,"voldata.RData"))
  save(un_dates,file= paste0(path0,"labels.RData"))
  save(info,file=paste0(path0,"info.RData"))
  save(parameters,file=paste0(path0,"parameters.RData"))
  log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
  
  
  #Wrinting metadata to database Task column
  log_to_file(message = "<b>Step 11/11: Writing task parameter to database</b>",file = logfile)
  write_metadata_to_database(parameters)
  log_to_file(message = " <b style='color:green'> ✔ </b>  Finished writing task parameter",logfile)
  
  log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Context Volatility </b>",logfile)
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}

