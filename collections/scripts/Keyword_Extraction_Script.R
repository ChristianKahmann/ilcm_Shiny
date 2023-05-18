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
  library(udpipe)
  library(textrank)
  #load parameters
  load("collections/tmp/tmp.RData")
  parameters_original<-parameters
  
  #load collection 
  log_to_file(message = "<b>Step 1/6: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  #load data from database
  log_to_file(message = "<b>Step 2/6: Loading data from database</b>",file = logfile)
  db_data<-get_token_meta_and_language_from_db(get_meta = F,get_language = T,get_global_doc_ids = F,host=host,port=db_port,id=info[[1]],dataset=info[[2]])
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  #sanity check
  log_to_file(message = "<b>Step 3/6: Sanity check</b>",file = logfile)
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
  
  
  #######################################
  # Interview data filter via speaker?!
  meta_body_replacemets<-NULL
  if(parameters$ke_interview_use_speaker==TRUE){
    log_to_file(message = "<b>Apply Speaker Restriction</b>",file = logfile)
    if(length(parameters$ke_interview_speaker_filter)>0){
      dataset = info[[2]][1,1]
      id_docs = info[[1]][,1]
      id_docs = paste0(id_docs,collapse=" ")
      id_docs<-stringr::str_replace_all(string = as.character(id_docs),pattern = " ",",")
      
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
      rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
      data_interview<-RMariaDB::dbGetQuery(mydb, paste("select * from interview_info where id_doc in (",id_docs,")",
                                                       " and trim(dataset)='",dataset,"';",sep = ""))
      speaker_ids <- which(grepl(x = data_interview$sprecher,pattern = stringr::regex(paste0("(",parameters$ke_interview_speaker_filter,")",collapse="|"))))
      good_rows <- paste0(data_interview$id_interview[speaker_ids],"_",data_interview$id_interview_row[speaker_ids])
      
      db_data$token <- db_data$token[which(paste0(db_data$token$id_interview,"_",db_data$token$id_interview_row)%in%good_rows),]
      meta_body_replacemets <- unlist(lapply(unique(db_data$token$id),FUN = function(x){
        paste0(db_data$token$word[which(db_data$token$id==x)],collapse=" ")
      }))
    }
    else{
      log_to_file(message = "&emsp;<b> No Filter was specified. Using whole texts instead</b>",logfile)
    }
  }
  
  
  #######################################
  
  
  
  #preparing parameters
  log_to_file(message = "<b>Step 4/6: Preparing input parameters</b>",file = logfile)
  parameters<-prepare_input_parameters(parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  #get keywords
  token<-db_data$token[,2:7]
  colnames(token)<-c("doc_id","sentence_id","token_id","token","lemma","upos")
  if(parameters$lowercase==TRUE){
    token$token<-tolower(token$token)
    token$lemma<-tolower(token$lemma)
  }
  if(parameters$baseform_reduction=="lemma"){
    term<-"lemma"
  }
  else{
    term<-"token"
  }
  log_to_file(message = paste0("<b>Step 5/6: Identifying Keywords using strategy ",parameters$KE_no_ref_method,"</b>"),file = logfile)
  if(parameters$KE_no_ref_method=="RAKE"){
    relevant=token$upos %in% parameters$KE_filter
    if(length(relevant)>0){
      stats <- keywords_rake(x = token, term = term, group = "sentence_id", ngram_max = parameters$KE_no_ref_ngram_max,n_min = parameters$KE_no_ref_n_min,
                             relevant =token$upos %in% parameters$KE_filter,sep = parameters$KE_seperator)
      correct_number_of_words <- unlist(lapply(stringr::str_split(string = stats$keyword, pattern = "_",simplify = F),FUN = length))
      stats$ngram <- correct_number_of_words 
    }
    else{
      log_to_file(message = "&emsp;<b style='color:red'>&#10008; No relevant words were selected to be used in RAKE. Check your settings.</b>",logfile)
      stop("no relevant words selected for RAKE")
    }
  }
  if(parameters$KE_no_ref_method=="PMI Collocation"){
    stats <- keywords_collocation(x = token, term = term, group = "sentence_id",ngram_max = parameters$KE_no_ref_ngram_max,n_min = parameters$KE_no_ref_n_min,sep = parameters$KE_seperator )
  }
  if(parameters$KE_no_ref_method=="Phrase Sequence"){
    token$upos<-stringr::str_replace_all(string = token$upos,pattern = "CONJ",replacement = "CCONJ")
    token$upos<-stringr::str_replace_all(string = token$upos,pattern = "SCCONJ",replacement = "CCONJ")
    token$upos<-stringr::str_replace_all(string = token$upos,pattern = "SPACE",replacement = "X")
    token$phrase_tag <- as_phrasemachine(token$upos, type = "upos")
    stats <- keywords_phrases(x = token$phrase_tag, term = token[,term], 
                              pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                              is_regex = TRUE, detailed = FALSE,sep = parameters$KE_seperator)
  }
  if(parameters$KE_no_ref_method=="Textrank"){
    relevant=token$upos %in% parameters$KE_filter
    if(length(relevant)>0){
      stats <- textrank_keywords(x = token[,term],ngram_max = parameters$KE_no_ref_ngram_max,relevant = relevant,sep = parameters$KE_seperator )$keywords
    }
    else{
      log_to_file(message = "&emsp;<b style='color:red'>&#10008; No relevant words were selected to be used in RAKE. Check your settings.</b>",logfile)
      stop("no relevant words selected for Textrank")
    }
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Keywords identified",file = logfile)
  
  #Saving results
  log_to_file(message = "<b>Step 6/6: Saving results</b>",file = logfile)
  path0<-paste0("collections/results/keyword-extraction/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  dir.create(path0)
  method<-parameters$KE_no_ref_method
  save(stats,method,file = paste0(path0,"stats.RData"))
  save(info,file=paste0(path0,"info.RData"))
  parameters<-parameters_original
  save(parameters,file=paste0(path0,"parameters.RData"))
  log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
  
  log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Keyword Extraction </b>",logfile)
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  
}) 

if(class(error)=="try-error"){
  try({
    system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  })
  try({
    RMariaDB::dbDisconnect(mydb)
  })
  log_to_file(message = "&emsp;<b style='color:red'>An error has occurred: </b>",file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}