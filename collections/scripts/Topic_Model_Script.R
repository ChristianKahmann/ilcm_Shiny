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
  log_to_file(message = "<b>Step 1/13: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  
  
  
  #load data from database
  log_to_file(message = "<b>Step 2/13: Loading data from database</b>",file = logfile)
  db_data<-get_token_meta_and_language_from_db(get_meta = F,get_language = T,get_global_doc_ids = F,host=host,port=db_port,id=info[[1]],dataset=info[[2]])
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  
  
  #sanity check
  log_to_file(message = "<b>Step 3/13: Sanity check</b>",file = logfile)
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
  log_to_file(message = "<b>Step 4/13: Preparing input parameters</b>",file = logfile)
  parameters<-prepare_input_parameters(parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  
  
  #get original documents 
  log_to_file(message = "<b>Step 5/13: Create original documents</b>",file = logfile)
  doc_ids<-unique(db_data$token[,2])
  documents_original<-unlist(lapply(X = doc_ids,FUN = function(x){
    paste(db_data$token[which(db_data$token[,1]==x),4],collapse=" ")
  }))
  documents_original<-cbind(doc_ids,documents_original)  
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)

  
  #get metadata
  log_to_file(message = "<b>Step 6/13: Getting metadata for detailed metadata analysis from database</b>",file = logfile)
  if(parameters$tm_detailed_meta==TRUE){
    meta_data<-get_meta_data_for_detailed_topic_analysis(host = host,port = db_port,ids = info[[3]],datasets = unique(info[[2]]),token = db_data$token)
    meta<-meta_data$meta
    meta_names<-meta_data$meta_names
  }
  else{
    log_to_file(message = " &emsp; Detailed metadata analysis not selected ",file = logfile)
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  
  
  
  #preparing token object
  log_to_file(message = "<b>Step 7/13: Preparing token object</b>",file = logfile)
  db_data$token<-prepare_token_object(token = db_data$token,parameters=parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing token object",file = logfile)
  
  
  
  
  
  #calculating dtm
  log_to_file(message = "<b>Step 8/13: Calculating DTM</b>",file = logfile)
  dtm<-calculate_dtm(token = db_data$token,parameters = parameters,lang = db_data$language)
  log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished pre-processing with",dim(dtm)[1], "documents and ",dim(dtm)[2], "features"),file = logfile)
  
  
  
  
  
  log_to_file(message = "<b>Step 9/13: Clean vocabulary from non asci2 characters</b>",file = logfile)
  #just keep alpha
  # remove<-which(!grepl("[[:alpha:]]", colnames(dtm)) & !grepl("[[:digit:]]", colnames(dtm)))
  # if(length(remove)>0){
  #   dtm<-dtm[,-remove]
  # }
  # #remove all that contains non asci2
  # remove<-which(unlist(lapply(colnames(dtm),function(i){max(utf8ToInt(i))}))>256)
  # if(length(remove)>0){
  #   dtm<-dtm[,-remove]
  # }
  # #remove all that contains non asci2
  # remove<-which(unlist(lapply(colnames(dtm),function(i){min(utf8ToInt(i))}))<33)
  # if(length(remove)>0){
  #   dtm<-dtm[,-remove]
  # }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  
  
  
  
  #remove empty documents
  log_to_file(message = "<b>Step 10/13: Removing empty documents</b>",file = logfile)
  empty<-which(Matrix::rowSums(dtm)==0)
  if(length(empty)>0){
    dtm<-dtm[-empty,]
    documents<-documents[-empty,]
    documents_original<-documents_original[-empty,]
    info[[6]]<-data.frame(info[[6]][-empty,1],stringsAsFactors = F)
    if(parameters$tm_detailed_meta==TRUE){
      meta<-meta[-empty,]
    }
  }
  log_to_file(message = paste0("<b style='color:green'> ✔ </b>  Deleted ",length(empty)," documents from topic model, because they are empty with the current settings"),logfile)  
  
  
  
  
  
  #calculate topic model
  log_to_file(message = "<b>Step 11/13: Create Topic Model</b>",file = logfile)
  t <- tmca.unsupervised::tmodel$new(method =parameters$tm_method)
  t$input_preprocessed(dtm = dtm,documents)
  t$set_parameters(par_list = list("alpha"=parameters$tm_alpha,"K"=parameters$tm_number_of_topics))
  t$create_tm()
  
  
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished calculating topic model",file = logfile)
  
  log_to_file(message = "<b>Step 12/13: Create Variables for Visulization</b>",file = logfile)
  model<-t$get_model()
  theta<-model$theta
  phi<-model$phi
  doc.length<-Matrix::rowSums(dtm)
  vocab<-colnames(phi)
  term.frequency<-Matrix::colSums((dtm[,vocab]))
  topic.frequency <- colSums(theta * doc.length)
  topic.proportion <- topic.frequency/sum(topic.frequency)
  o <- order(topic.proportion, decreasing = TRUE)
  phi <- phi[o, ]
  theta <- theta[, o]
  topic.frequency <- topic.frequency[o]
  topic.proportion <- topic.proportion[o]
  json <- LDAvis::createJSON(
    phi = phi, 
    theta = theta, 
    doc.length = doc.length, 
    vocab = vocab, 
    term.frequency = term.frequency,
    reorder.topics=FALSE
  )
  log_to_file(message = "  <b style='color:green'> ✔ </b>  ",file = logfile)
  
  
  
  
  #Saving results
  log_to_file(message = "<b>Step 13/13: Saving results</b>",file = logfile)
  lang<-db_data$language
  path0<-paste0("collections/results/topic-model/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  dir.create(path0)
  save(model,t,theta,phi,doc.length,vocab,term.frequency,json,info,lang,file = paste0(path0,"data_TM.RData"))
  save(dtm,file=paste0(path0,"dtm_TM.RData"))
  save(documents_original,file=paste0(path0,"documents_TM.RData"))
  #save(rel_counts,freqs,file=paste0(path0,"est_counts_TM.RData"))
  if(parameters$tm_detailed_meta==TRUE){
    save(meta,meta_names,file=paste0(path0,"meta_TM.RData"))
  }
  save(info,file=paste0(path0,"info.RData"))
  parameters<-parameters_original
  save(parameters,lang,file=paste0(path0,"parameters.RData"))
  log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
  
  
  

  
  log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Topic Model </b>",logfile)
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  
}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}


