source("global/text_functions.R")
source("global/log_to_file.R")
source("global/rbind_huge_sparse_Matrix.R")
source("config_file.R")
source("global/functions_used_in_scripts.R")
source("global/match_language.R")

#process
error<-try(expr = {
  library(Matrix)
  library(dplyr)
  library(spacyr)
  library(udpipe)
  #load parameters
  load("collections/tmp/tmp.RData")
  parameters_original<-parameters
  
  #load collection 
  log_to_file(message = "<b>Step 1/7: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  
  
  #load data from database
  log_to_file(message = "<b>Step 2/7: Loading data from database</b>",file = logfile)
  db_data<-get_token_meta_and_language_from_db(get_meta = F,get_language = T,get_global_doc_ids = F,host=host,port=db_port,id=info[[1]],dataset=info[[2]])
  #token<-db_data$token[,c("id","word")]
  #colnames(token)<-c("doc_id","token")
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  ids<-info[[3]]
  ids<- paste(ids[,1],collapse=", ")
  meta<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from documents where id in (",ids,");"))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  
  #sanity check
  log_to_file(message = "<b>Step 3/7: Sanity check</b>",file = logfile)
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
  log_to_file(message = "<b>Step 4/7: Preparing input parameters</b>",file = logfile)
  parameters<-prepare_input_parameters(param = parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  #annotate texts
  #Fetching model
  log_to_file(message = "<b>Step 5/7:Fetching model</b>",file = logfile)
  models<-list.files("collections/ud_models/",full.names = T)
  lang<-match_language_udpipe(db_data$language)
  if(any(grepl(pattern = lang,x = models))){
    model<-models[which(grepl(pattern = lang,x = models))[1]]
    log_to_file(message = paste0("&emsp; Model found: ",model),logfile)
  }
  else{
    model<-udpipe_download_model(language = lang,model_dir = "collections/ud_models/")$file_model
    log_to_file(message = paste0("&emsp; Model downloaded: ",model),logfile)
  }
  model<-udpipe_load_model(file = model)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished fetching model",file = logfile)
  
  
  
  
  #annotating
  log_to_file(message = "<b>Step 6/7:Annotating the data</b>",file = logfile)
  txt<-meta$body
  txt_split<-split(txt,seq(1,length(txt),by=ceiling(length(txt)/10)))
  doc_id_split<-split(paste(meta$dataset,meta$id_doc,sep = "_"),seq(1,length(paste(meta$dataset,meta$id_doc,sep = "_")),by=ceiling(length(paste(meta$dataset,meta$id_doc,sep = "_"))/10)))
  annotation<-NULL
  loghelper<-floor(seq(1,length(txt_split),length.out = 11))[2:11]
  names(loghelper)<-c(10,20,30,40,50,60,70,80,90,100)
  for(i in 1:length(txt_split)){
    if(parameters$cores==1){
      x <- udpipe_annotate(model, x = txt_split[[i]],doc_id = doc_id_split[[i]])
      annotation <- rbind(annotation,as.data.frame(x)[,c("doc_id","paragraph_id","sentence_id","token_id","token","lemma","upos","xpos","feats","head_token_id","dep_rel")])
      log_to_file(message = paste0("&emsp; ",names(which(loghelper==i)),"% of documents processed (syntactic parsing)"),logfile)
    }
    else{
      a<-pbmcapply::pbmclapply(X = 1:length(txt_split[[i]]),mc.cores = parameters$cores,FUN = function(x){
        as.data.frame(udpipe_annotate(model, x = txt_split[[i]][x],doc_id = doc_id_split[[i]][x]))[,c("doc_id","paragraph_id","sentence_id","token_id","token","lemma","upos","xpos","feats","head_token_id","dep_rel")]
      })
      annotation <- rbind(annotation,data.table::rbindlist(a))
      log_to_file(message = paste0("&emsp; ",names(which(loghelper==i)),"% of documents processed (syntactic parsing)"),logfile)
    }
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished annotating the data",file = logfile)
  
  
  
  #Saving results
  log_to_file(message = "<b>Step 7/7: Saving results</b>",file = logfile)
  path0<-paste0("collections/results/syntactic-parsing/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  dir.create(path0)
  save(annotation,file = paste0(path0,"annotations.RData"))
  save(meta,file = paste0(path0,"meta.RData"))
  save(info,file=paste0(path0,"info.RData"))
  parameters<-parameters_original
  save(parameters,file=paste0(path0,"parameters.RData"))
  log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
  
  

  
  log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Syntactic Parsing </b>",logfile)
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  
}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}

