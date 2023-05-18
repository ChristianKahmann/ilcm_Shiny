source("global/text_functions.R")
source("global/log_to_file.R")
source("config_file.R")
source("global/functions_used_in_scripts.R")

error<-try(expr = {
  library(Matrix)
  library(dplyr)
  library(readr)
  library(plyr)
  #load parameters
  load("collections/tmp/tmp.RData")
  parameters_original<-parameters
  
  
  #load collection 
  log_to_file(message = "<b>Step 1/7: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  #load data from database
  log_to_file(message = "<b>Step 2/13: Loading data from database</b>",file = logfile)
  db_data<-get_token_meta_and_language_from_db(get_meta = F,get_language = T,get_global_doc_ids = F,host=host,port=db_port,id=info[[1]],dataset=info[[2]])
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  # #load data from database
  # log_to_file(message = "<b>Step 2/7: Loading data from database</b>",file = logfile)
  path0<-paste0("collections/results/sentiment_analysis/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  # # mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  # # rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  # # ids<-info[[3]]
  # # ids<- paste(ids[,1],collapse=", ")
  # # meta<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from documents where id in (",ids,");"))
  # # RMariaDB::dbDisconnect(mydb)
  # log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  #######################################
  # Interview data filter via speaker?!
  meta_body_replacemets<-NULL
  if(parameters$sa_interview_use_speaker==TRUE){
    log_to_file(message = "<b>Apply Speaker Restriction</b>",file = logfile)
    if(length(parameters$sa_interview_speaker_filter)>0){
      dataset = info[[2]][1,1]
      id_docs = info[[1]][,1]
      id_docs = paste0(id_docs,collapse=" ")
      id_docs<-stringr::str_replace_all(string = as.character(id_docs),pattern = " ",",")
      
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
      rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
      data_interview<-RMariaDB::dbGetQuery(mydb, paste("select * from interview_info where id_doc in (",id_docs,")",
                                                       " and trim(dataset)='",dataset,"';",sep = ""))
      speaker_ids <- which(grepl(x = data_interview$sprecher,pattern = stringr::regex(paste0("(",parameters$sa_interview_speaker_filter,")",collapse="|"))))
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
  log_to_file(message = "<b>Step 3/7: Preparing input parameters</b>",file = logfile)
  parameters<-prepare_input_parameters(parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  #get metadata
  #log_to_file(message = "<b>Step 6/13: Getting metadata for detailed metadata analysis from database</b>",file = logfile)
  meta_data<-get_meta_data_for_detailed_topic_analysis(host = host,port = db_port,ids = info[[3]],datasets = unique(info[[2]]),token = db_data$token)
  meta<-meta_data$meta
  meta_names<-meta_data$meta_names
  if(!is.null(meta_body_replacemets)){
    meta <- meta[which(meta$id_doc%in%unique(db_data$token$id)),]
    meta$body<-meta_body_replacemets
  }
  #log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  #calculate dfm object
  log_to_file(message = "<b>Step 4/7: Calculate tokens_object object</b>",file = logfile)
  tokens_object<-calculate_sentiments_analysis_tokens_object(parameters = parameters,meta = meta)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  
  #import sentiments
  log_to_file(message = "<b>Step 5/7: Importing sentiments</b>",file = logfile)
  Sentiments <- as.data.frame(read_csv(paste0("collections/sentiments/",parameters$Sentiment_Dictionary), 
                                       col_names = TRUE, col_types = cols(X1 = col_skip())))
  Sentiments<-Sentiments[,(ncol(Sentiments)-1):(ncol(Sentiments))]
  colnames(Sentiments)<-c("word","score")
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished importing sentiments",file = logfile)
  
  
  
  #calculate sentiment scores
  log_to_file(message = "<b>Step 6/7: Calculating sentiment scores</b>",file = logfile)
  scores<-matrix(c(0),dim(meta)[1],1)
  loghelper<-floor(seq(1,dim(meta)[1],length.out = 11))[2:11]
  names(loghelper)<-c(10,20,30,40,50,60,70,80,90,100)
  tokens_object<-as.list(tokens_object)
  for(i in 1:dim(meta)[1]){
    worte<-data.frame(word=tokens_object[[i]])
    #Sentimets_reducred<-Sentiments[which(Sentiments[,1]%in%worte[,1]),]
    score<-plyr::join(worte,Sentiments,type="inner")
    if(parameters$Document_Score_Aggregation=="mean"){
      scores[i,1]<-mean(score[,"score"],na.rm=T)
    }
    if(parameters$Document_Score_Aggregation=="sum"){
      scores[i,1]<-sum(score[,"score"],na.rm=T)
    }
    if(i %in% loghelper){
      log_to_file(message = paste0("&emsp; ",names(which(loghelper==i)),"% of documents processed (",i,")"),logfile)
    }
    
  }
  meta<-cbind(meta,scores)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished calculating sentiments scores",file = logfile)
  

  
  #Saving results
  log_to_file(message = "<b>Step 7/7: Saving results</b>",file = logfile)
  dir.create(path0)
  save(meta,file = paste0(path0,"data_Senti.RData"))
  parameters<-parameters_original
  save(parameters,file=paste0(path0,"parameters.RData"))
  #save(meta,meta_names,file=paste0(path0,"meta_SA.RData"))
  save(info,file=paste0(path0,"info.RData"))
  log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
  
  

  
  log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Sentiment Analysis </b>",logfile)
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

