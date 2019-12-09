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
  log_to_file(message = "<b>Step 2/7: Loading data from database</b>",file = logfile)
  path0<-paste0("collections/results/sentiment_analysis/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  ids<-info[[3]]
  ids<- paste(ids[,1],collapse=", ")
  meta<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from documents where id in (",ids,");"))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  
  #preparing parameters
  log_to_file(message = "<b>Step 3/7: Preparing input parameters</b>",file = logfile)
  parameters<-prepare_input_parameters(parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  
  #calculate dfm object
  log_to_file(message = "<b>Step 4/7: Calculate tokens_object object</b>",file = logfile)
  tokens_object<-calculate_sentiments_analysis_tokens_object(parameters = parameters,meta = meta)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  
  #import sentiments
  log_to_file(message = "<b>Step 5/7: Importing sentiments</b>",file = logfile)
  Sentiments <- as.data.frame(read_csv(paste0("collections/sentiments/",parameters$Sentiment_Dictionary), 
                                       col_names = TRUE, col_types = cols(X1 = col_skip())))
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
  save(info,file=paste0(path0,"info.RData"))
  log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
  
  

  
  log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Sentiment Analysis </b>",logfile)
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  
}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}

