source("global/text_functions.R")
source("global/log_to_file.R")
source("config_file.R")

error<-try(expr = {
  load("collections/tmp/tmp.RData")
  library(Matrix)
  library(dplyr)
  library(readr)
  library(plyr)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file("##############logs###############",logfile)
  
  
  path0<-paste0("collections/results/sentiment_analysis/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  #getting data from db
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  ids<-info[[3]]
  ids<- paste(ids[,1],collapse=", ")
  meta<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from documents where id in (",ids,");"))
  RMariaDB::dbDisconnect(mydb)
  
  
  
  log_to_file(message = "finished loading data from database",file = logfile)
  
  
  #get parameters
  parameters_no_NA<-lapply(X = parameters,FUN = function(x){
    if(is.na(x)){
      x<-NULL
    }
    return(x)
  })
  
  collection<-parameters_no_NA[[1]]
  baseform_reduction<-parameters_no_NA[[2]]
  stemming=FALSE
  if(baseform_reduction=="stemming"){
    stemming=TRUE 
  }
  ngrams<-as.numeric(parameters_no_NA[[3]])
  remove_custom<-parameters_no_NA[[4]]
  lowercase=parameters_no_NA[[5]]
  Sentiments_dict<-parameters_no_NA[[6]]
  avg=parameters_no_NA[[7]]
  #save parameters object for writing the metadata to DB
  parameters_for_meta<-data.frame(id=process_info[[1]],
                                  collection=parameters[[1]],
                                  task=process_info[[3]],
                                  started=process_info[[4]],
                                  baseform_reduction=parameters[[2]],
                                  lowercase=lowercase,
                                  ngrams=paste(as.numeric(parameters[[3]]),collapse=", "),
                                  Sentiment_Dictionary=parameters[[6]],
                                  Document_Score_Aggreagation=parameters[[7]]
                                  
  )
  
  
  control=list(
    tokenize="word",
    ngrams=ngrams,
    stem=stemming,
    remove_custom=unlist(stringr::str_split(string = remove_custom,pattern = ",")),
    tolower = lowercase,
    prune=list(
      min_termfreq= NULL,
      max_termfreq= NULL,
      min_docfreq=NULL, 
      max_docfreq=NULL,
      termfreq_type=NULL,
      docfreq_type=NULL
    )
  )
  
  colnames(meta)[7]<-"text"
    
  tow<-tmca.util::TextObjectWrapper$new()
  #set log destination
  tow$logging(logfile)
  #set input
  tow$input(x = meta)
  #process data 
  x<-tow$process(control = control,backend = "quanteda")
  #reset the preprocessing object
  tow$reset()
  log_to_file(message = "texts preprocessed",logfile)
  
  #get sentimentscore for every document
  #import sentiments
  Sentiments <- as.data.frame(read_csv(paste0("collections/sentiments/",Sentiments_dict), 
                         col_names = TRUE, col_types = cols(X1 = col_skip())))
  
  colnames(Sentiments)<-c("word","score")
  scores<-matrix(c(0),dim(meta)[1],1)
  for(i in 1:dim(meta)[1]){
    worte<-data.frame(word=x[[i,1]])
    Sentimets_reducred<-Sentiments[which(Sentiments[,1]%in%worte[,1]),]
    score<-join(worte,Sentiments,type="inner")
    if(avg=="mean"){
      scores[i,1]<-mean(score[,"score"],na.rm=T)
    }
    if(avg=="sum"){
      scores[i,1]<-sum(score[,"score"],na.rm=T)
    }
    if(i%%100==0)log_to_file(message = paste0("calculated scores for ",i," of ",dim(meta)[1]," documents"),logfile)
  }
  
  meta<-cbind(meta,scores)
  log_to_file(message = "finished calculating Sentiment scores",logfile)
  
  dir.create(path0)
  save(meta,file = paste0(path0,"data_Senti.RData"))
 
  log_to_file(message = "finished saving results",logfile)
  
  write_metadata_to_database(parameters_for_meta)
  
  log_to_file(message = "finished wrinting results metadata to database",logfile)
  
  
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  RMariaDB::dbDisconnect(mydb)
  
}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}


