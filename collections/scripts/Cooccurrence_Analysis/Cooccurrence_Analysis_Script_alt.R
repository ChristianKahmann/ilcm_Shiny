
source("global/text_functions.R")
source("global/log_to_file.R")
source("config_file.R")
#process
error<-try(expr = {
  load("collections/tmp/tmp.RData")
  library(Matrix)
  library(dplyr)
  library(spacyr)
  spacy_initialize()
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  
  path0<-paste0("collections/results/cooccurrence-analysis/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  log_to_file("##############logs###############",logfile)
  token<-NULL
  #getting data from db
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host)
  rs <-RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  d<-data.frame(id=info[[1]],dataset=info[[2]])
  for(i in 1:length(unique(d[,2]))){
    ids<-paste(d[which(d[,2]==unique(d[,2])[i]),1],collapse = " ")
    ids<-stringr::str_replace_all(string = as.character(ids),pattern = " ",",")
    token<-rbind(token,RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",unique(d[,2])[i],"' and id in (",ids,");",sep="")))
  }
  if(length(unique(d[,2]))>1){
    token[,2]<-as.numeric(factor(paste(token[,1],token[,2],sep="_")))
  }
  
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
  minchar<-parameters_no_NA[[3]]
  ngrams<-as.numeric(parameters_no_NA[[4]])
  remove_stopwords<-parameters_no_NA[[5]]
  lowercase<-parameters_no_NA[[6]]
  remove_numbers<-parameters_no_NA[[7]]
  remove_punctuation<-parameters_no_NA[[8]]
  remove_hyphenation<-parameters_no_NA[[9]]
  min_termfreq<-parameters_no_NA[[10]]
  max_termfreq<-parameters_no_NA[[11]]
  min_docfreq<-parameters_no_NA[[12]]
  max_docfreq<-parameters_no_NA[[13]]
  min_cooc_freq<-parameters_no_NA[[14]]
  cooc_type<-parameters_no_NA[[15]]
  sentences_as_documents=FALSE
  if(cooc_type=="Sen"){
    sentences_as_documents=TRUE
  }
  remove_custom<-parameters_no_NA[[16]]
  consolidate_entities<-parameters_no_NA[[17]]
  blacklist<-parameters_no_NA[[18]]
  pos_types<-parameters_no_NA[[19]]
  entity_types<-parameters_no_NA[[20]]
  termfreq_type<-parameters_no_NA[[21]]
  docfreq_type<-parameters_no_NA[[22]]
  
  if(!is.null(blacklist)){
    blacklist_words<-readChar(con=paste0("collections/blacklists/",blacklist,".txt"),nchars = file.info(paste0("collections/blacklists/",blacklist,".txt"))$size)
    remove_custom<-stringr::str_replace_all(string = paste(blacklist_words,",",remove_custom),pattern = ",,",replacement = ",")
    remove_custom<-stringr::str_replace_all(string=remove_custom,pattern = "\n",replacement = "")
    remove_custom<-stringr::str_replace_all(string=remove_custom,pattern = ", ",replacement = ",")
    remove_custom<-stringr::str_replace_all(string=remove_custom,pattern = " ,",replacement = ",")
    if(substr(remove_custom,nchar(remove_custom),nchar(remove_custom))==","){
      remove_custom<-substr(remove_custom,1,(nchar(remove_custom)-1))
    }
  }
  #save parameters object for writing the task metadata to DB
  parameters_for_meta<-data.frame(id=process_info[[1]],
                                  collection=parameters[[1]],
                                  task=process_info[[3]],
                                  started=process_info[[4]],
                                  baseform_reduction=parameters[[2]],
                                  minchar=paste(parameters[[3]],collapse=", "),
                                  ngrams=paste(as.numeric(parameters[[4]]),collapse=", "),
                                  remove_stopwords=parameters[[5]],
                                  lowercase=parameters[[6]],
                                  remove_numbers=parameters[[7]],
                                  remove_punctuation=parameters[[8]],
                                  remove_hyphenation=parameters[[9]],
                                  min_term=parameters[[10]],
                                  max_term=parameters[[11]],
                                  min_doc=parameters[[12]],
                                  max_doc=parameters[[13]],
                                  min_cooc_freq=parameters[[14]],
                                  cooc_type=parameters[[15]],
                                  consolidate=parameters[[17]],
                                  blacklist=parameters[[18]],
                                  POS_Types=parameters[[19]],
                                  NER_Tags=parameters[[20]],
                                  Pruning_term=parameters[[21]],
                                  Pruning_doc=parameters[[22]]
  )
  

  control=list(
    ngrams=ngrams,
    stem=stemming,
    remove_stopwords=remove_stopwords,
    remove_numbers=remove_numbers,
    remove_punctuation=remove_punctuation,
    remove_hyphenation=remove_hyphenation,
    remove_custom=unlist(stringr::str_split(string = remove_custom,pattern = ",")),
    tolower = lowercase,
    prune=list(
      min_termfreq= (min_termfreq),
      max_termfreq= (max_termfreq),
      min_docfreq=(min_docfreq), 
      max_docfreq=(max_docfreq),
      termfreq_type=termfreq_type,
      docfreq_type=docfreq_type
    ),
    char_length=minchar
  )
  
  #consolidate entities
  token<-token[,c(2,3,4,5,6,7,8)]
  class(token)<-c("spacyr_parsed","data.frame")
  colnames(token)<-c("doc_id"  ,    "sentence_id", "token_id"  ,  "token"    ,   "lemma"     ,  "pos"    ,     "entity" )
  gc()
  if(consolidate_entities==T){
    token<-spacyr::entity_consolidate(token)
    gc()
    log_to_file(message = "finished consolidating entities",file = logfile)
  }
  
  
  
  if(baseform_reduction=="lemma"){
    token<-token[,c(1,2,3,5,4,6,7)]
  }
  class(token)<-c("spacyr_parsed","data.frame")
  colnames(token)<-c("doc_id"  ,    "sentence_id", "token_id"  ,  "token"    ,   "lemma"     ,  "pos"    ,     "entity" )
  
  #filter for pos or ner tags
  if(!is.element(el = "all",set = pos_types)){
    token<-token[which(token[,6]%in%pos_types),]
  }
  
  if(!is.element(el = "all",set = entity_types)){
    if(consolidate_entities==F){
      token<-spacyr::entity_consolidate(token)
      log_to_file(message = "finished consolidating entities",file = logfile)
    }
    token<-token[which(token[,7]%in%entity_types),]
  }
  gc()
  tow<-tmca.util::TextObjectWrapper$new()
  #set log destination
  tow$logging(logfile)
  #set input
  tow$input(x = token)
  #
  if(sentences_as_documents==T){
    control_spacy=list(
      sentence_as_documents=sentences_as_documents
    )
    x<-tow$process(control = control_spacy,backend = "spacyr")
    gc()
    token[,1]<-paste0(token[,1],".",token[,2])
    token<-token[,c("doc_id","token")]
    
  }
  rm(x)
  gc()
  #process data and get dtm
  dtm<-tow$process(control = control,backend = "quanteda")%>%
    tow$output(format = "sparseMatrix")
  #reset Text wrapper object
  tow$reset()
  
  dtm<-tmca.util::make_binary(dtm = dtm)
  
  coocsCalc <- tmca.cooccurrence::Coocc$new(dtm)
  coocsCalc$set_significanceThreshold(0)
  coocsCalc$set_minCoocFreq(as.integer(min_cooc_freq))
  coocsCalc$set_maxCoocFreq(1000000)
  
  coocsCalc$set_measure("DICE")
  coocs_matrix_dice<-coocsCalc$ccoocs()
  log_to_file(message = "finshed calculating coocs with Dice-Significance measure",logfile)
  
  coocsCalc$set_measure("MI")
  coocs_matrix_mi<-coocsCalc$ccoocs()
  log_to_file(message = "finshed calculating coocs with Mutual Information measure",logfile)
  
  coocsCalc$set_measure("LOGLIK")
  coocs_matrix_log<-coocsCalc$ccoocs()
  log_to_file(message = "finshed calculating coocs with Log-likelihood measure",logfile)
  
  gc()
  
  #delete entries for words no co-occurrence
  diag(coocs_matrix_dice)<-0
  CS<-colSums(coocs_matrix_dice)
  coocs_matrix_dice<-coocs_matrix_dice[which(CS>0),which(CS>0)]
  coocs_matrix_mi<-coocs_matrix_mi[which(CS>0),which(CS>0)]
  coocs_matrix_log<-coocs_matrix_log[which(CS>0),which(CS>0)]
  
  terms<-colnames(coocs_matrix_dice)
  token<-token[,c("doc_id","token")]
  
  dir.create(path0)
  save(terms,info,token,file=paste0(path0,"data_Coocs.RData"))
  save(coocs_matrix_dice,file=paste0(path0,"dice.RData"))
  save(coocs_matrix_mi,file=paste0(path0,"mi.RData"))
  save(coocs_matrix_log,file=paste0(path0,"log.RData"))
  
  save(dtm,file=paste0(path0,"dtm.RData"))
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




