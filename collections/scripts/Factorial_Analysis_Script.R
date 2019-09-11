source("global/text_functions.R")
source("global/log_to_file.R")
source("global/rbind_huge_sparse_Matrix.R")
source("config_file.R")

error<-try(expr = {
  load("collections/tmp/tmp.RData")
  library(Matrix)
  library(dplyr)
  library(spacyr)
  spacy_initialize()
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  
  log_to_file("##############logs###############",logfile)
  token<-NULL
  meta=NULL
  #getting data from db
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  d<-data.frame(id=info[[1]],dataset=info[[2]])
  for(i in 1:length(unique(d[,2]))){
    ids<-paste(d[which(d[,2]==unique(d[,2])[i]),1],collapse = " ")
    ids<-stringr::str_replace_all(string = as.character(ids),pattern = " ",",")
    token<-rbind(token,RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",unique(d[,2])[i],"' and id in (",ids,");",sep="")))
    meta<-rbind(meta,RMariaDB::dbGetQuery(mydb, paste("select id_doc,dataset,date,token,mde1,mde2,mde3,mde4,mde5,mde6,mde7,mde8,mde9  from documents where dataset='",unique(d[,2])[i],"' and id_doc in (",ids,");",sep="")))
  }
  ava<-RMariaDB::dbGetQuery(mydb, paste0("SELECT * FROM ilcm.metadata_names where dataset in('",paste(unique(d[,2]),collapse="','"),"');"))

  #get language // so far just use language of first document for all
  language<-as.character(RMariaDB::dbGetQuery(mydb, paste("select language from documents where dataset='",d[1,2],"' and id_doc =",d[1,1],";",sep="")))
  
  #reduce to need metadata
  empty_metadata<-names(which(apply(ava,MARGIN = 2,function(x){all(is.na(x))})))
  if(length(empty_metadata)>0){
    meta<-meta[,-which(colnames(meta)%in%empty_metadata)]
  }
  meta_new<-data.frame()
  #split metadata for differing datasets/metadata names
  for(d in unique(info[[2]])){
    meta_tmp<-meta[which(meta[,"dataset"]==d),] 
    meta_tmp<-meta_tmp[,which(apply(meta_tmp,2,function(x){!all(is.na(x))}))]
    if(dim(meta_tmp)[1]>0){
      colnames(meta_tmp)<-c("id_doc","dataset","date","token",(ava)[which(ava[,1]==d),2:length(colnames(ava))][!is.na(ava[which(ava[,1]==d),-1])])
      meta_new<-plyr::rbind.fill(meta_new,meta_tmp)
    }
  }
  
  meta_names<-colnames(meta_new)[which(!colnames(meta_new)%in%c("id_doc","dataset","date","token"))]
  if(length(meta)>0){
    meta_new<-meta_new[,c("id_doc","dataset","date","token",meta_names)]
  }
  
  meta<-meta_new
  
  meta[,3]<-as.character(as.Date(meta[,3]))
  
  x<-as.numeric(factor(paste(token[,1],token[,2],sep="_")))
  token[,2]<-x
  meta[,1]<-unique(x)
  
  
  
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
  remove_custom<-parameters_no_NA[[14]]
  consolidate_entities<-parameters_no_NA[[15]]
  blacklist<-parameters_no_NA[[16]]
  pos_types<-parameters_no_NA[[17]]
  entity_types<-parameters_no_NA[[18]]
  termfreq_type<-parameters_no_NA[[19]]
  docfreq_type<-parameters_no_NA[[20]]
  
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
  
  #save parameters object for writing the metadata to DB
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
                                  consolidate=parameters[[15]],
                                  Pruning_term=parameters[[19]],
                                  Pruning_doc=parameters[[20]],
                                  blacklist=parameters[[16]],
                                  POS_Types=paste(parameters[[17]],collapse=", "),
                                  NER_Tags=paste(parameters[[18]],collapse=", ")
  )
  

  #consolidate entities
  token<-token[,c(2,3,4,5,6,7,8)]
  class(token)<-c("spacyr_parsed","data.frame")
  colnames(token)<-c("doc_id"  ,    "sentence_id", "token_id"  ,  "token"    ,   "lemma"     ,  "pos"    ,     "entity" )
  if(consolidate_entities==T){
    token<-spacyr::entity_consolidate(token)
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
    log_to_file(message = "finished filtering for pos types",file = logfile)
  }
  
  if(!is.element(el = "all",set = entity_types)){
    if(consolidate_entities==F){
      token<-spacyr::entity_consolidate(token)
      log_to_file(message = "finished consolidating entities",file = logfile)
    }
    token<-token[which(token[,7]%in%entity_types),]
  }
  
  
  
  control=list(
    language=language,
    ngrams=ngrams,
    stem=stemming,
    remove_stopwords=remove_stopwords,
    remove_numbers=remove_numbers,
    remove_punctuation=remove_punctuation,
    remove_hyphenation=remove_hyphenation,
    remove_custom=unlist(stringr::str_split(string = remove_custom,pattern = ",")),
    tolower = lowercase,
    char_length=minchar
  )
  #split token
  split<-split(unique(token[,1]), ceiling(seq_along(unique(token[,1]))/100))
  log_to_file(message = paste0("split corpus in ",length(split)," chunks"),logfile)
  for(i in 1:length(split)){
    log_to_file(message = paste0("preprocessing for chunk ",i," of ",length(split)),logfile)
    #log_to_file(message = "______________________________________________",logfile)
    tow<-tmca.util::TextObjectWrapper$new()
    tow$logging("silent")
    tow$input(x = token[which(token[,1]%in%split[[i]]),])
    if(i==1){
      dtm_glob<-tow$process(control = control,backend = "quanteda")%>%
        tow$output(format = "sparseMatrix")
    }
    else{
      dtm_glob<-rBind_huge(dtm_glob,tow$process(control = control,backend = "quanteda")%>%
                             tow$output(format = "sparseMatrix"))
    }
    try({
      rm(x)
    })
    print(i)
  }
  
  gc()
  dtm<-quanteda::as.dfm(x = dtm_glob)  
  tow$.__enclos_env__$private$internal_representation<-dtm
  control_prune=list(
    prune=list(
      min_termfreq= (min_termfreq),
      max_termfreq= (max_termfreq),
      min_docfreq=(min_docfreq), 
      max_docfreq=(max_docfreq),
      termfreq_type=termfreq_type,
      docfreq_type=docfreq_type
    )
  )
  
  dtm<-tow$process(control = control_prune,backend = "quanteda")%>%
    tow$output(format = "sparseMatrix")
  rm(tow)
  gc()

  
  log_to_file(message = paste("finished pre-processing with",dim(dtm)[1], "documents and ",dim(dtm)[2], "words"),file = logfile)
  
  task_name<-paste((string=readLines(logfile)[1:2]),collapse = "_")
  task_name<-stringr::str_replace(string = task_name,pattern = ".RData",replacement = "")
  path0<-paste0("collections/results/factorial-analysis/",task_name,"/")
  dir.create(path0)
  
  save(dtm,file=paste0(path0,"dtm.RData"))
  save(info,file=paste0(path0,"info.RData"))
  save(meta,file=paste0(path0,"meta.RData"))
  
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


