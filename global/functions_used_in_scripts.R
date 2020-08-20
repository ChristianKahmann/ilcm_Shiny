library(tidyverse)
library(shinyWidgets)

#' getMetaData for given ids and datasets (which means via GUI a selected a collection)
#'
#' @param collectionIDs 
#' @param collectionDataSet 
#' @param host 
#' @param port 
#'
#' @return a list of size 2 with the following elements
#' list
#'		meta: dataframe
#'			id: internal ids
#'			dataset: [Name of imported dataset, e.g. MMF-2020-06-15 for each id]
#'			id_doc = docID / MMF-postID 
#'			title:
#'			date:
#'			[metaDataColumnName1] e.g. typeOfThreat from mde1
#'			[metaDataColumnName2] e.g. sourceOfThreat from mde2
#'			[metaDataColumnName3] e.g. lat from mde3
#'			[metaDataColumnName4] e.g. lon from mde4
#'			...
#'			
#'		meta_names: dataframe
#'			dataset: [Name of imported dataset, e.g. MMF-2020-06-15
#'			mde1: [metaDataColumnName1], e.g. typeOfThreat 
#'			mde2: [metaDataColumnName2], e.g. sourceOfThreat
#'			mde3: [metaDataColumnName3], e.g. lat 
#'			mde4: [metaDataColumnName4], e.g. lon
#'			...
#' @export
#'
#' @examples
getMetaData <- function(collectionIDs, collectionDataSet, host, port){
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  
  uniqueCollectionDataSets <- unique(collectionDataSet)
  d<-data.frame(id=collectionIDs,dataset=collectionDataSet)
  meta=NULL
  for(i in 1:length(uniqueCollectionDataSets)){
    ids<-paste(d[which(d[,2]==unique(d[,2])[i]),1],collapse = " ")
    ids<-stringr::str_replace_all(string = as.character(ids),pattern = " ",",")
    
    statement <-  paste("select id, dataset, id_doc, title, date, mde1, mde2, mde3, mde4, mde5, mde6, mde7, mde8, mde9, last_modified from documents where dataset='",unique(uniqueCollectionDataSets[i]),"' and id_doc in (",ids,");",sep="")
    dbResult <- RMariaDB::dbGetQuery(mydb,statement = statement)
    
    meta_names<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from metadata_names where dataset in ('",uniqueCollectionDataSets[i],"');"))
    resultWithMetaNames <- combineMetaDataWithMetaNamesForMDEs(meta = dbResult, meta_names = meta_names)
    
    meta<-rbind(meta,resultWithMetaNames)
    
  }
  
  
  RMariaDB::dbDisconnect(mydb)
  
  return(list(meta = meta, meta_names = meta_names))
}

get_token_meta_and_language_from_db<-function(get_meta=T,get_language=T,get_global_doc_ids=F,host=NULL,port=NULL,id,dataset){
  token<-NULL
  meta=NULL
  language=NULL
  global_doc_ids<-NULL
  #getting data from db
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=port)
  rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  d<-data.frame(id=id,dataset=dataset)
  for(i in 1:length(unique(d[,2]))){
    ids<-paste(d[which(d[,2]==unique(d[,2])[i]),1],collapse = " ")
    ids<-stringr::str_replace_all(string = as.character(ids),pattern = " ",",")
    token<-rbind(token,RMariaDB::dbGetQuery(mydb, paste("select * from token where dataset='",unique(d[,2])[i],"' and id in (",ids,");",sep="")))
    if(get_meta==T){
      meta<-rbind(meta,RMariaDB::dbGetQuery(mydb, paste("select id_doc ,date from documents where dataset='",unique(d[,2])[i],"' and id_doc in (",ids,");",sep="")))
    }
    if(get_global_doc_ids==T){
      global_doc_ids<-c(global_doc_ids,RMariaDB::dbGetQuery(mydb, paste("select id from documents where dataset='",unique(d[,2])[i],"' and id_doc in (",ids,");",sep="")))
    }
  }
  
  #x<-as.numeric(factor(paste(token[,1],token[,2],sep="_")))
  x<-paste(token[,1],token[,2],sep="_")
  token[,2]<-x
  
  if(get_meta==T){
    meta[,2]<-as.character(as.Date(meta[,2]))
    meta[,1]<-unique(x)
  }
  #get language // so far just use language of first document for all
  if(get_language==T){
    language<-as.character(RMariaDB::dbGetQuery(mydb, paste("select language from documents where dataset='",d[1,2],"' and id_doc =",d[1,1],";",sep="")))
  }
  RMariaDB::dbDisconnect(mydb)
  if(language=="eng"){
    language<-"en"
  }
  return(list(token=token,meta=meta,language=language,global_doc_ids=global_doc_ids))
}


prepare_input_parameters<-function(param){
  #stemming?
  param$stemming<-FALSE
  try({
    if(param$baseform_reduction=="stemming"){
      param$stemming<-TRUE
    }
  })
  #assign("stemming",stemming,envir=.GlobalEnv)
  #sentence as documents?
  param$sentences_as_documents<-FALSE
  try({
    if(param$cooc_window=="Sentence"){
      param$sentences_as_documents<-TRUE
    }
  })
  #assign("sentences_as_documents",sentences_as_documents,envir=.GlobalEnv)
  #append blacklist words to custom removal words
  if(!is.null(param$blacklist)){
    if(param$use_blacklist==TRUE){
      blacklist_words<-readChar(con=paste0("collections/blacklists/",param$blacklist,".txt"),nchars = file.info(paste0("collections/blacklists/",param$blacklist,".txt"))$size)
      remove_custom<-stringr::str_replace_all(string = paste(blacklist_words,",",param$remove_custom),pattern = ",,",replacement = ",")
      remove_custom<-stringr::str_replace_all(string=remove_custom,pattern = "\n",replacement = "")
      remove_custom<-stringr::str_replace_all(string=remove_custom,pattern = ", ",replacement = ",")
      remove_custom<-stringr::str_replace_all(string=remove_custom,pattern = " ,",replacement = ",")
      if(substr(remove_custom,nchar(remove_custom),nchar(remove_custom))==","){
        remove_custom<-substr(remove_custom,1,(nchar(remove_custom)-1))
      }
      param$remove_custom<-remove_custom
    }
  }
  param$remove_custom<-stringr::str_remove_all(string = param$remove_custom,pattern=" ")
  #append whitelist words to custom keep words
  if(!is.null(param$whitelist)){
    if(param$use_whitelist==TRUE){
      whitelist_words<-readChar(con=paste0("collections/whitelists/",param$whitelist,".txt"),nchars = file.info(paste0("collections/whitelists/",param$whitelist,".txt"))$size)
      keep_custom<-stringr::str_replace_all(string = paste(whitelist_words,",",param$keep_custom),pattern = ",,",replacement = ",")
      keep_custom<-stringr::str_replace_all(string=keep_custom,pattern = "\n",replacement = "")
      keep_custom<-stringr::str_replace_all(string=keep_custom,pattern = ", ",replacement = ",")
      keep_custom<-stringr::str_replace_all(string=keep_custom,pattern = " ,",replacement = ",")
      if(substr(keep_custom,nchar(keep_custom),nchar(keep_custom))==","){
        keep_custom<-substr(keep_custom,1,(nchar(keep_custom)-1))
      }
      param$keep_custom<-keep_custom
    }
  }
  param$keep_custom<-stringr::str_remove_all(string = param$keep_custom,pattern=" ")
  
  
  try({class(param$ngrams)<-"numeric"})
  param$id<-process_info[[1]]
  param$task<-process_info[[3]]
  param$started<-process_info[[4]]
  try({
    if(is.na(param$min_term)){
      param$min_term<-NULL
    }
    if(is.na(param$max_term)){
      param$max_term<-NULL
    }
    if(is.na(param$min_document)){
      param$min_document<-NULL
    }
    if(is.na(param$max_document)){
      param$max_document<-NULL
    }
  })
  return(param)
}


prepare_token_object<-function(token,parameters){
  #consolidate entities
  token<-token[,c(2,3,4,5,6,7,8)]
  class(token)<-c("spacyr_parsed","data.frame")
  colnames(token)<-c("doc_id"  ,   "sentence_id", "token_id"  ,  "token"    ,   "lemma"     ,  "pos"    ,     "entity" )
  try({
    if(parameters$consolidate_entities==T){
      log_to_file(message = "&emsp; Consolidating...",file = logfile)
      spacyr::spacy_initialize()
      token<-spacyr::entity_consolidate(token)
      log_to_file(message = "&emsp; Finished consolidating entities",file = logfile)
    }
  })
  try({
    if(parameters$baseform_reduction=="lemma"){
      token<-token[,c(1,2,3,5,4,6,7)]
    }
  })
  class(token)<-c("spacyr_parsed","data.frame")
  colnames(token)<-c("doc_id"  ,    "sentence_id", "token_id"  ,  "token"    ,   "lemma"     ,  "pos"    ,     "entity" )
  try({
    if(parameters$sentences_as_documents==T){
      token[,"doc_id"]<-paste(token[,"doc_id"],token[,"sentence_id"],sep="_")
      if(parameters$task=="Cooccurrence_Analysis"){
        token_orig<-token[,c("doc_id","token")]
        assign("token",token_orig,envir=.GlobalEnv)
      }
    }
  })
  #filter for pos or ner tags
  try({
    if(!is.element(el = "all",set = parameters$reduce_POS)){
      #whitelist sercure
      safe<-which(token[,4]%in%unique(unlist(stringr::str_split(string = parameters$keep_custom,pattern = ","))))
      reduce<-which(token[,6]%in%parameters$reduce_POS)
      token<-token[unique(union(reduce,safe)),]
      log_to_file(message = "&emsp; Finished filtering for pos types",file = logfile)
    }
  })
  try({
    if(!is.element(el = "all",set = parameters$reduce_NER)){
      if(parameters$consolidate_entities==F){
        log_to_file(message = "&emsp; Consolidating...due to NER Filter settings",file = logfile)
        spacyr::spacy_initialize()
        token<-spacyr::entity_consolidate(token)
        log_to_file(message = "&emsp; Finished consolidating entities",file = logfile)
      }
      safe<-which(token[,4]%in%parameters$keep_custom)
      reduce<-which(token[,7]%in%parameters$reduce_NER)
      token<-token[unique(union(reduce,safe)),]
    }
  })
  return(token)
}


if_empty_return_NULL<-function(string){
  vector<-unique(unlist(stringr::str_split(string = string,pattern = ",")))
  if(nchar(vector)==0){
    return(NULL)
  }
  else{
    return(vector)
  }
}


calculate_dtm<-function(token,parameters,tibble=F,lang){
  tow<-tmca.util::TextObjectWrapper$new()
  control=plyr::compact(
    list(
      language=lang,
      ngrams=parameters$ngrams,
      stem=parameters$stemming,
      remove_stopwords=parameters$remove_stopwords,
      remove_numbers=parameters$remove_numbers,
      remove_all_numbers=parameters$remove_numbers_all,
      remove_punctuation=parameters$remove_punctuation,
      remove_hyphenation=parameters$remove_hyphenation,
      remove_custom=if_empty_return_NULL(parameters$remove_custom),
      save_custom=if_empty_return_NULL(parameters$keep_custom),
      tolower = parameters$lowercase,
      char_length=parameters$min_char,
      expand_save_custom=parameters$whitelist_expand,
      just_save_custom=parameters$whitelist_only
    )
  )
  #split token
  splitsize<-ceiling(100000/(dim(token)[1]/length(unique(token[,1]))))
  split<-split(unique(token[,1]), ceiling(seq_along(unique(token[,1]))/splitsize))
  
  log_to_file(message = paste0("&emsp; split corpus in ",length(split)," chunks"),logfile)
  loghelper<-floor(seq(1,length(split),length.out = 11))[2:11]
  names(loghelper)<-c(10,20,30,40,50,60,70,80,90,100)
  for(i in 1:length(split)){
    tow$logging("silent")
    tow$input(x = token[which(token[,1]%in%split[[i]]),])
    if(i==1){
      dtm_glob<-tow$process(control = control,backend = "quanteda")%>%
        tow$output(format = "sparseMatrix")
    }
    else{
      dtm_local<-tow$process(control = control,backend = "quanteda")%>%
        tow$output(format = "sparseMatrix")
      if(dim(dtm_local)[2]>0){
        dtm_glob<-rBind_huge(dtm_glob,dtm_local)
      }
    }
    if(i %in% loghelper){
      if(length(split)>10){
        log_to_file(message = paste0("&emsp; ",names(which(loghelper==i)),"% of documents processed (",splitsize*i,")"),logfile)
      }
      else{
        log_to_file(message = paste0("&emsp; ",names(which(loghelper==i)),"% of documents processed"),logfile)
      }
    }
  }
  gc()
  log_to_file(message = paste0("&emsp; Pruning of documents"),logfile)
  dtm<-quanteda::as.dfm(x = dtm_glob)  
  tow$.__enclos_env__$private$internal_representation<-dtm
  control_prune=plyr::compact(
    list(
      save_custom=if_empty_return_NULL(parameters$keep_custom),
      expand_save_custom=parameters$whitelist_expand,
      just_save_custom=parameters$whitelist_only,
      prune=list(
        min_termfreq= parameters$min_term,
        max_termfreq= parameters$max_term,
        min_docfreq= parameters$min_document, 
        max_docfreq=parameters$max_document,
        termfreq_type=parameters$termfreq_type,
        docfreq_type=parameters$docfreq_type
      )
    )
  )
  x<-tow$process(control = control_prune,backend = "quanteda")
  log_to_file(message = paste0("&emsp; Finished pruning of documents"),logfile)
  gc()
  if(parameters$task=="Topic_Model"){
    documents<-tow$output(format = "documents")
    assign("documents",documents,envir=.GlobalEnv)
  }
  if(tibble==T){
    feature_list<-data.frame(tow$output(format = "tibble",x = x)[,1:2],stringsAsFactors = F)
    rm(tow)
    return(feature_list)
  }
  else{
    dtm<-tow$output(format = "sparseMatrix",x=x)
    rm(tow)
    return(dtm) 
  }
}



calculate_diachronic_cooccurrences<-function(dtm,parameters,meta){
  ids<-stringr::str_split(string = rownames(dtm),pattern = "_",simplify = T)[,1:2]
  ids<-paste(ids[,1],ids[,2],sep="_")
  coocsCalc <- tmca.cooccurrence::Coocc$new(dtm)
  
  #calculate cooc-slices
  diachron_Coocs<-list()
  if(parameters$va_timeintervall=="week"){
    db_data$meta[,2]<<-strftime(as.character(as.matrix(db_data$meta[,2])),format="%Y-%V")
  }
  if(parameters$va_timeintervall=="month"){
    db_data$meta[,2]<<-(substr(as.matrix(db_data$meta[,2]),1,7))
  }
  if(parameters$va_timeintervall=="year"){
    db_data$meta[,2]<<-(substr(as.matrix(db_data$meta[,2]),1,4))
  }
  un_dates<-as.matrix(unique(db_data$meta[,2]))
  un_dates<-un_dates[order(un_dates,decreasing = F)]
  
  freq<-matrix(c(0),dim(dtm)[2],length(un_dates))
  rownames(freq)<-colnames(dtm)
  count<-0
  coocsCalc$set_measure(parameters$cooc_significance_measure)
  coocsCalc$set_significanceThreshold(0)
  coocsCalc$set_minCoocFreq(as.integer(parameters$min_cooc_freq))
  coocsCalc$set_maxCoocFreq(1000000)
  
  loghelper<-floor(seq(1,length(un_dates),length.out = 11))[2:11]
  names(loghelper)<-c(10,20,30,40,50,60,70,80,90,100)
  for(d in un_dates){
    count<-count+1
    idx<-which(ids%in%db_data$meta[which(db_data$meta[,2]==d),1])
    coocsCalc$set_binDTM(dtm[idx,,drop=F])
    diachron_Coocs[[count]]<-coocsCalc$ccoocs()
    freq[,count]<-colSums(dtm[idx,,drop=F])
    if(count %in% loghelper){
      if(length(un_dates)>10){
        log_to_file(message = paste0("&emsp; ",names(which(loghelper==count)),"% of unique points in time processed (",d,")"),logfile)
      }
      else{
        log_to_file(message = paste0("&emsp; ",names(which(loghelper==count)),"% of unique points in time processed"),logfile)
      }
    }
  }
  
  log_to_file(message = paste0("&emsp; Calculating global co-occurrence matrix"),logfile)
  coocsCalc$set_binDTM(dtm)
  global<-coocsCalc$ccoocs()
  log_to_file(message = "&emsp; Finished calculating global co-occurrences matrix",logfile)
  
  terms<-colnames(dtm)
  terms_to_use<-terms
  empty_terms<-which(Matrix::rowSums(global)==0)
  if(length(empty_terms)>0){
    terms_to_use<-terms[-empty_terms]
  }
  return(list(diachron_Coocs=diachron_Coocs,word_Frequencies=freq,global_Coocs=global,terms=terms,terms_to_use=terms_to_use,empty_terms=empty_terms,un_dates=un_dates))
}



get_ner_and_pos_tags<-function(token,parameters,terms){
  if(parameters$baseform_reduction=="lemma"){
    ner_tags<-token[which(tolower(token[,"lemma"])%in%terms),c("lemma","entity")]
    ner_tags[,1]<-tolower(ner_tags[,1])
    ner_tags<-unique(ner_tags)
    ner_tags<-aggregate(x = ner_tags[,2],by=list(ner_tags[,1]),FUN=Mode)
    
    
    pos_tags<-token[which(tolower(token[,"lemma"])%in%terms),c("lemma","pos")]
    pos_tags[,1]<-tolower(pos_tags[,1])
    pos_tags<-unique(pos_tags)
    pos_tags<-aggregate(x = pos_tags[,2],by=list(pos_tags[,1]),FUN=Mode)
  }
  else{
    ner_tags<-token[which(tolower(token[,"token"])%in%terms),c("token","entity")]
    ner_tags[,1]<-tolower(ner_tags[,1])
    ner_tags<-unique(ner_tags)
    ner_tags<-aggregate(x = ner_tags[,2],by=list(ner_tags[,1]),FUN=Mode)
    
    
    pos_tags<-token[which(tolower(token[,"token"])%in%terms),c("token","pos")]
    pos_tags[,1]<-tolower(pos_tags[,1])
    pos_tags<-unique(pos_tags)
    pos_tags<-aggregate(x = pos_tags[,2],by=list(pos_tags[,1]),FUN=Mode)
  }
  return(list(pos_tags=pos_tags,ner_tags=ner_tags))
}



calculate_sentiments_analysis_tokens_object<-function(parameters,meta){
  control=list(
    tokenize="word",
    ngrams=parameters$ngrams,
    stem=parameters$stemming,
    remove_custom=unlist(stringr::str_split(string = parameters$remove_custom,pattern = ",")),
    tolower = parameters$lowercase,
    prune=list(
      min_termfreq= NULL,
      max_termfreq= NULL,
      min_docfreq=NULL, 
      max_docfreq=NULL,
      termfreq_type=NULL,
      docfreq_type=NULL
    )
  )
  colnames(meta)[5]<<-"text"
  
  tow<-tmca.util::TextObjectWrapper$new()
  #set log destination
  tow$logging("silent")
  #set input
  tow$input(x = meta)
  #process data 
  x<-tow$process(control = control,backend = "quanteda")
  #reset the preprocessing object
  tow$reset()
  return(x)
}


get_original_documents<-function(token){
  TOW = tmca.util::TextObjectWrapper$new()
  class(token) <- c("spacyr_parsed", "data.frame")
  TOW$input(token)
  #Get original Text from text object wrapper
  original_text <- TOW$get_original_documents()
  original_text<-original_text[gtools::mixedorder(original_text[,1]),]
  remove(TOW)
  return(original_text)
}



calculate_cooccurrences_all_measures<-function(dtm){
  #process data and get dtm
  dtm<-tmca.util::make_binary(dtm = dtm)
  coocsCalc <- tmca.cooccurrence::Coocc$new(dtm)
  coocsCalc$set_significanceThreshold(0)
  coocsCalc$set_minCoocFreq(as.integer(parameters$min_cooc_freq))
  coocsCalc$set_maxCoocFreq(10000000)
  
  log_to_file(message = "&emsp; Calculating coocs with Dice-Significance measure",logfile)
  coocsCalc$set_measure("DICE")
  coocs_matrix_dice<-coocsCalc$ccoocs()
  log_to_file(message = "&emsp;  ✔ ",logfile)
  gc()
  log_to_file(message = "&emsp; Calculating coocs with Count measure",logfile)
  coocsCalc$set_measure("COUNT")
  coocs_matrix_count<-coocsCalc$ccoocs()
  log_to_file(message = "&emsp;  ✔ ",logfile)
  gc()
  log_to_file(message = "&emsp; Calculating coocs with Mutual Information measure",logfile)
  coocsCalc$set_measure("MI")
  coocs_matrix_mi<-coocsCalc$ccoocs()
  log_to_file(message = "&emsp;  ✔ ",logfile)
  gc()
  log_to_file(message = "&emsp; Calculating coocs with Log-likelihood measure",logfile)
  coocsCalc$set_measure("LOGLIK")
  coocs_matrix_log<-coocsCalc$ccoocs()
  log_to_file(message = "&emsp;  ✔ ",logfile)
  
  gc()
  #delete entries for words no co-occurrence
  diag(coocs_matrix_dice)<-0
  #CS<-colSums(coocs_matrix_dice)
  #coocs_matrix_count<-coocs_matrix_count[which(CS>0),which(CS>0)]
  #coocs_matrix_dice<-coocs_matrix_dice[which(CS>0),which(CS>0)]
  #coocs_matrix_mi<-coocs_matrix_mi[which(CS>0),which(CS>0)]
  #coocs_matrix_log<-coocs_matrix_log[which(CS>0),which(CS>0)]
  #gc()
  
  terms<-colnames(coocs_matrix_dice)
  
  gc() 
  return(list(coocs_matrix_dice=coocs_matrix_dice,coocs_matrix_count=coocs_matrix_count,coocs_matrix_log=coocs_matrix_log,coocs_matrix_mi=coocs_matrix_mi,terms=terms))
}



get_meta_data_for_detailed_topic_analysis<-function(host,port,ids,datasets,token){
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  ids<- paste(ids[,1],collapse=", ")
  #meta<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from documents where id in (",ids,");"))
  meta<-RMariaDB::dbGetQuery(mydb,statement = paste0("Select REPLACE(body, CHAR(0),'') body,id,dataset,id_doc,title,date,token,language,entities,collections,mde1,mde2,mde3,mde4,mde5,mde6,mde7,mde8,mde9,last_modified from documents where id in (",ids,");"))
  meta<-meta[,c("id","dataset","id_doc","title","body","date","token","language","entities","collections","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9","last_modified")]
  
  meta_names<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from metadata_names where dataset in ('",datasets,"');"))
  # if(length(unique(info[[2]]))>1){
  #   #meta[,"id_doc"]<-as.numeric(factor(paste(db_data$token[,1],db_data$token[,2],sep="_")))
  # }
  meta[,"id_doc"]<-unique(token[,2])
  RMariaDB::dbDisconnect(mydb)
  return(list(
    meta=meta,
    meta_names=meta_names
  ))
}



calculate_diachron_frequencies<-function(dtm,meta){
  
  meta<-meta[which(meta[,1]%in%rownames(dtm)),]
  
  bin_dtm<-tmca.util::make_binary(dtm = dtm)
  vocab<-colnames(dtm)
  #transform dates
  dates_day<-meta[,2]
  dates_week<-strftime(as.character(meta[,"date"]),format = "%Y-%V")
  dates_month<-substr(meta[,2],1,7)
  dates_year<-substr(meta[,2],1,4)
  #get unique dates
  un_dates_day<-unique(dates_day)
  un_dates_week<-unique(dates_week)
  un_dates_month<-unique(dates_month)
  un_dates_year<-unique(dates_year)
  
  #create frequency matrices
  freqs_day<-matrix(c(0),length(un_dates_day),length(vocab))
  colnames(freqs_day)<-vocab
  rownames(freqs_day)<-un_dates_day
  freqs_week<-matrix(c(0),length(un_dates_week),length(vocab))
  colnames(freqs_week)<-vocab
  rownames(freqs_week)<-un_dates_week
  freqs_month<-matrix(c(0),length(un_dates_month),length(vocab))
  colnames(freqs_month)<-vocab
  rownames(freqs_month)<-un_dates_month
  freqs_year<-matrix(c(0),length(un_dates_year),length(vocab))
  colnames(freqs_year)<-vocab
  rownames(freqs_year)<-un_dates_year
  
  doc_freqs_day<-matrix(c(0),length(un_dates_day),length(vocab))
  colnames(doc_freqs_day)<-vocab
  rownames(doc_freqs_day)<-un_dates_day
  doc_freqs_week<-matrix(c(0),length(un_dates_week),length(vocab))
  colnames(doc_freqs_week)<-vocab
  rownames(doc_freqs_week)<-un_dates_week
  doc_freqs_month<-matrix(c(0),length(un_dates_month),length(vocab))
  colnames(doc_freqs_month)<-vocab
  rownames(doc_freqs_month)<-un_dates_month
  doc_freqs_year<-matrix(c(0),length(un_dates_year),length(vocab))
  colnames(doc_freqs_year)<-vocab
  rownames(doc_freqs_year)<-un_dates_year
  
  #calculate frequencies on daily basis
  log_to_file(message = "&emsp; Calculating frequencies on daily basis",logfile)
  for(i in 1:length(un_dates_day)){
    freqs_day[i,]<-colSums(x = dtm[which(dates_day==un_dates_day[i]),,drop=FALSE])
    doc_freqs_day[i,]<-colSums(x = bin_dtm[which(dates_day==un_dates_day[i]),,drop=FALSE])
  }
  log_to_file(message = "&emsp;  ✔ ",logfile)
  
  #calculate frequencies on weekly basis
  log_to_file(message = "&emsp; Calculating frequencies on weekly basis",logfile)
  for(i in 1:length(un_dates_week)){
    freqs_week[i,]<-colSums(x = dtm[which(dates_week==un_dates_week[i]),,drop=FALSE])
    doc_freqs_week[i,]<-colSums(x = bin_dtm[which(dates_week==un_dates_week[i]),,drop=FALSE])
  }
  log_to_file(message = "&emsp;  ✔ ",logfile)
  
  #calculate frequencies on monthly basis
  log_to_file(message = "&emsp; Calculating frequencies on montly basis",logfile)
  for(i in 1:length(un_dates_month)){
    freqs_month[i,]<-colSums(x = dtm[which(dates_month==un_dates_month[i]),,drop=FALSE])
    doc_freqs_month[i,]<-colSums(x = bin_dtm[which(dates_month==un_dates_month[i]),,drop=FALSE])
  }
  log_to_file(message = "&emsp;  ✔ ",logfile)
  
  #calculate frequencies on yearly basis
  log_to_file(message = "&emsp; Calculating frequencies on annual basis",logfile)
  for(i in 1:length(un_dates_year)){
    freqs_year[i,]<-colSums(x = dtm[which(dates_year==un_dates_year[i]),,drop=FALSE])
    doc_freqs_year[i,]<-colSums(x = bin_dtm[which(dates_year==un_dates_year[i]),,drop=FALSE])
  }
  log_to_file(message = "&emsp;  ✔ ",logfile)
  
  #calculating relative frequencies
  log_to_file(message = "&emsp; Calculating relative frequencies",logfile)
  rel_freqs_day<-freqs_day/rowSums(freqs_day)
  rel_freqs_week<-freqs_week/rowSums(freqs_week)
  rel_freqs_month<-freqs_month/rowSums(freqs_month)
  rel_freqs_year<-freqs_year/rowSums(freqs_year)
  rel_doc_freqs_day<-doc_freqs_day/unlist(lapply(un_dates_day,FUN = function(x){return(length(which(dates_day==x)))}))
  rel_doc_freqs_week<-doc_freqs_week/unlist(lapply(un_dates_week,FUN = function(x){return(length(which(dates_week==x)))}))
  rel_doc_freqs_month<-doc_freqs_month/unlist(lapply(un_dates_month,FUN = function(x){return(length(which(dates_month==x)))}))
  rel_doc_freqs_year<-doc_freqs_year/unlist(lapply(un_dates_year,FUN = function(x){return(length(which(dates_year==x)))}))
  log_to_file(message = "&emsp;  ✔ ",logfile)
  
  return(
    list(doc_freqs_year=doc_freqs_year,
         doc_freqs_month=doc_freqs_month,
         doc_freqs_week=doc_freqs_week,
         doc_freqs_day=doc_freqs_day,
         freqs_year=freqs_year,
         freqs_month=freqs_month,
         freqs_week=freqs_week,
         freqs_day=freqs_day,
         rel_doc_freqs_year=rel_doc_freqs_year,
         rel_doc_freqs_month=rel_doc_freqs_month,
         rel_doc_freqs_week=rel_doc_freqs_week,
         rel_doc_freqs_day=rel_doc_freqs_day,
         rel_freqs_year=rel_freqs_year,
         rel_freqs_month=rel_freqs_month,
         rel_freqs_week=rel_freqs_week,
         rel_freqs_day=rel_freqs_day)
  )
}




calculate_dtm_for_dictionary_extraction<-function(parameters,lang,token){
  tow<-tmca.util::TextObjectWrapper$new()
  control=plyr::compact(
    list(
      language=lang,
      ngrams=parameters$ngrams,
      stem=parameters$stemming,
      remove_stopwords=parameters$remove_stopwords,
      remove_numbers=parameters$remove_numbers,
      remove_all_numbers=parameters$remove_numbers_all,
      remove_punctuation=parameters$remove_punctuation,
      remove_hyphenation=parameters$remove_hyphenation,
      remove_custom=if_empty_return_NULL(parameters$remove_custom),
      save_custom=if_empty_return_NULL(parameters$keep_custom),
      tolower = parameters$lowercase,
      char_length=parameters$min_char,
      expand_save_custom=parameters$whitelist_expand
    )
  )
  if(parameters$de_use_context_filter==TRUE){
    if(nchar(parameters$de_context_filter)>0){
      if(parameters$de_Context_Unit=="sentence"){
        token[,"doc_id"]<-paste(token[,"doc_id"],token[,"sentence_id"],sep="_")
        #calculate stm
        splitsize<-ceiling(100000/(dim(token)[1]/length(unique(token[,1]))))
        split<-split(unique(token[,1]), ceiling(seq_along(unique(token[,1]))/splitsize))
        log_to_file(message = paste0("&emsp; split corpus in ",length(split)," chunks to calculate sentence-term-matrix"),logfile)
        loghelper<-floor(seq(1,length(split),length.out = 11))[2:11]
        names(loghelper)<-c(10,20,30,40,50,60,70,80,90,100)
        for(i in 1:length(split)){
          tow$logging("silent")
          tow$input(x = token[which(token[,1]%in%split[[i]]),])
          if(i==1){
            stm_glob<-tow$process(control = control,backend = "quanteda")%>%
              tow$output(format = "sparseMatrix")
          }
          else{
            stm_local<-tow$process(control = control,backend = "quanteda")%>%
              tow$output(format = "sparseMatrix")
            if(dim(stm_local)[2]>0){
              stm_glob<-rBind_huge(stm_glob,stm_local)
            }
          }
          if(i %in% loghelper){
            if(length(split)>10){
              log_to_file(message = paste0("&emsp; ",names(which(loghelper==i)),"% of documents processed (",splitsize*i,")"),logfile)
            }
            else{
              log_to_file(message = paste0("&emsp; ",names(which(loghelper==i)),"% of documents processed"),logfile)
            }
          }
        }
        gc()
        log_to_file(message = paste0("&emsp; Pruning of documents"),logfile)
        stm<-quanteda::as.dfm(x = stm_glob)  
        tow$.__enclos_env__$private$internal_representation<-stm
        control_prune=plyr::compact(
          list(
            save_custom=if_empty_return_NULL(parameters$keep_custom),
            expand_save_custom=parameters$whitelist_expand,
            prune=list(
              min_termfreq= parameters$min_term,
              max_termfreq= parameters$max_term,
              min_docfreq= parameters$min_document, 
              max_docfreq=parameters$max_document,
              termfreq_type=parameters$termfreq_type,
              docfreq_type=parameters$docfreq_type
            )
          )
        )
        stm<-tow$process(control = control_prune,backend = "quanteda")%>%
          tow$output(format = "sparseMatrix")
        #stm<-tow$process(control = control_spacy,backend = "spacyr")%>%
        #  tow$output(format = "sparseMatrix")
        #context filter
        log_to_file(message = paste0("&emsp; Filtering sentences for context filter"),logfile)
        if(parameters$de_context_filter_mode==T){
          orig_sentences<-tow$output(format = "documents")
          sentences_left<-orig_sentences[grep(pattern =parameters$de_context_filter,x = orig_sentences[,2],ignore.case = T,perl = T ,value = F),1]
          if(length(sentences_left)<1){
            log_to_file(message = "&emsp;<b style='color:red'>&#10008; The context filter matched no sentence. Maybe you need to adjust it.</b>",file = logfile)
            stop("empty stm")
          }
          else{
            stm<-stm[sentences_left,]
          }
        }
        else{
          blacklist<-unlist(stringr::str_split(string =  parameters$de_context_filter,pattern = ",",simplify = F))
          if(length(blacklist)>0){
            if(length(blacklist)>1){
              stm<-stm[which(rowSums(stm[,blacklist])>0),]
            }
            else{
              stm<-stm[which((stm[,blacklist])>0),]
            }
          }
        }
        #recreate dtm
        rownames(stm)<-stringr::str_replace(string = rownames(stm),pattern = "_[0-9]{1,5}$",replacement="")
        dtm<-Matrix.utils::aggregate.Matrix(x = stm,groupings = as.factor(rownames(stm)),fun = "sum")
      }
      #if context unit= document level
      else{
        tow$reset()
        token<-db_data$token
        splitsize<-ceiling(100000/(dim(token)[1]/length(unique(token[,1]))))
        split<-split(unique(token[,1]), ceiling(seq_along(unique(token[,1]))/splitsize))
        
        log_to_file(message = paste0("&emsp; split corpus in ",length(split)," chunks"),logfile)
        loghelper<-floor(seq(1,length(split),length.out = 11))[2:11]
        names(loghelper)<-c(10,20,30,40,50,60,70,80,90,100)
        for(i in 1:length(split)){
          tow$logging("silent")
          tow$input(x = token[which(token[,1]%in%split[[i]]),])
          if(i==1){
            dtm_glob<-tow$process(control = control,backend = "quanteda")%>%
              tow$output(format = "sparseMatrix")
          }
          else{
            dtm_local<-tow$process(control = control,backend = "quanteda")%>%
              tow$output(format = "sparseMatrix")
            if(dim(dtm_local)[2]>0){
              dtm_glob<-rBind_huge(dtm_glob,dtm_local)
            }
          }
          if(i %in% loghelper){
            if(length(split)>10){
              log_to_file(message = paste0("&emsp; ",names(which(loghelper==i)),"% of documents processed (",splitsize*i,")"),logfile)
            }
            else{
              log_to_file(message = paste0("&emsp; ",names(which(loghelper==i)),"% of documents processed"),logfile)
            }
          }
        }
        gc()
        log_to_file(message = paste0("&emsp; Pruning of documents"),logfile)
        dtm<-quanteda::as.dfm(x = dtm_glob)  
        tow$.__enclos_env__$private$internal_representation<-dtm
        control_prune=plyr::compact(
          list(
            save_custom=if_empty_return_NULL(parameters$keep_custom),
            expand_save_custom=parameters$whitelist_expand,
            prune=list(
              min_termfreq= parameters$min_term,
              max_termfreq= parameters$max_term,
              min_docfreq= parameters$min_document, 
              max_docfreq=parameters$max_document,
              termfreq_type=parameters$termfreq_type,
              docfreq_type=parameters$docfreq_type
            )
          )
        )
        dtm<-tow$process(control = control_prune,backend = "quanteda")%>%
          tow$output(format = "sparseMatrix")
        log_to_file(message = paste0("&emsp; Filtering documents for context filter"),logfile)
        if(parameters$de_context_filter_mode==T){
          orig_documents<-tow$output(format = "documents")
          documents_left<-orig_documents[grep(pattern =parameters$de_context_filter,x = orig_documents[,2],ignore.case = T,perl = T ,value = F),1]
          if(dim(documents_left)[1]<1){
            log_to_file(message = "&emsp;<b style='color:red'>&#10008; The context filter matched no documents. Maybe you need to adjust it.</b>",file = logfile)
            stop("empty dtm")
          }
          else{
            dtm<-dtm[documents_left,]
          }
        }
        else{
          blacklist<-unlist(stringr::str_split(string = parameters$de_context_filter,pattern = ",",simplify = F))
          if(length(blacklist)>0){
            if(length(blacklist)>1){
              dtm<-dtm[which(rowSums(stm[,blacklist])>0),]
            }
            else{
              dtm<-dtm[which((dtm[,blacklist])>0),]
            }
          }
        }
      }
    }
  }
  else{
    token<-db_data$token
    splitsize<-ceiling(100000/(dim(token)[1]/length(unique(token[,1]))))
    split<-split(unique(token[,1]), ceiling(seq_along(unique(token[,1]))/splitsize))
    
    log_to_file(message = paste0("&emsp; split corpus in ",length(split)," chunks"),logfile)
    loghelper<-floor(seq(1,length(split),length.out = 11))[2:11]
    names(loghelper)<-c(10,20,30,40,50,60,70,80,90,100)
    for(i in 1:length(split)){
      tow$logging("silent")
      tow$input(x = token[which(token[,1]%in%split[[i]]),])
      if(i==1){
        dtm_glob<-tow$process(control = control,backend = "quanteda")%>%
          tow$output(format = "sparseMatrix")
      }
      else{
        dtm_local<-tow$process(control = control,backend = "quanteda")%>%
          tow$output(format = "sparseMatrix")
        if(dim(dtm_local)[2]>0){
          dtm_glob<-rBind_huge(dtm_glob,dtm_local)
        }
      }
      if(i %in% loghelper){
        if(length(split)>10){
          log_to_file(message = paste0("&emsp; ",names(which(loghelper==i)),"% of documents processed (",splitsize*i,")"),logfile)
        }
        else{
          log_to_file(message = paste0("&emsp; ",names(which(loghelper==i)),"% of documents processed"),logfile)
        }
      }
    }
    gc()
    log_to_file(message = paste0("&emsp; Pruning of documents"),logfile)
    dtm<-quanteda::as.dfm(x = dtm_glob)  
    tow$.__enclos_env__$private$internal_representation<-dtm
    control_prune=plyr::compact(
      list(
        save_custom=if_empty_return_NULL(parameters$keep_custom),
        expand_save_custom=parameters$whitelist_expand,
        just_save_custom=parameters$whitelist_only,
        prune=list(
          min_termfreq= parameters$min_term,
          max_termfreq= parameters$max_term,
          min_docfreq= parameters$min_document, 
          max_docfreq=parameters$max_document,
          termfreq_type=parameters$termfreq_type,
          docfreq_type=parameters$docfreq_type
        )
      )
    )
    x<-tow$process(control = control_prune,backend = "quanteda")
    log_to_file(message = paste0("&emsp; Finished pruning of documents"),logfile)
    gc()
    dtm<-tow$output(format = "sparseMatrix",x=x)
    rm(tow)
    
  }
  return(dtm)
}


calculate_dictioanry_frequencies<-function(meta,dtm,dict_terms,conceptnames,dicts_available,bin_dtm){
  #transform dates
  dates_day<-meta[,2]
  dates_week<-strftime(as.character(meta[,"date"]),format = "%Y-%V")
  dates_month<-substr(meta[,2],1,7)
  dates_year<-substr(meta[,2],1,4)
  #get unique dates
  un_dates_day<-unique(dates_day)
  un_dates_week<-unique(dates_week)
  un_dates_month<-unique(dates_month)
  un_dates_year<-unique(dates_year)
  
  #create frequency matrices
  freqs_day<-Matrix(c(0),length(un_dates_day),length(dict_terms))
  colnames(freqs_day)<-dict_terms
  rownames(freqs_day)<-un_dates_day
  freqs_week<-Matrix(c(0),length(un_dates_week),length(dict_terms))
  colnames(freqs_week)<-dict_terms
  rownames(freqs_week)<-un_dates_week
  freqs_month<-Matrix(c(0),length(un_dates_month),length(dict_terms))
  colnames(freqs_month)<-dict_terms
  rownames(freqs_month)<-un_dates_month
  freqs_year<-Matrix(c(0),length(un_dates_year),length(dict_terms))
  colnames(freqs_year)<-dict_terms
  rownames(freqs_year)<-un_dates_year
  
  doc_freqs_day<-Matrix(c(0),length(un_dates_day),length(dict_terms))
  colnames(doc_freqs_day)<-dict_terms
  rownames(doc_freqs_day)<-un_dates_day
  doc_freqs_week<-Matrix(c(0),length(un_dates_week),length(dict_terms))
  colnames(doc_freqs_week)<-dict_terms
  rownames(doc_freqs_week)<-un_dates_week
  doc_freqs_month<-Matrix(c(0),length(un_dates_month),length(dict_terms))
  colnames(doc_freqs_month)<-dict_terms
  rownames(doc_freqs_month)<-un_dates_month
  doc_freqs_year<-Matrix(c(0),length(un_dates_year),length(dict_terms))
  colnames(doc_freqs_year)<-dict_terms
  rownames(doc_freqs_year)<-un_dates_year
  
  #calculate frequencies on daily basis
  for(i in 1:length(un_dates_day)){
    freqs_day[i,]<-colSums(x = dtm[which(dates_day==un_dates_day[i]),dict_terms,drop=FALSE])
    doc_freqs_day[i,]<-colSums(x = bin_dtm[which(dates_day==un_dates_day[i]),dict_terms,drop=FALSE])
  }
  #calculate frequencies on weekly basis
  for(i in 1:length(un_dates_week)){
    freqs_week[i,]<-colSums(x = dtm[which(dates_week==un_dates_week[i]),dict_terms,drop=FALSE])
    doc_freqs_week[i,]<-colSums(x = bin_dtm[which(dates_week==un_dates_week[i]),dict_terms,drop=FALSE])
  }
  #calculate frequencies on monthly basis
  for(i in 1:length(un_dates_month)){
    freqs_month[i,]<-colSums(x = dtm[which(dates_month==un_dates_month[i]),dict_terms,drop=FALSE])
    doc_freqs_month[i,]<-colSums(x = bin_dtm[which(dates_month==un_dates_month[i]),dict_terms,drop=FALSE])
  }
  #calculate frequencies on yearly basis
  for(i in 1:length(un_dates_year)){
    freqs_year[i,]<-colSums(x = dtm[which(dates_year==un_dates_year[i]),dict_terms,drop=FALSE])
    doc_freqs_year[i,]<-colSums(x = bin_dtm[which(dates_year==un_dates_year[i]),dict_terms,drop=FALSE])
  }
  
  #aggreagte to dictionaries
  freqs_day_dict<-Matrix(c(0),length(un_dates_day),length(conceptnames))
  colnames(freqs_day_dict)<-unlist(conceptnames)
  rownames(freqs_day_dict)<-un_dates_day
  freqs_week_dict<-Matrix(c(0),length(un_dates_week),length(conceptnames))
  colnames(freqs_week_dict)<-unlist(conceptnames)
  rownames(freqs_week_dict)<-un_dates_week
  freqs_month_dict<-Matrix(c(0),length(un_dates_month),length(conceptnames))
  colnames(freqs_month_dict)<-unlist(conceptnames)
  rownames(freqs_month_dict)<-un_dates_month
  freqs_year_dict<-Matrix(c(0),length(un_dates_year),length(conceptnames))
  colnames(freqs_year_dict)<-unlist(conceptnames)
  rownames(freqs_year_dict)<-un_dates_year
  
  doc_freqs_day_dict<-Matrix(c(0),length(un_dates_day),length(conceptnames))
  colnames(doc_freqs_day_dict)<-unlist(conceptnames)
  rownames(doc_freqs_day_dict)<-un_dates_day
  doc_freqs_week_dict<-Matrix(c(0),length(un_dates_week),length(conceptnames))
  colnames(doc_freqs_week_dict)<-unlist(conceptnames)
  rownames(doc_freqs_week_dict)<-un_dates_week
  doc_freqs_month_dict<-Matrix(c(0),length(un_dates_month),length(conceptnames))
  colnames(doc_freqs_month_dict)<-unlist(conceptnames)
  rownames(doc_freqs_month_dict)<-un_dates_month
  doc_freqs_year_dict<-Matrix(c(0),length(un_dates_year),length(conceptnames))
  colnames(doc_freqs_year_dict)<-unlist(conceptnames)
  rownames(doc_freqs_year_dict)<-un_dates_year
  
  for(l in 1:length(conceptnames)){
    try({freqs_day_dict[,l]<-rowSums(freqs_day[,dicts_available[[l]]])})
    try({freqs_week_dict[,l]<-rowSums(freqs_week[,dicts_available[[l]]])})
    try({freqs_month_dict[,l]<-rowSums(freqs_month[,dicts_available[[l]]])})
    try({freqs_year_dict[,l]<-rowSums(freqs_year[,dicts_available[[l]],drop=F])})
    
    try({doc_freqs_day_dict[,l]<-unlist(lapply(un_dates_day,FUN = function(x){return(length(which(rowSums(bin_dtm[which(dates_day==x),dicts_available[[l]],drop=F])>0)))}))})
    try({doc_freqs_week_dict[,l]<-unlist(lapply(un_dates_week,FUN = function(x){return(length(which(rowSums(bin_dtm[which(dates_week==x),dicts_available[[l]],drop=F])>0)))}))})
    try({doc_freqs_month_dict[,l]<-unlist(lapply(un_dates_month,FUN = function(x){return(length(which(rowSums(bin_dtm[which(dates_month==x),dicts_available[[l]],drop=F])>0)))}))})
    try({doc_freqs_year_dict[,l]<-unlist(lapply(un_dates_year,FUN = function(x){return(length(which(rowSums(bin_dtm[which(dates_year==x),dicts_available[[l]],drop=F])>0)))}))})
  }
  
  
  #calculating relative frequencies
  rel_freqs_day_dict<-freqs_day_dict/unlist(lapply(un_dates_day,FUN = function(x){return(sum(dtm[which(dates_day==x),]))}))
  rel_freqs_week_dict<-freqs_week_dict/unlist(lapply(un_dates_week,FUN = function(x){return(sum(dtm[which(dates_week==x),]))}))
  rel_freqs_month_dict<-freqs_month_dict/unlist(lapply(un_dates_month,FUN = function(x){return(sum(dtm[which(dates_month==x),]))}))
  rel_freqs_year_dict<-freqs_year_dict/unlist(lapply(un_dates_year,FUN = function(x){return(sum(dtm[which(dates_year==x),]))}))
  rel_doc_freqs_day_dict<-doc_freqs_day_dict/unlist(lapply(un_dates_day,FUN = function(x){return(length(which(dates_day==x)))}))
  rel_doc_freqs_week_dict<-doc_freqs_week_dict/unlist(lapply(un_dates_week,FUN = function(x){return(length(which(dates_week==x)))}))
  rel_doc_freqs_month_dict<-doc_freqs_month_dict/unlist(lapply(un_dates_month,FUN = function(x){return(length(which(dates_month==x)))}))
  rel_doc_freqs_year_dict<-doc_freqs_year_dict/unlist(lapply(un_dates_year,FUN = function(x){return(length(which(dates_year==x)))}))
  
  
  return(
    list(doc_freqs_year_dict=doc_freqs_year_dict,
         doc_freqs_month_dict=doc_freqs_month_dict,
         doc_freqs_week_dict=doc_freqs_week_dict,
         doc_freqs_day_dict=doc_freqs_day_dict,
         freqs_year_dict=freqs_year_dict,
         freqs_month_dict=freqs_month_dict,
         freqs_week_dict=freqs_week_dict,
         freqs_day_dict=freqs_day_dict,
         rel_doc_freqs_year_dict=rel_doc_freqs_year_dict,
         rel_doc_freqs_month_dict=rel_doc_freqs_month_dict,
         rel_doc_freqs_week_dict=rel_doc_freqs_week_dict,
         rel_doc_freqs_day_dict=rel_doc_freqs_day_dict,
         rel_freqs_year_dict=rel_freqs_year_dict,
         rel_freqs_month_dict=rel_freqs_month_dict,
         rel_freqs_week_dict=rel_freqs_week_dict,
         rel_freqs_day_dict=rel_freqs_day_dict)
  )
  
}


# function to copy a list but remove values being NULL and character values being empty
# work around for:
# parameters are set via GUI/App, which result in empty character values when not set, this should be equal to not being set
# for all optional parameters not set (=NULL), they will be removed from the resulting parameter list, because parent abstract class of tm_abstr stops with error when parameters have a value of NULL
copyListButRemoveNullValuesAndEmptyStringValues = function(inputList){
  
  if(!is.list(inputList)){
    stop("Parameters in argument are not a list ")
  }
  resultList = list()
  namesUsed <- names(inputList)
  for (name in namesUsed ) {
    print(name)
    listValue <- inputList[[name]]
    if(is.character(listValue) && nchar(listValue)==0){
      listValue <- NULL
    }
    if(is.null(listValue)){
      next # skip/do not include this parameter (work around for optional parameters which are not set (set to NULL)) because abstract class stops with error when a parameter is NULL. So this removes these parameters completely which only gives a warning for missing parameters.
    }

    resultList[[name]] <- listValue
  }
  return (resultList)
  
}

# function
# meta data: set names from mde1/mde2 etc to real meta names
combineMetaDataWithMetaNamesForMDEs <- function(meta, meta_names){
  colNamesUsed <- colnames(meta)
  colnamesInclMetaNames <- character(length(colNamesUsed))
  colNameCounter <-0
  for(colName in colNamesUsed){
    colNameCounter <- colNameCounter+1
    if(startsWith(x=colName, prefix = "mde")){
      colnamesInclMetaNames[colNameCounter] <- meta_names[[colName]]
    }else{
      colnamesInclMetaNames[colNameCounter] <- colName
    }
  }
  metaDataToUse <- meta
  colnames(metaDataToUse) <- colnamesInclMetaNames
  return (metaDataToUse)
}

getParametersFromRData <- function(pathToResultsFolder, specificFolderName){
  load(paste(pathToResultsFolder,specificFolderName,"/parameters.RData", sep=""))
  return(parameters)
}


# data is a matrix having columns "task id", "collection" and "creation time" as first 3 elements in this order from which the sepcific folder name is created by pasting with underscore e.g. "165_myCollection_2019-11-27 15:20:40"
# this data object is created at output$more_details_topic_table selecting the selected row from values$tasks_tm (which is a matrix like described above but with multiple entries for all results) created by output$Topic_Results in the value "data_finished"
# this is used as a work around to not refactor all existing code but to also get parameters from RData object using the given data produced with the existing code before.
# it is necessary to get parameters from RData object because Structural Topic Models have many additional parameters than used before. The storage of these parameters in the database would lots of additional code work and it is planned to read them from RData in the furture and skip storage of parameters in database 
getSpecificResultFolderNameFromSelectedTopic <- function(data){
  specificResultFolderName <- paste(data[1],data[2],data[3], sep = "_")
  
  return(specificResultFolderName)
}

# convenience function to get available distinct values even if column contains multiple values separated by separator
getAvailableValues <- function(dataToUse, columnName, columnContainsMultipleValues = F, separator = ",", replaceNullWith = NULL, replaceNAWith = NA, replaceEmptyWith =""){
  if(!columnContainsMultipleValues){
    result <- unique(dataToUse[[columnName]])
  }else{
    collectedValues <- NULL
    for(i in 1:dim(dataToUse)[1]){
      entry <- dataToUse[[columnName]][i]
      entries <- str_split(string = entry, pattern = separator)
      collectedValues <- unique(c(collectedValues, entries[[1]]))
    }
    result <- collectedValues
  }
  
  return (result)
}

getAvailableValuesForGivenColumns <- function(dataToUse, columnNames, columnNamesContainingMultiValues, separatorsToUseForColumnsWithMultivalues){
  result <- vector("list", length(columnNames))
  names(result) <- columnNames

  for(columnName in columnNames){
    isMultiValue <- (columnName %in% columnNamesContainingMultiValues)
    multiValueSeparator <- NULL
    if(isMultiValue){
      multiValueSeparator <- separatorsToUseForColumnsWithMultivalues[which(columnNamesContainingMultiValues==columnName)[[1]]]
    }
    availableValues <- getAvailableValues(dataToUse,columnName = columnName, columnContainsMultipleValues = isMultiValue, separator = multiValueSeparator)
    result[[columnName]] <- availableValues
  }
  return(result)
}

#' Covenience function to calculate distributions of values. Works also for multi-value data.
#'
#' @param inputData data frame with columnNames, meaning names(inputData) is set
#' @param columnNamesOfColumnsToUse which column names to consider for calculation, if null all are taken
#' @param dataWithColumnNamesAndAvailableValues available/possible values to check (can be retrieved via fuction getAvailableValues above)
#' @param includeValuesNotUsed if false, values from dataWithColumnNamesAndAvailableValues which do not occur in the data will be removed
#' @param columnsWithMultiValues a list with columnNames which contain multi values
#' @param separatorsForMultiValues if set, the enries for columns with multivalues will be splitted by the given separator and counted afterwards. If no separators are set (separatorsForMultiValues== NULL), grep will be used. This works only if the values are no substrings of each other. If there are substrings, the substring will be found more in containing values which means falsely counted more often than expected
#' @param nameEmptyStringInStatsAs In case a entry has an empty string the name would of teh stats will be also "". Here you can set an alternative name, e.g. "EMPTY - NOTHING SET!"
#'
#' @return a named list with columnNames as names of the list, list entry: dataframe with columns: valueName, frequency, percent 
calcStats <- function(inputData, columnNamesOfColumnsToUse, dataWithColumnNamesAndAvailableValues, includeValuesNotUsed, columnsWithMultiValues, separatorsForMultiValues, nameEmptyStringInStatsAs){
  
  if(is.null(columnNamesOfColumnsToUse)){
    columnNamesOfColumnsToUse <- names(inputData)
  }
  numberOfEntriesTotal <- dim(inputData)[1]
  stats <- list()
  for(columnName in columnNamesOfColumnsToUse){
    
    availableValuesForGivenColumn <- dataWithColumnNamesAndAvailableValues[[columnName]]
    
    statsForColumn <- data.frame(metaDataName = availableValuesForGivenColumn,
                                 numberOfEntries = rep(as.numeric(0)), 
                                 percent = rep(as.numeric(0)),
                                 stringsAsFactors = F
    )
    
    if(columnName %in% columnsWithMultiValues){
      
      if(!is.null(separatorsForMultiValues)){# separators are defined, should be used
        
        # separate each line and add up occurence of each value in statsForColumn
        indexOfSeparator <- which(columnsWithMultiValues == columnName)[[1]]
        separatorForMultiValuesForGivenColumn <- separatorsForMultiValues[indexOfSeparator]
 
        for(i in 1:dim(inputData)[1]){
          entry <- inputData[[columnName]][i]
          if(is.na(entry) | entry ==""){ # NA and empty values will be treated separately later
            next
          }
          splittedValues <- strsplit(entry,separatorForMultiValuesForGivenColumn)[[1]]
          for(splittedValue in splittedValues){
            indexOfSplittedValue <- which(statsForColumn$metaDataName == splittedValue)[[1]]
            statsForColumn$numberOfEntries[indexOfSplittedValue] <- statsForColumn$numberOfEntries[indexOfSplittedValue]+1
          }
        }
        
      }else{# no separators defined, use grep (only produces correct results if there are no values which are substrings of another value)
        for(currentValue in availableValuesForGivenColumn){# NA and empty values will be treated separately later
          if(is.na(currentValue) | currentValue ==""){
            next
          }
          numberOfEntries <- dim(inputData[grep(x = inputData[[columnName]], pattern = currentValue, fixed = T), ])[1]
          indexOfCurrentValue <- which(statsForColumn$metaDataName == currentValue)[[1]]
          statsForColumn$numberOfEntries[indexOfCurrentValue] <- numberOfEntries
        }
      }
      
    }else{# no multivalue
      # perform single value exact match 
      for(currentValue in availableValuesForGivenColumn){
        if(is.na(currentValue) | currentValue ==""){# NA and empty values will be treated separately later
          next
        }
        numberOfEntries <- dim(inputData[which(inputData[[columnName]]== currentValue),])[1]
        indexOfCurrentValue <- which(statsForColumn$metaDataName == currentValue)[[1]]
        statsForColumn$numberOfEntries[indexOfCurrentValue] <- numberOfEntries
      }
    }
    
    # special treatment of empty values and NA
    numberOfEntriesWithNA <- length(which(is.na(inputData[[columnName]])))
    if(numberOfEntriesWithNA>0){
      if(NA %in% availableValuesForGivenColumn){
        indexOfNAValue <- which(is.na(statsForColumn$metaDataName))[[1]]
        statsForColumn$numberOfEntries[indexOfNAValue] <- numberOfEntriesWithNA
      }else{
        statsForColumn %>% add_row(metaDataName = NA,numberOfEntries = numberOfEntriesWithNA, percent = 0)# percent will be calculated later
      }
    }
    numberOfEntriesWithEmptyString <- length(which(inputData[[columnName]]==""))
    nameToUseInStatsForEmptyString =""
    if(!is.null(nameEmptyStringInStatsAs)){
      nameToUseInStatsForEmptyString <- nameEmptyStringInStatsAs
    }
    if("" %in% availableValuesForGivenColumn){# if yes, it might have a wrong number because of grep matching "" to each entry
      indexOfEmptyValue <- which(statsForColumn$metaDataName =="")[[1]]
      statsForColumn$metaDataName[indexOfEmptyValue] <- nameToUseInStatsForEmptyString
      statsForColumn$numberOfEntries[indexOfEmptyValue] <- numberOfEntriesWithEmptyString
    }else{
      if(numberOfEntriesWithEmptyString>0){
        statsForColumn %>% add_row(metaDataName = nameToUseInStatsForEmptyString,numberOfEntries = numberOfEntriesWithEmptyString, percent = 0)# percent will be calculated later
      }
    }
    
    # exclude values not used if configured
    if(!includeValuesNotUsed){
      indexesOfValuesNotUsed <- which(statsForColumn$numberOfEntries==0)
      statsForColumnJustUsedValues <- statsForColumn
      statsForColumnJustUsedValues[indexesOfValuesNotUsed,] <- NULL
      
    }
    
    # calc percent
    statsForColumn$percent <- unlist(lapply(X = statsForColumn$numberOfEntries, FUN = function(x){
      percentValue <- 0
      if(x >0){percentValue <- x/numberOfEntriesTotal*100}
      return (percentValue)
    }))
    
    stats[[columnName]] <- statsForColumn
  }
  
  return(stats)
}





#' Convenience function to calc different numeric values (sum,min,max,mean,median) for columns of given data
#'
#' @param inputData a data frame with columnNames, meaning names(inputData) is set
#' @param columnsToUseWithNumericContent Specify which columns to use. If NULL, automatcially all columns used which are numeric (is.numeric ==T)
#'
#' @return a dataframe with given columnNames as columnNames, the results (like sum,min,max,..) as rows
#' @export
#'
#' @examples
calcStatsForNumeric <- function(inputData, columnsToUseWithNumericContent){
  if(is.null(columnsToUseWithNumericContent)){
    columnsToUseWithNumericContent <- c()
    availableColumns <- names(inputData)
    for(columnName in availableColumns){
      if(is.numeric(inputData[[columnName]])){
        columnsToUseWithNumericContent <- c(columnsToUseWithNumericContent,columnName)
      }
    }
    columnsToUseWithNumericContent 
  }
  
  namesOfCalculations <- c("sum","min","max","mean","median")
  statsForNumeric <- data.frame(matrix(ncol = length(columnsToUseWithNumericContent), nrow = length(namesOfCalculations))) 
  names(statsForNumeric) <- columnsToUseWithNumericContent
  row.names(statsForNumeric)<- namesOfCalculations
  for(columnName in columnsToUseWithNumericContent){
    dataOfColumn <- inputData[[columnName]]
    # exclude null / NA / empty
    indicesToExclude <- which(is.null(dataOfColumn) | is.na(dataOfColumn) | nchar(dataOfColumn)==0 | dataOfColumn == "NA")
    dataToUseForCalc <- dataOfColumn
    if(length(indicesToExclude)>0){
      dataToUseForCalc <- dataOfColumn[-indicesToExclude]
    }
    if(length(dataToUseForCalc)==0){# no data left
      statsForNumeric[[columnName]] <- c(NA,NA,NA,NA,NA)
    }else{
      dataToUseForCalc <- as.numeric(dataToUseForCalc)
      statsForNumeric[[columnName]] <- c(sum(dataToUseForCalc),min(dataToUseForCalc),max(dataToUseForCalc),mean(dataToUseForCalc),median(dataToUseForCalc))
    }
  }
  return(statsForNumeric)
}

#' Convenience function to filter a given data frame based on given input for different columns. Content of data might be multivalue. In this case grep is used to find the mathing entries.
#'
#' @param dataToFilter data frame with named columns
#' @param columnNamesOfDataToFilter the columnNames of dataToFilter to use (might be restricted)
#' @param columnsNamesWithMultiValueData an array listing all columnNames within dataToFilter which are multivalues. For these grep is used to identify matches
#' @param filterInput the input variable from shiny
#' @param prefixInFilterNameForUniqueIdentification when the field names of the input filter have a unique prefix (to make a difference between a$id and b$id the name in input might be input$a_id with "a_" as prefix) 
#'
#' @return a data frame like dataToFilter reduced to the entries matching the given filters
#' @export
#'
#' @examples
filterDataBasedOnInputFilterFields <- function(dataToFilter,columnNamesOfDataToFilter,columnsNamesWithMultiValueData,filterInput,prefixInFilterNameForUniqueIdentification){
  result <- dataToFilter
  for(columnName in columnNamesOfDataToFilter){
    columnNameWithPrefix <- paste0(prefixInFilterNameForUniqueIdentification,columnName)
    if(length(filterInput[[columnNameWithPrefix]])>0){
      # if multi value field
      if(columnName %in% columnsNamesWithMultiValueData){
        indicesCollected <- NULL
        for (i in 1:length(filterInput[[columnNameWithPrefix]])) {
          searchString <- filterInput[[columnNameWithPrefix]][i]
          indicesWithSearchString <- grep(x=result[[columnName]], pattern = searchString, fixed = T)
          indicesCollected <- c(indicesCollected,indicesWithSearchString)
        }
        indicesCollected <- unique(indicesCollected)
        result<- result[indicesCollected,]
        
      }else{ # single value field
        result <- result[which(result[[columnName]] %in% filterInput[[columnNameWithPrefix]]),] 
      }
    }
  }
  return (result)
}

#create select input lists
createSelectInputsForColumnsAndValues <- function(fieldNamesWithValues,prefixForUniqueIdentificationUsedForInputId){
  availableFieldNames <- names(fieldNamesWithValues)
  selectionInputList <- list(length(availableFieldNames))
  for(fieldName in availableFieldNames){
    columnNameWithPrename <- paste0(prefixForUniqueIdentificationUsedForInputId,fieldName)
    selectInputEntry <- selectInput(inputId=columnNameWithPrename, label=h4(fieldName), choices= fieldNamesWithValues[[fieldName]], multiple = T)
    selectionInputList[[fieldName]] <- selectInputEntry
  }
  return(selectionInputList)
}

calcStatsGeocodingResult <- function(geocodingResultData, columnsToCalcDistributions,availableValues,includeValuesNotUsedForDistribution,performSingleValueExactMatchInsteadOfGrep, columnsToUseForNumericStats){
  stats <- list()
  dataForStats <- geocodingResultData
  stats$numberOfDinstinctDocs <- length(unique(dataForStats[["areaId"]]))
  stats$numberOfDistinctLocations <- length(unique(dataForStats[["latlon"]]))
  stats$numberOfLocations <- sum(dataForStats[["frequencyInArea"]])
  stats$distributions <- calcStats(dataForStats, columnsToCalcDistributions, availableValues, includeValuesNotUsed = includeValuesNotUsedForDistribution, performSingleValueExactMatchInsteadOfGrep = performSingleValueExactMatchInsteadOfGrep)
  stats$numericInfos <- calcStatsForNumeric(inputData = dataForStats, columnsToUseWithNumericContent  = columnsToUseForNumericStats)
  return(stats)
}

calcStatsMetaData <- function(metaData, columnsToCalcDistributions,availableValues,includeValuesNotUsedForDistribution,performSingleValueExactMatchInsteadOfGrep, columnsToUseForNumericStats){
  stats <- list()
  dataForStats <- metaData
  stats$numberOfDinstinctDocs <- length(unique(dataForStats[["id_doc"]]))
  stats$distributions <- calcStats(dataForStats, columnsToCalcDistributions, availableValues, includeValuesNotUsed = includeValuesNotUsedForDistribution, performSingleValueExactMatchInsteadOfGrep = performSingleValueExactMatchInsteadOfGrep)
  stats$numericInfos <- calcStatsForNumeric(inputData = dataForStats, columnsToUseWithNumericContent  = columnsToUseForNumericStats)
  return(stats)
}

calcStatsPerMapPoint <- function(geocodingResultReducedToPointData, 
                                 geocodingResult_columnsToCalcDistributions, 
                                 geocodingResult_availableValues, 
                                 geocodingResult_performSingleValueExactMatchInsteadOfGrep,
                                 geocodingResult_columnsToCalcNumericInfos, 
                                 geocodingResult_includeValuesNotUsed, 
                                 metaDataReducedToPointData, 
                                 metaData_columnsToCalcDistributions, 
                                 metaData_availableValues, 
                                 metaData_performSingleValueExactMatchInsteadOfGrep,
                                 metaData_columnsToCalcNumericInfos, 
                                 metaData_includeValuesNotUsed
){
  
  stats <- list()
  metaReduced <- metaDataReducedToPointData
  stats$geoCodingResult_numberOfDistinctLocationsBasedOnLatLon <- length(unique(geocodingResultReducedToPointData$display_name))
  stats$geoCodingResult_distinctLocationNames <- sort(unique(geocodingResultReducedToPointData$display_name))
  stats$geoCodingResult_numberOfTimesLocationsFound <- sum(geocodingResultReducedToPointData$frequencyInArea)
  stats$geocodingResult_numberOfDictinctDocsLocationFoundIn <- length(unique(geocodingResultReducedToPointData$areaId))
  
  stats$geocodingResult_distributionInDocs <- data.frame("docsContainingLocation" = geocodingResultReducedToPointData$areaId,
                                                         "frequencyInDoc" = geocodingResultReducedToPointData$frequencyInArea)
  stats$geocodingResult_distributionInDocs$title <- unlist(lapply(stats$geocodingResult_distributionInDocs[["docsContainingLocation"]], FUN = function(x){metaDataReducedToPointData[which(metaDataReducedToPointData[[metaData_columnNameForMatchWithOtherData]]==x),]$title}))
  
  stats$geocodingResult_distributions <- calcStats(geocodingResultReducedToPointData, geocodingResult_columnsToCalcDistributions, geocodingResult_availableValues, geocodingResult_includeValuesNotUsed, geocodingResult_performSingleValueExactMatchInsteadOfGrep)
  stats$geocodingResult_numericStats <- calcStatsForNumeric(geocodingResultReducedToPointData, geocodingResult_columnsToCalcNumericInfos)
  
  
  stats$metaData_distributions <- calcStats(metaDataReducedToPointData, metaData_columnsToCalcDistributions, metaData_availableValues, metaData_includeValuesNotUsed, metaData_performSingleValueExactMatchInsteadOfGrep)
  stats$metaData_numericStats <- calcStatsForNumeric(metaDataReducedToPointData, metaData_columnsToCalcNumericInfos)
  
  #print(names(stats))
  
  return(stats)
}

createPlotsForDistributionData <- function(statsDistributionData){
    finalPlots <- list()
    statsToPlot <- statsDistributionData
    distributionAspects <- names(statsToPlot)
    for(aspectName in distributionAspects){
      dataToPlot <- statsToPlot[[aspectName]]
      dataToPlot <- dataToPlot[order(-dataToPlot$numberOfEntries,dataToPlot$metaDataName),]
      if(sortByValueDesc){
        dataToPlot$metaDataName <- factor(dataToPlot$metaDataName, levels = unique(dataToPlot$metaDataName)[order(dataToPlot$numberOfEntries, decreasing = TRUE)])
      }
      dataAsPlotly <- plot_ly(x = dataToPlot$metaDataName, y = dataToPlot$numberOfEntries, name = aspectName, type = "bar")
      
      finalPlots[[aspectName]] <-dataAsPlotly
    }
    return(subplot(finalPlots))
  
}


