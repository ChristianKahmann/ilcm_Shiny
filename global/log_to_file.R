log_to_file<-function(message,file){
  message<-paste(Sys.time(),message,sep = ": ")
  write(message,file = file,append = T)
}


write_metadata_to_database<-function(parameters,host,port){
  entry_for_db<-data.frame(matrix(c(NA),1,70))
  if(!is.null(parameters$id)){
    entry_for_db[1,1]<-parameters$id
  }
  
  if(!is.null(parameters$collection)){
    entry_for_db[1,2]<-as.character(parameters$collection)
  }
  
  if(!is.null(parameters$task)){
    entry_for_db[1,3]<-as.character(parameters$task)
  }
  
  if(!is.null(parameters$started)){
    entry_for_db[1,4]<-as.character(parameters$started)
  }
  
  if(!is.null(parameters$remove_stopwords)){
    entry_for_db[1,5]<-as.character(parameters$remove_stopwords)
  }
  
  if(!is.null(parameters$min_char)){
    entry_for_db[1,6]<-as.character(paste(parameters$min_char,collapse=", "))
  }
  
  if(!is.null(parameters$baseform_reduction)){
    entry_for_db[1,7]<-as.character(parameters$baseform_reduction)
  }
  
  if(!is.null(parameters$termfreq_type)){
    entry_for_db[1,8]<-as.character(parameters$termfreq_type)
  }
  
  if(!is.null(parameters$min_term)){
    entry_for_db[1,9]<-parameters$min_term
  }
  
  if(!is.null(parameters$max_term)){
    entry_for_db[1,10]<-parameters$max_term
  }
  
  if(!is.null(parameters$docfreq_type)){
    entry_for_db[1,11]<-as.character(parameters$docfreq_type)
  }
  
  if(!is.null(parameters$min_doc)){
    entry_for_db[1,12]<-parameters$min_doc
  }
  
  if(!is.null(parameters$max_doc)){
    entry_for_db[1,13]<-parameters$max_doc
  }
  
  if(!is.null(parameters$remove_numbers)){
    entry_for_db[1,14]<-as.character(parameters$remove_numbers)
  }
  
  if(!is.null(parameters$remove_punctuation)){
    entry_for_db[1,15]<-as.character(parameters$remove_punctuation)
  }
  
  if(!is.null(parameters$remove_hyphenation)){
    entry_for_db[1,16]<-as.character(parameters$remove_hyphenation)
  }
  
  if(!is.null(parameters$lowercase)){
    entry_for_db[1,17]<-as.character(parameters$lowercase)
  }
  
  if(!is.null(parameters$tm_number_of_topics)){
    entry_for_db[1,18]<-parameters$tm_number_of_topics
  }
  
  if(!is.null(parameters$tm_alpha)){
    entry_for_db[1,19]<-parameters$tm_alpha
  }
  
  if(!is.null(parameters$tm_method)){
    entry_for_db[1,20]<-as.character(parameters$tm_method)
  }
  
  if(!is.null(parameters$min_cooc_freq)){
    entry_for_db[1,21]<-parameters$min_cooc_freq
  }
  
  if(!is.null(parameters$va_timeintervall)){
    entry_for_db[1,22]<-as.character(parameters$va_timeintervall)
  }
  
  if(!is.null(parameters$va_history)){
    entry_for_db[1,23]<-parameters$va_history
  }
  
  if(!is.null(parameters$va_method)){
    entry_for_db[1,24]<-as.character(parameters$va_method)
  }
  
  if(!is.null(parameters$cooc_window)){
    entry_for_db[1,25]<-as.character(parameters$cooc_window)
  }
  
  if(!is.null(parameters$ngrams)){
    entry_for_db[1,26]<-as.character(paste(parameters$ngrams,collapse=", "))
  }
  
  if(!is.null(parameters$blacklist)){
    if(!is.null(parameters$use_blacklist)){
      if(parameters$use_blacklist==TRUE){
        entry_for_db[1,27]<-as.character(parameters$blacklist)
      }
    }
  }
  
  if(!is.null(parameters$whitelist)){
    if(!is.null(parameters$use_whitelist)){
      if(parameters$use_whitelist==TRUE){
        entry_for_db[1,28]<-as.character(parameters$whitelist)
      }
    }
  }
  
  if(!is.null(parameters$consolidate_entities)){
    entry_for_db[1,29]<-as.character(parameters$consolidate_entities)
  }
  
  if(!is.null(parameters$reduce_POS)){
    entry_for_db[1,30]<-as.character(paste(parameters$reduce_POS,collapse=", "))
  }
  
  if(!is.null(parameters$reduce_NER)){
    entry_for_db[1,31]<-as.character(paste(parameters$reduce_NER,collapse=", "))
  }
  
  if(!is.null(parameters$va_weightfactor)){
    entry_for_db[1,32]<-as.character(parameters$va_weightfactor)
  }
  
  if(!is.null(parameters$de_use_context_filter)){
    if(parameters$de_use_context_filter==TRUE){
      if(!is.null(parameters$de_context_filter)){
        entry_for_db[1,33]<-as.character(parameters$de_context_filter)
      }
      
      if(!is.null(parameters$de_context_filter_mode)){
        entry_for_db[1,34]<-as.character(parameters$de_context_filter_mode)
      }
      
      if(!is.null(parameters$de_Context_Unit)){
        entry_for_db[1,35]<-as.character(parameters$de_Context_Unit)
      }
    }
  }
  if(!is.null(parameters$Dictionary)){
    if(!is.null(parameters$de_use_reg_exp)){
      if(parameters$de_use_reg_exp==FALSE){
        entry_for_db[1,36]<-as.character(parameters$Dictionary) 
      }
    }
    else{
      entry_for_db[1,36]<-as.character(parameters$Dictionary)
    }
  }
  
  if(!is.null(parameters$Project)){
    entry_for_db[1,37]<-as.character(parameters$Project)
  }
  
  if(!is.null(parameters$cl_Mode)){
    entry_for_db[1,38]<-as.character(parameters$cl_Mode)
  }
  
  if(!is.null(parameters$cl_Category)){
    entry_for_db[1,39]<-as.character(parameters$cl_Category)
  }
  
  if(!is.null(parameters$cl_positive_Threshold)){
    entry_for_db[1,40]<-as.character(parameters$cl_positive_Threshold)
  }
  
  if(!is.null(parameters$Sentiment_Dictionary)){
    entry_for_db[1,41]<-as.character(parameters$Sentiment_Dictionary)
  }
  if(!is.null(parameters$Document_Score_Aggregation)){
    entry_for_db[1,42]<-as.character(parameters$Document_Score_Aggregation)
  }
  if(!is.null(parameters$remove_numbers_all)){
    entry_for_db[1,44]<-as.character(parameters$remove_numbers_all)
  }
  if(!is.null(parameters$whitelist_only)){
    entry_for_db[1,45]<-as.character(parameters$whitelist_only)
  }
  if(!is.null(parameters$whitelist_expand)){
    entry_for_db[1,46]<-as.character(parameters$whitelist_expand)
  }
  if(!is.null(parameters$remove_custom)){
    entry_for_db[1,47]<-as.character(paste(parameters$remove_custom,collapse=", "))
  }
  if(!is.null(parameters$keep_custom)){
    entry_for_db[1,48]<-as.character(paste(parameters$keep_custom,collapse=", "))
  }
  if(!is.null(parameters$cooc_significance_measure)){
    entry_for_db[1,49]<-as.character(parameters$cooc_significance_measure)
  }
  
  #vector representation
  if(!is.null(parameters$class_use_model)){
    entry_for_db[1,51]<-as.character(parameters$class_use_model)
    if(parameters$class_use_model==TRUE){
      if(!is.null(parameters$class_model)){
        entry_for_db[1,52]<-as.character(parameters$class_model)
      }
    }
    else{
      if(!is.null(parameters$w2v_vectors)){
        entry_for_db[1,54]<-as.character(parameters$w2v_vectors)
      }
      if(!is.null(parameters$w2v_threads)){
        entry_for_db[1,55]<-as.character(parameters$w2v_threads)
      }
      if(!is.null(parameters$w2v_window)){
        entry_for_db[1,56]<-as.character(parameters$w2v_window)
      }
      if(!is.null(parameters$w2v_iterations)){
        entry_for_db[1,57]<-as.character(parameters$w2v_iterations)
      }
      if(!is.null(parameters$w2v_neg_samples)){
        entry_for_db[1,58]<-as.character(parameters$w2v_neg_samples)
      }
      if(!is.null(parameters$w2v_min_occ)){
        entry_for_db[1,59]<-as.character(parameters$w2v_min_occ)
      }
    }
  }
  if(!is.null(parameters$class_dim_reduction)){
    entry_for_db[1,53]<-as.character(parameters$class_dim_reduction)
  }
  
  
  
  if(!is.null(parameters$de_reg_exp)){
    entry_for_db[1,60]<-as.character(parameters$de_reg_exp)
  }
  if(!is.null(parameters$DD_strategy)){
    entry_for_db[1,61]<-as.character(parameters$DD_strategy)
  }
  if(!is.null(parameters$DD_similarity_measure)){
    entry_for_db[1,62]<-as.character(parameters$DD_similarity_measure)
  }
  if(!is.null(parameters$DD_treshold)){
    entry_for_db[1,63]<-as.character(parameters$DD_treshold)
  }
  if(!is.null(parameters$cl_active_learning_strategy)){
    entry_for_db[1,64]<-as.character(parameters$cl_active_learning_strategy)
  }
  if(!is.null(parameters$cl_c)){
    entry_for_db[1,43]<-as.character(parameters$cl_c)
  }
  #KeywordExtraction
  if(!is.null(parameters$KE_Mode)){
    entry_for_db[1,65]<-as.character(parameters$KE_Mode)
    if(parameters$KE_Mode=="without reference"){
      if(!is.null(parameters$KE_no_ref_method)){
        entry_for_db[1,66]<-as.character(parameters$KE_no_ref_method)
        if(parameters$KE_no_ref_method=="RAKE" || parameters$KE_no_ref_method=="Textrank"){
          if(!is.null(parameters$KE_filter)){
            entry_for_db[1,69]<-paste(as.character(parameters$KE_filter),collapse=" ")
          }
        }
        if(parameters$KE_no_ref_method=="Phrase Sequence"){
          if(!is.null(parameters$KE_phrase)){
            entry_for_db[1,70]<-as.character(parameters$KE_phrase)
          }
        }
      }
      if(!is.null(parameters$KE_no_ref_n_min)){
        entry_for_db[1,67]<-as.character(parameters$KE_no_ref_n_min)
      }
      if(!is.null(parameters$KE_no_ref_ngram_max)){
        entry_for_db[1,68]<-as.character(parameters$KE_no_ref_ngram_max)
      }
    }
  }
  
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=port)
  #check if databse uses same amount of parameters; if not just save those parameters in database format; update to later docker image will solve this issue 
  columns_in_db<-RMariaDB::dbGetQuery(conn = mydb,statement = "Show Columns from ilcm.Tasks;")
  query<-paste0('Insert into Tasks Values("',paste0(entry_for_db[1,1:dim(columns_in_db)[1]],collapse='", "'),'");')
  query<-stringr::str_replace_all(string = query,pattern = '\"NA\"',replacement = "NULL")
  RMariaDB::dbBegin(conn = mydb)
  rs <- RMariaDB::dbSendQuery(mydb, query)
  RMariaDB::dbCommit(mydb)
  RMariaDB::dbDisconnect(mydb)
}


# TODO: please specify what metadata is expected here (it is not the same metadata used at other places in code (metadata of documents) but apparently something different
# it seems that a matrix is required listing the taskIds and the collection names, however a metadata<-matrix(metadata,ncol=3) is performed meaning 3 columns though only the first 2 are used?
# additionally this function doesn't seem to be at the correct place (logtofile.R)
# further question: why are the parameters retrieved from database, they are also stored in collections/results/ For getting other things like the calculation result or metadata this folder is used and not the database. Why is it different here?
get_parameters_from_database<-function(metadata){
  source("config_file.R")
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  metadata<-matrix(metadata,ncol=3)
  if(length(metadata>0)){
    parameters<-list()
    for(i in 1:dim(metadata)[1]){
      parameters[[i]]<-RMariaDB::dbGetQuery(mydb,paste0('SELECT * FROM ilcm.Tasks where id=',metadata[i,1],' and collection="',paste0(metadata[i,2]),'";'))
    }
    parameters<-do.call(rbind,parameters)
    RMariaDB::dbDisconnect(mydb)
    parameters[,"keep custom"]<-substr(parameters[,"keep custom"],1,50)
    parameters[,"remove custom"]<-substr(parameters[,"remove custom"],1,50)
    return(unique(parameters))
  }
  else{
    RMariaDB::dbDisconnect(mydb)
    return()
  }
}


replace_TRUE_FALSE<-function(dt){
  for(i in 1:dim(dt)[2]){
    dt[,i]<- stringr::str_replace_all(string =dt[,i] ,pattern = "TRUE",replacement =  as.character(shiny::icon("check",class = "check")))
    dt[,i]<- stringr::str_replace_all(string =dt[,i] ,pattern = "FALSE",replacement =  as.character(shiny::icon("times",class="times")))
  }
  return(dt)
}


delete_result_from_datbase<-function(meta){
  source("config_file.R")
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  RMariaDB::dbSendStatement(mydb,paste0('Delete from ilcm.Tasks where id=',meta[1],' and collection="',paste0(meta[2],'.RData"'),';'))
  RMariaDB::dbDisconnect(mydb)
}