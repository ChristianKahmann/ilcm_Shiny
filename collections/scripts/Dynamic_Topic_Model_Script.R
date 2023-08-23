source("global/text_functions.R")
source("global/log_to_file.R")
source("global/rbind_huge_sparse_Matrix.R")
source("config_file.R")
source("global/functions_used_in_scripts.R")
library("tmca.unsupervised")

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
  
  #at least two points in time
  log_to_file(message = "&emsp; more than one point in time?",logfile)
  if(length(unique(info[[6]][,1]))>1){
    log_to_file(message = "&emsp; ✔",logfile)
  }
  else{
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; Just a single point in time found. Dynamic Topic Modeling needs at least two points in time.</b>",logfile)
    stop("Just one point in time")
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished sanity checks",file = logfile)
  
  
  
  
  #preparing parameters
  log_to_file(message = "<b>Step 4/13: Preparing input parameters</b>",file = logfile)
  parameters<-prepare_input_parameters(parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  
  
  
  #get original documents 
  log_to_file(message = "<b>Step 5/13: Create original documents</b>",file = logfile)
  doc_ids<-unique(db_data$token[,2])
  db_data$token <- data.table::as.data.table(db_data$token[which(db_data$token[,2]%in%doc_ids),])
  documents_original<-db_data$token[, list(text = paste(word, collapse=" ")), by = id]
  colnames(documents_original)<-c("doc_ids","documents_original")
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  #get metadata
  log_to_file(message = "<b>Step 6/13: Getting metadata for detailed metadata analysis from database</b>",file = logfile)
  meta_data<-get_meta_data_for_detailed_topic_analysis(host = host,port = db_port,ids = info[[3]],datasets = unique(info[[2]]),token = db_data$token)
  meta<-meta_data$meta
  meta_names<-meta_data$meta_names
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  
  # #remove locations for transnorms project
  #  db_data$token<-remove_locations(token=db_data$token)
  
  
  
  #preparing token object
  log_to_file(message = "<b>Step 7/13: Preparing token object</b>",file = logfile)
  db_data$token<-prepare_token_object(token = db_data$token,parameters=parameters)
  
  
  # if documents were deleted by reducing to certain ners or pos tags ensure meta and documents ahs the same dimensions
  docs_avail <- unique(db_data$token$doc_id)
  if(length(docs_avail)!=nrow(meta)){
    meta <- meta[which(meta$id_doc%in%docs_avail),]
    documents_original_avail <- which(documents_original$doc_ids%in%docs_avail)
    documents_original<- documents_original[documents_original_avail,]
  }
  
  #split documents
  if(parameters$dtm_chunk_documents==TRUE){
    log_to_file(message = paste0("&emsp;Splitting documents..."),file = logfile)
    tsplit<-parameters$dtm_chunk_documents_n
    log_to_file(message = paste0("&emsp;Using a Splitsize of max. ",tsplit," word per chunk"),file = logfile)
    token_old<-db_data$token
    token_new<-NULL
    documents<-unique(token_old$doc_id)
    number_of_documents<-length(documents)
    if(number_of_documents>10){
      log_levels =  round(seq(1, number_of_documents, length.out = 11), digits = 0)[-1]
    }
    else{
      log_levels = 1:number_of_documents
    }
    for(i in 1:number_of_documents){
      current_token<-token_old[which(token_old$doc_id==documents[i]),]
      if(nrow(current_token)>tsplit){
        meta_row_rel<-which(meta$id_doc==documents[i])
        meta_current<-meta[meta_row_rel,,drop=F]
        doc_id_orig<-meta_current[1,3]
        title_orig<-meta_current[1,4]
        number_of_splits <- ceiling(nrow(current_token)/tsplit)
        split_length <- ceiling(nrow(current_token)/number_of_splits)
        split_boundaries <- 1
        for( k in 1:(number_of_splits-1)){
          split_boundaries<-c(split_boundaries,
                              k * split_length,
                              (k * split_length + 1))
        }
        split_boundaries <- c(split_boundaries, nrow(current_token))
        
        for(j in 1:(length(split_boundaries)/2)){
          split_start<-split_boundaries[(2*(j-1)+1)]
          split_end <-split_boundaries[(2*j)]
          
          current_token[split_start:split_end,1] <- paste0(documents[i],"_",j)
          meta_current[1,3]<-paste0(doc_id_orig,"_",j)
          meta_current[1,4]<-paste0(title_orig,"_",j)
          text<-paste0(current_token[split_start:split_end,4],collapse = " ")
          meta_current$body<-text
          meta<-rbind(meta,meta_current)
        }
        meta<-meta[-meta_row_rel,]
        token_new<-rbind(token_new,current_token)
      }
      else{
        token_new<-rbind(token_new,current_token)
      }
      if(i%in%log_levels){
        percentage =  10*which(log_levels==i)
        log_to_file(message = paste0("&emsp;",percentage,"% finished splitting"),file = logfile)
      }
    }
    db_data$token<-token_new
    meta<-meta[order(meta$id_doc,decreasing = F),]
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing token object",file = logfile)
  
  
  
  
  
  #calculating dtm
  log_to_file(message = "<b>Step 8/13: Calculating DTM</b>",file = logfile)
  dtm<-calculate_dtm(token = db_data$token,parameters = parameters,lang = db_data$language)
  meta<-meta[which(meta$id_doc%in%rownames(dtm)),]
  log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished pre-processing with",dim(dtm)[1], "documents and ",dim(dtm)[2], "features"),file = logfile)
  
  #save.image(paste0("data_TM_Pruning_",Sys.Date(),".RData"))
  
  
  log_to_file(message = "<b>Step 9/13: Clean vocabulary from non asci2 characters</b>",file = logfile)
  #just keep alpha
  # remove<-which(!grepl("[[:alpha:]]", colnames(dtm)) & !grepl("[[:digit:]]", colnames(dtm)))
  # if(length(remove)>0){
  #   dtm<-dtm[,-remove]
  # }
  # #remove all that contains non asci2
  remove<-which(unlist(lapply(colnames(dtm),function(i){max(utf8ToInt(i))}))>256)
  if(length(remove)>0){
    dtm<-dtm[,-remove]
  }
  # #remove all that contains non asci2
  remove<-which(unlist(lapply(colnames(dtm),function(i){min(utf8ToInt(i))}))<33)
  if(length(remove)>0){
    dtm<-dtm[,-remove]
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  
  
  
  
  #remove empty documents
  log_to_file(message = "<b>Step 10/13: Removing empty documents</b>",file = logfile)
  empty<-which(Matrix::rowSums(dtm)==0)
  if(length(empty)>0){
    dtm<-dtm[-empty,]
    documents_original<-documents_original[-empty,]
    info[[6]]<-data.frame(info[[6]][-empty,1],stringsAsFactors = F)
    meta<-meta[-empty,]
    
  }
  log_to_file(message = paste0("<b style='color:green'> ✔ </b>  Deleted ",length(empty)," documents from topic model, because they are empty with the current settings"),logfile)  
  
  
  
  
  
  
  #create time slices
  #make sure every dtm uses at leat two words
  single_word_documents<-which(rowSums(dtm)==1)
  if(length(single_word_documents)>0){
    dtm<-dtm[-single_word_documents,]
    meta<-meta[-single_word_documents,]
  }
  #reoder dtm and meta object by dates
  order<-order(meta$date,decreasing = F)
  meta<-meta[order,]
  dtm<-dtm[order,]
  
  log_to_file(message = "<b>Step 11/13: Create Time Slices</b>",file = logfile)
  dates<-meta$date
  if(parameters$dtm_Date_Type=="Decade"){
    dates<-paste0(substr(dates,0,3),"0")
  }
  if(parameters$dtm_Date_Type=="Year"){
    dates<-substr(dates,0,4)
  }
  if(parameters$dtm_Date_Type=="Month"){
    dates<-substr(dates,0,7)
  }
  if(parameters$dtm_Date_Type=="Week"){
    dates<-strftime(as.character(dates),format="%Y-%V")
  }
  if(parameters$dtm_Date_Type=="Day"){
    dates<-substr(dates,0,11)
  }
  unique_dates <- unique(dates)
  doc_belongings_to_time_slices<-rep(0,nrow(dtm))
  time_slices <- NULL 
  time_slice_names<-NULL
  if(parameters$dtm_split_how=="By Date"){
    n<-parameters$dtm_Date_n
    for (i in 1:ceiling(length(unique_dates)/n)){
      date_start<-unique_dates[(((i-1)*n)+1)]
      date_end<-unique_dates[min(length(unique_dates),(((i)*n)))]
      if(n==1){
        time_slice_names<-c(time_slice_names,date_start)
      }
      else{
        if(date_start==date_end){
          time_slice_names<-c(time_slice_names,date_start)
        }
        else{
          time_slice_names<-c(time_slice_names,paste0(date_start," - ",date_end))
        }
        
      }
      time_slices<-c(time_slices,length(which(dates%in%unique_dates[(((i-1)*n)+1):min(length(unique_dates),(((i)*n)))])))
      doc_belongings_to_time_slices[which(dates%in%unique_dates[(((i-1)*n)+1):min(length(unique_dates),(((i)*n)))])]<-i
    }
  }
  else{
    n<-parameters$dtm_chunksize
    if(n>length(unique_dates)){
      log_to_file(message = "&emsp;<b style='color:red'>&#10008; The number of chunks is bigger than the number of available dates.</b>",logfile)
      stop("Number of Chunks bigger than number of unique dates")
    }
    dates_table<-data.frame(table(dates),stringsAsFactors = F)
    dates_table<-data.frame(min_date=dates_table$dates,max_date=dates_table$dates,count=dates_table$Freq,all_dates=as.character(dates_table$dates),stringsAsFactors = F)
    if(nrow(dates_table)>n){
      repeat{
        min<-which.min(unlist(lapply(1:(nrow(dates_table)-1),FUN = function(x){
          sum(as.numeric(dates_table$count[c(x,(x+1))]))
        })
        )
        )
        ind<-c(min,(min+1))
        min_date<-min(as.character(dates_table$min_date[ind]))
        max_date<-max(as.character(dates_table$max_date[ind]))
        count_new<-sum(as.numeric(dates_table$count[ind]))
        all_dates_in_this_chunk<-paste0(unique(union(dates_table$all_dates[ind[1]],dates_table$all_dates[ind[2]])),collapse = ",")
        dates_table<-rbind(dates_table,c(min_date,max_date,count_new,all_dates_in_this_chunk))
        dates_table<-dates_table[-ind,]
        dates_table<-dates_table[order(dates_table$min_date,decreasing = F),]
        if(nrow(dates_table)==n){
          break
        }
      }
    }
    time_slices<-as.numeric(dates_table$count)
    time_slice_names<-unlist(lapply(X = 1:nrow(dates_table),FUN = function(x){
      min_date<-as.character(dates_table$min_date[x])
      max_date<-as.character(dates_table$max_date[x])
      if(min_date==max_date){
        return(max_date) 
      }
      else{
        return(paste(min_date,"-",max_date))
      }
    })
    )
    for(i in 1:nrow(dates_table)){
      doc_belongings_to_time_slices[which(dates%in%stringr::str_split(string = dates_table$all_dates[i],pattern = ",",simplify = T))]<-i
    }
    
  }
  time_slice_names<-paste0(time_slice_names," (",time_slices,")")
  
  log_to_file(message = paste0("<b style='color:green'> ✔ </b>  Created ",length(time_slices)," Time Slices with an average number of documents per time slice of: ",
                               round(mean(time_slices),digits = 2),"+-",round(sd(time_slices),digits = 2)),logfile)  
  
  
  
  #calculate Dynamic Topic Model
  log_to_file(message = "<b>Step 12/13: Calcualte Dynamic Topic Model</b>",file = logfile)
  library(gensimr)
  library(reticulate)
  #reticulate python path as specified in config_file.R
  reticulate::use_python(python = reticulate_python_path,required = T)

  vocab<-colnames(dtm)
  texts_from_dtm<-list()
  log_to_file(message = "&emsp;Creating vocabulary vector",file = logfile)
  log_helper<- round(seq(from=0,to=nrow(dtm),length.out=11)[2:11])
  for(i in 1:nrow(dtm)){
    text<-list()
    count=0
    repeat_idx<-which(dtm[i,]>0)
    repeat_numbers<-dtm[i,repeat_idx]
    repeat_vocab<-vocab[repeat_idx]
    repeat_df<-cbind(words=repeat_vocab,idx=repeat_idx,times=repeat_numbers)
    text<-(rep(repeat_df[,1],repeat_df[,3]))
    names(text)<-NULL
    text<-as.list(text)
    if(i%in%log_helper){
      print(i)
      log_to_file(message = paste0("&emsp;",(which(log_helper==i)*10) ,"% of documents processed"),file = logfile)
    }
    texts_from_dtm[[i]]<-text
  }
  
  log_to_file(message = "&emsp;Start Python Script",file = logfile)
  dictionary <- corpora_dictionary(docs = texts_from_dtm)
  corpus <- doc2bow(dictionary, texts_from_dtm)
  num_topics<-as.integer(parameters$tm_number_of_topics)
  # mode<-parameters$dtm_mode
  #initialize_lda<-parameters$dtm_initialize_lda
  top_chain_variance<-parameters$dtm_top_chain_variance
  alpha<-parameters$tm_alpha
  write(x = time_slices,file = "collections/tmp/time_sclices.txt")
  reticulate::py_save_object(object = corpus,filename = "collections/tmp/test_corpus")
  reticulate::py_save_object(object = r_to_py(num_topics),filename = "collections/tmp/num_topics")
  reticulate::py_save_object(object = dictionary,filename = "collections/tmp/test_dictionary")
  reticulate::py_save_object(object = r_to_py(as.integer(time_slices)),filename = "collections/tmp/time_slices")
  #reticulate::py_save_object(object = r_to_py(as.character(mode)),filename = "collections/tmp/mode")
  #reticulate::py_save_object(object = r_to_py(initialize_lda),filename = "collections/tmp/initialize_lda")
  reticulate::py_save_object(object = r_to_py(as.numeric(top_chain_variance)),filename = "collections/tmp/top_chain_variance")
  reticulate::py_save_object(object = r_to_py(as.numeric(alpha)),filename = "collections/tmp/alpha")
  # start python script
  reticulate::source_python(file = "global/run_dtm.py")
  
  # fetch results
  results<-list()
  for(i in 1:length(time_slices)){
    results[[i]]<-model$dtm_vis(corpus = corpus, time = as.integer(i-1))
  }
  
  results_additional <- list()
  results_additional$time_slices<-time_slices
  results_additional$doc_belongings_to_time_slices<-doc_belongings_to_time_slices
  results_additional$time_slice_names<-time_slice_names
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  #Saving results
  log_to_file(message = "<b>Step 13/13: Saving results</b>",file = logfile)
  lang<-db_data$language
  path0<-paste0("collections/results/dynamic-topic-model/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  dir.create(path0)
  save(model,results,results_additional,t,vocab,info,lang,file = paste0(path0,"data_TM.RData"))
  write(paste(vocab,collapse=","),file = paste0(path0,"vocab_task",parameters$id,".txt"))
  saveRDS(vocab,file=paste0(path0,"vocab_task",parameters$id,".RDS"))
  save(dtm,file=paste0(path0,"dtm_TM.RData"))
  save(documents_original,file=paste0(path0,"documents_TM.RData"))
  #save(rel_counts,freqs,file=paste0(path0,"est_counts_TM.RData"))
  save(meta,meta_names,file=paste0(path0,"meta_TM.RData"))
  save(info,file=paste0(path0,"info.RData"))
  parameters<-parameters_original
  save(parameters,lang,file=paste0(path0,"parameters.RData"))
  log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
  
  
  log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Dynamic Topic Modeling </b>",logfile)
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


