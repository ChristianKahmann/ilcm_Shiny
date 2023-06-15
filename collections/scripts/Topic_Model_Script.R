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
  
  
  
  #preparing token object
  log_to_file(message = "<b>Step 7/13: Preparing token object</b>",file = logfile)
  db_data$token<-prepare_token_object(token = db_data$token,parameters=parameters)
  
  
  #split documents
  if(parameters$tm_chunk_documents==TRUE){
    log_to_file(message = paste0("&emsp;Splitting documents..."),file = logfile)
    tsplit<-parameters$tm_chunk_documents_n
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
  log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished pre-processing with",dim(dtm)[1], "documents and ",dim(dtm)[2], "features"),file = logfile)
  
  #save.image(paste0("data_TM_Pruning_",Sys.Date(),".RData"))
  
  
  log_to_file(message = "<b>Step 9/13: Clean vocabulary from non asci2 characters</b>",file = logfile)
  #just keep alpha
  # remove<-which(!grepl("[[:alpha:]]", colnames(dtm)) & !grepl("[[:digit:]]", colnames(dtm)))
  # if(length(remove)>0){
  #   dtm<-dtm[,-remove]
  # }
  # #remove all that contains non asci2
  meta<-meta[order(match(meta$id_doc,rownames(dtm))),]
  documents<-documents[order(match(documents$doc_id,rownames(dtm))),]
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
    documents<-documents[-empty,]
    documents_original<-documents_original[-empty,]
    info[[6]]<-data.frame(info[[6]][-empty,1],stringsAsFactors = F)
    meta<-meta[-empty,]
  }
  
  log_to_file(message = paste0("<b style='color:green'> ✔ </b>  Deleted ",length(empty)," documents from topic model, because they are empty with the current settings"),logfile)  
  
  
  
  
  
  #calculate topic model
  log_to_file(message = "<b>Step 11/13: Create Topic Model</b>",file = logfile)
  if(isTRUE(parameters_original$tm_use_precalculated_topic_model)){
    log_to_file(message = "&emsp;Use of a predefinded topic model",file = logfile)
    info_orig<-info
    load(paste0("collections/results/topic-model/",parameters_original$tm_precalculated_topic_model,"/data_TM.RData"))
    info<-info_orig
    phi_pre<-phi
    rm(info_orig)
    #infer new documents using old topic model
    log_to_file(message = "&emsp;Infering new documents...",file = logfile)
    
    result<-NULL
    for(d in 1:nrow(dtm)){
      print(d)
      result<-rbind(result,t$infer_topics(dtm = dtm[d,,drop=F]))
    }
    
    t$.__enclos_env__$private$theta<-result
    
    
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished calculating topic model",file = logfile)
    
    log_to_file(message = "<b>Step 12/13: Create Variables for Visulization</b>",file = logfile)
    model<-t$get_model()
    theta<-result
    phi<-model$phi
    phi<-phi[rownames(phi_pre),]
    #set column names and row names for theta and phi
    
    if(is.null(colnames(theta))){colnames(theta) <- 1:ncol(theta)} # topic names
    if(is.null(rownames(theta))){rownames(theta) <- dtm@Dimnames$docs} # doc names
    
    if(is.null(colnames(phi))){colnames(phi) <- dtm@Dimnames$features} # vocab
    if(is.null(rownames(phi))){rownames(phi) <- 1:nrow(phi)} # topic names
    
    theta<-theta[,rownames(phi_pre)]
    doc.length<-Matrix::rowSums(dtm)
    vocab<-colnames(phi)
    #append unknown vocab to dtm
    unknown_vocab<-Matrix(c(0),nrow(dtm),length(setdiff(vocab,colnames(dtm))))
    colnames(unknown_vocab)<-setdiff(vocab,colnames(dtm))
    dtm<-cbind(dtm,unknown_vocab)
    term.frequency<-Matrix::colSums((dtm[,vocab]))
    topic.frequency <- colSums(theta * doc.length)
    topic.proportion <- topic.frequency/sum(topic.frequency)
    o <- order(topic.proportion, decreasing = TRUE)
    # phi <- phi[o, ]
    # theta <- theta[, o]
    # topic.frequency <- topic.frequency[o]
    # topic.proportion <- topic.proportion[o]
    json <- LDAvis::createJSON(
      phi = phi, 
      theta = theta, 
      doc.length = doc.length, 
      vocab = vocab, 
      term.frequency = term.frequency,
      reorder.topics=FALSE
    )
    log_to_file(message = "  <b style='color:green'> ✔ </b>  ",file = logfile)
    
    
  }
  else{
    #delete columns from dtm if they dont occur
    empty<-which(colSums(dtm)==0)
    if(length(empty)>0){
      dtm<-dtm[,-empty]
    }
    
    
    t <- tmca.unsupervised::tmodel$new(method =parameters$tm_method)
    t$input_preprocessed(dtm = dtm,documents)
    
    # specific for Structural Topic Models because it has different parameters
    if(parameters$tm_method == "stm"){
      
      # meta data: set names from mde1/mde2 etc to real meta names
      metaDataToUse <- combineMetaDataWithMetaNamesForMDEs(meta, meta_names)
      # get all stm parameters from parameters and also set metadata
      parametersNeededForSTM <- t$get_parameters() # work around for missing parameters
      parametersNeededForSTM[["metaData"]] <- metaDataToUse
      parametersNeededForSTM[["K"]] <- parameters$tm_number_of_topics
      prevalence <- NULL
      if(nchar(parameters$stm_prevalenceFormula)>0) {prevalence <- as.formula(parameters$stm_prevalenceFormula)}
      parametersNeededForSTM[["prevalence"]] <- prevalence
      contentFormula <- NULL
      if(nchar(parameters$stm_contentFormula)>0) {contentFormula <- as.formula(parameters$stm_contentFormula)}
      parametersNeededForSTM[["content"]] <- contentFormula
      parametersNeededForSTM[["init.type"]] <- parameters$stm_init_type
      parametersNeededForSTM[["max.em.its"]] <- parameters$stm_max_em_its
      parametersNeededForSTM[["emtol"]] <- parameters$stm_emtol
      parametersNeededForSTM[["LDAbeta"]] <- parameters$stm_LDAbeta
      parametersNeededForSTM[["interactions"]] <- parameters$stm_interactions
      parametersNeededForSTM[["ngroups"]] <- parameters$stm_ngroups
      parametersNeededForSTM[["gamma.prior"]] <- parameters$stm_gamma_prior
      parametersNeededForSTM[["sigma.prior"]] <- parameters$stm_sigma_prior
      parametersNeededForSTM[["kappa.prior"]] <- parameters$stm_kappa_prior
      
      parametersToUse <- copyListButRemoveNullValuesAndEmptyStringValues(parametersNeededForSTM)
      
      t$set_parameters(par_list = parametersToUse)
      
      
    }
    else{
      t$set_parameters(par_list = list("alpha"=parameters$tm_alpha,"K"=parameters$tm_number_of_topics))
    }
    
    t$create_tm()
    
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished calculating topic model",file = logfile)
    
    log_to_file(message = "<b>Step 12/13: Create Variables for Visulization</b>",file = logfile)
    if(parameters$tm_method == "stm"){
      model<-t$get_model()
      theta<-model$theta
      phi<-model$phi
      model$stm_model<-t$.__enclos_env__$self$.__enclos_env__$private$tm_machine$.__enclos_env__$private$model
      
    }
    else{
      model<-t$get_model()
      theta<-model$theta
      phi<-model$phi
    }
    #set column names and row names for theta and phi
    if(is.null(colnames(theta))){colnames(theta) <- sapply(seq(1:parameters$tm_number_of_topics), function(x) x)} # topic names
    if(is.null(rownames(theta))){rownames(theta) <- dtm@Dimnames$docs} # doc names
    if(is.null(colnames(phi))){colnames(phi) <- dtm@Dimnames$features} # vocab
    if(is.null(rownames(phi))){rownames(phi) <- sapply(seq(1:parameters$tm_number_of_topics), function(x) x)} # topic names
    
    
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
  }
  
  
  #Saving results
  log_to_file(message = "<b>Step 13/13: Saving results</b>",file = logfile)
  lang<-db_data$language
  path0<-paste0("collections/results/topic-model/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  dir.create(path0)
  save(model,t,theta,phi,doc.length,vocab,term.frequency,json,info,lang,file = paste0(path0,"data_TM.RData"))
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
  
  
  
  
  log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Topic Model </b>",logfile)
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


