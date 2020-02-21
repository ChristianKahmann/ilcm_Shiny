source("global/text_functions.R")
source("global/log_to_file.R")
source("global/rbind_huge_sparse_Matrix.R")
source("config_file.R")
source("global/functions_used_in_scripts.R")

error<-try(expr = {
  library(Matrix)
  library(dplyr)
  library(readr)
  library(plyr)
  library(wordVectors)
  #load parameters
  load("collections/tmp/tmp.RData")
  
  #load collection 
  log_to_file(message = "<b>Step 1: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  #preparing parameters
  log_to_file(message = "<b>Step 2: Preparing input parameters</b>",file = logfile)
  parameters_original<-parameters
  parameters<-prepare_input_parameters(param = parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  
  #distinguish between creating a new model from data and using an existing model and jsut visulize it
  if(parameters$class_use_model==FALSE){
    log_to_file(message = "<b> Creating w2v model from collection data</b>",file = logfile)
    #getting data from db
    log_to_file(message = "<b>Step 3/9: Loading data from database</b>",file = logfile)
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
    rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
    ids<-info[[3]]
    ids<- paste(ids[,1],collapse=", ")
    texts<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select body from documents where id in (",ids,");"))
    lang<-as.character(RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select language from documents where id =",info[[3]][1,1],";")))
    RMariaDB::dbDisconnect(mydb)
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
    
    
    
 
    #sanity check
    log_to_file(message = "<b>Step 4/9: Sanity check</b>",file = logfile)
    #specified language has quanteda stopwordlist?
    if(parameters$remove_stopwords==T){
      log_to_file(message = "&emsp; stopwords available for specified language?",logfile)
      if(!(lang%in%stopwords::stopwords_getlanguages(source="stopwords-iso"))){
        log_to_file(message = "&emsp;<b style='color:red'>&#10008; The specified language is not included in stop word list. You can maybe specify a stopwordlist using the blacklist functionality.</b>",file = logfile)
        stop("Stopwords not available for found language")
      }
      else{
        log_to_file(message = "&emsp; ✔",logfile)
      }
    }
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished sanity checks",file = logfile)
    
    
    
    
    log_to_file(message = "<b>Step 5/9: Preprocessing texts</b>",file = logfile)
    text<-paste(texts[,1],collapse=" ")
    #remove newline
    text<-stringr::str_replace_all(string = text,pattern ="\\\\n" ,replacement = " ")
    text<-stringr::str_replace_all(string = text,pattern ="\\n" ,replacement = " ")
    
    if(parameters$remove_punctuation==TRUE){
      log_to_file(message = "&emsp;removing punctuation...",file = logfile)
      text<-stringr::str_remove_all(string = text,pattern="[[:punct:]]+")
      gc()
    }
    if(parameters$lowercase==TRUE){
      log_to_file(message = "&emsp;lowercasing...",file = logfile)
      words<-strsplit(text," ")[[1]]
      words<-tolower(words)
      text<-paste(words,collapse=" ")
      gc()
    }
    if(parameters$w2v_min_occ>1){
      log_to_file(message = "&emsp;removing words occuring not often enough",file = logfile)
      words<-strsplit(text," ")[[1]]
      words.freq<-table(unlist(words))
      wordfreq<-cbind(names(words.freq),as.integer(words.freq))
      toDel<-wordfreq[which(as.numeric(wordfreq[,2])<parameters$w2v_min_occ),1]
      words<-words[-which(words%in%toDel)]
      text<-paste(words,collapse=" ")
    }
    if(parameters$remove_stopwords==TRUE){
      log_to_file(message = "&emsp;removing stopwords...",file = logfile)
      words<-strsplit(text," ")[[1]]
      stopword_idx<-which(words%in%quanteda::stopwords(lang))
      if(length(stopword_idx)>0){
        words<-words[-stopword_idx]
      }
      text<-paste(words,collapse=" ")
      gc()
    }
    #remove double whitespaces
    text<-stringr::str_replace_all(string = text,pattern = "   ",replacement = " ")
    text<-stringr::str_replace_all(string = text,pattern = "  ",replacement = " ")
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preprocessing texts",file = logfile)
    
    
    
    
    log_to_file(message = "<b>Step 6/9: Writing text to input.txt file</b>",file = logfile)
    path0<-paste0("collections/results/vector-space-representation/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
    path0<-stringr::str_replace_all(string = path0,pattern = " ",replacement = "_")
    dir.create(path0)
    dir.create(paste0(path0,"training"))
    #setwd(dir = path0)
    write(text,file = paste0(path0,"training//inputtext.txt"))
    log_to_file(message = "  <b style='color:green'> ✔ </b>  ",file = logfile)
    
    
    
    log_to_file(message = "<b>Step 7/9: Binding n-grams together</b>",file = logfile)
    #here one could also set min_count and threshold for ngrams
    wordVectors::prep_word2vec(origin = paste0(path0,"training//inputtext.txt"),destination = paste0(path0,"training/input.txt"),lowercase=F,bundle_ngrams=parameters$ngrams,min_count=parameters$w2v_min_occ)
    log_to_file(message = "  <b style='color:green'> ✔ </b>  ",file = logfile)
    
    
    
    
    
    
    log_to_file(message = "<b>Step 8/9: Calculating Model...This might take a while</b>",file = logfile)
    model = train_word2vec(paste0(path0,"training//input.txt"),paste0(path0,"vectors.bin"),vectors=parameters$w2v_vectors,threads=parameters$w2v_threads,
                           window=parameters$w2v_window,iter=parameters$w2v_iterations,negative_samples=parameters$w2v_neg_samples)
    log_to_file(message = "  <b style='color:green'> ✔ Finished creating w2v model</b>  ",file = logfile)
    
    
    
    if(parameters$class_dim_reduction==TRUE){
      log_to_file(message = "<b> Calculating dimension reduction</b>",file = logfile)
      log_to_file(message = "&emsp;calculate pca for visualization...",file = logfile)
      pca<-stats::predict(stats::prcomp(as.matrix(model)))[,1:2]
      log_to_file(message = "&emsp;calculate tsne for visualization...",file = logfile)
      #tsne<-tsne::tsne(as.matrix(model),max_iter = 250)
      tsne<-Rtsne::Rtsne(X = as.matrix(model),max_iter=250)$Y
      rownames(tsne)<-rownames(model)
      log_to_file(message = "  <b style='color:green'> ✔ Finished dimension reduction </b>  ",file = logfile)
      
      
      
      log_to_file(message = "<b>Step 9/9: Saving results</b>",file = logfile)
      save(pca,file=paste0(path0,"pca.RData"))
      save(tsne,file=paste0(path0,"tsne.RData"))
      save(info,file=paste0(path0,"info.RData"))
      parameters<-parameters_original
      save(parameters,file=paste0(path0,"parameters.RData"))
      log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
    }


    log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Vector Space Representation </b>",logfile)
    system(paste("mv ",logfile," collections/logs/finished/",sep=""))

  }
  else{
    log_to_file(message = "<b> Using existing model</b>",file = logfile)
    
    log_to_file(message = "<b>Step 3/5: Loading model</b>",file = logfile)
    
    if(stringr::str_detect(string = parameters$class_model,pattern = ".bin")){
      model<-read.vectors(filename = paste0("collections/word_vector_models/",parameters$class_model))
    }
    else{
      model<-read.vectors(filename = paste0("collections/word_vector_models/",parameters$class_model),binary = F)
    }
    log_to_file(message = "  <b style='color:green'> ✔ Finished loading model</b>  ",file = logfile)
    
    path0<-paste0("collections/results/vector-space-representation/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
    path0<-stringr::str_replace_all(string = path0,pattern = " ",replacement = "_")
    dir.create(path0)
    if(parameters$class_dim_reduction==TRUE){
      log_to_file(message = "<b> Calculating dimension reduction</b>",file = logfile)
      log_to_file(message = "&emsp;calculate pca for visualization...",file = logfile)
      pca<-stats::predict(stats::prcomp(as.matrix(model)))[,1:2]
      log_to_file(message = "&emsp;calculate tsne for visualization...",file = logfile)
      #tsne<-tsne::tsne(as.matrix(model),max_iter = 250)
      tsne<-Rtsne::Rtsne(X = as.matrix(model),max_iter=250)$Y
      rownames(tsne)<-rownames(model)
      log_to_file(message = "  <b style='color:green'> ✔ Finished dimension reduction </b>  ",file = logfile)
      
      
      
      log_to_file(message = "<b>Step 4/5: Saving results</b>",file = logfile)
      save(pca,file=paste0(path0,"pca.RData"))
      save(tsne,file=paste0(path0,"tsne.RData"))
      save(info,file=paste0(path0,"info.RData"))
      parameters<-parameters_original
      save(parameters,file=paste0(path0,"parameters.RData"))
      log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
    }
    save(model,file=paste0(path0,"//model.RData"))
    
    
    log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Vector Space Representation </b>",logfile)
    system(paste("mv ",logfile," collections/logs/finished/",sep=""))
    
    
    
  }
}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}

