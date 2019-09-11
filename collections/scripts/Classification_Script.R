source("global/text_functions.R")
source("global/log_to_file.R")
source("config_file.R")
source("global/TextProcessingBackend.R")
source("global/TextObjectWrapper.R")
source("global/classification_model.R")
source("global/rbind_huge_sparse_Matrix.R")
source("global/functions_used_in_scripts.R")

error<-try(expr = {
  library(Matrix)
  library(dplyr)
  library(spacyr)
  library(magrittr)
  load("collections/tmp/tmp.RData")
  #spacy_initialize()
  
  
  #load collection 
  log_to_file(message = "<b>Step 1/14: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  
  
  #load data from database
  log_to_file(message = "<b>Step 2/14: Loading data from database</b>",file = logfile)
  db_data<-get_token_meta_and_language_from_db(get_language=T,get_global_doc_ids=T)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  
  #getting annotations for chosen collection,project and category
  log_to_file(message = "<b>Step 3/14: Loading annotations from database</b>",file = logfile)
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  annotations<-RMariaDB::dbGetQuery(mydb, paste("select * from Annotations where global_doc_id in (",paste(unlist(db_data$global_doc_ids),collapse=","),");",sep=""))
  classifications_approved<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from annotations_classification where project='",parameters$Project,"';"))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading annotations from database",file = logfile)
  
  
  #preparing parameters
  log_to_file(message = "<b>Step 4/14: Preparing input parameters</b>",file = logfile)
  prepare_input_parameters()
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  #load dictionary
  log_to_file(message = "<b>Step 5/14: Try loading dictionary</b>",file = logfile)
  try({
    if(parameters$use_dictionary==TRUE){
      load(paste0("collections/dictionaries/",dict,".RData"))
      log_to_file(message = "  <b style='color:green'> ✔ </b>  Dictionary loaded",file = logfile)
    }
    else{
      dict<-""
      log_to_file(message = "  <b style='color:green'> ✔ </b>  Not using a dictionary in this task",file = logfile)
    }
  })
  
  
  
  
  #load annotation set according to chosen project
  log_to_file(message = "<b>Step 6/14: Loading annotation set</b>",file = logfile)
  load(paste0("collections/annotation_schemes/",parameters$Project,".RData"))
  names_overall<-names(unlist(anno))
  anno<-unlist(anno)
  name_tag<-names_overall[which(grepl(pattern = paste0(NULL,".name"),x = names_overall))]
  names<-anno[name_tag]
  color_tag<-names_overall[which(grepl(pattern = paste0(NULL,".color"),x = names_overall))]
  colors<-anno[color_tag]
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Annotationset loaded",file = logfile)
  
  
  
  #set path right for saving results according to chosen mode
  if(parameters$cl_Mode=="Evaluate Training Set"){
    path0<-paste0("collections/results/classification/","evaluateTraining/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  }
  if(parameters$cl_Mode=="Produce 50 new active learning examples"){
    path0<-paste0("collections/results/classification/","activeLearning/",parameters$Project,"/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  }
  if(parameters$cl_Mode=="Classify on entire collection"){
    path0<-paste0("collections/results/classification/","classifyCollection/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  }
  if(parameters$cl_Mode=="Active learning on whole documents"){
    path0<-paste0("collections/results/classification/","activeLearning_documents/",parameters$Project,"/",paste(process_info[[1]],process_info[[2]],process_info[[4]],sep="_"),"/")
  }
  
  
  #preparing token object
  log_to_file(message = "<b>Step 7/14: Preparing token object</b>",file = logfile)
  datasets<-db_data$token[,1]
  db_data$token<-prepare_token_object(token = db_data$token)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing token object",file = logfile)
  
  
  
  #getting original documents
  log_to_file(message = "<b>Step 8/14: Getting original documents/sentences</b>",file = logfile)
  original_text<-get_original_documents()
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished building original documents/sentences",file = logfile)
  
  
  
  
  #formatting classification input as a tibble
  log_to_file(message = "<b>Step 9/14: Formatting classification input</b>",file = logfile)
  feature_list<-calculate_dtm(tibble=T)
  log_to_file(message = paste0("&emsp; Reordering feature list"),logfile)
  feature_list<-feature_list[gtools::mixedorder(feature_list[,1]),]
  colnames(feature_list) <- c("ref_id", "feature")
  feature_list$id <- as.integer(factor(feature_list$ref_id))
  ref_id_cut <- unique(feature_list$ref_id)
  #check internal format; A list containing named vactors that carry the information
  my_gold <-
    list() #data.table::data.table(ref_id = unique(test_processed$ref_id))
  my_gold$ref_id <- ref_id_cut
  #Create empty representations for all possible fields
  my_gold$gold <- rep(NA, length(my_gold$ref_id))
  my_gold$dict <- rep(NA, length(my_gold$ref_id))
  my_gold$predicted <- rep(NA, length(my_gold$ref_id))
  my_gold$set <- rep("train", length(my_gold$ref_id))
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished formatting classification input",file = logfile)
  
  
  
  
  #init classifier
  log_to_file(message = "<b>Step 10/14: Initiating classifier</b>",file = logfile)
  classifier <- cmodel$new(method = "LiblineaR::LiblineaR")
  classifier$.__enclos_env__$private$silent = T
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Initiating classifier",file = logfile)
  
  
  
  
  #insert made annotations and approved classifications
  log_to_file(message = "<b>Step 11/14: Inserting made annotations and approved classifications as training input</b>",file = logfile)
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  count=0
  try({
    for(i in 1:dim(annotations)[1]){
      id_ws<-paste(annotations[i,"from"]:annotations[i,"to"],collapse = ", ")
      identifier<-paste(annotations[i,"dataset"],annotations[i,"id"],unique(RMariaDB::dbGetQuery(mydb, paste("select sid from token where dataset='",annotations[i,"dataset"],"' and id= ",annotations[i,"id"],";",sep=""))[annotations[i,"from"]:annotations[i,"to"],1]),sep="_")
      try({
        my_gold$gold[which(ref_id_cut%in%unlist(identifier))]<-list(data.table(class=as.character(annotations[i,"Annotation"]),coder=as.character(annotations[i,"User"]),timestamp=as.character(annotations[i,"Annotation_Date"])))
        count=count+1
      })
    }
    #for approved classifications
    for(i in 1:dim(classifications_approved)[1]){
      identifier<-paste(tolower(classifications_approved[i,1]),classifications_approved[i,2],classifications_approved[i,3],sep="_")
      try({
        my_gold$gold[which(ref_id_cut%in%unlist(identifier))]<-list(data.table(class=as.character(classifications_approved[i,"category"]),coder="approved classification",timestamp=as.character(classifications_approved[i,"timestamp"])))
        count=count+1
      })
    }
  })
  RMariaDB::dbDisconnect(mydb)
  if(count<1 && parameters$use_dictionary==F){
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No annotation was found for your specified collection and annotation set.
              Also you specified not to use a dictionary. The system can not calculate classifications without any training examples generated by annotations or using a dictionary.</b>",logfile)
    stop("No Training data")
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  
  #set input of classifier
  log_to_file(message = "<b>Step 12/14: Starting up classifier with training data</b>",file = logfile)
  classifier$set_input(
    corpus = data.table(ref_id = original_text$doc_id[original_text$doc_id %in% ref_id_cut], text = original_text$token[original_text$doc_id %in% ref_id_cut]),
    features = feature_list[feature_list$ref_id %in% ref_id_cut, ],
    gold = my_gold
  )
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  
  
  
  
  
  #######learning examples#########################################################################
  if(parameters$cl_Mode=="Produce 50 new active learning examples"){
    log_to_file(message = "<b>Step 13/14: Producing 50 new active learning examples</b>",file = logfile)
    log_to_file(message = paste("&emsp;",count, "Training sets (annotations and approved classifications) were found and used"),file = logfile)
    
    if(parameters$use_dictionary==TRUE){
      log_to_file(message = "&emsp; Dictionary lookup",file = logfile)
      classifier$active_learn_dictionary_examples(dictionary = dict)
      examples<-classifier$active_learning(
        strategy = "LCB",
        batch_size = 50,
        positive_class = parameters$cl_Category,
        from_dictionary_hits = T,
        labels = names(dict),
        cross = 10,
        coder = "AN",
        save_out=TRUE
      )
    }
    else{
      log_to_file(message = "&emsp; Retrieving examples without dictionary...",file = logfile)
      examples<-classifier$active_learning(
        strategy = "LCB",
        batch_size = 50,
        positive_class = parameters$cl_Category,
        from_dictionary_hits = F,
        labels = names(dict),
        cross = 10,
        coder = "AN",
        save_out=TRUE
      )
    }
    
    result<-classifier$train_and_evaluate_cross(k = min(10,ceiling(count/2)))
    data<-original_text[which(original_text[,1]%in%examples),]
    
    data<-cbind(data,approved=FALSE,denied=FALSE,ignored=FALSE,other="unknown")
    data<-data.frame(data,stringsAsFactors = F)
    colnames(data)<-c("doc_id_global","token","approved","denied","ignored","other")
    learning_meta<-list()
    learning_meta[["color"]]<-colors[which(names==parameters$cl_Category)]
    learning_meta[["category"]]<-parameters$cl_Category
    learning_meta[["collection"]]<-parameters$collection
    learning_meta[["date"]]<-Sys.time()
    learning_meta[["log"]]<-list(precision=runif(1,0,1),recall=runif(1,0,1))
    learning_meta[["project"]]<-parameters$Project
    dir.create(path = path0,recursive = T)
    save(learning_meta,data,result,file=paste0(path0,"training_examples.RData"))
    save(parameters,file=paste0(path0,"parameters.RData"))
    save(info,file=paste0(path0,"info.RData"))
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished producing 50 new active learning examples ",file = logfile)
  }
  if(parameters$cl_Mode=="Evaluate Training Set"){
    log_to_file(message = "<b>Step 13/14: Evaluating Training Set</b>",file = logfile)
    result<-classifier$train_and_evaluate_cross(k = min(10,ceiling(count/2)))
    dir.create(path = path0,recursive = T)
    save(result,file=paste0(path0,"result.RData"))
    save(parameters,file=paste0(path0,"parameters.RData"))
    save(info,file=paste0(path0,"info.RData"))
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  }
  if(parameters$cl_Mode=="Active learning on whole documents"){
    log_to_file(message = "<b>Step 13/14: Active learning on whole documents</b>",file = logfile)
    classifier$train()
    #sample documents
    doc_ids<-unique(stringr::str_replace(string = ref_id_cut,pattern = "_[0-9]+$",replacement = ""))
    #remove those, who were already annotated
    annotated_doc_ids<-unique(stringr::str_replace(string = ref_id_cut[which(!is.na(my_gold$gold))],pattern = "_[0-9]+$",replacement = ""))
    doc_ids<-setdiff(doc_ids,annotated_doc_ids)
    random_sample<-sample(x = doc_ids,size = 10,replace = F)
    
    #extending doc_ids to sentence_ids
    random_sample_sentences<-ref_id_cut[which(stringr::str_replace(string = ref_id_cut,pattern = "_[0-9]+$",replacement = "")%in%random_sample)]
    labels<-classifier$predict(ref_id = random_sample_sentences,save_out=TRUE)
    labels[["manual"]]<-rep(NULL,length(labels$predictions))
    log_to_file(message = paste0("&emsp;",length(labels$predictions)," predictions were made"),logfile)
    #prepare output
    #get texts
    log_to_file(message = "&emsp; Preparing output...",logfile)
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
    texts<-data.frame(dataset=NULL,id_doc=NULL,sen_id=NULL,text=NULL,title=NULL,date=NULL,approved=NULL,denied=NULL,ignored=NULL,other=NULL,stringsAsFactors = F)
    for(i in 1:length(labels$predictions)){
      x<-stringr::str_split(string = rownames(labels$probabilities)[i],pattern = "_",simplify = T)
      dataset<-x[1]
      id_doc=x[2]
      sen_id=x[3]
      text<-paste(RMariaDB::dbGetQuery(mydb, paste("select word from token where dataset='",dataset,"' and id =",id_doc," and sid=",sen_id,";",sep=""))[,1],collapse=" ")
      title<-RMariaDB::dbGetQuery(mydb, paste("select title from documents where dataset='",dataset,"' and id_doc =",id_doc,";",sep=""))
      date<-as.character(RMariaDB::dbGetQuery(mydb, paste("select date from documents where dataset='",dataset,"' and id_doc =",id_doc,";",sep="")))
      texts<-rbind(texts,data.frame(dataset,id_doc,sen_id,text,title,date,FALSE,FALSE,FALSE,"unknown",stringsAsFactors = F))
    }
    ord<-paste(texts[,1],texts[,2],texts[,3],sep="_")
    ord<-gtools::mixedorder(ord)
    texts<-texts[ord,]
    colnames(texts)<-c("dataset","id_doc","sen_id","text","title","date","approved","denied","ignored","other")
    labels$predictions<-labels$predictions[ord]
    labels$probabilities<-labels$probabilities[ord,]
    
    log_to_file(message = "&emsp; Saving output...",logfile)
    dir.create(path = path0,recursive = T)
    project<-parameters$Project
    save(texts,labels,project,file=paste0(path0,"examples.RData"))
    save(parameters,file=paste0(path0,"parameters.RData"))
    save(info,file=paste0(path0,"info.RData"))
    RMariaDB::dbDisconnect(mydb)
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  }
  if(parameters$cl_Mode=="Classify on entire collection"){
    log_to_file(message = "<b>Step 13/14: Classification on entire collection</b>",file = logfile)
    classifier$train()
    labels<-classifier$predict(ref_id = ref_id_cut,save_out=TRUE)
    log_to_file(message = paste0("&emsp;",length(labels$predictions)," predictions were made"),logfile)
    log_to_file(message = paste0("&emsp; collecting dates for made classifications..."),logfile)
    predictions<-labels$predictions
    probabilities<-apply(X = labels$probabilities,MARGIN = 1,FUN = max)
    keep<- which(probabilities>parameters$cl_positive_Threshold)
    log_to_file(message = paste0("&emsp;",length(keep)," predictions had a probability higher than the set threshold"),logfile)
    if(length(keep)<1){
      
      dates<-NULL
      original_text<-NULL
    }
    else{
      predictions<-predictions[keep]
      probabilities<-probabilities[keep]
      classes<-as.character(unique(as.character(predictions)))
      dates<-list()
      for(i in classes){
        ident<-stringr::str_split(string = names(predictions[which(predictions==i)]),pattern = "_",simplify = T)[,1:2,drop=F]
        date<-lapply(1:dim(ident)[1],FUN = function(x){
          as.character(as.Date(RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select date from documents where dataset='",ident[x,1],"' and id_doc=",ident[x,2],";"))[1,1]))
        })
        dates[[as.character(i)]]<-unlist(date)
      }
      original_text<-original_text[keep,]
      original_text<-cbind(cbind(as.character(predictions),probabilities),original_text)
      
    }
    
    result<-classifier$train_and_evaluate_cross(k = min(10,ceiling(count/2)))
    dir.create(path = path0,recursive = T)
    save(dates,predictions,labels,result,file=paste0(path0,"result.RData"))
    save(original_text,file=paste0(path0,"texts.RData"))
    save(parameters,file=paste0(path0,"parameters.RData"))
    save(info,file=paste0(path0,"info.RData"))
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  }
  
  
  #Wrinting metadata to database Task column
  log_to_file(message = "<b>Step 14/14: Writing task parameter to database</b>",file = logfile)
  write_metadata_to_database(parameters)
  log_to_file(message = " <b style='color:green'> ✔ </b>  Finished writing task parameter",logfile)
  
  log_to_file(message = " <b style='color:green'>Process finished successfully.</b>",logfile)
  
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  RMariaDB::dbDisconnect(mydb)
}) 

if(class(error)=="try-error"){
  system(paste("mv ",logfile," collections/logs/failed/",sep=""))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
}

