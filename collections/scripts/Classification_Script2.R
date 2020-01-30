source("global/text_functions.R")
source("global/log_to_file.R")
source("config_file.R")
source("global/TextProcessingBackend.R")
source("global/TextObjectWrapper.R")
source("global/classification_model.R")
source("global/rbind_huge_sparse_Matrix.R")
source("global/functions_used_in_scripts.R")
source("global/utils.R")

error<-try(expr = {
  library(Matrix)
  library(dplyr)
  library(spacyr)
  library(magrittr)
  require(LiblineaR)
  require(SparseM)
  
  load("collections/tmp/tmp.RData")
  #spacy_initialize()
  
  
  #load collection 
  log_to_file(message = "<b>Step 1/16: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  
  
  #load data from database
  log_to_file(message = "<b>Step 2/16: Loading data from database</b>",file = logfile)
  db_data<-get_token_meta_and_language_from_db(get_language=T,get_global_doc_ids=T)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  
  #getting annotations for chosen collection,project and category
  log_to_file(message = "<b>Step 3/16: Loading annotations from database</b>",file = logfile)
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  annotations<-RMariaDB::dbGetQuery(mydb, paste("select * from Annotations where Anno_set='",parameters$Project,"';",sep=""))
  classifications_approved<-RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select * from annotations_classification where project='",parameters$Project,"';"))
  RMariaDB::dbDisconnect(mydb)
  #check for document/sentece level annotations
  if(parameters$cooc_window=="Document"){
    annotations<-annotations[which(annotations$document_annotation=="TRUE"),]
    classifications_approved<-classifications_approved[which(classifications_approved$document_annotation=="TRUE"),]
  }
  if(parameters$cooc_window=="Sentence"){
    annotations<-annotations[which(annotations$document_annotation=="FALSE"),]
    classifications_approved<-classifications_approved[which(classifications_approved$document_annotation=="FALSE"),]
  }
  
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading annotations from database",file = logfile)
  
  
  #preparing parameters
  log_to_file(message = "<b>Step 4/16: Preparing input parameters</b>",file = logfile)
  prepare_input_parameters()
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  #sanity check
  log_to_file(message = "<b>Step 5/16: Sanity check</b>",file = logfile)
  #token object not empty
  log_to_file(message = "&emsp; token object not empty?",logfile)
  if(dim(db_data$token)[1]>1){
    log_to_file(message = "&emsp; ✔",logfile)
  }
  else{
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No documents were found in the database for the specified collection.</b>",logfile)
    stop("Token empty")
  }
  #annotations for documents in collection?
  anno_ids<-unique(c(paste0(annotations[,"dataset"],"_",annotations[,"id"]),paste0(classifications_approved[,"dataset"],"_",classifications_approved[,"doc_id"])))
  if(length(anno_ids)==0){
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No annotations found for chosen project.</b>",logfile)
    stop("No annotations")
  }
  else{
    log_to_file(message = paste0("&emsp; ✔ ",length(anno_ids)," Annotations or approved classifications found that belong to project: ",parameters$Project),logfile)
  }
  anno_ids_in_token<-intersect(anno_ids,unique(db_data$token$id))
  if(length(anno_ids_in_token)==0){
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No annotations belong to chosen collection</b>",logfile)
    stop("No annotations for collection")
  }
  else{
    log_to_file(message = paste0("&emsp; ✔ ",length(anno_ids_in_token)," Annotations found that belong to chosen collection"),logfile)
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished sanity checks",file = logfile)
  
  
  
  #get original documents 
  log_to_file(message = "<b>Step 5/16: Create original documents</b>",file = logfile)
  if(parameters$cooc_window=="Document"){
    doc_ids<-db_data$token[,2]
  }
  else{
    doc_ids<-paste(db_data$token[,2],db_data$token[,3],sep="_")
  }
  token<-cbind(db_data$token,doc_ids)
  documents_original<-aggregate(word ~ doc_ids,token,paste,collapse=" ")
  documents_original[,1]<-as.character(documents_original[,1])
  rownames(documents_original)<-documents_original[,1]
  documents_original<-documents_original[unique(doc_ids),]
  colnames(documents_original)<-c("doc_id","token")
  
  # documents_original<-unlist(lapply(X = unique(doc_ids),FUN = function(x){
  #  paste(db_data$token[which(doc_ids==x),5],collapse=" ")
  # }))
  # documents_original<-cbind(unique(doc_ids),documents_original)
  # 
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  
  
  
  #load dictionary
  log_to_file(message = "<b>Step 6/16: Try loading dictionary</b>",file = logfile)
  try({
    if(parameters$use_dictionary==TRUE){
      load(paste0("collections/dictionaries/",parameters$Dictioanry,".RData"))
      log_to_file(message = "  <b style='color:green'> ✔ </b>  Dictionary loaded",file = logfile)
    }
    else{
      dict<-""
      log_to_file(message = "  <b style='color:green'> ✔ </b>  Not using a dictionary in this task",file = logfile)
    }
  })
  
  
  
  
  #load annotation set according to chosen project
  log_to_file(message = "<b>Step 7/16: Loading annotation set</b>",file = logfile)
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
  log_to_file(message = "<b>Step 8/16: Preparing token object</b>",file = logfile)
  datasets<-db_data$token[,1]
  db_data$token<-prepare_token_object(token = db_data$token)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing token object",file = logfile)
  
  
  
  #getting original documents
  log_to_file(message = "<b>Step 9/16: Getting original documents/sentences</b>",file = logfile)
  original_text<-get_original_documents()
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished building original documents/sentences",file = logfile)
  
  
  
  
  #formatting classification input as a tibble
  log_to_file(message = "<b>Step 10/16: Formatting classification input</b>",file = logfile)
  #feature_list<-calculate_dtm(tibble=T)
  dtm<-calculate_dtm(tibble=F)
  #get_annotated_doc_ids
  log_to_file(message = paste0("&emsp; Reduce feature space"),logfile)
  pos_ident<-NULL
  #anno_pos<-annotations[which(annotations[,"Annotation"]==parameters$cl_Category),]
  anno_pos<-annotations
  if(parameters$cooc_window=="Sentence"){
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
    for(i in 1:dim(anno_pos)[1]){
      id_ws<-paste(anno_pos[i,"from"]:anno_pos[i,"to"],collapse = ", ")
      identifier<-paste(anno_pos[i,"dataset"],anno_pos[i,"id"],unique(RMariaDB::dbGetQuery(mydb, paste("select sid from token where dataset='",anno_pos[i,"dataset"],"' and id= ",anno_pos[i,"id"],";",sep=""))[anno_pos[i,"from"]:anno_pos[i,"to"],1]),sep="_")
      pos_ident<-c(pos_ident,identifier)
    }
    RMariaDB::dbDisconnect(mydb)
    #anno_appr_pos<-classifications_approved[intersect(which(classifications_approved[,"category"]==parameters$cl_Category),which(classifications_approved[,"status"]=="approved")),]
    #anno_appr_pos<-rbind(anno_appr_pos,classifications_approved[which(classifications_approved[,"status"]==paste0("denied_",parameters$cl_Category)),])
    ign<-which(classifications_approved[,"status"]=="ignored")
    if(length(ign)>0){
      anno_appr<-classifications_approved[-ign,]
    }
    else{
      anno_appr<-classifications_approved
    }
    for(i in 1:dim(anno_appr)[1]){
      identifier<-paste(tolower(anno_appr[i,1]),anno_appr[i,2],anno_appr[i,3],sep="_")
      pos_ident<-c(pos_ident,identifier)
    }
    
    #reduce dtm by "ignored" examples
    ignored<-classifications_approved[ign,]
    ignored_ids<-NULL
    if(dim(ignored)[1]>0){
      for(i in 1:dim(ignored)[1]){
        identifier<-paste(tolower(ignored[i,1]),ignored[i,2],ignored[i,3],sep="_")
        ignored_ids<-c(ignored_ids,identifier)
      }
      ignored_ids<-which(rownames(dtm)%in%ignored_ids)
      if(length(ignored_ids)>0){
        dtm<-dtm[-ignored_ids,]
      }
    }
    #pos_ident<-pos_ident[which(pos_ident%in%rownames(dtm))]
    #x<-dtm[pos_ident,]
    #features<-which(colSums(x)>0)
    #dtm<-dtm[,features]
  }
  if(parameters$cooc_window=="Document"){
    for(i in 1:dim(anno_pos)[1]){
      identifier<-paste(anno_pos[i,"dataset"],anno_pos[i,"id"],sep="_")
      pos_ident<-c(pos_ident,identifier)
    }
    #anno_appr_pos<-classifications_approved[intersect(which(classifications_approved[,"category"]==parameters$cl_Category),which(classifications_approved[,"status"]=="approved")),]
    #anno_appr_pos<-rbind(anno_appr_pos,classifications_approved[which(classifications_approved[,"status"]==paste0("denied_",parameters$cl_Category)),])
    ign<-which(classifications_approved[,"status"]=="ignored")
    if(length(ign)>0){
      anno_appr<-classifications_approved[-ign,]
    }
    else{
      anno_appr<-classifications_approved
    }
    if(dim(anno_appr)[1]>0){
      for(i in 1:dim(anno_appr)[1]){
        identifier<-paste(tolower(anno_appr[i,1]),anno_appr[i,2],sep="_")
        pos_ident<-c(pos_ident,identifier)
      }
    }
    #reduce dtm by "ignored" examples
    ignored<-classifications_approved[ign,]
    ignored_ids<-NULL
    if(dim(ignored)[1]>0){
      for(i in 1:dim(ignored)[1]){
        identifier<-paste(tolower(ignored[i,1]),ignored[i,2],sep="_")
        ignored_ids<-c(ignored_ids,identifier)
      }
    }
    ignored_ids<-which(rownames(dtm)%in%ignored_ids)
    if(length(ignored_ids)>0){
      dtm<-dtm[-ignored_ids,]
    }
    #pos_ident<-pos_ident[which(pos_ident%in%rownames(dtm))]
    #x<-dtm[pos_ident,]
    #features<-which(colSums(x)>0)
    #dtm<-dtm[,features]
  }
 
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished formatting classification input",file = logfile)
  
  
  
  
  #init classifier
  log_to_file(message = "<b>Step 11/16: Initiating classifier</b>",file = logfile)
  #  classifier <- cmodel$new(method = "LiblineaR::LiblineaR")
  #  classifier$.__enclos_env__$private$silent = T
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Initiating classifier",file = logfile)
  
  
  
  
  #insert made annotations and approved classifications
  log_to_file(message = "<b>Step 12/16: Inserting made annotations and approved classifications as training input</b>",file = logfile)
  gold_table<-matrix(c(0),0,4)
  if(parameters$cooc_window=="Sentence"){
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
    count=0
    try({
      for(i in 1:dim(annotations)[1]){
        id_ws<-paste(annotations[i,"from"]:annotations[i,"to"],collapse = ", ")
        identifier<-paste(annotations[i,"dataset"],annotations[i,"id"],unique(RMariaDB::dbGetQuery(mydb, paste("select sid from token where dataset='",annotations[i,"dataset"],"' and id= ",annotations[i,"id"],";",sep=""))[annotations[i,"from"]:annotations[i,"to"],1]),sep="_")
        try({
          # my_gold$gold[which(ref_id_cut%in%unlist(identifier))]<-list(data.table(class=as.character(annotations[i,"Annotation"]),coder=as.character(annotations[i,"User"]),timestamp=as.character(annotations[i,"Annotation_Date"])))
          count=count+1
          gold_table<-rbind(gold_table,cbind(identifier,as.character(annotations[i,"Annotation"]),as.character(annotations[i,"User"]),as.character(annotations[i,"Annotation_Date"])))
        })
      }
      #for approved classifications
      if(length(ign)>1){
        class_appr<-classifications_approved[-ign,]
      }
      else{
        class_appr<-classifications_approved
      }
      for(i in 1:dim(class_appr)[1]){
        identifier<-paste(tolower(class_appr[i,1]),class_appr[i,2],class_appr[i,3],sep="_")
        try({
          if(as.character(class_appr[i,"status"])!="denied_"){
            if(grepl("denied_",as.character(class_appr[i,"status"]))){
              if(grepl("NEG",as.character(class_appr[i,"status"]))){
                #  my_gold$gold[which(ref_id_cut%in%unlist(identifier))]<-list(data.table(class="NEG",coder="classified as NEG during active learning",timestamp=as.character(class_appr[i,"timestamp"])))
                count=count+1
                gold_table<-rbind(gold_table,cbind(identifier,"NEG","classified as NEG during active learning",as.character(class_appr[i,"timestamp"])))
              }
              else{
                category<-as.character(stringr::str_split(string = as.character(class_appr[i,"status"]),pattern = "_",simplify = T)[1,2])
                # my_gold$gold[which(ref_id_cut%in%unlist(identifier))]<-list(data.table(class=category,coder="marked as opposite class during active learning",timestamp=as.character(class_appr[i,"timestamp"])))
                count=count+1
                gold_table<-rbind(gold_table,cbind(identifier,category,"marked as opposite class during active learning",as.character(class_appr[i,"timestamp"])))
              }
            }
            else{
              # my_gold$gold[which(ref_id_cut%in%unlist(identifier))]<-list(data.table(class=as.character(class_appr[i,"category"]),coder="approved classification",timestamp=as.character(class_appr[i,"timestamp"])))
              count=count+1
              gold_table<-rbind(gold_table,cbind(identifier,as.character(class_appr[i,"category"]),"approved classification",as.character(class_appr[i,"timestamp"])))
            }
          }
        })
      }
    })
    RMariaDB::dbDisconnect(mydb)
  }
  if(parameters$cooc_window=="Document"){
    count=0
    try({
      for(i in 1:dim(annotations)[1]){
        identifier<-paste(anno_pos[i,"dataset"],anno_pos[i,"id"],sep="_")
        try({
          #  my_gold$gold[which(ref_id_cut%in%unlist(identifier))]<-list(data.table(class=as.character(annotations[i,"Annotation"]),coder=as.character(annotations[i,"User"]),timestamp=as.character(annotations[i,"Annotation_Date"])))
          count=count+1
          gold_table<-rbind(gold_table,c(identifier,as.character(annotations[i,"Annotation"]),as.character(annotations[i,"User"]),as.character(annotations[i,"Annotation_Date"])))
        })
      }
      #for approved classifications
      if(length(ign)>1){
        class_appr<-classifications_approved[-ign,]
      }
      else{
        class_appr<-classifications_approved
      }
      for(i in 1:dim(class_appr)[1]){
        identifier<-paste(tolower(class_appr[i,1]),class_appr[i,2],sep="_")
        try({
          if(as.character(class_appr[i,"status"])!="denied_"){
            if(grepl("denied_",as.character(class_appr[i,"status"]))){
              if(grepl("NEG",as.character(class_appr[i,"status"]))){
                #    my_gold$gold[which(ref_id_cut%in%unlist(identifier))]<-list(data.table(class="NEG",coder="classified as NEG during active learning",timestamp=as.character(class_appr[i,"timestamp"])))
                count=count+1
                gold_table<-rbind(gold_table,c(identifier,"NEG","classified as NEG during active learning",as.character(class_appr[i,"timestamp"])))
              }
              else{
                category<-as.character(stringr::str_split(string = as.character(class_appr[i,"status"]),pattern = "_",simplify = T)[1,2])
                # my_gold$gold[which(ref_id_cut%in%unlist(identifier))]<-list(data.table(class=category,coder="marked as opposite class during active learning",timestamp=as.character(class_appr[i,"timestamp"])))
                count=count+1
                gold_table<-rbind(gold_table,c(identifier,category,"marked as opposite class during active learning",as.character(class_appr[i,"timestamp"])))
              }
            }
            else{
              #  my_gold$gold[which(ref_id_cut%in%unlist(identifier))]<-list(data.table(class=as.character(class_appr[i,"category"]),coder="approved classification",timestamp=as.character(class_appr[i,"timestamp"])))
              count=count+1
              gold_table<-rbind(gold_table,c(identifier,as.character(class_appr[i,"category"]),"approved classification",as.character(class_appr[i,"timestamp"])))
            }
          }
        })
      }
    })
    
  }
  
  
  
  if(count<1 && parameters$use_dictionary==F){
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No annotation was found for your specified collection and annotation set.
              Also you chose not to use a dictionary. The system can not calculate classifications without any training examples generated by annotations or using a dictionary.</b>",logfile)
    stop("No Training data")
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  #set input of classifier
  log_to_file(message = "<b>Step 13/16: Starting up classifier with training data</b>",file = logfile)
  #classifier$set_input(
  #  corpus = data.table(ref_id = original_text$doc_id[original_text$doc_id %in% ref_id_cut], text = original_text$token[original_text$doc_id %in% ref_id_cut]),
  #  features = feature_list[feature_list$ref_id %in% ref_id_cut, ],
  #  gold = my_gold
  #)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  
  
  
  
  
  #######learning examples#########################################################################
  if(parameters$cl_Mode=="Produce 50 new active learning examples"){
    log_to_file(message = "<b>Step 14/16: Producing 50 new active learning examples</b>",file = logfile)
    log_to_file(message = paste("&emsp;",count, "Training sets (annotations and approved classifications) were found and used"),file = logfile)
    
    if(parameters$use_dictionary==TRUE){
      log_to_file(message = "&emsp; Dictionary lookup",file = logfile)
      training_dict=NULL
      for(entry in names(dict))
      {
        Short_dict <- list()
        Short_dict[[entry]] <- dict[[entry]]
        Short_dict <- quanteda::dictionary(Short_dict)
        training_dict <- rbind(training_dict,cbind(unique(quanteda::kwic(quanteda::corpus(documents_original$token,docnames=documents_original$doc_id),pattern = Short_dict,window = 5,valuetype="regex")$docname),entry,"dictionary lookup",as.character(Sys.time())))
      }
      
      already_known<-which(training_dict[,1]%in%gold_table[,1])
      if(length(already_known)>0){
        training_dict<-training_dict[-already_known,]
      }
      gold_table<-rbind(gold_table,training_dict)
      log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
    }
    
    log_to_file(message = "&emsp; Training Classifier",file = logfile)
   # gold_table[which(gold_table[,2]!=parameters$cl_Category),2]<-"NEG"
    idx<-which(gold_table[,1]%in%rownames(dtm))
    selector_idx<-gold_table[idx,1]
    #reduce feature space to features of gold documents
    gold_dtm<-dtm[selector_idx,]
    features<-which(colSums(gold_dtm)>0)
    dtm<-dtm[,features]
    trainingDTM <-convertMatrixToSparseM(quanteda::as.dfm(dtm[selector_idx, ]))
    trainingLabels <- gold_table[idx,2]
    model <- LiblineaR(trainingDTM, trainingLabels)
    test_selector_idx<-setdiff(selector_idx,gold_table[which(gold_table[,3]=="dictionary lookup"),1])
    testDTM<-convertMatrixToSparseM(quanteda::as.dfm(dtm[-which(rownames(dtm)%in%test_selector_idx),]))
    predicted <- predict(model, testDTM,proba = T) 
    log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
    
    log_to_file(message = "&emsp; Cross Validation",file = logfile)
    cParameterValues <- c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)
    result <- NULL
    for (cParameter in cParameterValues) {
      print(paste0("C = ", cParameter))
      evalMeasures <- k_fold_cross_validation(trainingDTM, trainingLabels, cost = cParameter,k = 10)
      result <- c(result, evalMeasures["F"])
    }
    log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
    
    log_to_file(message = "&emsp; Choose active learning examples",file = logfile)
    if (parameters$cl_active_learning_strategy == "LC") {
      boundary_distances <- abs(predicted$probabilities[,parameters$cl_Category] - 0.5)
      uncertain_decisions <- order(boundary_distances)[1:50]
      #unset_labels <- which(!rownames(dtm)%in%gold_table[,1])
      #uncertain_decisions <- intersect(uncertain_decisions,unset_labels)[1:50]
      examples <- rownames(dtm[-which(rownames(dtm)%in%test_selector_idx),])[uncertain_decisions]
    }
    if (parameters$cl_active_learning_strategy == "MC") {
      boundary_distances <- abs(predicted$probabilities[,parameters$cl_Category] - 1)
      uncertain_decisions <- order(boundary_distances)[1:50]
      #unset_labels <- which(!rownames(dtm)%in%gold_table[,1])
      #uncertain_decisions <- intersect(uncertain_decisions,unset_labels)[1:50]
      examples <- rownames(dtm[-which(rownames(dtm)%in%test_selector_idx),])[uncertain_decisions]
    }
    if (parameters$cl_active_learning_strategy == "LCB") {
      pp <- length(which(gold_table[idx,2] == parameters$cl_Category)) / dim(gold_table)[1]
      pmax <- mean(c(0.5, 1 - pp))
      prob_positive <- predicted$probabilities[, parameters$cl_Category]
      lidx <- prob_positive < pmax
      uncertain_decisions <- rep(0, length(predicted$predicted))
      uncertain_decisions[lidx] <- prob_positive[lidx] / pmax
      uncertain_decisions[!lidx] <- (1 - prob_positive[!lidx]) / (1 - pmax)
      uncertain_decisions <- order(boundary_distances,decreasing=T)[1:50]
      #unset_labels <- which(!rownames(dtm)%in%gold_table[,1])
      #uncertain_decisions <- intersect(uncertain_decisions,unset_labels)[1:50]
      examples <- rownames(dtm[-which(rownames(dtm)%in%test_selector_idx),])[uncertain_decisions]
    }
    log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
    
    log_to_file(message = "&emsp; Extraction of most distinctive features",file = logfile)
    feature_matrix<-model$W
    colnames(feature_matrix)[1:(ncol(feature_matrix)-1)]<-colnames(dtm)
    
    log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
    
    
    data<-documents_original[which(documents_original[,1]%in%examples),]
    data<-cbind(data,approved=FALSE,denied=FALSE,ignored=FALSE,other=" ")
    data<-data.frame(data,stringsAsFactors = F)
    colnames(data)<-c("doc_id_global","token","approved","denied","ignored","other")
    learning_meta<-list()
    learning_meta[["color"]]<-colors[which(names==parameters$cl_Category)]
    learning_meta[["category"]]<-parameters$cl_Category
    learning_meta[["collection"]]<-parameters$collection
    learning_meta[["date"]]<-Sys.time()
    learning_meta[["log"]]<-list(precision=runif(1,0,1),recall=runif(1,0,1))
    learning_meta[["project"]]<-parameters$Project
    learning_meta[["strategy"]]<-parameters$cl_active_learning_strategy
    learning_meta[["context_unit"]]<-parameters$cooc_window
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished producing 50 new active learning examples ",file = logfile)
    
    log_to_file(message = "<b>Step 15/16: Saving results</b>",file = logfile)
    dir.create(path = path0,recursive = T)
    save(learning_meta,data,result,file=paste0(path0,"training_examples.RData"))
    save(parameters,file=paste0(path0,"parameters.RData"))
    save(info,file=paste0(path0,"info.RData"))
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  }
  if(parameters$cl_Mode=="Evaluate Training Set"){
    log_to_file(message = "<b>Step 14/16: Evaluating Training Set</b>",file = logfile)
    result<-classifier$train_and_evaluate_cross(k = min(10,ceiling(count/2)))
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
    
    
    log_to_file(message = "<b>Step 15/16: Saving results</b>",file = logfile)
    dir.create(path = path0,recursive = T)
    save(result,file=paste0(path0,"result.RData"))
    save(parameters,file=paste0(path0,"parameters.RData"))
    save(info,file=paste0(path0,"info.RData"))
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  }
  if(parameters$cl_Mode=="Active learning on whole documents"){
    log_to_file(message = "<b>Step 14/16: Active learning on whole documents</b>",file = logfile)
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
    
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
    
    
    log_to_file(message = "<b>Step 15/16: Saving results</b>",file = logfile)
    dir.create(path = path0,recursive = T)
    project<-parameters$Project
    save(texts,labels,project,file=paste0(path0,"examples.RData"))
    save(parameters,file=paste0(path0,"parameters.RData"))
    save(info,file=paste0(path0,"info.RData"))
    RMariaDB::dbDisconnect(mydb)
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  }
  if(parameters$cl_Mode=="Classify on entire collection"){
    log_to_file(message = "<b>Step 14/16: Classification on entire collection</b>",file = logfile)
    classifier$train()
    ref_id_unclassified<-setdiff(ref_id_cut,unique(ref_id_cut[which(!is.na(my_gold$gold))]))
    labels<-classifier$predict(ref_id = ref_id_unclassified,save_out=TRUE)
    log_to_file(message = paste0("&emsp;",length(labels$predictions)," predictions were made"),logfile)
    log_to_file(message = paste0("&emsp; collecting dates for made classifications..."),logfile)
    predictions<-labels$predictions
    names_predictions<-names(predictions)
    predictions<-as.character(levels(predictions)[predictions])
    names(predictions)<-names_predictions
    
    probabilities<-apply(X = labels$probabilities,MARGIN = 1,FUN = max)
    #add handmade annotations/classifcations
    names_gold<-my_gold[[1]][which(!is.na(my_gold$gold))]
    class_gold<-as.character(do.call(rbind,my_gold[[2]][which(!is.na(my_gold$gold))])$class)
    names(class_gold)<-names_gold
    predictions<-append(predictions,class_gold)
    
    class_gold<-rep(1,length(names_gold))
    names(class_gold)<-names_gold
    probabilities<-c(probabilities,class_gold)
    
    keep<- intersect(which(probabilities>parameters$cl_positive_Threshold),which(predictions!="NEG"))
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
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
      for(i in classes){
        ident<-stringr::str_split(string = names(predictions[which(predictions==i)]),pattern = "_",simplify = T)[,1:2,drop=F]
        date<-lapply(1:dim(ident)[1],FUN = function(x){
          as.character(as.Date(RMariaDB::dbGetQuery(conn = mydb,statement = paste0("Select date from documents where dataset='",ident[x,1],"' and id_doc=",ident[x,2],";"))[1,1]))
        })
        dates[[as.character(i)]]<-unlist(date)
      }
      RMariaDB::dbDisconnect(mydb)
      
      # original_text<-original_text[keep,]
      # original_text<-cbind(cbind(as.character(predictions),probabilities),original_text)
      
      orig<-documents_original[names(predictions),]
      original_text<-cbind(cbind(as.character(predictions),probabilities),orig)
    }
    
    result<-classifier$train_and_evaluate_cross(k = min(10,ceiling(count/2)))
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
    
    
    log_to_file(message = "<b>Step 15/16: Saving results</b>",file = logfile)
    dir.create(path = path0,recursive = T)
    save(dates,predictions,labels,result,file=paste0(path0,"result.RData"))
    save(original_text,file=paste0(path0,"texts.RData"))
    save(parameters,file=paste0(path0,"parameters.RData"))
    save(info,file=paste0(path0,"info.RData"))
    log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  }
  
  
  #Wrinting metadata to database Task column
  log_to_file(message = "<b>Step 16/16: Writing task parameter to database</b>",file = logfile)
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

