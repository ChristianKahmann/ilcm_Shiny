## functions for classification tasks

#' check for document/sentece level annotations
check_annotations<-function(parameters, annotations, classifications_approved){
  if(parameters$cooc_window=="Document"){
    annotations<-annotations[which(annotations$document_annotation=="TRUE"),]
    classifications_approved<-classifications_approved[which(classifications_approved$document_annotation=="TRUE"),]
  }
  if(parameters$cooc_window=="Sentence"){
    annotations<-annotations[which(annotations$document_annotation=="FALSE"),]
    classifications_approved<-classifications_approved[which(classifications_approved$document_annotation=="FALSE"),]
  }
  return(list(annotations=annotations, classifications_approved = classifications_approved))
}

#' annotations for documents in collection?
annotation_in_collection<-function(anno_ids,db_data, parameters){
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
  return(anno_ids_in_token)
}

#' get original documents
get_og_doc<-function(parameters, db_data){
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
  return(documents_original)
}

#' formatting classification input as tibble (sentence)
formatting_sentence<-function(dtm, anno_pos, pos_ident, classifications_approved){
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
  return(list(dtm=dtm,ign = ign))
}

#' formatting classification input as tibble (document)
formatting_document<-function(dtm, anno_pos, pos_ident, classifications_approved){
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
  return(list(dtm=dtm,ign = ign))
}

#' insert made annotations and approved classifications
check_classification_sentence<-function(class_appr, gold_table){
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
  return(list(gold_table = gold_table, count = count))
}
#'insert made annotations and approved classifications
check_classification_document<-function(class_appr, gold_table, count){
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
  return(list(gold_table = gold_table, count = count))
}
############################################
#           learning example               #
############################################
#' create 50 active learning sample for choosen class
#' @param parameters (list of set parameters from task scheduler)
#' @param gold_table (assigns classes to already annotated documents)
#' @param dtm (current document term matrix)
set_learning_samples_svm<-function(parameters, gold_table, dtm){
  if(parameters$use_dictionary==TRUE){
    log_to_file(message = "&emsp; Dictionary lookup",file = logfile)
    training_dict=NULL
    for(entry in names(dict))
    {
      Short_dict <- list()
      Short_dict[[entry]] <- dict[[entry]]
      Short_dict <- quanteda::dictionary(Short_dict)
      training_dict <- rbind(training_dict,cbind(unique(quanteda::kwic(quanteda::corpus(documents_original$token,docnames=documents_original$doc_id),pattern = Short_dict,window = 5)$docname),entry,"dictionary lookup",as.character(Sys.time())))
    }
    
    already_known<-which(training_dict[,1]%in%gold_table[,1])
    if(length(already_known)>0){
      training_dict<-training_dict[-already_known,]
    }
    gold_table<-rbind(gold_table,training_dict)
    log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
  }
  
  log_to_file(message = "&emsp; Training Classifier",file = logfile)
  gold_table[which(gold_table[,2]!=parameters$cl_Category),2]<-"NEG"
  #check if enough NEG data avaiable
  tmp_labels <- table(gold_table[,2])
  ratio <- 0
  more_examples <-  0 - sum(tmp_labels)
  if(!is.na(tmp_labels[parameters$cl_Category])){
    ratio <- tmp_labels[parameters$cl_Category]/sum(tmp_labels)
    more_examples <- 2*tmp_labels[parameters$cl_Category] - sum(tmp_labels)
  }
  if(ratio > 0.5){
    gold_table <- rbind(gold_table, cbind(sample(setdiff(rownames(dtm),gold_table[,1]),more_examples,replace = F),"NEG","sampled negative examples",as.character(Sys.time())))
  }
  idx<-which(gold_table[,1]%in%rownames(dtm))
  selector_idx<-gold_table[idx,1]
  #reduce feature space to features of gold documents
  gold_dtm<-dtm[selector_idx,]
  features<-which(colSums(gold_dtm)>0)
  dtm<-dtm[,features]
  dtm<-dtm[,order(colnames(dtm))]
  trainingDTM <-convertMatrixToSparseM(quanteda::as.dfm(dtm[selector_idx, ]))
  trainingLabels <- gold_table[idx,2]
  names(trainingLabels)<-gold_table[idx,1]
  c_weights <- table(trainingLabels) / length(trainingLabels)
  c_weights <- abs(c_weights - 1) 
  #start.time <- Sys.time()
  model <- LiblineaR(trainingDTM, trainingLabels,wi=c_weights,cost = parameters$cl_c,epsilon = 0.01,bias = 1)
  #print(head(model))
  #end.time <- Sys.time()
  #time_model <- end.time - start.time
  #print("model time:")
  #print(time_model)
  testDTM<-convertMatrixToSparseM(quanteda::as.dfm(dtm))
  #start.time <- Sys.time()
  predicted <- predict(model, testDTM,proba = T) 
  #end.time <- Sys.time()
  #time_pred <- end.time - start.time
  #print("prediction time:")
  #print(time_pred)
  log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
  
  log_to_file(message = "&emsp; Cross Validation",file = logfile)
  cParameterValues <- c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)
  result=NULL
  results_complete<-list()
  count=0
  for (cParameter in cParameterValues) {
    count=count+1
    print(paste0("C = ", cParameter))
    #if enough training data available use k=10, else min number of trainign samples
    evalMeasures <- k_fold_cross_validation(trainingDTM, trainingLabels, cost = cParameter,k = min(10,dim(trainingDTM)[1]))
    print(evalMeasures$means)
    result <- c(result, evalMeasures$means["F"])
    results_complete[[count]]<-evalMeasures$complete
  }
  log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
  
  log_to_file(message = "&emsp; Choose active learning examples",file = logfile)
  if (parameters$cl_active_learning_strategy == "LC") {
    
    boundary_distances <- abs(predicted$probabilities[,parameters$cl_Category] - 0.5)
    uncertain_decisions <- order(boundary_distances)
    unset_labels <- which(!rownames(dtm)%in%gold_table[which(!gold_table[,2]%in%c("dictionary lookup","sampled negative examples")),1])
    uncertain_decisions <- intersect(uncertain_decisions,unset_labels)[1:50]
    examples <- rownames(dtm)[uncertain_decisions]
  }
  if (parameters$cl_active_learning_strategy == "MC") {
    boundary_distances <- abs(predicted$probabilities[,parameters$cl_Category] - 1)
    certain_decisions <- order(boundary_distances)
    unset_labels <- which(!rownames(dtm)%in%gold_table[which(!gold_table[,2]%in%c("dictionary lookup","sampled negative examples")),1])
    certain_decisions <- intersect(certain_decisions,unset_labels)[1:50]
    examples <- rownames(dtm)[certain_decisions]
  }
  if (parameters$cl_active_learning_strategy == "LCB") {
    pp <- length(which(gold_table[idx,2] == parameters$cl_Category)) / dim(gold_table)[1]
    pmax <- mean(c(0.5, 1 - pp))
    prob_positive <- predicted$probabilities[, parameters$cl_Category]
    lidx <- prob_positive < pmax
    uncertain_decisions <- rep(0, length(predicted$predicted))
    uncertain_decisions[lidx] <- prob_positive[lidx] / pmax
    uncertain_decisions[!lidx] <- (1 - prob_positive[!lidx]) / (1 - pmax)
    uncertain_decisions <- order(uncertain_decisions,decreasing=T)
    unset_labels <- which(!rownames(dtm)%in%gold_table[which(!gold_table[,2]%in%c("dictionary lookup","sampled negative examples")),1])
    uncertain_decisions <- intersect(uncertain_decisions,unset_labels)[1:50]
    examples <- rownames(dtm)[uncertain_decisions]
  }
  log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
  
  log_to_file(message = "&emsp; Extraction of most distinctive features",file = logfile)
  feature_matrix<-model$W
 
  colnames(feature_matrix)[1:(ncol(feature_matrix)-1)]<-colnames(dtm)
  #delete bias term from feature matrix
  feature_matrix<-feature_matrix[,-ncol(feature_matrix),drop=F]
  
  word_counts<-colSums(dtm) 
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
  
  log_to_file(message = "<b>Step 13/13: Saving results</b>",file = logfile)
  dir.create(path = path0,recursive = T)
  saveRDS(features,file=paste0(path0,"vocab_task",parameters$id,".RDS"))
  write(paste(features,collapse=","),file = paste0(path0,"vocab_task",parameters$id,".txt"))
  save(learning_meta,data,result,file=paste0(path0,"training_examples.RData"))
  save(results_complete,file = paste0(path0,"results_complete.RData"))
  save(feature_matrix,word_counts,file=paste0(path0,"feature_matrix.RData"))
  save(parameters,file=paste0(path0,"parameters.RData"))
  save(info,file=paste0(path0,"info.RData"))
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
}

############################################
#           Training Set Evaluation        #
############################################
#' evaluate the trainings set
#' @param parameters (list of set parameters from task scheduler)
#' @param gold_table (assigns classes to already annotated documents)
#' @param dtm (current document term matrix)
set_training_eval<-function(parameters, gold_table, dtm){
  idx<-which(gold_table[,1]%in%rownames(dtm))
  selector_idx<-gold_table[idx,1]
  #reduce feature space to features of gold documents
  gold_dtm<-dtm[selector_idx,]
  features<-which(colSums(gold_dtm)>0)
  dtm<-dtm[,features]
  dtm<-dtm[,order(colnames(dtm))]
  trainingDTM <-convertMatrixToSparseM(quanteda::as.dfm(dtm[selector_idx, ]))
  trainingLabels <- gold_table[idx,2]
  names(trainingLabels)<-gold_table[idx,1]
  
  cParameterValues <- c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)
  result <- NULL
  results_complete<-list()
  count=0
  #start.time <- Sys.time()
  for (cParameter in cParameterValues) {
    count=count+1
    print(paste0("C = ", cParameter))
    #if enough training data available use k=10, else min number of trainign samples
    evalMeasures <- k_fold_cross_validation(trainingDTM, trainingLabels, cost = cParameter,k = min(10,dim(trainingDTM)[1]))
    print(evalMeasures$means)
    result <- c(result, evalMeasures$means["F"])
    results_complete[[count]]<-evalMeasures$complete
  }
  #end.time <- Sys.time()
  #time_vali <- end.time - start.time
  #print("k-fold validation time:")
  #print(time_vali)
  
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  log_to_file(message = "<b>Step 13/13: Saving results</b>",file = logfile)
  dir.create(path = path0,recursive = T)
  save(result,file=paste0(path0,"result.RData"))
  saveRDS(features,file=paste0(path0,"vocab_task",parameters$id,".RDS"))
  write(paste(features,collapse=","),file = paste0(path0,"vocab_task",parameters$id,".txt"))
  save(results_complete,file = paste0(path0,"results_complete.RData"))
  save(parameters,file=paste0(path0,"parameters.RData"))
  save(info,file=paste0(path0,"info.RData"))
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
}

############################################
#           learning whole                 #
############################################
#' active learning on whole document (when context unit "sentence" is selected)
#' @param parameters (list of set parameters from task scheduler)
#' @param gold_table (assigns classes to already annotated documents)
#' @param dtm (current document term matrix)
set_active_learning_whole_svm<-function(parameters, gold_table, dtm){
  if(length(unique(gold_table[,2]))==1){
    gold_table <- rbind(gold_table, cbind(sample(setdiff(rownames(dtm),gold_table[,1]),dim(gold_table)[1],replace = F),"NEG","sampled negative examples",as.character(Sys.time())))
  }
  idx<-which(gold_table[,1]%in%rownames(dtm))
  selector_idx<-gold_table[idx,1]
  #reduce feature space to features of gold documents
  gold_dtm<-dtm[selector_idx,]
  features<-which(colSums(gold_dtm)>0)
  dtm<-dtm[,features]
  dtm<-dtm[,order(colnames(dtm))]
  trainingDTM <-convertMatrixToSparseM(quanteda::as.dfm(dtm[selector_idx, ]))
  trainingLabels <- gold_table[idx,2]
  names(trainingLabels)<-gold_table[idx,1]
  c_weights <- table(trainingLabels) / length(trainingLabels)
  c_weights <- abs(c_weights - 1) 
  #start.time <- Sys.time()
  model <- LiblineaR(trainingDTM, trainingLabels,wi=c_weights,cost = parameters$cl_c,epsilon = 0.01,bias = 1)
  #end.time <- Sys.time()
  #time_model <- end.time - start.time
  #print("model time:")
  #print(time_model)
  #sample documents
  sentence_ids<-setdiff(rownames(dtm),gold_table[,1])
  known_docs<-unique(stringr::str_extract(string = gold_table[,1],pattern = ".+?_[0-9]+"))
  doc_ids<-setdiff(unique(stringr::str_extract(string = sentence_ids,pattern = ".+?_[0-9]+")),known_docs)
  if(length(doc_ids)==1){
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No untagged document was found</b>",logfile)
    stop("No untagged document left")
  }
  random_sample<-sample(doc_ids,min(10,length(doc_ids)))
  #extending doc_ids to sentence_ids
  random_sample_sentences<-setdiff(rownames(dtm),gold_table[,1])[which(stringr::str_replace(string = setdiff(rownames(dtm),gold_table[,1]),pattern = "_[0-9]+$",replacement = "")%in%random_sample)]
  
  testDTM<-convertMatrixToSparseM(quanteda::as.dfm(dtm[random_sample_sentences,]))
  #start.time <- Sys.time()
  labels <- predict(model, testDTM,proba = T) 
  #end.time <- Sys.time()
  #time_pred <- end.time - start.time
  #print("prediction time:")
#  print(time_pred)
  names(labels$predictions)<-random_sample_sentences
  rownames(labels$probabilities)<-random_sample_sentences
  
  #remove prediction of class "NEG"
  NEG_predictions<-which(labels$predictions=="NEG")
  if(length(NEG_predictions)>0){
    labels$predictions<-labels$predictions[-NEG_predictions]
    labels$probabilities<-labels$probabilities[-NEG_predictions,]
  }
  #check whether enough predictions have been made
  if(length(labels$predictions)<1){
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No prediction on a target class was found</b>",logfile)
    stop("No predictions left")
  }
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
  
  log_to_file(message = "<b>Step 13/13: Saving results</b>",file = logfile)
  dir.create(path = path0,recursive = T)
  project<-parameters$Project
  save(texts,labels,project,file=paste0(path0,"examples.RData"))
  saveRDS(features,file=paste0(path0,"vocab_task",parameters$id,".RDS"))
  write(paste(features,collapse=","),file = paste0(path0,"vocab_task",parameters$id,".txt"))
  save(parameters,file=paste0(path0,"parameters.RData"))
  save(info,file=paste0(path0,"info.RData"))
  RMariaDB::dbDisconnect(mydb)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
}

############################################
#       Classify on entire collection      #
############################################
#' classify on entire collection
#' @param parameters (list of set parameters from task scheduler)
#' @param gold_table (assigns classes to already annotated documents)
#' @param dtm (current document term matrix)
classify_whole_collection_svm<-function(parameters, gold_table, dtm){
  #use neg examples in training classifier and remove examples tagged as neg afterwards
  #gold_table<-gold_table[which(gold_table[,2]!="NEG"),]
  idx<-which(gold_table[,1]%in%rownames(dtm))
  selector_idx<-gold_table[idx,1]
  gold_dtm<-dtm[selector_idx,]
  features<-which(colSums(gold_dtm)>0)
  dtm<-dtm[,features]
  dtm<-dtm[,order(colnames(dtm))]
  trainingDTM <-convertMatrixToSparseM(quanteda::as.dfm(dtm[selector_idx, ]))
  trainingLabels <- gold_table[idx,2]
  names(trainingLabels)<-gold_table[idx,1]
  c_weights <- table(trainingLabels) / length(trainingLabels)
  c_weights <- abs(c_weights - 1) 
  #start.time <- Sys.time()
  model <- LiblineaR(trainingDTM, trainingLabels,wi=c_weights,cost = parameters$cl_c,epsilon = 0.01,bias = 1)
  #end.time <- Sys.time()
  #time_model <- end.time - start.time
  #print("model time:")
  #print(time_model)
  feature_matrix<-model$W
  colnames(feature_matrix)[1:(ncol(feature_matrix)-1)]<-colnames(dtm[selector_idx, ])
  # delete bias term from feature matrix
  feature_matrix<-feature_matrix[,-ncol(feature_matrix),drop=F]
  #print(head(feature_matrix))
  # if only 2 categories were used, transform feature matrix
  if(nrow(feature_matrix)==1){
    feature_matrix<-rbind(feature_matrix,(feature_matrix*-1))
    rownames(feature_matrix)<-setdiff(unique(gold_table[,2]),"NEG")
  }
  #print(head(feature_matrix))
  word_counts<-colSums(dtm)  
  
  testDTM<-convertMatrixToSparseM(quanteda::as.dfm(dtm))
  #start.time<- Sys.time()
  predicted <- predict(model, testDTM,proba = T) 
  #end.time <- Sys.time()
  #time_pred <- end.time - start.time
  #print("prediction time:")
  #print(time_pred)
  #predictions<-as.character(predicted$predictions)
  
  probabilities<-predicted$probabilities
  names(predictions)<-rownames(dtm)
  rownames(probabilities)<-rownames(dtm)
  probabilities<-apply(probabilities,1,max)
  
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
    
    orig<-documents_original[names(predictions),]
    original_text<-cbind(cbind(as.character(predictions),probabilities),orig)
  }
  log_to_file(message = "&emsp; Cross Validation",file = logfile)
  cParameterValues <- c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)
  result=NULL
  results_complete<-list()
  count=0
  for (cParameter in cParameterValues) {
    count=count+1
    print(paste0("C = ", cParameter))
    #if enough trainign data available use k=10, else min number of trainign samples
    evalMeasures <- k_fold_cross_validation(trainingDTM, trainingLabels, cost = cParameter,k = min(10,dim(trainingDTM)[1]))
    print(evalMeasures$means)
    result <- c(result, evalMeasures$means["F"])
    results_complete[[count]]<-evalMeasures$complete
  }
  log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
  
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
  
  
  log_to_file(message = "<b>Step 13/13: Saving results</b>",file = logfile)
  dir.create(path = path0,recursive = T)
  
  lang<-db_data$language
  save(dates,predictions,labels,probabilities,result,file=paste0(path0,"result.RData"))
  save(feature_matrix,word_counts,file=paste0(path0,"feature_matrix.RData"))
  saveRDS(features,file=paste0(path0,"vocab_task",parameters$id,".RDS"))
  write(paste(features,collapse=","),file = paste0(path0,"vocab_task",parameters$id,".txt"))
  save(results_complete,file = paste0(path0,"results_complete.RData"))
  save(original_text,file=paste0(path0,"texts.RData"))
  parameters<-parameters_original
  save(parameters,lang,file=paste0(path0,"parameters.RData"))
  save(info,file=paste0(path0,"info.RData"))
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished ",file = logfile)
}