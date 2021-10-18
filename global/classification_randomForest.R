# Problem:
#### 50 Beispiele werden nicht angezeigt 
#### feature matrix hat noch das falsche format
set_learning_samples_rF<-function(parameters, gold_table, dtm){
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
###########
  ## effizienter
  trainingDTM <-data.frame(as.matrix(dtm[selector_idx, ]),stringsAsFactors=False)
  trainingDTM$class <-gold_table[idx,2]
 
  set.seed(71)
  model <-randomForest(as.factor(class) ~ .,data =trainingDTM,importance=TRUE,
                       proximity=TRUE,type="classification")
  #print(head(model))
  testDTM<-data.frame(as.matrix(dtm))
  #testDTM<-convertMatrixToSparseM(quanteda::as.dfm(dtm))
  predicted <- predict(model, testDTM, type="prob") 
  print(head(predicted))
###########
  log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
  log_to_file(message = "&emsp; Cross Validation",file = logfile)
  cParameterValues <- c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)
  result=NULL
  results_complete<-list()
  count=0
  trainingDTM_og <-convertMatrixToSparseM(quanteda::as.dfm(dtm[selector_idx, ]))
  trainingLabels <- gold_table[idx,2]
  names(trainingLabels)<-gold_table[idx,1]
  for (cParameter in cParameterValues) {
    count=count+1
    print(paste0("C = ", cParameter))
    #if enough training data available use k=10, else min number of trainign samples
    evalMeasures <- k_fold_cross_validation(trainingDTM_og, trainingLabels, cost = cParameter,k = min(10,dim(trainingDTM_og)[1]))
    #print(evalMeasures$means)
    result <- c(result, evalMeasures$means["F"])
    results_complete[[count]]<-evalMeasures$complete
  }
  log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
  
  log_to_file(message = "&emsp; Choose active learning examples",file = logfile)
  if (parameters$cl_active_learning_strategy == "LC") {
    boundary_distances <- abs(predicted[,parameters$cl_Category] - 0.5)
    uncertain_decisions <- order(boundary_distances)
    unset_labels <- which(!rownames(dtm)%in%gold_table[which(!gold_table[,2]%in%c("dictionary lookup","sampled negative examples")),1])
    uncertain_decisions <- intersect(uncertain_decisions,unset_labels)[1:50]
    examples <- rownames(dtm)[uncertain_decisions]
  }
  if (parameters$cl_active_learning_strategy == "MC") {
    boundary_distances <- abs(predicted[,parameters$cl_Category] - 1)
    certain_decisions <- order(boundary_distances)
    unset_labels <- which(!rownames(dtm)%in%gold_table[which(!gold_table[,2]%in%c("dictionary lookup","sampled negative examples")),1])
    certain_decisions <- intersect(certain_decisions,unset_labels)[1:50]
    examples <- rownames(dtm)[certain_decisions]
  }
  if (parameters$cl_active_learning_strategy == "LCB") {
    pp <- length(which(gold_table[idx,2] == parameters$cl_Category)) / dim(gold_table)[1]
    pmax <- mean(c(0.5, 1 - pp))
    prob_positive <- predicted[, parameters$cl_Category]
    lidx <- prob_positive < pmax
    uncertain_decisions <- rep(0, length(predicted))
    uncertain_decisions[lidx] <- prob_positive[lidx] / pmax
    uncertain_decisions[!lidx] <- (1 - prob_positive[!lidx]) / (1 - pmax)
    uncertain_decisions <- order(uncertain_decisions,decreasing=T)
    unset_labels <- which(!rownames(dtm)%in%gold_table[which(!gold_table[,2]%in%c("dictionary lookup","sampled negative examples")),1])
    uncertain_decisions <- intersect(uncertain_decisions,unset_labels)[1:50]
    examples <- rownames(dtm)[uncertain_decisions]
  }
  
  log_to_file(message = "&emsp; Extraction of most distinctive features",file = logfile)
  ####
  feature_matrix<-varImpPlot(model, scale = TRUE)
  #colnames(feature_matrix)[1:(ncol(feature_matrix)-1)]<-colnames(dtm)
  ####
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

# Problem:
#### zeigt immer nur eine klasse und dadurch kann nicht klassifiziert werden
#### kann aber aktuell nicht auf die dokumente zugreifen um das zu validieren
set_active_learning_whole_rF<-function(parameters, gold_table, dtm){
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
  
###
  trainingDTM <-data.frame(as.matrix(dtm[selector_idx, ]),stringsAsFactors=False)
  trainingDTM$class <-gold_table[idx,2]
  print(unique(trainingDTM$class))
  set.seed(71)
  model <-randomForest(as.factor(class) ~ .,data =trainingDTM,importance=TRUE,
                       proximity=TRUE,type="classification")
###
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
  
  testDTM<-data.frame(as.matrix(dtm))
  #testDTM<-convertMatrixToSparseM(quanteda::as.dfm(dtm))
  labels <- predict(model, testDTM, type="prob") 
  
  colnames(labels)<-random_sample_sentences
  rownames(labels)<-random_sample_sentences
  
  #remove prediction of class "NEG"
  NEG_predictions<-which(colnames(labels)=="NEG")
  if(length(NEG_predictions)>0){
    labels<-labels[-NEG_predictions]
    
  }
  #check whether enough predictions have been made
  if(length(labels)<1){
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No prediction on a target class was found</b>",logfile)
    stop("No predictions left")
  }
  labels[["manual"]]<-rep(NULL,length(labels))
  log_to_file(message = paste0("&emsp;",length(labels)," predictions were made"),logfile)
  #prepare output
  #get texts
  log_to_file(message = "&emsp; Preparing output...",logfile)
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=host,port=db_port)
  texts<-data.frame(dataset=NULL,id_doc=NULL,sen_id=NULL,text=NULL,title=NULL,date=NULL,approved=NULL,denied=NULL,ignored=NULL,other=NULL,stringsAsFactors = F)
  for(i in 1:length(labels)){
    x<-stringr::str_split(string = rownames(labels)[i],pattern = "_",simplify = T)
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
  labels<-labels[ord]
  #labels$probabilities<-labels$probabilities[ord,]
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

# Problem:
### Feature Matrix hat das falsche Format
classify_whole_collection_rF<-function(parameters, gold_table, dtm){
  #use neg examples in training classifier and remove examples tagged as neg afterwards
  #gold_table<-gold_table[which(gold_table[,2]!="NEG"),]
  idx<-which(gold_table[,1]%in%rownames(dtm))
  selector_idx<-gold_table[idx,1]
  gold_dtm<-dtm[selector_idx,]
  features<-which(colSums(gold_dtm)>0)
  dtm<-dtm[,features]
  dtm<-dtm[,order(colnames(dtm))]

####
  trainingDTM <-data.frame(as.matrix(dtm[selector_idx, ]),stringsAsFactors=False)
  trainingDTM$class <-gold_table[idx,2]
  
  set.seed(71)
  model <-randomForest(as.factor(class) ~ .,data =trainingDTM,importance=TRUE,
                       proximity=TRUE,type="classification")
  
  feature_matrix<-varImpPlot(model, scale = TRUE)
#####
  #colnames(feature_matrix)[1:(ncol(feature_matrix)-1)]<-colnames(dtm[selector_idx, ])
  # delete bias term from feature matrix
  feature_matrix<-feature_matrix[,-ncol(feature_matrix),drop=F]
  # if only 2 categories were used, transform feature matrix
  if(nrow(feature_matrix)==1){
    feature_matrix<-rbind(feature_matrix,(feature_matrix*-1))
    rownames(feature_matrix)<-setdiff(unique(gold_table[,2]),"NEG")
  }
  word_counts<-colSums(dtm)  
####
  testDTM<-data.frame(as.matrix(dtm))
  #testDTM<-convertMatrixToSparseM(quanteda::as.dfm(dtm))
  predicted <- predict(model, testDTM, type="prob") 
####
  predictions<-as.character(predicted)
  probabilities<-predicted
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
    trainingDTM_og <-convertMatrixToSparseM(quanteda::as.dfm(dtm[selector_idx, ]))
    trainingLabels <- gold_table[idx,2]
    #if enough trainign data available use k=10, else min number of trainign samples
    evalMeasures <- k_fold_cross_validation(trainingDTM_og, trainingLabels, cost = cParameter,k = min(10,dim(trainingDTM_og)[1]))
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