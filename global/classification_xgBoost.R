library(xgboost)
library(Matrix)
set_learning_samples_xgb<-function(parameters, gold_table, dtm){
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
  #trainingDTM <- as(quanteda::as.dfm(dtm[selector_idx, ]), "sparseMatrix")
  trainingLabels <- as.numeric(factor(gold_table[idx,2]))
  #names(trainingLabels)<-gold_table[idx,1]
  c_weights <- table(trainingLabels) / length(trainingLabels)
  c_weights <- abs(c_weights - 1) 
#####
  trainingLabels[trainingLabels == "1"] <- "0"
  trainingLabels[trainingLabels == "2"] <- "1"
  trainingDTM <-as.matrix(dtm[selector_idx, ])
  trainingDTM <- as(trainingDTM, "dgCMatrix")
  model <- xgboost(data = trainingDTM, label = trainingLabels, max.depth = 2, eta = 1, nthread = 2, nround = 2)
  #print((model))
  testDTM<-as(quanteda::as.dfm(dtm),"dgCMatrix")
  # returns vector of lenght of nrow(testDTM)
  predicted <- predict(model, testDTM) 
  #print(head(testDTM,10))
  # to do: herausfinden, ob 1 oder 0 die gewählte klasse repräsentiert
  print(head(predicted))
#####
  log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
  
  log_to_file(message = "&emsp; Cross Validation",file = logfile)
  cParameterValues <- c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)
  result=NULL
  results_complete<-list()
  count=0
  trainingDTM_og <-convertMatrixToSparseM(quanteda::as.dfm(dtm[selector_idx, ]))
  trainingLabels_og <- gold_table[idx,2]
  for (cParameter in cParameterValues) {
    count=count+1
    #print(paste0("C = ", cParameter))
    #if enough training data available use k=10, else min number of trainign samples
    evalMeasures <- k_fold_cross_validation(trainingDTM_og, trainingLabels_og, cost = cParameter,k = min(10,dim(trainingDTM_og)[1]))
    #print(evalMeasures$means)
    result <- c(result, evalMeasures$means["F"])
    results_complete[[count]]<-evalMeasures$complete
  }
  log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
  
  log_to_file(message = "&emsp; Choose active learning examples",file = logfile)
  if (parameters$cl_active_learning_strategy == "LC") {
    #print(head(predicted$probabilities))
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
  log_to_file(message = "  &emsp; ✔ Finished ",file = logfile)
  
  log_to_file(message = "&emsp; Extraction of most distinctive features",file = logfile)
  feature_matrix<-xgb.importance(model)
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