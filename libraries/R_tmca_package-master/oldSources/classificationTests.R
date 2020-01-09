convertToSparseM <- function(X) {
  X.csc <- new("matrix.csc", ra = as.numeric(X@x),
               ja = X@i + 1L,
               ia = X@p + 1L,
               dimension = X@Dim)
  return(as.matrix.csr(X.csc))
}

trainAndTestSVMAddedFeature <- function(dtm_train,topics_train,labels_train,dtm_test,topics_test){
  
  dtm_train <- makeTFIDF(dtm_train)
  
  #dtm_train <- dtm_train >= 1 + 0.
  #topics_train <- topics_train >= 0.1 + 0.
  #dtm_test <- dtm_test >= 1 + 0.
  #topics_test <- topics_test >= 0.1 + 0.
  
  
  
  label_counts <- table(labels_train)
  label_names <- names(label_counts)
  minCount <- min(label_counts)  
  selectIndizes <- c()
  
  for(i in 1:length(label_counts))
  {
    idx <- which(labels_train==label_names[i])
    selectIndizes <- union(selectIndizes, sample(x=idx,size=minCount,replace = T))
  }
   #enrich dtm with topic feature
  DTM_combined_train <- Matrix::cbind2(dtm_train, topics_train)
  #DTM_combined_train <- Matrix::cbind2(dtm_train)
  
  #train SVM with target
  hc <- heuristicC(data = as.matrix(DTM_combined_train[sample(1:nrow(DTM_combined_train),5000),]))
  #hc <- heuristicC(data = as.matrix(DTM_combined_train[selectIndizes,]))
  print(hc)
  
  model=LiblineaR(data=convertToSparseM(DTM_combined_train),target=labels_train,type=0,cost=hc,bias=TRUE,cross=0,verbose=T)
  #model=LiblineaR(data=convertToSparseM(DTM_combined_train[selectIndizes,]),target=labels_train[selectIndizes],type=0,cost=hc,bias=TRUE,cross=0,verbose=T)
  
  #test SVM with testset
  dtm_test <- makeTFIDF(dtm_test)
  DTM_test <- Matrix::cbind2(dtm_test, topics_test)
  
  predictor <- predict(model,convertToSparseM(DTM_test),proba=T)
  
  return(predictor)
  
}

trainAndTestSVM <- function(topics_train,labels_train,topics_test){
  
  #topics_train <- topics_train >= 0.1 + 0.
  #topics_test <- topics_test >= 0.1 + 0.
  
  label_counts <- table(labels_train)
  label_names <- names(label_counts)
  minCount <- min(label_counts)  
  selectIndizes <- c()
  
  
  for(i in 1:length(label_counts))
  {
    idx <- which(labels_train==label_names[i])
    selectIndizes <- union(selectIndizes, sample(x=idx,size=minCount,replace = T))
  }
  
  #train SVM with target
  hc <- heuristicC(data = as.matrix(topics_train[sample(1:nrow(topics_train),15000),]))
  #hc <- heuristicC(data = as.matrix(topics_train[selectIndizes,]))
  print(hc)
  
  model=LiblineaR(data=convertToSparseM(topics_train),target=labels_train,type=0,cost=hc,bias=TRUE,cross=0,verbose=T)
  #model=LiblineaR(data=as.matrix(topics_train[selectIndizes,]),target=labels_train[selectIndizes],type=0,cost=hc,bias=TRUE,cross=0,verbose=T)
  
  #test SVM with testset
  
   predictor <- predict(model,as.matrix(topics_test),proba=T)
  
  return(predictor)
  
}

trainAndTestClusterMedian <-function(topics_train,labels_train,topics_test){
  
  #create cluterset for each label with topic information
  classes <- names(table(labels_train))
  
  #build median for each cluster
  meds <- matrix(0,nrow=length(classes),ncol=ncol(topics_train))
  
  for(i in 1:length(classes))
  {
    indexes <- which(labels_train == classes[i])
    meds[i,] <- sapply(1:25,function(x){
      return(median(topics_train[indexes,x]))
    })
  }
  
  #testset comparison for each doc to median cluser
  #Test SVM with topic feature
  #EVTL JS testen
  
  #euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  euc.dist <- function(x1, x2) sqrt(colSums((x1 - t(x2)) ^ 2))
  
  result <- sapply(1:dim(topics_test)[1], function(x){
    
    example <- topics_test[x,]
    
    dists <- euc.dist(example,meds)
      
    
    #dists <- apply(meds,1,function(x){
    #  return(euc.dist(example,x))
    #})
    
    return(which.min(dists))
  })
  
  predictor <- list(predictions=classes[result])
  #predictor$probabilities <- result
  
  return(predictor)
  #Evaluation by nearest assignment
  
}

trainAndTestClusterMI <- function(){
  
  #create cluterset for each label with topic information
  
}