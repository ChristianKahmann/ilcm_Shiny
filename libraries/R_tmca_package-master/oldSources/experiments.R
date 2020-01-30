processModels <-function(name, K, fileSource)
{
  selector_train <- which(as.integer(rownames(pruned[[name]])) %in% (which(source %in% c(paste0(fileSource,".train")))))
  model <- processToTM((pruned[[name]])[selector_train,], K)
  train <- t(model$document_expects)/rowSums(t(model$document_expects))
  
  selector_test <- which(as.integer(rownames(pruned[[name]])) %in% (which(source %in% c(paste0(fileSource,".test")))))
  predict <- inferTopicsForUnseenDocs(model, (pruned[[name]])[selector_test,], K)
  test <- predict/rowSums(predict)
  
  result <- list()
  result$selector_train <- selector_train 
  result$selector_test <- selector_test
  result$train <- train
  result$test <- test
  setwd("~/EACL_Mehler_Niekler")
  save(result,file=paste0("./result/",name,K,".Rdata"))
}

experiment <- function(name, K)
{
  
  #setwd("~/EACL_Mehler_Niekler")
  
  x <- load(file=paste0("./result/",name,K,".Rdata"))
  loadedModel <- get(x)
  
  myResult <- list()
  
  train_indexes <- as.integer(rownames(pruned[[name]])[loadedModel$selector_train])
  test_indexes <- as.integer(rownames(pruned[[name]])[loadedModel$selector_test])
  
  #Method 1 Adding Vectors SVM
  result <- trainAndTestSVMAddedFeature((pruned[[name]])[loadedModel$selector_train,],
                                        loadedModel$train,
                                        category[train_indexes],
                                        (pruned[[name]])[loadedModel$selector_test,],
                                        loadedModel$test)
  
  myResult$SVMFTM <- F.measure(result$predictions,category[test_indexes])
  save(myResult,file=paste0("./result/",name,K,"BIN_RESULTS.Rdata"))
  #Method 2 Just Topics SVM
  result <- trainAndTestSVM(loadedModel$train,
                            category[train_indexes],
                            loadedModel$test)
  myResult$SVMTM <- F.measure(result$predictions,category[test_indexes])
  save(myResult,file=paste0("./result/",name,K,"BIN_RESULTS.Rdata"))
  #Method 3 Topics Median
  #result <- trainAndTestClusterMedian(loadedModel$train,
  #                                    category[loadedModel$selector_train],
  #                                    loadedModel$test)
  #myResult$MED <- F.measure(result$predictions,category[loadedModel$selector_test])
  
  
  #save(myResult,file=paste0("./result/",name,K,"RESULTS.Rdata"))
  
  
}