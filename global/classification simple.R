###easy classification###

require(LiblineaR)
require(SparseM)
source("global/utils.R")

annotatedDTM <- quanteda::as.dfm(dtm[gold_table[,1], ])
annotatedDTM <- convertMatrixToSparseM(annotatedDTM)
annotatedLabels <- gold_table[,2]

# split into training and test set
selector_idx <- rep(c(rep(TRUE, 4), FALSE), length.out = nrow(gold_table))
trainingDTM <- annotatedDTM[selector_idx, ]
trainingLabels <- annotatedLabels[selector_idx]
testDTM <- annotatedDTM[!selector_idx, ]
testLabels <- annotatedLabels[!selector_idx]

# create LR classification model
model <- LiblineaR(trainingDTM, trainingLabels)
summary(model)

classification <- predict(model, testDTM) 
predictedLabels <- classification$predictions
contingencyTable <- table(predictedLabels, testLabels)
print(contingencyTable) 

accuracy <- sum(diag(contingencyTable)) / length(testLabels)
print(accuracy)

F.measure(predictedLabels, testLabels, positiveClassName = "Handball")


get_k_fold_logical_indexes <- function(j, k, n) {
  if (j > k) stop("Cannot select fold larger than nFolds")
  fold_lidx <- rep(FALSE, k)
  fold_lidx[j] <- TRUE
  fold_lidx <- rep(fold_lidx, length.out = n)
  return(fold_lidx)
}

# Example usage
get_k_fold_logical_indexes(1, k = 10, n = 12)


k <- 10
evalMeasures <- NULL
for (j in 1:k) {
  # create j-th boolean selection vector
  currentFold <- get_k_fold_logical_indexes(j, k, nrow(trainingDTM))
  
  # select training data split
  foldDTM <- annotatedDTM[!currentFold, ]
  foldLabels <- annotatedLabels[!currentFold]
  
  # create model
  model <- LiblineaR(foldDTM, foldLabels)
  
  # select test data split
  testSet <- annotatedDTM[currentFold, ]
  testLabels <- annotatedLabels[currentFold]
  
  # predict test labels
  predictedLabels <- predict(model, testSet)$predictions
  
  # evaluate predicted against test labels
  kthEvaluation <- F.measure(predictedLabels, testLabels, positiveClassName = "Handball")
  
  # combine evaluation measures for k runs
  evalMeasures <- rbind(evalMeasures, kthEvaluation)
}
# Final evaluation values of k runs:
print(evalMeasures)
print(colMeans(evalMeasures))


cParameterValues <- c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)
fValues <- NULL

for (cParameter in cParameterValues) {
  print(paste0("C = ", cParameter))
  evalMeasures <- k_fold_cross_validation(annotatedDTM, annotatedLabels, cost = cParameter,k = 4)
  fValues <- c(fValues, evalMeasures["F"])
}

plot(fValues, type="o", col="green", xaxt="n")
axis(1,at=1:length(cParameterValues), labels = cParameterValues)


best_C <- optimize_C(annotatedDTM, annotatedLabels)


final_model <- LiblineaR(annotatedDTM, annotatedLabels, cost = best_C,verbose = T,type = 2)
final_labels <- predict(final_model, convertSlamToSparseM(dtm))$predictions
table(final_labels) / sum(table(final_labels))

predict(final_model, convertSlamToSparseM(dtm),proba = T)$probabilities->a
a<-cbind(rownames(dtm),a)


#######dictionary hits 
gold <- private$classify_machine$get_gold()

for(entry in names(dictionary))
{
  Short_dict <- list()
  Short_dict[[entry]] <- dictionary[[entry]]
  Short_dict <- quanteda::dictionary(Short_dict)
  toks <- quanteda::kwic(quanteda::corpus(private$corpus$text,docnames=private$corpus$ref_id),pattern = Short_dict,window = 5)
  gold$dict[gold$ref_id %in% toks$docname] <-  entry
}
private$classify_machine$set_gold(gold)


####### active learnign strategies
predicted <- private$classify_machine$get_gold()$predicted

if (strategy == "random") {
  # select random
  selected_queries <- sample(gold$ref_id[!is.na(predicted$predictions) & predicted$predictions == positive_class], batch_size)
  
} else if (strategy == "LC") {
  # select least certain
  
  boundary_distances <- abs(predicted$probabilities[,positive_class] - 0.5)
  # boundary_distances <- abs(predicted_labels_u$decisionValues[, 1])
  uncertain_decisions <- order(boundary_distances)
  unset_labels <- which(is.na(gold$gold))
  uncertain_decisions <- intersect(uncertain_decisions,unset_labels)[1:batch_size]
  
  selected_queries <- gold$ref_id[uncertain_decisions]
  
} else if (strategy == "MC") {
  # select most certain
  boundary_distances <- abs(predicted$probabilities[,positive_class] - 1)
  # boundary_distances <- abs(predicted_labels_u$decisionValues[, 1])
  certain_decisions <- order(boundary_distances)
  unset_labels <- which(is.na(gold$gold))
  certain_decisions <- intersect(certain_decisions,unset_labels)[1:batch_size]
  selected_queries <- gold$ref_id[certain_decisions]
  
} else if (strategy == "DIFF") {
  # difference to last run
}  else if (strategy == "LCB") {
  
  pp <- length(which(gold$gold == positive_class)) / length(which(!is.na(gold$gold)))#ONLY GOLD SECURE
  pmax <- mean(c(0.5, 1 - pp))
  #predicted_labels_u <- predict(model, u_dfm, proba = T)
  prob_positive <- predicted$probabilities[, positive_class]#OK
  lidx <- prob_positive < pmax
  uncertain_decisions <- rep(0, length(predicted$predicted))
  uncertain_decisions[lidx] <- prob_positive[lidx] / pmax
  uncertain_decisions[!lidx] <- (1 - prob_positive[!lidx]) / (1 - pmax)
  
  # order and select
  #uncertain_decisions <- order(uncertain_decisions, decreasing = T)[1:batch_size]
  
  uncertain_decisions <- order(uncertain_decisions, decreasing = T)
  unset_labels <- which(is.na(gold$gold))
  uncertain_decisions <- intersect(uncertain_decisions,unset_labels)[1:batch_size]
  
  selected_queries <- gold$ref_id[uncertain_decisions]