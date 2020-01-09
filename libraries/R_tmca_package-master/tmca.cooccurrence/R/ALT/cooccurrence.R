coocs <- function(x, ...) UseMethod("coocs")

#' Calculate co-occurrences from binary document-term-matrices
#' @param binDTM a DocumentTermMatrix in the form of a Matrix::dgCMatrix
#' @param minCoocFreq a number specifying the minimal times a cooccurrence has to appear, for it not beeing set to 0
#' @param significanceThreshhold a Number which determindes a Threshold. Significances below this Threshold will be set to zero.
#' @param measure The significance measure (DICE,LOGLIK,MI,COUNT). Defaults to DICE.
#' @return a significance-matrix based on the given \code{binDTM}
#' @examples
#' coocs(binDTM)
coocs.dgCMatrix <- function(binDTM, measure = "DICE", significanceThreshold = 1.0, minCoocFreq = 2) {
  
  if(class(binDTM)[1] != "dgCMatrix") stop("DTM must be \"dsCMatrix\" of package \"Matrix\".")
  # Check for existing colnames
  if(is.null(colnames(binDTM))) stop ("DTM has to have colnames")
  # Ensure binary DTM
  if (any(binDTM > 1)) {
    binDTM <- binDTM >= 1 + 0
  }
  # check if binDTM is empty
  if (sum(binDTM)==0) return(Matrix(c(0),dim(binDTM)[2],dim(binDTM)[2],sparse=T))
  resultCoocCounts<-calcCoocCounts(binDTM,minCoocFreq)
  
  finalSig<-calcSignificanceValues(k = resultCoocCounts[[1]],kj = resultCoocCounts[[2]],coocCounts = resultCoocCounts[[3]],relWords = resultCoocCounts[[4]],finalSig=resultCoocCounts[[5]],measure = measure,significanceThreshold = significanceThreshold)
  return(finalSig)
}


#' Calculate then cooccurrence-counts of a binary DocumentTermMatrix
#'
#' @param binDTM a DocumentTermMatrix in the form of a Matrix::dgCMatrix
#' @param minCoocFreq a number specifying the minimal times a cooccurrence has to appear, for it not beeing set to 0
#' @return the calculates matrix of cooccurrence-Counts with dimension #Terms * #Terms
#' @examples
calcCoocCounts<-function(binDTM,minCoocFreq)
{
  # calculate cooccurrence counts
  coocCounts <- Matrix::t(binDTM) %*% binDTM
  
  #DELETE NA'S, Much faster on table object
  tmp <- Matrix::summary(coocCounts)
  
  #delete vocab whith no coocs
  tmp[tmp[,"x"] < minCoocFreq,"x"] <- 0
  
  #set diagonals to 0's
  tmp[tmp[,1] == tmp[,2],"x"] <- 0
  
  coocCounts <-Matrix::sparseMatrix(i=tmp[,1], j=tmp[,2], x=tmp[,3],
                                    dimnames=dimnames(coocCounts),dims = dim(coocCounts))
  
  finalSig <-  Matrix::sparseMatrix(i=tmp[,1], j=tmp[,2], x=tmp[,3],
                                    dimnames=dimnames(coocCounts),dims = dim(coocCounts))
  k <- nrow(binDTM)
  kj <- Matrix::colSums(binDTM)
  names(kj) <- colnames(binDTM)
  tmp_sig <- vector(mode="numeric", length=length(kj))
  names(tmp_sig) <- colnames(binDTM)
  
  relWords <- colnames(binDTM)
  relWords <- intersect(names(which(kj > 0)),colnames(coocCounts)[which(rowSums(coocCounts) > 0)])
  
  return(list(k,kj,coocCounts,relWords,finalSig))
}

#' Calculate the cooccurrence-based Significance-Values for a DocumentTermMatrix 
#'
#' @param k a number of documents
#' @param kj a vector containing the number of words for all documents
#' @param coocCounts sparseMatrix containing the cooccurrence-Counts
#' @param relWords vector of relevant terms
#' @param finalSig a sparseMatrix for saving the results
#' @param measure a String determining the significance Measure :DICE, LOGLIK, MI, COUNT
#' @param significanceThreshhold a Number which determindes a Threshold. Significances below this Threshold will be set to zero.
#' @return The calculates Significance-Values
calcSignificanceValues<-function(k,kj,coocCounts,relWords,finalSig,measure="DICE",significanceThreshold = 1.0)
{
  
  for(i in 1:length(relWords)){
    
    coocTerm <- relWords[i]
    
    # retrieve numbers for statistic calculation
    ki <- kj[coocTerm]
    kij <- coocCounts[coocTerm, ]
    
    #ONLY kij > 0 makes sense so select like thos - Will be faster for large data
    #index <- which(kij > 0)
    #kjLocal <- kj[index]
    #kijLocal <- kij[index]
    
    ki <- ki+0.001
    kj_help <- kj+0.001
    
    # calculate statistics
    switch(measure,
           DICE = {
             dicesig <- 2 * kij / (ki + kj_help)
             #dicesig <- dicesig[order(dicesig, decreasing=TRUE)]
             sig <- dicesig
           },
           LOGLIK = {
             logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj_help * log(kj_help)) + (kij * log(kij))
                            + (k - ki - kj_help + kij) * log(k - ki - kj_help + kij)
                            + (ki - kij) * log(ki - kij) + (kj_help - kij) * log(kj_help - kij)
                            - (k - ki) * log(k - ki) - (k - kj_help) * log(k - kj_help))
             #logsig <- logsig[order(logsig, decreasing=T)]
             sig <- logsig
           },
           MI = {
             mutualInformationSig <- log(k * kij / (ki * kj_help))
             #mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]
             sig <- mutualInformationSig
           },
           COUNT = {
             sig <- coocCounts[coocTerm,]/sum(coocCounts)
           },
           {
             # sig <- sort(kij, decreasing = TRUE)
           }
    )
    
    #sig <- sig[-match(coocTerm, names(sig))]
    sig[is.na(sig)] <- 0
    sig[sig > 0 & sig < significanceThreshold] <- 0
    #is sig really sparse
    #finalSig[coocTerm,index] <- sig
    finalSig[coocTerm,] <- sig
  }
  #finalSig[relWords,] <- result
  return(finalSig)
  
  
}



get_top_coocs = function(sigMatrix, decr = TRUE, dtm2 = NULL, N = 300) {

  dictTermsDTM <- colnames(sigMatrix)
  topCoocsIdx <- arrayInd(order(sigMatrix, decreasing=decr)[1:(N*2)], dim(sigMatrix))

  if (is.null(dtm2)) {
    topCoocs <- t(apply(topCoocsIdx, 1, FUN=function(x) c(dictTermsDTM[x], sigMatrix[x[1], x[2]])))
  } else {
    topCoocs <- t(apply(topCoocsIdx, 1, FUN=function(x) c(dictTermsDTM[x], sigMatrix[x[1], x[2]], dtm2[x[1], x[2]], dtm2[x[1],x[2]] * 100 / sigMatrix[x[1], x[2]])))
  }

  topCoocs <- topCoocs[seq(1, N*2, 2), ]
  return(topCoocs)
}
