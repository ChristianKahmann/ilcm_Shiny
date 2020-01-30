

#' Calclulates specificity
#'
#' @param termCountsTarget A vector of counts
#' @param termCountsComparison A vector of counts
#' @param term A Term
#' @return Specificiyt score
keyness_specificity <-  function(termCountsTarget, termCountsComparison, term) {

  # corpus size
  k_corpusSize <- sum(termCountsComparison)
  # word frequency in entire corpus
  k_i_termFrq <- termCountsComparison[term]
  # word frequency in sub corpus
  k_ij_observerved_termFrq_in_subCorpus <- termCountsTarget[term]
  # size of sub corpus
  k_j_subCorpusSize <- sum(termCountsTarget)
  
  # example from text book
  #k_corpusSize = 160000
  #k_i_termFrq = 36
  #k_ij_observerved_termFrq_in_subCorpus = 5
  #k_j_subCorpusSize = 20000
  
  specificity <- 0
  
  if (k_ij_observerved_termFrq_in_subCorpus > 0) {
    hyperRes <- dhyper(c(0:k_j_subCorpusSize), k_i_termFrq, k_corpusSize - k_i_termFrq, k_j_subCorpusSize)
    probSum <- sum(hyperRes[k_ij_observerved_termFrq_in_subCorpus+1:k_j_subCorpusSize+1], na.rm=TRUE)
    #plot(hyperRes)
    #print(hyperRes[1:50])
    #print(k_ij_observerved_termFrq_in_subCorpus)
    #print(k_i)  
    #print(hyperRes[n+1:k_i+1])  
    #print(probSum)
    specificity <- floor(log10(probSum)) * -1
  }
  
  if (is.infinite(specificity)) warning("Specificity is infinite. Values too high or no reference data?")
  
  return(specificity)
}





keyness_log_likelihood <- function(termCountsTarget, termCountsComparison, addOneSmoothing = TRUE, minSignificance = 6.63) {
  uniqueTerms <- setdiff(names(termCountsTarget), names(termCountsComparison))
  
  if (addOneSmoothing) {
    zeroCounts <- rep(0, length(uniqueTerms))
    names(zeroCounts) <- uniqueTerms
    termCountsComparison <- c(termCountsComparison, zeroCounts)
    
    termCountsTarget <- termCountsTarget + 1
    termCountsComparison <- termCountsComparison + 1
  }
  
  termsToCompare <- intersect(names(termCountsTarget), names(termCountsComparison))
  
  a <- termCountsTarget[termsToCompare]
  b <- termCountsComparison[termsToCompare]
  c <- sum(termCountsTarget)
  d <- sum(termCountsComparison)
  Expected1 = c * (a+b) / (c+d)
  Expected2 = d * (a+b) / (c+d)
  t1 <- a * log((a/Expected1))
  t2 <- b * log((b/Expected2))
  logLikelihood <- 2 * (t1 + t2)
  
  # compare relative frequencies to indicate over/underuse
  relA <- a / c
  relB <- b / d
  # underused terms are multiplied by -1
  logLikelihood[relA < relB] <- logLikelihood[relA < relB] * -1
  
  logLikelihood[logLikelihood < minSignificance] <- 0
  
  return(logLikelihood)
}


# http://colinglab.humnet.unipi.it/wp-content/uploads/2012/12/Europhras2015-EXTra.pdf
keyness_pmi <- function(termCountsTarget, termCountsComparison) {
}

# Keyness part-of-speech


# paste multi-word units