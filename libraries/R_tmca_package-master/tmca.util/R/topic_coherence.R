# Berechnet topic coehrence  nach(Mimno et al. 2011)
# - params: Dokument-Term-Matrix
# - LDA Modell
# - Anzahl an höchstwahrscheinlichen Termen zur Berechnung der Dokument-Kookkurrenz

tmca_topic_coherence <- function(DTM, phi, N = 25) {
  
  # Ensure matrix or Matrix-format (convert if slam)
  require(Matrix)
  require(slam)
  if (is.simple_triplet_matrix(DTM)) {
    DTM <- sparseMatrix(i=DTM$i, j=DTM$j, x=DTM$v, dims=c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }
  
  K <- nrow(phi)
  DTMBIN <- DTM >= 1 + 0
  documentFrequency <- Matrix::colSums(DTMBIN)
  names(documentFrequency) <- colnames(DTMBIN)
  
  topNtermsPerTopic <- apply(phi, 1, function(x) colnames(phi)[order(x, decreasing = TRUE)[1:N]]) # top.topic.words(phi, N, by.score=FALSE)
  
  
  allTopicModelTerms <- unique(as.vector(topNtermsPerTopic))
  DTMBIN <- DTMBIN[, allTopicModelTerms]
  
  
  DTMBINCooc <- Matrix::t(DTMBIN) %*% DTMBIN
  DTMBINCooc <- Matrix::t((DTMBINCooc + 1) / Matrix::colSums(DTMBIN))
  DTMBINCooc <- log(DTMBINCooc)
  DTMBINCooc <- as.matrix(DTMBINCooc)
  
  coherence <- rep(0, K)
  pb <- txtProgressBar(max = K)
  for (topicIdx in 1:K) {
    setTxtProgressBar(pb, topicIdx)
    topWordsOfTopic <- topNtermsPerTopic[, topicIdx]
    coherence[topicIdx] <- 0
    for (m in 2:length(topWordsOfTopic)) {
      for (l in 1:(m-1)) {
        mTerm <- topWordsOfTopic[m]
        lTerm <- topWordsOfTopic[l]
        coherence[topicIdx] <- coherence[topicIdx] + DTMBINCooc[mTerm, lTerm]
      }
    }
  }
  close(pb)
  
  return(coherence)
}