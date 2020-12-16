#' calculate the relevance of words given a certain topic 
#' from: https://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf
#' @param lambda parameter, which weights the two summands
#' @param phi topic word distribution
#' @param theta document topic distribution
#' @param doc.length number of words per document
#' @return a matrix containing the relevance scores for all combinations of words and topics 
calculate_topic_relevance<-function(lambda=0.3, phi, theta, doc.length){
  " Sanity Checks for the input arguments"
  if (lambda < 0 || lambda > 1) {
    stop("lambda is not in the valid range between 0 and 1")
  }
  if (nrow(phi)!=ncol(theta)) {
    stop("phi and theta show different number of topics")
  }
  if (nrow(theta)!=length(doc.length)) {
    stop("theta and doc.length show different number of documents")
  }
  topic.frequency <- colSums(theta * doc.length)
  topic.proportion <- topic.frequency/sum(topic.frequency)
  term.topic.frequency <- phi * topic.frequency  
  term.frequency <- colSums(term.topic.frequency)
  term.proportion <- term.frequency/sum(term.frequency)
  phi <- t(phi)
  lift <- phi/term.proportion
  relevance <- lambda*log(phi) + (1 - lambda)*log(lift)
  return(relevance)
}
