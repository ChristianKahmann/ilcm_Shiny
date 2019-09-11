calculate_topic_relevance<-function(lambda=0.3,phi,theta,doc.length){
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