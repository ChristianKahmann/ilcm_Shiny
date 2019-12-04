processToTM <- function(dtm,K)
{
  
  corpusLDA <- dtm2ldaformat(as.simple_triplet_matrix(dtm),omit_empty = T)
  
  model <- lda.collapsed.gibbs.sampler(
    corpusLDA$documents,
    K,
    vocab = corpusLDA$vocab, burnin = 200,
    #num.iterations = 1, #test
    num.iterations = 300, # 100 Gibbs iterations should be enough
    alpha = 5/K,
    eta = 0.1,
    initial=NULL,
    trace = 10L,
    compute.log.likelihood = F,
    freeze.topics=F
  )
  
  
  return(model)
}

inferTopicsForUnseenDocs <- function(topicModel, dtm, K)
{
  
  # repeat topic inference for unseen docs several times and take
  # the mean of assigned topic counts over all runs
  iterations <- 5
  #iterations <- 2 # test
  topicCountsPerDoc <- Matrix(0, nrow=nrow(dtm), ncol=K)
  
  #delete vocab from corpus data which isn't appropriate
  #to dtm
  dtm <- dtm[,as.vector(colnames(topicModel$topics))]
  
  #back to list
  corpusLDA <- dtm2ldaformat(as.simple_triplet_matrix(dtm),omit_empty = F)
  
  corpusData <- corpusLDA$documents
  corpusVocab <- corpusLDA$vocab
  
  result <- lapply(X = 1:iterations, FUN = inferModel, model = topicModel, corpusData = corpusData, corpusVocab = corpusVocab, alpha = 5/K, eta=0.1, K=K)
  
  for (iteration in 1:iterations) {
    topicCountsPerDoc <- topicCountsPerDoc + t(result[[iteration]])
  }
  topicCountsPerDoc <- topicCountsPerDoc / iterations
  colnames(topicCountsPerDoc) <- sapply(seq(1:K), function(x) paste0("t",x))
  return(topicCountsPerDoc)
}

inferModel <- function(x,model,corpusData,corpusVocab,alpha,eta,K){
  
  print(paste0("Iteration ", x))
  result_unseen_documents <- lda.collapsed.gibbs.sampler(
    corpusData,
    K,
    vocab = corpusVocab,
    #num.iterations = 1, #test
    num.iterations = 50, # 100 Gibbs iterations should be enough
    alpha = alpha,
    eta = eta,
    initial=list(topics=model$topics, topic_sums=model$topic_sums),
    trace = 1L,
    compute.log.likelihood = T,
    freeze.topics=TRUE
  )
  
  return(result_unseen_documents$document_sums)
  
}