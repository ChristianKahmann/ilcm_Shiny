


getModelAlignments <-
  function(modelEvaluationData,
           topWordsToMatch =
             100,
           similarityThreshold = 0.2,
           verbose = F) {
    #source("compare.R")
    numModels <- nrow(modelEvaluationData)
    if (numModels < 2)
      stop("Nothing to compare, got just than one model!")
    cat(c(
      "Parameters: nWords",
      topWordsToMatch,
      "| threshold",
      similarityThreshold,
      "\n"
    ))
    pairs <-
      combn(as.character(modelEvaluationData$modelFileName),
            2,
            simplify = F)
    allReliabilities <- rep(0, length(pairs))
    i <- 0
    for (pair in pairs) {
      i <- i + 1
      cat(c("------", "\n"))
      cat(c(pair[1], "\n"))
      cat(c(pair[2], "\n"))
      tm1 <- get(load(file = pair[1]))
      tm2 <- get(load(file = pair[2]))
      alignment <- alignTopicModels(tm1, tm2, topWordsToMatch,
                                    similarityThreshold)
      printAlignedTopics(alignment, verbose = verbose)
      allReliabilities[i] <- alignment$reliability
    }
    return(allReliabilities)
  }


#####################################################
## functions to compare topic models ################
#####################################################

topicProbability <- function(topics) {
  token <- sum(topics)
  prob <- c()
  for (i in 1:nrow(topics)) {
    ts <- sum(topics[i,]) / token
    prob <- c(prob, ts)
  }
  return(prob)
}


toComparable <- function(topics, topWordsToMatch) {
  for (i in 1:nrow(topics)) {
    ts <- topics[i,] / sum(topics[i,])
    left <- names(ts[order(-ts)][1:topWordsToMatch])
    topics[i,] <- 0
    topics[i, left] <- ts[left]
  }
  return(topics)
}


TM_Aligner <- function(topics1,
                       topics2,
                       thres,
                       probs1,
                       probs2,
                       topWordsToMatch) {
  K <- nrow(topics1)
  reliability <- 0
  cosineDists <- as.matrix(1 - topics1 %*% t(topics2) /
                             (sqrt(rowSums(topics1 ^ 2) %*% t(
                               rowSums(topics2 ^ 2)
                             ))))
  minIndexes <- apply(cosineDists, 2, which.min)
  mins <- apply(cosineDists, 2, min)
  
  alignedTopics <- list()
  alignedTopics$ids <- matrix(0, nrow = K, ncol = 2)
  alignedTopics$probabilities <- matrix(0, nrow = K, ncol = 2)
  alignedTopics$sharedTerms <- vector("list", K)
  alignedTopics$distance <- rep(0, K)
  
  for (i in 1:K) {
    index <- arrayInd(which.min(cosineDists), .dim = c(K, K))
    value <- min(cosineDists)
    if (value > thres)
      break
    
    cosineDists[index[1],] <- 1
    cosineDists[, index[2]] <- 1
    
    alignedTopics$ids[i,] <- c(index[1], index[2])
    alignedTopics$probabilities[i,] <- c(probs1[index[1]],
                                         probs1[index[2]])
    alignedTopics$sharedTerms[[i]] <- intersect(names(topics1[index[1],][order(-topics1[index[1],])][1:topWordsToMatch]),
                                                names(topics2[index[2],][order(-topics2[index[2],])][1:topWordsToMatch]))
    
    reliability = reliability + 1
    
    alignedTopics$distance[i] <- value
    
  }
  alignedTopics$reliability <- reliability / K
  return(alignedTopics)
}


alignTopicModels <- function(tm1,
                             tm2,
                             topWordsToMatch = 50,
                             similarityThreshold = 0.2) {
  c_topics1 <- toComparable(tm1$topics, topWordsToMatch)
  c_topics_p1 <- topicProbability(tm1$topics)
  c_topics2 <- toComparable(tm2$topics, topWordsToMatch)
  c_topics_p2 <- topicProbability(tm2$topics)
  alignedTopics <-
    TM_Aligner(
      c_topics1,
      c_topics2,
      similarityThreshold,
      c_topics_p1,
      c_topics_p2,
      topWordsToMatch
    )
  return(alignedTopics)
}


printAlignedTopics <- function(alignedTopics, verbose = F) {
  if (verbose) {
    for (i in 1:length(alignedTopics$sharedTerms)) {
      if (length(alignedTopics$sharedTerms[[i]]) > 0) {
        cat(c(
          "___________________________________________________________",
          "\n"
        ))
        cat(c("Shared terms:", alignedTopics$sharedTerms[[i]], "\n"))
        cat(c(
          "Distance:",
          sprintf("%.4f", alignedTopics$distance[i]),
          "\n"
        ))
        cat(c(
          "Alignment:",
          alignedTopics$ids[i, 1],
          "TO",
          alignedTopics$ids[i, 2],
          "\n"
        ))
        cat(c(
          "Probabilities:",
          sprintf("%.4f",
                  alignedTopics$probabilities[i, 1]),
          "TO:",
          sprintf("%.4f",
                  alignedTopics$probabilities[i, 2]),
          "\n"
        ))
      }
    }
    cat(c(
      "===========================================================",
      "\n"
    ))
  }
  cat(c(
    "RELIABILITY:",
    sprintf("%.4f", alignedTopics$reliability),
    "\n"
  ))
}
