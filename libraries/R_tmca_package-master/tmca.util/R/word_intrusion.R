# Implementation of https://www.umiacs.umd.edu/~jbg/docs/nips2009-rtl.pdf
tm_word_intrusion <- function(beta, runs = 20, setsize = 6, topic = "random", chance_correction = T) {

  readIntruder <- function()
  {
    n <- readline(prompt="Intruder: ")
    if (n == "") {
      cat("Choose between: ", 1 , " and ", setsize, "\n")
      return (readIntruder())
    }
    n <- as.integer(n)
    if (is.na(n)){
      n <- readIntruder()
    }
    if (n < 1 | n > setsize){
      n <- readIntruder()
    }
    return(n)
  }

  correct_selections <- 0
  K <- nrow(beta)

  for (iter in 1:runs) {

    cat("Word intrusion", iter, "of", runs, "\n")
    # browser()
    # select a topic
    if (topic == "random") {
      k <- sample(1:K, 1)
    } else {
      k <- topic
    }

    # select setsize - 1 most probable words
    top_words <- sort(beta[k, ], decreasing = T)
    top_words_setsize <- names(top_words[1:(setsize - 1)])

    # select intruder word with low probability from current topic, but high probability in some other topic
    V <- ncol(beta)
    top30 <- min(c(V, 30))
    high_prob_terms <- apply(beta[-k, ], 1, FUN = function(x) {
      names(sort(x, decreasing = T)[1:top30])
    })
    high_prob_terms <- unique(as.vector(high_prob_terms))
    low_prob_terms <- names(top_words[ceiling(V * 0.25):V])
    selection_terms <- intersect(high_prob_terms, low_prob_terms)

    intruder <- sample(selection_terms, 1)

    eval_list <- sample(c(top_words_setsize, intruder))
    intruder_true_position <- which(eval_list == intruder)

    for (i in 1:setsize) cat("[", i, "] ", eval_list[i], "\n")

    intruder_user_selection <- readIntruder()


    if (intruder_true_position == intruder_user_selection) {
      cat("CORRECT SELECTION\n")
      correct_selections <- correct_selections + 1
    } else {
      cat("FALSE SELECTION\n")
    }

  }

  performance <- correct_selections / runs

  if (chance_correction) {
    random_probabilty <- 1 / setsize
    performance <- (performance - random_probabilty) / (1 - random_probabilty)
  }

  cat("Performance:", performance, " (", correct_selections, " out of ", runs, ")\n")

  return(performance)
}
