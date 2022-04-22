create_LDA_JSON<-function (phi = matrix(), theta = matrix(), doc.length = integer(), 
          vocab = character(), term.frequency = integer(), R = 30, 
          lambda.step = 0.01, mds.method = jsPCA, cluster, plot.opts = list(xlab = "PC1", 
                                                                            ylab = "PC2"), reorder.topics = TRUE, ...) 
{
  dp <- dim(phi)
  dt <- dim(theta)
  N <- sum(doc.length)
  W <- length(vocab)
  D <- length(doc.length)
  K <- dt[2]
  if (dp[1] != K) 
    stop("Number of rows of phi does not match \n      number of columns of theta; both should be equal to the number of topics \n      in the model.")
  if (D != dt[1]) 
    stop("Length of doc.length not equal \n      to the number of rows in theta; both should be equal to the number of \n      documents in the data.")
  if (dp[2] != W) 
    stop("Number of terms in vocabulary does \n      not match the number of columns of phi (where each row of phi is a\n      probability distribution of terms for a given topic).")
  if (length(term.frequency) != W) 
    stop("Length of term.frequency \n      not equal to the number of terms in the vocabulary.")
  if (any(nchar(vocab) == 0)) 
    stop("One or more terms in the vocabulary\n      has zero characters -- all terms must have at least one character.")
  phi.test <- all.equal(rowSums(phi), rep(1, K), check.attributes = FALSE)
  theta.test <- all.equal(rowSums(theta), rep(1, dt[1]), check.attributes = FALSE)
  if (!isTRUE(phi.test)) 
    warning("Rows of phi don't all sum to 1.")
  if (!isTRUE(theta.test)) 
    warning("Rows of theta don't all sum to 1.")
  topic.frequency <- colSums(theta * doc.length)
  topic.proportion <- topic.frequency/sum(topic.frequency)
  if (reorder.topics) 
    o <- order(topic.proportion, decreasing = TRUE)
  else o <- seq_along(topic.proportion)
  phi <- phi[o, ]
  theta <- theta[, o]
  topic.frequency <- topic.frequency[o]
  topic.proportion <- topic.proportion[o]
  mds.res <- mds.method(phi)
  if (is.matrix(mds.res)) {
    colnames(mds.res) <- c("x", "y")
  }
  else if (is.data.frame(mds.res)) {
    names(mds.res) <- c("x", "y")
  }
  else {
    warning("Result of mds.method should be a matrix or data.frame.")
  }
  mds.df <- data.frame(mds.res, topics = seq_len(K), Freq = topic.proportion * 
                         100, cluster = 1, stringsAsFactors = FALSE)
  zero_values<-which(term.frequency==0)
  #term.frequency[which(term.frequency==0)]<-0.00000001
  #phi_normalized_per_word<-t(t(phi)/colSums(phi))
  #term.topic.frequency <- t(t(phi_normalized_per_word) * term.frequency)
  term.topic.frequency<-phi*topic.frequency
  term.frequency <- colSums(term.topic.frequency)
  #term.frequency[not_null_term_frequency]<-real_term_frequency[not_null_term_frequency]
  #browser()
  stopifnot(all(term.frequency > 0))
  term.proportion <- term.frequency/sum(term.frequency)
  phi <- t(phi)
  topic.given.term <- phi/rowSums(phi)
  kernel <- topic.given.term * log(sweep(topic.given.term, 
                                         MARGIN = 2, topic.proportion, `/`))
  distinctiveness <- rowSums(kernel)
  saliency <- term.proportion * distinctiveness
  default.terms <- vocab[order(saliency, decreasing = TRUE)][1:R]
  counts <- as.integer(term.frequency[match(default.terms, 
                                            vocab)])
  Rs <- rev(seq_len(R))
  default <- data.frame(Term = default.terms, logprob = Rs, 
                        loglift = Rs, Freq = counts, Total = counts, Category = "Default", 
                        stringsAsFactors = FALSE)
  topic_seq <- rep(seq_len(K), each = R)
  category <- paste0("Topic", topic_seq)
  lift <- phi/term.proportion
  find_relevance <- function(i) {
    relevance <- i * log(phi) + (1 - i) * log(lift)
    idx <- apply(relevance, 2, function(x) order(x, decreasing = TRUE)[seq_len(R)])
    indices <- cbind(c(idx), topic_seq)
    data.frame(Term = vocab[idx], Category = category, logprob = round(log(phi[indices]), 
                                                                       4), loglift = round(log(lift[indices]), 4), stringsAsFactors = FALSE)
  }
  lambda.seq <- seq(0, 1, by = lambda.step)
  if (missing(cluster)) {
    tinfo <- lapply(as.list(lambda.seq), find_relevance)
  }
  else {
    tinfo <- parallel::parLapply(cluster, as.list(lambda.seq), 
                                 find_relevance)
  }
  tinfo <- unique(do.call("rbind", tinfo))
  tinfo$Total <- term.frequency[match(tinfo$Term, vocab)]
  rownames(term.topic.frequency) <- paste0("Topic", seq_len(K))
  colnames(term.topic.frequency) <- vocab
  tinfo$Freq <- term.topic.frequency[as.matrix(tinfo[c("Category", 
                                                       "Term")])]
  tinfo <- rbind(default, tinfo)
  ut <- sort(unique(tinfo$Term))
  m <- sort(match(ut, vocab))
  tmp <- term.topic.frequency[, m]
  r <- row(tmp)[tmp >= 0.5]
  c <- col(tmp)[tmp >= 0.5]
  dd <- data.frame(Term = vocab[m][c], Topic = r, Freq = round(tmp[cbind(r, 
                                                                         c)]), stringsAsFactors = FALSE)
  dd[, "Freq"] <- dd[, "Freq"]/term.frequency[match(dd[, "Term"], 
                                                    vocab)]
  token.table <- dd[order(dd[, 1], dd[, 2]), ]
  RJSONIO::toJSON(list(mdsDat = mds.df, tinfo = tinfo, token.table = token.table, 
                       R = R, lambda.step = lambda.step, plot.opts = plot.opts, 
                       topic.order = o))
}