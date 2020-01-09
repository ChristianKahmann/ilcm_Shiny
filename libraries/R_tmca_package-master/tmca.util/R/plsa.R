colNorm <- function(m) {
  t(t(m)/colSums(m))
}

rnd_matrix <- function(m) {
  apply(m, 2, FUN = function(x) {
    runif(length(x), min=0, max=1)
  })
}


plsa <- function(DTM, K, maxIter = 100, verbose = false) {
  
  if (is.simple_triplet_matrix(DTM)) {
    DTM <- sparseMatrix(i=DTM$i, j=DTM$j, x=DTM$v, dims=c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }
  
  # Assuming DTM as input, so transpose to TDM first
  TDM <- t(DTM)
  rm(DTM)
  
  D <- ncol(TDM)
  V <- nrow(TDM)
  
  nIter <- 100
  
  total <- sum(TDM)
  td <- colNorm(matrix(1, nrow=K, ncol=D));
  wt <- colNorm(rnd_matrix(matrix(0, nrow = V, ncol = K)));
  
  loglikelihood <- NULL
  
  for (iteration in 1:maxIter) {
    
    # e-step
    tmp <- colNorm(td * (t(wt) %*% (TDM / (wt %*% td))))
    if (is.nan(sum(tmp))) {
      break;
    }
    td <- tmp
    
    # m-step
    tmp <- colNorm(wt * ((TDM / (wt %*% td)) %*% t(td)))
    if (is.nan(sum(tmp))) {
      break;
    }
    wt <- tmp
    
    # loglik
    loglik <- sum(TDM * log(wt %*% td))
    loglikelihood <- c(loglikelihood, loglik)
    
    if (verbose) {
      cat("Iteration ", iteration, ", Log-likelihood ", loglik, "\n", sep = "")
    }
    
  }
  
  colnames(td) <- colnames(TDM)
  rownames(wt) <- rownames(TDM)
  
  result <- list(
    p_t = rowMeans(td),
    p_t_d = as.matrix(t(td)),
    p_w_t = as.matrix(t(wt)),
    loglikelihood = loglikelihood
  )
  
}