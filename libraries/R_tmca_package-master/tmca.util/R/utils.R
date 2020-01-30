

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

range11 <- function(x){2 * ((x-min(x))/(max(x)-min(x))) - 1}

cosine_distance <- function(x){
  as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

KLD <- function(x,y) sum(x *log(x/y))
JSD <- function(x,y) sqrt(0.5 * KLD(x, (x+y)/2) + 0.5 * KLD(y, (x+y)/2))

# Jenson-Shannon divergence
jenson_shannon_distance <- function(inMatrix, pseudocount=0.000001, ...) {

  nCols <- length(colnames(inMatrix))
  nRows <- length(rownames(inMatrix))

  colnames <- colnames(inMatrix)
  resultsMatrix <- matrix(0, nCols, nRows)

  inMatrix <- apply(inMatrix,1:2,function(x) ifelse (x == 0, pseudocount, x))

  for(i in 1:nCols) {
    for(j in 1:nRows) {
      resultsMatrix[i, j] <- JSD(as.vector(inMatrix[, i]), as.vector(inMatrix[, j]))
    }
  }

  colnames(resultsMatrix) <- colnames
  rownames(resultsMatrix) <- colnames
  resultsMatrix <- as.dist(resultsMatrix)

  return(resultsMatrix)

}



chi_square_statistic <- function (x, correct = TRUE) {

  if (is.matrix(x)) {

    if (min(dim(x)) == 1L) return(0)

    nr <- as.integer(nrow(x))
    nc <- as.integer(ncol(x))

    if (is.na(nr) || is.na(nc) || is.na(nr * nc))
      stop("Invalid nrow(x) or ncol(x)", domain = NA)

    if ((n <- sum(x)) == 0)
      stop("At least one entry of 'x' must be positive")

    sr <- rowSums(x)
    sc <- colSums(x)

    E <- outer(sr, sc, "*")/n
    v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3
    V <- outer(sr, sc, v, n)
    dimnames(E) <- dimnames(x)

    if (correct && nrow(x) == 2L && ncol(x) == 2L) {
      YATES <- min(0.5, abs(x - E))
    } else {
      YATES <- 0
    }

    chi_square <- sum((abs(x - E) - YATES)^2/E)

  } else {
    stop("x must be a matrix")
  }

  return(chi_square)
}
