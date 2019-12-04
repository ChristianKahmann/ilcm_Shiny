
#' Sparse Matrix Conversion: slam to Matrix
#'
#' @param X A sparse matrix from the slam package
#'
#' @return A sparse Matrix from the Matrix package
#' @export
#'
#' @examples
slam_to_Matrix <- function(X) {
  # Ensure matrix or Matrix-format (convert if SparseM)
  if (slam::is.simple_triplet_matrix(X)) {
    X <- Matrix::sparseMatrix(i=X$i, j=X$j, x=X$v, dims=c(X$nrow, X$ncol), dimnames = dimnames(X))
  }
  return(X)
}

#' Sparse Matrix Conversion: Matrix to SparseM
#'
#' @param X A sparse matrix from the Matrix package
#'
#' @return A sparse Matrix from the SparseM package ("matrix.csc")
#' @export
#'
#' @examples
Matrix_to_SparseM <- function(X) {
  X.csc <- new("matrix.csc", ra = X@x,
               ja = X@i + 1L,
               ia = X@p + 1L,
               dimension = X@Dim)
  return(as.matrix.csr(X.csc))
}
