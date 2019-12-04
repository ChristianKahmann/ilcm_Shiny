prune_relative <- function(dtm, minPercent=0.0005, maxPercent=0.75)
{

  binDTM <- c();
  binDTM <- dtm >= 1 + 0



  df <- Matrix::colSums(binDTM)

  toDelete <- union(which(df < nrow(dtm)*minPercent), which(df > nrow(dtm)*maxPercent))
  if(length(toDelete)-dim(dtm)[2]==-1){
    dtm <--as(Matrix::Matrix(dtm[,-toDelete]),class(dtm)[1])
  }
  else
    dtm <- dtm[,-toDelete]

  return(dtm)
}


prune_absolute<-function(dtm, min=2,max=100)
{
  binDTM <- c();
  binDTM <- dtm >= 1 + 0


  df <- Matrix::colSums(binDTM)

  toDelete <- union(which(df < min), which(df > max))
  if(length(toDelete)-dim(dtm)[2]==-1){
    dtm <--as(Matrix::Matrix(dtm[,-toDelete]),class(dtm)[1])
  }
  else
    dtm <- dtm[,-toDelete]

  return(dtm)
}



#TESTEN
make_binary <- function(dtm)
{
  binDTM <- c();
  binDTM <- dtm >= 1 + 0
  #stay in same Matrix-class // otherwise it will coerce to logic sparse Matrix, which is not supported in other tmca-functions
  binDTM<-methods::as(binDTM,class(dtm)[1])
  return(binDTM)
}

makeTFIDF <- function(dtm)
{
  #binDTM <- c();
  #binDTM <- dtm >= 1 + 0
  df <- Matrix::colSums(dtm)
  idf <- df
  idf[idf>0] <- log(dim(dtm)[1]/idf[idf>0])
  dtm <- dtm %*% Matrix::Diagonal(x = idf)
  return(dtm)

}


slam_to_Matrix <- function(DTM) {

  # Ensure matrix or Matrix-format (convert if SparseM)

  if (slam::is.simple_triplet_matrix(DTM)) {
    DTM <- Matrix::sparseMatrix(i=DTM$i, j=DTM$j, x=DTM$v, dims=c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }
  return(DTM)

}



