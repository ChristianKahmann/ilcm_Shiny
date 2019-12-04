prune <- function(dtm, minPercent=0.0005, maxPercent=0.035)
{
  
  binDTM <- c();
  binDTM <- dtm >= 1 + 0
  
  
  
  df <- colSums(binDTM)
  toDelete <- which(df < 50)
  toDelete <- union(toDelete, which(df > nrow(dtm)*maxPercent))
  
  dtm <- dtm[,-toDelete]
  
  toDeleteDocs <- unique(which(rowSums(dtm) < 10))
 
  dtm <- dtm[-toDeleteDocs,]
  
  return(dtm)
                    
}

makeTFIDF <- function(dtm)
{
  
  binDTM <- c();
  binDTM <- dtm >= 1 + 0
  
  df <- colSums(binDTM)
  dtm <- t(t(dtm) * log(dim(dtm)[1]/df))
 
  return(dtm)
  
}