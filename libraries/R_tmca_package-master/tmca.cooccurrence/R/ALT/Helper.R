getDiachronicFreqs <- function(dates, binDTM, term)
{
  
  DTMreduced <- binDTM[, term]
  
  #only for tf not df
  #total <- slam::row_sums(binDTM)
  
  #sentence frequency
  total <- rep(1,nrow(binDTM))
  
  timeseries <- data.frame(dates, total, as.matrix(DTMreduced),stringsAsFactors = F)
  
  colnames(timeseries) <- c("Date","Total","TermCount")
  
  timeseries <- timeseries[order(timeseries[,"Date"]), ]
  timeseriesAgg <- aggregate(as.matrix(timeseries[, c(2:3)]) ~ Date, timeseries, FUN = sum)
  
  # Beispiel für die Resultierende Tabelle
  timeseriesAgg[, 3] <- timeseriesAgg[, 3] / timeseriesAgg[, "Total"]
  
  return(timeseriesAgg)
}

createStrippedDates <- function(dates, aggr = "MONTH")
{
  #check date pattern
  d <- try(as.Date( dates[1], format= "%Y-%m-%d" ) )
  if( class(d) == "try-error" || is.na(d)) stop( "Date must be in \"%Y-%m-%d\" format!" )
  
  switch(aggr,
         MONTH = return(substring(unlist(dates),1,7)),
         DAYS = return(substring(unlist(dates),1,10)),
         YEARS = return(substring(unlist(dates),1,4)),
         WEEKS = return(format(as.Date(dates,format="%Y-%m-%d"),"%W")))
}

getTimesliceCoocs <- function(X,  distinctDates, datesTmp, minSig, binDTM, glob_sig)
{
  write(cat("Current date: " , distinctDates[X]), stdout())
  
  tmpDTM <- binDTM[which(datesTmp==distinctDates[X]),]
  
 # tmpDTM <- tmpDTM[,-which(colSums(tmpDTM) == 0)]
  if(length(which(colSums(tmpDTM) == 0))>0) { tmpDTM <- tmpDTM[,-which(colSums(tmpDTM) == 0)] }
  else { tmpDTM <- tmpDTM[,] }
  write(cat("Current date: " , distinctDates[X]," Number of Docs: " , nrow(tmpDTM)," Starting..."), stdout())
  
  #sig <- calculateCoocStatistics(tmpDTM, measure ="LOGLIK", significanceThreshold = minimumLLSignificancy)
  sig <- coocs(tmpDTM, measure ="DICE", significanceThreshold = minSig, minCoocFreq = 2)
  
  finalSig <-  Matrix(0, nrow = ncol(binDTM), ncol = ncol(binDTM),sparse = T,dimnames = list(colnames(binDTM),colnames(binDTM)))
  
  #print(dim(finalSig))
  #print(dim(glob_sig))
  
  finalSig[colnames(sig),rownames(sig)] <- sig
  
  #replace 0 sig wih global observation
  
  triple_final <- summary(finalSig)
  triple_glob <- summary(glob_sig)
  
  idx <- !(triple_final[,1:2] %in% triple_glob[,1:2])
  
  triple_final <- rbind(triple_final,triple_glob[idx,])
  
  finalSig <-sparseMatrix(i=triple_final[,1], j=triple_final[,2], x=triple_final[,3],
                          dimnames=dimnames(finalSig),dims = dim(finalSig))
  write( cat("Current date: " , distinctDates[X]," Significance calculation complete."), stdout())
  
  #sig[sig < minSig] <- 0
  #sig[sig > 0 & sig < minimumLLSignificancy] <- 0
  
  gc()
  
  return(finalSig)
}


getTimesliceCoocs_no_replace <- function(X,  distinctDates, datesTmp, minSig, binDTM, glob_sig)
{
  write(cat("Current date: " , distinctDates[X]), stdout())
  
  tmpDTM <- binDTM[which(datesTmp==distinctDates[X]),]
  #tmpDTM <- tmpDTM[,-which(colSums(tmpDTM) == 0)]
  #if(length(which(colSums(tmpDTM) == 0))>0) { tmpDTM <- tmpDTM[,-which(colSums(tmpDTM) == 0)] } 
  #else { tmpDTM <- tmpDTM[,] }
  write(cat("Current date: " , distinctDates[X]," Number of Docs: " , nrow(tmpDTM)," Starting..."), stdout())
  
  #sig <- calculateCoocStatistics(tmpDTM, measure ="LOGLIK", significanceThreshold = minimumLLSignificancy)
  sig <- coocs(tmpDTM, measure ="DICE", significanceThreshold = minSig)
  
  #finalSig <-  Matrix(0, nrow = ncol(binDTM), ncol = ncol(binDTM),sparse = T,dimnames = list(colnames(binDTM),colnames(binDTM)))
  
  #print(dim(finalSig))
  #print(dim(glob_sig))
  
  #finalSig[colnames(sig),rownames(sig)] <- sig
  
  #replace 0 sig wih global observation NOT
  
 # triple_final <- summary(finalSig)
  #triple_glob <- summary(glob_sig)
  
  #idx <- !(triple_final[,1:2] %in% triple_glob[,1:2])
#  
 # triple_final <- rbind(triple_final,triple_glob[idx,])
  
  #finalSig <-sparseMatrix(i=triple_final[,1], j=triple_final[,2], x=triple_final[,3],
  #                        dimnames=dimnames(finalSig),dims = dim(finalSig))
  write( cat("Current date: " , distinctDates[X]," Significance calculation complete."), stdout())
  
  #sig[sig < minSig] <- 0
  #sig[sig > 0 & sig < minimumLLSignificancy] <- 0
  
  gc()
  
  return(sig)
}