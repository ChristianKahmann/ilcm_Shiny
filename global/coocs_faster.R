
ccoocs <- function(binDTM, measure = "DICE", significanceThreshold = 1.0, minCoocFreq = 1,cores=3) {
  
  #if(class(binDTM)[1] != "dgCMatrix") stop("DTM must be \"dsCMatrix\" of package \"Matrix\".")
  
  # Ensure binary DTM
  if (any(binDTM > 1)) {
    binDTM <- binDTM >= 1 + 0
  }
  
  # calculate cooccurrence counts
  coocCounts <- t(binDTM) %*% binDTM
  
  #DELETE NA'S, Much faster on table object
  tmp <- Matrix::summary(coocCounts)
  
  #delete vocab whith no coocs
  
  tmp[tmp[,"x"] < minCoocFreq,"x"] <- 0
  
  #set diagonals to 0's
  tmp[tmp[,1] == tmp[,2],"x"] <- 0
  
  coocCounts <-Matrix::sparseMatrix(i=tmp[,1], j=tmp[,2], x=tmp[,3],
                                    dimnames=dimnames(coocCounts),dims = dim(coocCounts))
  
  finalSig <-  Matrix::Matrix(0, nrow = nrow(coocCounts), ncol = ncol(coocCounts),sparse = T,dimnames = dimnames(coocCounts))
  
  k <- nrow(binDTM)
  kj <- colSums(binDTM)
  names(kj) <- colnames(binDTM)
  tmp_sig <- vector(mode="numeric", length=length(kj))
  names(tmp_sig) <- colnames(binDTM)
  
  relWords <- colnames(binDTM)
  # relWords <- intersect(names(which(kj > 0)),colnames(coocCounts)[which(rowSums(coocCounts) > 0)])
  
  # for (coocTerm in relWords)
  # {
  #  help_function(coocTerm)
  # }
  
  #t3<-Sys.time()
  #finalSig<-Matrix(unlist(mclapply(relWords,help_function)),ncol = length(relWords))
  #t4<-Sys.time()-t3
  #print(t4)
  switch(measure,
         DICE={    
           
           cuts<-max((dim(binDTM)[2]%/%200),cores)
           
           split <- split(1:dim(binDTM)[2], as.numeric(cut(seq_along(1:dim(binDTM)[2]),cuts)))
           dims<-dim(binDTM)[2]
           #cl <- parallel::makeCluster(cores,type="FORK",outfile="log.txt")
           #cl<-parallel::makeForkCluster(nnodes = 3,outfile='log.txt',homogeneous=TRUE,port=11001) 
           
           #parallel::clusterCall(cl, function(x) .libPaths(x), .libPaths())
           #parallel::setDefaultCluster(cl)
           #parallel::clusterExport(NULL, c('kj','dims','split',"coocCounts"),envir=environment())
           #parallel::clusterEvalQ(cl, library(Matrix))
           
           finalSig<-do.call(rbind,parallel::mclapply(mc.preschedule = T,mc.cleanup = T,mc.silent = T,mc.cores = cores,X = 1:cuts,FUN = function(f){
             
             # print(length(split[[f]]))
             # print(dim(binDTM)[2])
             help<-matrix(c(0),length(split[[f]]),dims)
             
             count=0
             for(i in split[[f]]){
               count=count+1
               help[count,]<-((kj+kj[i]+0.002))
             }
             
             return(Matrix((2*as.matrix(coocCounts[split[[f]],]))/(help),sparse = T))
             
             
           }
           
           )
           )
           #parallel::stopCluster(cl)
           colnames(finalSig)<-colnames(binDTM)
           rownames(finalSig)<-colnames(binDTM)
           return((finalSig))
         },
         MI={
           help<-matrix(c(0),dim(binDTM)[2],dim(binDTM)[2])
           for(i in 1:dim(binDTM)[2]){
             help[i,]<-((kj+0.001)*(kj[i]+0.001))
           }
           browser()
           finalSig<-log((k*as.matrix(coocCounts))/(help))
           rm(help)
           
           colnames(finalSig)<-colnames(binDTM)
           rownames(finalSig)<-colnames(binDTM)
           gc()
           return(finalSig)
           
         },
         COUNT = {
           finalSig <- coocCounts/sum(coocCounts)
           colnames(finalSig)<-colnames(binDTM)
           rownames(finalSig)<-colnames(binDTM)
           gc()
           return(Matrix(finalSig))
         }
  )
}

