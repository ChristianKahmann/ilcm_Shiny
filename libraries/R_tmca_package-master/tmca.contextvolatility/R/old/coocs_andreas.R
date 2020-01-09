ccoocs <- function(binDTM, measure = "DICE", significanceThreshold = 1.0,
                   maxCoocFreq = 500,minCoocFreq = 1,cores=2) {

  #if(class(binDTM)[1] != "dgCMatrix") stop("DTM must be \"dsCMatrix\" of package \"Matrix\".")

# Ensure binary DTM
if (any(binDTM > 1)) {
  binDTM <- binDTM >= 1 + 0
}

# calculate cooccurrence counts
coocCounts <- Matrix::t(binDTM) %*% binDTM

#DELETE NA'S, Much faster on table object
tmp <- Matrix::summary(coocCounts)

#delete vocab whith no coocs

tmp[tmp[,"x"] < minCoocFreq,"x"] <- 0
tmp[tmp[,"x"] > maxCoocFreq,"x"] <- 0

#set diagonals to 0's
tmp[tmp[,1] == tmp[,2],"x"] <- 0

coocCounts <-Matrix::sparseMatrix(i=tmp[,1], j=tmp[,2], x=tmp[,3],
                                  dimnames=dimnames(coocCounts),dims =
                                    dim(coocCounts))

finalSig <-  Matrix::Matrix(0, nrow = nrow(coocCounts), ncol =
                              ncol(coocCounts),sparse = T,dimnames = dimnames(coocCounts))

k <- nrow(binDTM)
kj <- Matrix::colSums(binDTM)
names(kj) <- colnames(binDTM)
tmp_sig <- vector(mode="numeric", length=length(kj))
names(tmp_sig) <- colnames(binDTM)

relWords <- colnames(binDTM)
# relWords <- intersect(names(which(kj >0)),colnames(coocCounts)[which(rowSums(coocCounts) > 0)])

# for (coocTerm in relWords)
# {
#  help_function(coocTerm)
# }

#t3<-Sys.time()
#finalSig<-Matrix(unlist(mclapply(relWords,help_function)),ncol =length(relWords))
#t4<-Sys.time()-t3
#print(t4)
switch(measure,
       DICE={

         tmp_c <- summary(coocCounts)

         freqs <- colSums(binDTM)

         p_1 <- freqs[tmp_c[,1]]
         p_2 <- freqs[tmp_c[,2]]

         p_det <- p_1 + p_2

         sig <- (2 * tmp_c$x)/p_det

         finalSig <- Matrix::sparseMatrix(i=tmp_c [,1], j=tmp_c [,2],
                                          x=sig,

                                          dimnames=dimnames(coocCounts),dims = dim(coocCounts))

         #parallel::stopCluster(cl)
         colnames(finalSig)<-colnames(binDTM)
         rownames(finalSig)<-colnames(binDTM)
         return((finalSig))
       },
       MI={

         #browser()
         #need to be sparse

         tmp_c <- summary(coocCounts)

         freqs <- colSums(binDTM)

         p_1 <- freqs[tmp_c[,1]]
         p_2 <- freqs[tmp_c[,2]]
         p_det <- log(p_1) + log(p_2)

         sig <- (log(tmp_c$x) + log(k)) - p_det

         sig[is.na(sig)] <- 0
         sig[is.infinite(sig)] <- 0

         finalSig <- Matrix::sparseMatrix(i=tmp_c [,1], j=tmp_c [,2],
                                          x=sig,
                                          dimnames=dimnames(coocCounts),dims =
                                            dim(coocCounts))

         colnames(finalSig)<-colnames(binDTM)
         rownames(finalSig)<-colnames(binDTM)
         gc()
         return(finalSig)

       },
       LOGLIK={

         #browser()
         #need to be sparse
         tmp_c <- summary(coocCounts)
         freqs <- colSums(binDTM)

         ki <- freqs[tmp_c[,1]]+0.001
         kj_help <- freqs[tmp_c[,2]]+0.001
         kij <- tmp_c$x

         sig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj_help *
                                                        log(kj_help)) + (kij * log(kij))
                     + (k - ki - kj_help + kij) * log(k - ki - kj_help
                                                      + kij)
                     + (ki - kij) * log(ki - kij) + (kj_help - kij) *
log(kj_help - kij)
                          - (k - ki) * log(k - ki) - (k - kj_help) * log(k -
kj_help))


           sig[is.na(sig)] <- 0
           sig[is.infinite(sig)] <- 0

           finalSig <- Matrix::sparseMatrix(i=tmp_c [,1], j=tmp_c [,2],
x=sig,

dimnames=dimnames(coocCounts),dims = dim(coocCounts))

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
