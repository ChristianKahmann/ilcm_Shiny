library(R6)
library(udpipe)
library(data.table)
library(tm)
library(rapportools)
library(textclean)
library(stringr)

Skip_cooc<-R6Class(
  "Skip_cooc",
  lock_objects=F,
  public = list(
    skip_tab=NULL,
    measure=NULL,
    minCoocFreq = NULL,
    maxCoocFreq = NULL,
    initialize = function(skip_tab,
                          measure="DICE",minCoocFreq = 1,
                          maxCoocFreq = 500){
      self$skip_tab <- skip_tab
      self$measure <- measure
      self$minCoocFreq <- minCoocFreq
      self$maxCoocFreq <- maxCoocFreq
    },
    set_skip_cooc = function(skip_tab){
      self$skip_tab = skip_tab
      invisible(self)
    },
    set_measure = function(measure){
      if(measure == "DICE" | measure == "LOGLIK" | measure == "MI" | measure == "COUNT"){
        self$measure = measure
        invisible(self)
      }else{
        print("No such measure.")
      }
    },
    set_minCoocFreq = function(minCoocFreq){
      self$minCoocFreq = minCoocFreq
      invisible(self)
    },
    set_maxCoocFreq = function(maxCoocFreq){
      self$maxCoocFreq = maxCoocFreq
      invisible(self)
    },
    get_skip_cooc = function(){
      return(self$skip_tab)
      invisible(self)
    },
    get_measure = function(){
      return(self$measure)
      invisible(self)
    },
    get_minCoocFreq = function(){
      return(self$minCoocFreq)
      invisible(self)
    },
    get_maxCoocFreq = function(){
      return(self$maxCoocFreq)
      invisible(self)
    },
    skip_ccoocs=
      function(){
        #length_tab1 <- length(unique(self$skip_tab$term1))
        #length_tab2 <- length(unique(self$skip_tab$term2))
        #print(length_tab1)
        #print(length_tab2)
        #TODO: finde neu Funktion zum erstellen einer Matrix
        # Idee: in Tabelle kommt gleiche Wort mehrfach vor - wird so in matrix übernommen = falsch
        skip_mat <- as.matrix(self$skip_tab,unique(self$skip_tab$term2))
        #print(summary(skip_mat))
        #test <- sparseMatrix(
        #  i = max(range_tab2,range_tab1), 
        #  j = max(range_tab2,range_tab1), 
        #  x = self$skip_tab$cooc,
        #  dims = c(length_tab1,length_tab2), 
        #  dimnames = list(unique(self$skip_tab$term2),unique(self$skip_tab$term2))
        #)
        #print(summary(test))
        tmp <- Matrix::summary(skip_mat)
        if(dim(tmp)[1]==0){
          return(Matrix(skip_mat))
        }
        
        #delete vocab whith no coocs
        tmp[tmp[, "x"] < self$minCoocFreq, "x"] <- 0
        tmp[tmp[, "x"] > self$maxCoocFreq, "x"] <- 0
        coocCounts <-
          Matrix::sparseMatrix(
            i = tmp[, 1],
            j = tmp[, 2],
            x = tmp[, 3],
            dimnames = dimnames(skip_mat),
            dims = dim(skip_mat)
          )
        
        finalSig <-
          Matrix::Matrix(
            0,
            nrow = nrow(coocCounts),
            ncol = ncol(coocCounts),
            sparse = T,
            dimnames = dimnames(coocCounts)
          )
        k <- nrow(skip_mat)
        kj<- colSums(skip_mat)
        tmp_sig<-vector(
          mode="numeric",length=length(kj))
        names(tmp_sig)<-colnames(skip_mat)
        relWords<-colnames(skip_mat)
        
        switch(
          self$measure,
          DICE={
            tmp_c <- summary(coocCounts)
            freqs <- colSums(skip_mat)
            
            p_1 <- freqs[tmp_c[,1]]
            p_2 <- freqs[tmp_c[,2]]
            
            p_det<-p_1+p_2
            
            sig <- (2 * tmp_c$x) / p_det
            finalSig <-
              Matrix::sparseMatrix(
                i = tmp_c [, 1],
                j = tmp_c [, 2],
                x = sig,
                dimnames = dimnames(coocCounts),
                dims = dim(coocCounts)
              )
            
            colnames(finalSig) <- colnames(skip_mat)
            rownames(finalSig) <- colnames(skip_mat)
            return((finalSig))
          },
          MI = {
            tmp_c <- summary(coocCounts)
            freqs <- colSums(skip_mat)
            
            p_1 <- freqs[tmp_c[, 1]]
            p_2 <- freqs[tmp_c[, 2]]
            p_det <- log(p_1) + log(p_2)
            
            sig <-
              (log(tmp_c$x) + log(k)) - p_det
            
            sig[is.na(sig)] <- 0
            sig[is.infinite(sig)] <- 0
            
            finalSig <-
              Matrix::sparseMatrix(
                i = tmp_c [, 1],
                j = tmp_c [, 2],
                x = sig,
                dimnames =
                  dimnames(coocCounts),
                dims =
                  dim(coocCounts)
              )
            
            colnames(finalSig) <- colnames(skip_mat)
            rownames(finalSig) <- colnames(skip_mat)
            gc()
            return(finalSig)
          },
          LOGLIK = {
            tmp_c <- summary(coocCounts)
            freqs <- colSums(skip_mat)
            
            ki <- freqs[tmp_c[, 1]] + 0.001
            kj_help <- freqs[tmp_c[, 2]] + 0.001
            kij <- tmp_c$x
            
            sig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj_help * log(kj_help)) + (kij * log(kij))
                        + (k - ki - kj_help + kij) * log(k - ki - kj_help + kij)
                        + (ki - kij) * log(ki - kij) + (kj_help - kij) * log(kj_help - kij)
                        - (k - ki) * log(k - ki) - (k - kj_help) * log(k - kj_help))
            
            
            sig[is.na(sig)] <- 0
            sig[is.infinite(sig)] <- 0
            
            finalSig <-
              Matrix::sparseMatrix(
                i = tmp_c [, 1],
                j = tmp_c [, 2],
                x = sig,
                dimnames =
                  dimnames(coocCounts),
                dims = dim(coocCounts)
              )
            
            colnames(finalSig) <- colnames(skip_mat)
            rownames(finalSig) <- colnames(skip_mat)
            gc()
            return(finalSig)
            
          },
          
          COUNT = {
            finalSig <- coocCounts
            colnames(finalSig) <- colnames(skip_mat)
            rownames(finalSig) <- colnames(skip_mat)
            gc()
            return(finalSig)
          }
        )
        
      }
  )
)