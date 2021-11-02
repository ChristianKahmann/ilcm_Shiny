library(R6)

Skip_cooc<-R6Class(
  "Skip_cooc",
  lock_objects=F,
  public = list(
    skip_tab=NULL,
    
    measure=NULL,
    maxCoocFreq = NULL,
    initialize = function(skip_tab,
                          
                          measure="DICE",
                          minCoocFreq=0,
                          maxCoocFreq = 500
                          ){
      self$skip_tab <- skip_tab
      self$measure <- measure
      self$maxCoocFreq <- maxCoocFreq
      self$minCoocFreq <- minCoocFreq
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
    set_maxCoocFreq = function(maxCoocFreq){
      self$maxCoocFreq = maxCoocFreq
      invisible(self)
    },
    set_minCoocFreq = function(minCoocFreq){
      self$minCoocFreq = minCoocFreq
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
    get_maxCoocFreq = function(){
      return(self$maxCoocFreq)
      invisible(self)
    },
    get_minCoocFreq = function(){
      return(self$minCoocFreq)
      invisible(self)
    },
    skip_ccoocs=
      function(){
        coocCounts <- self$skip_tab
        #browser()
        sym<-isSymmetric(coocCounts)
        
        tmp <- Matrix::summary(coocCounts)
        
        if(dim(tmp)[1]==0){
          return(Matrix(coocCounts))
        }
        #tmp[tmp[, "x"] > self$maxCoocFreq, "x"] <- 0
        #tmp[tmp[, "x"] < self$minCoocFreq, "x"] <- 0
        coocCounts <-
          Matrix::sparseMatrix(
            i = tmp[, 1],
            j = tmp[, 2],
            x = tmp[, 3],
            dimnames = dimnames(coocCounts),
            dims = dim(coocCounts)
          )
       
        finalSig <-
          Matrix::Matrix(
            0,
            nrow = nrow(coocCounts),
            ncol = ncol(coocCounts),
            sparse = T,
            dimnames = dimnames(coocCounts)
          )
        
        k <- nrow(self$skip_tab)
        kj<- colSums(self$skip_tab)
        tmp_sig<-vector(
          mode="numeric",length=length(kj))
        names(tmp_sig)<-colnames(self$skip_tab)
        relWords<-colnames(self$skip_tab)
        
        #browser()
        switch(
          self$measure,
          DICE = {
            
            tmp_c <- summary(coocCounts)
          
             #create instance to calculate rowSums without diagonal elements
            if(sym == TRUE){
             freqs <- colSums(self$skip_tab)
             print("sym")
          }else{
              no_dia<-self$skip_tab
              diag(no_dia)<-0
              freqs <- colSums(self$skip_tab)+rowSums(no_dia)
            }
            #freqs <- colSums(self$skip_tab)
            #browser()
            names(kj)<-colnames(self$skip_tab)
            print(freqs["pants"])
            p_1 <- freqs[tmp_c[, 1]]
            p_2 <- freqs[tmp_c[, 2]]
            
            p_det <- p_1 + p_2
            
            sig <- (2 * tmp_c$x) / p_det
            
            finalSig <-
              Matrix::sparseMatrix(
                i = tmp_c [, 1],
                j = tmp_c [, 2],
                x = sig,
                dimnames = dimnames(coocCounts),
                dims = dim(coocCounts)
              )
            
            #parallel::stopCluster(cl)
            colnames(finalSig) <- colnames(self$skip_tab)
            rownames(finalSig) <- colnames(self$skip_tab)
            #browser()
            return((finalSig))
            
          },
          MI = {
            #browser()
            #need to be sparse
            
            tmp_c <- summary(coocCounts)
            
            if(sym == TRUE){
              freqs <- rowSums(self$skip_tab)
            }else{
              no_dia<-self$skip_tab
              diag(no_dia)<-0
              freqs <- colSums(self$skip_tab)+rowSums(no_dia)
            }
            
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
            
            colnames(finalSig) <- colnames(self$skip_tab)
            rownames(finalSig) <- colnames(self$skip_tab)
            gc()
            #browser()
            return(finalSig)
            
          },
          LOGLIK = {
            #browser()
            #need to be sparse
            tmp_c <- summary(coocCounts)
            if(sym == TRUE){
              freqs <- rowSums(self$skip_tab)
            }else{
              no_dia<-self$skip_tab
              diag(no_dia)<-0
              freqs <- colSums(self$skip_tab)+rowSums(no_dia)
            }
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
            
            colnames(finalSig) <- colnames(self$skip_tab)
            rownames(finalSig) <- colnames(self$skip_tab)
            gc()
            return(finalSig)
            
          },
          
          COUNT = {
            finalSig <- coocCounts
            colnames(finalSig) <- colnames(self$skip_tab)
            rownames(finalSig) <- colnames(self$skip_tab)
            gc()
            return(finalSig)
          }
        )
        
      }
  )
)