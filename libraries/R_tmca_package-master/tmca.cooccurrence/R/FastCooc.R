# Ahmad Dawar Hakimi 2018
# NLP Group
# Leipzig University
library(R6)
############################################################################################################################
#'
#' @name coocs-r6
#' @title Fast co-occurrences calculation
#' @description Calculating co-occurrences of binary document-term-matrices
#' @format R6Class
#' @usage x <- Coocc$new(binDTM)
#' x$ccoocs
#' @section Methods:
#'   \describe{
#'     \item{\code{ccoocs()}}{Calculates co-occurences of binary DocumentTermMatrices and returns a significance matrix.}
#'     \item{\code{set_binDTM(binDTM)}}{Set the binary DocumentTermMatrix.}
#'     \item{\code{set_minCoocFreq(minCoocFreq)}}{Set the minCoocFreq.}
#'     \item{\code{set_maxCoocFreq(maxCoocFreq)}}{Set the maxCoocFreq.}
#'     \item{\code{set_significanceThreshold(significanceThreshold)}}{Set the significanceThreshold.}
#'     \item{\code{set_measure(measure)}}{Set the measure.}
#'     \item{\code{get_binDTM()}}{Returns the binary DocumentTermMatrix.}
#'     \item{\code{get_minCoocFreq()}}{Returns the minCoocFreq.}
#'     \item{\code{get_maxCoocFreq()}}{Returns the maxCoocFreq.}
#'     \item{\code{get_significanceThreshold()}}{Returns the significanceThreshold.}
#'     \item{\code{get_measure()}}{Returns the measure.}
#'     }
#'
#' @param binDTM a DocumentTermMatrix in the form of a Matrix::dgCMatrix
#' @param minCoocFreq a number specifying the minimal times a cooccurrence has to appear, for it not beeing set to 0
#' @param maxCoocFreq a number specifying the maximal times a cooccurrence can appear, for it not beeing set to 0
#' @param significanceThreshhold a number which determindes a threshold. Significances below this threshold will be set to zero.
#' @param measure The significance measure (DICE,LOGLIK,MI,COUNT). Defaults to DICE.
#' @return a significance-matrix based on the given \code{binDTM}
#' @export

Coocc <- R6Class(
  "Coocc",
  lock_objects = F,
  public = list(
    binDTM = NULL,
    measure = NULL,
    minCoocFreq = NULL,
    maxCoocFreq = NULL,
    significanceThreshhold = NULL,
    initialize = function(binDTM,
                          measure = 'DICE',
                          minCoocFreq = 1,
                          maxCoocFreq = 500,
                          significanceThreshold = 1) {
                              self$binDTM <- binDTM
                              self$measure <- measure
                              self$minCoocFreq <- minCoocFreq
                              self$maxCoocFreq <- maxCoocFreq
                              self$significanceThreshold <- significanceThreshold
    },
    set_binDTM = function(binDTM) {
      if (any(binDTM > 1)) {
        binDTM <- binDTM >= 1 + 0
      }
      self$binDTM = binDTM
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
    set_significanceThreshold = function(significanceThreshold){
      self$significanceThreshold = significanceThreshold
      invisible(self)
    },
    get_binDTM = function(){
      return(self$binDTM)
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
    get_significanceThreshold = function(){
      return(self$significanceThreshhold)
      invisible(self)
    },
    ccoocs =
      function() {
        # Ensure binary DTM
        if (any(self$binDTM > 1)) {
          self$binDTM <- self$binDTM >= 1 + 0
        }

        # calculate cooccurrence counts
        coocCounts <- t(self$binDTM) %*% self$binDTM

        #DELETE NA'S, Much faster on table object
        tmp <- Matrix::summary(coocCounts)
        if(dim(tmp)[1]==0){
            return(Matrix(coocCounts))
        }
        #delete vocab whith no coocs
        tmp[tmp[, "x"] < self$minCoocFreq, "x"] <- 0
        tmp[tmp[, "x"] > self$maxCoocFreq, "x"] <- 0

        #set diagonals to 0's
        tmp[tmp[, 1] == tmp[, 2], "x"] <- 0

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

        k <- nrow(self$binDTM)
        kj <- colSums(self$binDTM)
        names(kj) <- colnames(self$binDTM)
        tmp_sig <-
          vector(mode = "numeric", length = length(kj))
        names(tmp_sig) <- colnames(self$binDTM)

        relWords <- colnames(self$binDTM)

        switch(
          self$measure,
          DICE = {
            tmp_c <- summary(coocCounts)

            freqs <- colSums(self$binDTM)

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
            colnames(finalSig) <- colnames(self$binDTM)
            rownames(finalSig) <- colnames(self$binDTM)
            return((finalSig))
          },
          MI = {
            #browser()
            #need to be sparse

            tmp_c <- summary(coocCounts)

            freqs <- colSums(self$binDTM)

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

            colnames(finalSig) <- colnames(self$binDTM)
            rownames(finalSig) <- colnames(self$binDTM)
            gc()
            return(finalSig)

          },
          LOGLIK = {
            #browser()
            #need to be sparse
            tmp_c <- summary(coocCounts)
            freqs <- colSums(self$binDTM)

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

            colnames(finalSig) <- colnames(self$binDTM)
            rownames(finalSig) <- colnames(self$binDTM)
            gc()
            return(finalSig)

          },

          COUNT = {
            finalSig <- coocCounts
            colnames(finalSig) <- colnames(self$binDTM)
            rownames(finalSig) <- colnames(self$binDTM)
            gc()
            return(finalSig)
          }
        )
      }

    )
)
