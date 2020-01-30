# Andreas Niekler 2017
# NLP Group
# Leipzig University

##############################################################
#' An S4 class to represent a Topic Detection and Tracking Object.
#'
#' @slot history A document-term-matrix (dgCMatrix) containing a set of already processed documents.
#' @slot document_frequency A vector containing the information of the document frequencies of all allready seen words. This vector is sychronized by the history and new data batches.
#' @slot clusters A vector holding information on the cluster assignment of the processed documents.
#' @slot is A vector holding the exernal id information of the processed documents.
tdt_tracker <- setClass(
  "tdt_tracker",
  representation = list(
    history = "dgCMatrix",
    document_frequency = "vector",
    clusters = "vector",
    id = "vector"
  )
)

##############################################################
#' Initializes a tdt_tracker S4 object.
#'
#' @return The initialized tdt_tracker object.
setMethod("initialize", signature("tdt_tracker"),
          function(.Object, ...) {
            .Object <- callNextMethod()
            .Object@history <- as(Matrix(0,ncol=0,nrow=0,sparse = T),"dgCMatrix")
            .Object@document_frequency <- rep(0,0)
            .Object@clusters <- rep(0,0)
            .Object@id <- rep(0,0)
            return(.Object)
          })

##############################################################
#' Tracks a new document-term-matrix batch with a given tdt_tracker S4 object.
#'
#' @param o1 The tdt_tracker S4 object where the new documents should be added.
#'
#' @param new_dtm The document-term-matrix representing the new documents.
#'
#' @return The tdt_tracker S4 object containing the tracked new documents.
#'
#' @export
#' @docType methods
#' @rdname tdt-methods
#'
#' @examples
#' t1 <- new("tdt_tracker")
#' t1 <- track(t1, a_new_dtm)
setGeneric("track",function(o1, new_dtm){standardGeneric("track")})

##############################################################
#' @rdname tdt-methods
#' @aliases track,dgCMatrix,dgCMatrix-method
#' @docType methods
setMethod("track",
          signature(o1 = "tdt_tracker",new_dtm = "dgCMatrix"),definition=function(o1, new_dtm) {

            #If new_dtm is empty return because there is nothing todo
            if(nrow(new_dtm) < 1)
              return(o1)

            if(is.null(rownames(new_dtm)))
              stop("No id column is set for new documents!")

            N <- nrow(o1@clusters)
            V <- ncol(o1@history)

            o1@document_frequency <- update_document_frequency(
              o1@document_frequency,
              Matrix::colSums(tmca.util::make_binary(new_dtm))
            )

            tmp_dtm <- Matrix(0,ncol=length(o1@document_frequency),nrow=nrow(new_dtm),sparse=T)
            colnames(tmp_dtm) <- names( o1@document_frequency)
            tmp_dtm[,colnames(new_dtm)] <- new_dtm
            rownames(tmp_dtm) <- rownames(new_dtm)
            new_dtm <- tmp_dtm

            tmp_history <- Matrix(0,ncol=length(o1@document_frequency),nrow=nrow(o1@history),sparse=T)
            colnames(tmp_history) <- names( o1@document_frequency)

            if(!is.null(colnames(o1@history)))
              tmp_history[,colnames(o1@history)] <- o1@history

            o1@history <- tmp_history

            rm(tmp_history)
            rm(tmp_dtm)

            #colnames(new_dtm) <- names(o1@document_frequency)

            new_weighted_dtm <- create_documents(new_dtm,  o1@document_frequency, nrow(o1@history) + nrow(new_dtm))
            rownames(new_weighted_dtm) <- rownames(new_dtm)

            for(i in 1:nrow(new_weighted_dtm)){
              nearest_neighbor <- compare_documentsnearest(new_weighted_dtm[i,,drop=F], o1@history)
              o1@history <- rBind(o1@history, new_weighted_dtm[i,])
              #hier mussnun erweitert werden, um den neuen Test für das neue Dok zu bauen
              o1@id <- c(o1@id, rownames(new_weighted_dtm)[i])

              if(length(o1@clusters)<1)
              {
                o1@clusters <- c(1)
                next()
              }

              if(is.null(nearest_neighbor))
              {
                o1@clusters <- c(o1@clusters,max(o1@clusters) + 1)

              }
              else
              {
                o1@clusters <- c(o1@clusters,o1@clusters[nearest_neighbor])
                #print("pah")
              }
            }
            return(o1)
          })

##############################################################
#' Adds the += operator to add a vector to another
#'
#' @param e1 First vector
#' @param e2 Second vector
#'
#' @return The added value vector
#' @export
#'
#' @examples
#' v1 <- c(1,2,3,4)
#' v2 <- c(2,3,4,5)
#' v1 %+=% v2
#' v1
`%+=%` = function(e1,e2) {
  eval.parent(substitute(e1 <- e1 + e2))
}

##############################################################
#' Takes a vector of document frequencies and adds up new document frequency counts. Also aligns vocabulary to new words.
#'
#' @param document_frequency_vector Old document frequency vector
#' @param document_frequency_vector_update New document frequency vector
#'
#' @return The updated vector
#' @export
#'
#' @examples
#' o1@document_frequency <- update_document_frequency(
#'  o1@document_frequency,
#'  colSums(tmca.util::make_binary(new_dtm))
#' )
update_document_frequency <- function(document_frequency_vector, document_frequency_vector_update) {
  all_vocab <- union(names(document_frequency_vector), names(document_frequency_vector_update))
  document_frequency_vector_new <- rep(0,length(all_vocab))
  names(document_frequency_vector_new) <- all_vocab
  document_frequency_vector_new[names(document_frequency_vector)] <- document_frequency_vector
  document_frequency_vector_new[names(document_frequency_vector_update)] %+=%document_frequency_vector_update
  return(setNames(as.vector(document_frequency_vector_new),all_vocab))
}

##############################################################
#' Creates new TDT compatibe documents. This introduces a term weighting to the new document-term-matrix
#'
#' @param new_dtm The added document-term-matrix
#' @param document_frequency The document frequency histogram to calculate the weigthing
#' @param N The number of all documents included in the process (history + new_dtm)
#'
#' @return A document-term-matrix containing the largest 1000 term weights calculated from new_dtm.
#' @export
#'
#' @examples
#' new_weighted_dtm <- create_documents(new_dtm,  o1@document_frequency, nrow(o1@history) + nrow(new_dtm))
create_documents <- function(new_dtm, document_frequency,N){

  weigth_dtm <- new_dtm %*% diag(log((0.5+N)/document_frequency)/log(1.0 + N))
  colnames(weigth_dtm) <- colnames(new_dtm)

  #sort weights for docs, only set 1000 most weighted
  for(i in seq_len(nrow(weigth_dtm))){

    weigth_order <- order(weigth_dtm[i,],decreasing = T)[1:1000]
    weigth_order <- weigth_order[!is.na(weigth_order)]
    weigth_dtm[i,-weigth_order]<-0



  }

  return(weigth_dtm)
}

##############################################################
#' Compares a document to all other documents in the history in order to find a matching cluster
#'
#' @param document_vector TDT-weighted representation of the document about to add.
#' @param all_documents TDT-weighted representation of the history.
#'
#' @return A indicator of a matched document in the history or NULL if threshold was not exceeded.
#' @export
#'
#' @examples
#' compare_documentsnearest(new_weighted_dtm[i,,drop=F], o1@history)
compare_documentsnearest <- function(document_vector, all_documents){

  sim <- 0.21

  #reverse processing TODO
  sims <- unlist(apply(all_documents,1, function(x) sim_ab(as.matrix(document_vector),x)))

  if(length(sims)<1)
    return(NULL)

  sims[is.nan(sims)] <- 0

  m <- max(sims,index=T)

  if(sims[m] > sim)
  {
    return(m)
  }
  else
    return(NULL)

}

##############################################################
#' Calculates the term-weighting according to TDT. It is basically a TF-IDF weighting scheme.
#'
#' @param new_dtm The document-term-matrix to be processed.
#' @param df The document frequency information.
#' @param N The number of the documents.
#'
#' @return The weighted document-term-matrix
#' @export
get_weight <- function(new_dtm, df, N){
  result <- new_dtm %*% diag(log((0.5+N)/df)/log(1.0 + N))
  colnames(result) <- colnames(new_dtm)
  return(result)
}

##############################################################
#' Cosine similarity for 2 documents
#'
#' @param a Vector of document A.
#' @param b Vector of document B.
#'
#' @return Distance between 2 documents.
#' @export
sim_ab <- function(a,b)
{
  as.double(1-cosine_distance(rbind(a,b)))
}