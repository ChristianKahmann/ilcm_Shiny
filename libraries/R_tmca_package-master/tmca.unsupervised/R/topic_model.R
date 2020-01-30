# Janos Borst 2017
# NLP Group
# Leipzig University
library(R6)

############################################################################################################################
#' An Abstraction 'base class' for  topic modeling algorithms.
#'
#' @description It doesn't make sense to instantiate this class, however, no error will be thrown.
#' This is to imlement methods that apply to all algorithms only once (keep it DRY) and ensure the needed methods are implemented.
#' To implement a new Algorithm inherit from this class and implement the methods call(), input(), output() and inferTopics().
#'
#' @format R6Class
#' @usage No usage examples.
#' @field name  (private) Name of the topic modeling function to be used
#' @field package (private) package that contains the algorithm
#' @section Methods:
#' \describe{
#'   \item{\code{call()}}{This should execute the algorithm and save its results to the internal variable (private$model)}
#'   \item{\code{input(dtm = NULL)}}{ Input the training DTM, transform to the algorithm specific format and save to private$internal_representation.}
#'   \item{\code{output()}}{Tranform the internal representation}
#'   \item{\code{inferTopics(new_text=NULL)}}{Infer the topicdistribution with regards to the calculated model for yet unseen data.
#'               Return the document x topic matrix containing the probabilities of assignment. }
#'   \item{\code{set_parameters(par_list)}}{This method uses a List of named Parameters to set the appropriate values of the implemented algorithm.
#'                                          It is implemented in this class. It checks whether the input parameters are available in the implementation.
#'                                          It only sets the parameters if there are no mismatches. }
#'    \item{\code{info()}}{Returns the package and algorithm name.}
#' }
#'
tm_abstr <- R6Class(
  "tm_abstr",
  private = list(
    name = "",
    package = "",
    internal_representation = NULL,
    parameters = NULL,
    model = NULL
  ) ,
  
  public = list(
    call = function() {
      "This function should implement the call to the topicmodeling function"
      cat("Please imlpement this method")
      invisible(self)
    },
    input = function(dtm = NULL) {
      "This function should implement the transformation form SparseMatrix to the input format of the specific topicmodelling implementation and save internally to private$internal_representation"
      cat("Please implement a tranformation from dtm Sparse Matrix to the desired input format")
      invisible(self)
    },
    
    output = function() {
      "This function should transform the model output into a common format"
      cat(
        "Please implement a transformation from the result to a comman format\n
        The output should be a list containing:\n
        \t theta should be the probabilities for topic per document
        \t phi sould be probabilities for  topic by word"
      )
      invisible(self)
    },
    
    infer_topics = function(new_text = NULL) {
      "This function should infer the topic distribution of unseen text with a precalculated topic model"
      cat(
        "Implement this function by calculating the topic distribution of an input DTM which contains not yet seen documents.
        Return the Probability distribution (document x topic matrix)"
      )
      invisible(self)
    },
    
    info = function() {
      "This function returns the name an package of the used topic modeling implementation. No need to re-implement in the child class. Just set the variables private$name and private$package"
      return(paste(private$package, "::", private$name, sep = ""))
      invisible(self)
    },
    
    set_parameters = function(par_list) {
      "This sets the saves the initial parameters to private$parameters and checks whether parameters remain unset or aren't available. [ToDo]"
      #Check if it is even a list
      if (typeof(par_list) != "list")
        stop("Parameters in argument are not a list ")
      
      
      
      #Check if any Parameters are available
      if (length(private$parameters) <= 0) {
        print("Algorithm has no available Parameters.")
      }
      
      missing_parameters  = setdiff(names(private$parameters), names(par_list))
      notavailable_parameters   = setdiff(names(par_list), names(private$parameters))
      
      if (length(missing_parameters) != 0) {
        warning(paste(
          "The following parameters are possibly missing. Default Values?",
          paste(missing_parameters, collapse = ", ")
        ))
      }
      if (length(notavailable_parameters) != 0) {
        stop(paste(
          "The parameter(s) you are trying to set ist not available: ",
          paste(notavailable_parameters, collapse = ", ")
        ))
        print(notavailable_parameters)
      }
      
      #Iterate the given Parameterlist and set
      for (par_name in names(par_list)) {
        #no need to check again
        private$parameters [[par_name]] <- par_list[[par_name]]
        
      }
      #print(private$parameters)
      #Check again if all parameters was set ( no NULLs )
      if(private$name!= "stm"){
        for (par_name in names(private$parameters)) {
          if (is.null(private$parameters [[par_name]])) {
            stop(paste(par_name, " not set", sep = ""))
          }
        }
      }
      invisible(self)
    },
    
    get_parameters = function() {
      return (private$parameters)
    },
    
    get_available_parameters = function() {
      return (names(private$parameters))
    }
      )
  )

####################################################################################
#' Implementation of topicmodels::LDA algorithm
#'
#' @author Janos Borst
#' @description Implementation of the topicmodel::LDA algorithm. Implements all of the baseclass methods.
#'              Parameters are the number of topics K and the prior alpha
#' @format R6Class
#' @inherit tm_abstr
#' @slot name Sets the name of the method/function (identifier for the class)
#' @slot package Sets the name of the package the method was imported from
#' @slot parameters Named list of available parameters: alpha and K. This list determines which values CAN be set and also saves their value.
topicmodels_lda <- R6Class(
  "topicmodels_lda",
  inherit = tm_abstr,
  private = list(
    name = "LDA",
    package = "topicmodels",
    parameters = list(alpha = 0.6, num_iterations = 300, beta = 0.1, K = 10)
  ),
  public = list(
    input = function(input_matrix) {
      "This function just saves the sparseMatrix to the object. No need for transformation"
      private$internal_representation = input_matrix
      invisible(self)
    },
    call = function() {
      "Execute the Topicmodel estimation"
      private$model = topicmodels::LDA(
        private$internal_representation,
        k = as.integer(private$parameters$K),
       method = "Gibbs",
        control = list(
           alpha = private$parameters$alpha, estimate.beta = TRUE, verbose = 0, prefix = tempfile(),
           save = 0, keep = 0, seed = as.integer(1234), nstart = 1,
           best = TRUE, delta = private$parameters$beta, iter = private$parameters$num_iterations, burnin = 200)
      )
      invisible(self)
    },
    output = function() {
      "Transform and return term-topic and document-topic distribution as a list"
      l = list (
        theta = topicmodels::posterior(private$model)$topics,
        phi = topicmodels::posterior(private$model)$terms
      )
      colnames(l$phi) <-  private$model@terms
      rownames(l$theta) <- private$model@documents
      return (l)
    },
    infer_topics = function(dtm = NULL) {
      "Infer the topic distribution for unseen documents"
      result = topicmodels::LDA(
        dtm,
        k = as.integer(private$parameters$K),
        control = list(
          seed = 1234,
          alpha = private$parameters$alpha,
          estimate.beta = FALSE # Don't change the term topic distribution
        ),
        model = private$model # The estimated term-document-distribution
      )
      #collect and transform results (document-topic-matrix with named rows.)
      theta = result@gamma
      rownames(theta) <- result@documents
      return (theta)
    }
    
  )
)

####################################################################################
#'Implementation of LDA::lda.collapsed.gibbs.sampler
#'
#' @author Janos Borst
#' @field package (private) List of Registered methods
#' @field tm_machine (private) An instance of a Implementation of a Algorithm
#' @description Implementation of the topicmodel::LDA algorithm. Implements all of the baseclass methods. Parameters are the number of topics K and the prior alpha
#' @format R6Class
#' @inherit tm_abstr
#' @slot name Sets the name of the method/function (identifier for the class)
#' @slot package Sets the name of the package the method was imported from
#' @slot parameters Named list of available parameters: alpha and K.

lda_lda <- R6Class(
  "lda_lda",
  inherit = tm_abstr,
  private = list(
    name = "lda",
    package = "lda",
    parameters = list(alpha = 0.1, 
                      beta = 1 / 20, 
                      K = 10,
                      num_iterations = 300)
  ),
  public = list(
    initialize = function(method = "") {
      library(lda)
    },
    
    
    input = function(input_matrix) {
      lda_dtm <-
        topicmodels::dtm2ldaformat(slam::as.simple_triplet_matrix(input_matrix), omit_empty = T)
      
      # For now this library cant handle termfrequencies greater than one. So flatten out document. an occurence with higher frequency than 1 will be
      # changed to multiple occurences of the word with frequency one.
      for (i in names(lda_dtm$documents)) {
        word_index = lda_dtm$documents[[i]][1,]
        freq = lda_dtm$documents[[i]][2,]
        
        num = sum(freq[which(freq > 1)]) - sum(freq > 1, na.rm = TRUE) # Number of new entries to generate
        new_entries = matrix(as.integer(0), nrow = 2, ncol = num) # Allocate the matrix
        new_count = 1
        
        #collect the new entries
        for (j in which(freq > 1)) {
          for (c in seq(1, freq[j] - 1)) {
            new_entries[1, new_count] = as.integer(word_index[j])
            new_entries[2, new_count] = as.integer(1)
            new_count = new_count + 1
          }
          
        }
        
        lda_dtm$documents[[i]][2, which(freq > 1)] = as.integer(1) # set every count in the docuemnt to zero
        lda_dtm$documents[[i]] = cbind(lda_dtm$documents[[i]], new_entries) # add the new occurences
        
      }
      private$internal_representation = lda_dtm
      
      invisible(self)
      
    },
    
    call = function() {
      private$model <- lda.collapsed.gibbs.sampler(
        private$internal_representation$documents,
        K = private$parameters$K,
        vocab = private$internal_representation$vocab,
        burnin = 50,
        num.iterations = private$parameters$num_iterations,
        # 100 Gibbs iterations should be enough
        alpha = private$parameters$alpha,
        eta = private$parameters$beta,
        initial = NULL,
        trace = 0L,        #Messages off
        compute.log.likelihood = F,
        freeze.topics = F
      )
      invisible(self)
    },
    
    output = function() {
      #Normalize into probabilities
      l = list (
        theta = t(private$model$document_sums) / rowSums(t(private$model$document_sums))
        ,
        phi = private$model$topics / rowSums(private$model$topics)
      )
      
      colnames(l$phi) <- colnames(private$model$topics)
      return(l)
    },
    
    infer_topics = function(dtm = stop("Need new text to classify")) {
      "Infer topic distribution for new unseen documents"
      
      #Cut the vocabulary of the unseen text down to the vocabulary of the inferred model.
      new_text_cut = dtm[, intersect(as.vector(colnames(private$model$topics)), as.vector(colnames(dtm)))]
      
      #Create the new LDA::. format representation
      new_text_representation =  topicmodels::dtm2ldaformat(slam::as.simple_triplet_matrix(new_text_cut), omit_empty = F)
      l = lapply(seq_len(ncol(private$model$topics)), function(i)
        private$model$topics[, i])
      names(l) = colnames(private$model$topics)
      
      #Temporal definition of a evaluation function. This will be called 'iteration' times to take a mean of the topic assignments.
      tmp = function(x, new_docs, initial) {
        result_unseen_documents = lda.collapsed.gibbs.sampler(
          new_docs$documents,
          private$parameters$K,
          vocab = colnames(private$model$topics),
          num.iterations = 50,
          alpha = private$parameters$alpha,
          eta = private$parameters$beta,
          initial = list(
            topics = private$model$topics,
            topic_sums = private$model$topic_sums
          ),
          ,
          trace = 0L,
          # Messages off
          compute.log.likelihood = T,
          freeze.topics = TRUE
        )
        return (result_unseen_documents$document_sums)
      }
      
      iterations = 5 # Number of Iterations to calculate the mean over
      result <-
        lapply(X = 1:iterations,
               FUN = tmp,
               new_docs = new_text_representation)
      
      topicCountsPerDoc <-
        matrix(0,
               nrow = nrow(new_text_cut),
               ncol = as.integer(private$parameters$K))
      for (iteration in 1:iterations) {
        topicCountsPerDoc <- topicCountsPerDoc + t(result[[iteration]])
      }
      # Calculate mean
      topicCountsPerDoc <- topicCountsPerDoc / iterations
      colnames(topicCountsPerDoc) <-
        sapply(seq(1:private$parameters$K), function(x)
          paste0("t", x))
      gc()
      # Return a probability (normalize columnwise)
      return (topicCountsPerDoc / rowSums(topicCountsPerDoc))
      
    }
  )
)

####################################################################################
#' Implementation of Structural Topic Model (STM)
#'
#' @author Antje Schlaf
#' @description Implementation of the Structural Topic Model (stm) algorithm. Implements all of the baseclass methods.
#' @format R6Class
#' @inherit tm_abstr
#' @slot name Sets the name of the method/function (identifier for the class)
#' @slot package Sets the name of the package the method was imported from
#' @slot parameters Named list of available parameters: This list determines which values CAN be set and also saves their value.

stm_stm <- R6Class(
  
  "stm_stm",
  
  inherit = tm_abstr,
  
  private = list(
    
    name = "stm",
    package = "stm",
    parameters = list(
      #alpha = NULL, # alpha has no effect but needs to be listed because listed in GUI and therefore handed over via parameter list # TODO: adjust GUI
      K = 10,
      prevalence = NULL,
      content = NULL,
      metaData = NULL,
      init.type = "Spectral", # c("Spectral", "LDA", "Random", "Custom"),
      seed = NULL,
      max.em.its = 500,
      emtol = 0.00001,
      verbose = TRUE,
      reportevery = 5,
      LDAbeta = TRUE,
      interactions = TRUE,
      ngroups = 1,
      model = NULL,
      gamma.prior = "Pooled", # c("Pooled", "L1"),
      sigma.prior = 0,
      kappa.prior = "L1", # c("L1", "Jeffreys"),
      metaData = NULL,
      control = list()
    )
    
    
  ),
  
  public = list(
    
    initialize = function(method = "") {
      library(stm)
    },
    
    input = function(input_matrix) {
      "This function just saves the sparseMatrix to the object. No need for transformation"
      private$internal_representation = input_matrix
      invisible(self)
    },
    
    call = function() {
      
      "Execute the Topicmodel estimation"
      private$model = stm(
        documents = private$internal_representation,
        K = private$parameters$K,
        prevalence = private$parameters$prevalence,
        content = private$parameters$content,
        data = private$parameters$metaData,
        init.type = private$parameters$init.type,
        seed = private$parameters$seed,
        max.em.its = private$parameters$max.em.its,
        emtol = private$parameters$emtol,
        verbose = private$parameters$verbose,
        reportevery = private$parameters$reportevery,
        LDAbeta = private$parameters$LDAbeta,
        interactions = private$parameters$interactions,
        ngroups = private$parameters$ngroups,
        model = private$parameters$model,
        gamma.prior = private$parameters$gamma.prior,
        sigma.prior = private$parameters$sigma.prior,
        kappa.prior = private$parameters$kappa.prior
        
      )
      
      private$model$phi <- apply(X = private$model$beta$logbeta[[1]], FUN = function(x){exp(x)}, c(1,2))
      colnames(private$model$phi) <- private$model$vocab
      
      invisible(self)
    },
    
    output = function() {
      "Transform and return term-topic and document-topic distribution as a list"
      l = list (
        theta = private$model$theta,
        phi = private$model$phi,
        stm_model = private$model
      )
      colnames(l$phi) <-  private$model$vocab

      return (l)
    },
    
    infer_topics = function(dtm = NULL) {
      
      result <- stm::fitNewDocuments(model = private$model, documents = dtm) # TODO: add more parameters!
      
      theta <- result$theta
      
      return (theta)
    },
    
    extra_method = function(){
      print("extra function")
    }
    
  )
)


################################################################
# Register your new Algorthim here
#
################################################################

#source(topic_model_abstraction)
register = list("topicmodels::LDA" = topicmodels_lda,
                "LDA::lda.collapsed.gibbs.sampler" = lda_lda,
                "stm"= stm_stm
                )

register.topicmodel = function(name = stop("Name needed to identify the function"), func = stop("Please give a function to register")) {
  "Register a new implementation of topic model."
  #[ToDo] Test func for correct inheritance and function implementation???
  register[[name]] <<- func
}

##############################################################
#' A Topic Model Class
#' @description An R6 class to execute, evaluate and refine a topic model independently of the specific underlying algorithm.
#'              This uses an implementation of an algorithm as specified in the documentation of tm_abstr as engine.
#'              You can execute the methods to infer a topic model, start an evaluation task and refine parameters, or classify unseen Text.
#'              The algorithm to be used can be set at initialization time. See usage examples.
#' @usage
#' #See available algorithms
#' tmodel$list_methods()
#' #Create a new Instance. Specify the algorithm by putting in one of the strings from the output of the previous command.
#' t <- tmodel$new(method = "topicmodels::LDA")
#' #Input a textcorpus. This should contain a column with the name "text". The rows represent the documents.
#' t$input( datasource = data)
#' #or
#' t$input_preprocessed(corpus = data, dtm = dtm )
#' # Now you can create a topic model based on the input data
#' t$create_tm()
#' #After that you can infer topic distribution for unseen documents with:
#' $infer_topics(reuters_test_data)
#' #Or you can try to evaluate the current topicmodel by starting:
#' t$evaluate()
#'
#' #Chaining is possible like:
#' t$input(datasource = testdata)$create_tm()$evaluate()
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @section Methods:
#' \describe{
#'  \item{\code{set_parameters(par_list)}}{
#'        This method uses a List of named Parameters to set the appropriate values.
#'        The Parameters needed/available will be checked by the specific implementation.
#'  }
#'  \item{\code{get_parameters()}}{
#'         Get a list of the current parameterset.
#'  }
#'  \item{\code{get_available_parameters()}}{
#'         Get a list of available parameters.
#'  }
#'  \item{\code{get_model()}}{
#'        return the model parameters
#'        List containing named elements "phi" (topic-word matrix), and "theta" (document-topic matrix).
#'  }
#'  \item{\code{input(datasource = stop("Need text to input"))}}{
#'         Data input method. \code{datasource} should contain a column named "text". Each row will be treated as one document.
#'  }
#'  \item{\code{input_preprocessed (corpus = NULL, dtm = stop("Need DTM"))}}{
#'         Input put the preprocessed data and the corpus seperately. Same lines should correspond to the same documents.
#'         If you leave the corpus NULL the evaluation task will not work fully, model generation works nonetheless
#'  }
#'  \item{\code{create_tm()}}{
#'         Start estimating model parameters.
#'  }
#'  \item{\code{evaluate(measure = c("coherence", "topic_intrusion", "aligment", "word_intrusion"))}}{
#'        Start an interactive evaluation task on the currently estimated model.
#'        Specifically this will measure topic coherence, topic intrusion, word instrusion, and reproducability.
#'        
#'        "coherence" non-interactive
#'        "topic_intrusion" interactive (only available if corpus present)
#'        "aligment"  non-ineractive
#'        "word_intrusion" interactive
#'        
#'  }
#'  \item{\code{infer_topics(dtm = stop("Need dtm to infer topics"))}}{
#'        Infer a topic distribution with the current model for unseen text. new_text should be in the same format as \code{datasource} in \code {input()}
#'  }
#'  \item{\code{top_features = function(topic_nr, n=20)}}{
#'        Draws a Piechart of the n topc words for a topic
#'  }
#' }
tmodel <- R6Class(
  "tmodel",
  
  private = list(
    #Register of Functions
    register = register,
    method = NULL,
    
    corpus = NULL,
    dtm    = NULL,
    tm_machine = NULL,
    
    model = NULL,
    theta = NULL,
    phi   = NULL,
    evaluation = NULL,
    
    silent = F
  ),
  public = list(
    initialize = function(method = "") {
      "Initialize the the topicmodeling with a method"
      
      #Check if there's an implementation of the method registered
      if (!method %in% names(private$register))
        stop(paste("Method", method, "not known", sep = " "))
      # Instantiate the Topic Model Algorithm
      private$method <- method
      private$tm_machine <- private$register[[method]]$new()
      
      invisible(self)
    },
    
    set_parameters = function(par_list) {
      "To set the parameters they will be handed down to the instantiated topic model method. The Topic Model abstraction class checks for valid parameters"
      private$tm_machine$set_parameters(par_list = par_list)
      invisible(self)
    },
    
    get_parameters = function() {
      "Return a list of current parameters"
      return(private$tm_machine$get_parameters())
    },
    
    get_available_parameters = function() {
      "Return a list of available parameters for the current Method"
      return(private$tm_machine$get_available_parameters())
    },
    
    input = function(datasource = stop("Need text to input")) {
      "Input the full text source. Format is a Matrix with one column named 'text' containing one document per row"
      private$corpus = datasource
      private$dtm = tmca.util::deleteStopwordsFromDTM(tmca.util::process_dtm(data =
                                                                               private$corpus, use_cores = 7))
      private$tm_machine$input(input_matrix = private$dtm)
    },
    input_preprocessed = function(corpus = NULL, dtm = stop("Need DTM")) {
      "Possibility to input preprocessed DTM"
      if (is.null(corpus)) {
        message(
          "If Corpus not present the evaluation task will not work. Topic modelling and inference work nonetheless."
        )
      }
      private$corpus = corpus
      private$dtm = dtm
      private$tm_machine$input(input_matrix = private$dtm)
      invisible(self)
    },
    
    get_model = function() {
      "Return Model. A list containing theta and phi"
      return (private$model)
    },
    
    create_tm = function() {
      "Executing the underlying Algorithm to infer a Model on the input data"
      if (is.null(private$dtm)) {
        stop("Use input() or input_preprocessed() method first to input data.")
      }
      if (!private$silent)  {
        print("Creating Model...")
      }
      l = private$tm_machine$call()$output()
      
      private$model <- l
      private$theta <- l$theta
      private$phi <- l$phi
      rm(l)
      invisible(self)
    },
    
    evaluate = function(measure = c("coherence", "topic_intrusion", "aligment", "word_intrusion")) {
      "Evaluating the current model. Saving the Evaluation metrics to private$evaluation"
      if (!private$silent) {
        print("Evaluate...")
      }
      
      #_____
      # Topic Coherence
      if ("coherence" %in% measure) {
        private$evaluation$measure_topic_coherence = tmca.util::tmca_topic_coherence(DTM = private$dtm,
                                                                                     phi = private$phi,
                                                                                     N = 25)
      }
      # #_____
      # Topic Intrusion:
      if ("topic_intrusion" %in% measure) {
        if (!is.null(private$corpus)) {
          # Make Sure there are more than 10 Documents. Otherwise topic intrusion fails (10 runs default)
          if (private$tm_machine$.__enclos_env__$private$parameters$K < 8) {
            message(
              paste(
                "Setsize for topic intrusion: ",
                4,
                " Number of topics: ",
                private$tm_machine$.__enclos_env__$private$parameters$K,
                ". Choose K > 2* Setsize for better results."
              )
            )
          }
          private$evaluation$measure_topic_intrusion = tmca.util::tm_topic_intrusion(
            beta = private$phi,
            theta = private$theta,
            corpus = private$corpus
          )
        } else{
          message("No corpus for topic_intrusion")
        }
        
      }
      
      #_____
      # Topic Alignment
      # Create a temporal model to compare with the current model.
      if ("aligment" %in% measure) {
        tmp_tm_machine = private$register[[private$method]]$new()
        tmp_tm_machine$set_parameters(private$tm_machine$.__enclos_env__$private$parameters)
        tmp_model = tmp_tm_machine$input(private$dtm)$call()$output()
        aligned_topics = tmca.util::alignTopicModels(list(topics = private$phi), list(topics =
                                                                                        tmp_model$phi))
        private$evaluation$measure_reliability = aligned_topics$reliability
        tmca.util::printAlignedTopics(aligned_topics)
      }
      #_____
      # Word Intrusion
      if ("word_intrusion" %in% measure) {
        private$evaluation$measure_word_intrusion = tmca.util::tm_word_intrusion(private$phi)
      }
      
      return(private$evaluation)
    },
    
    infer_topics = function(dtm = stop("Need new text to classify")) {
      "Infer a topic distribution on unseen text"
      return(private$tm_machine$infer_topics(dtm = dtm))
      
    },
    top_features = function(topic_nr, n=20) {
      colors <- c(
        'rgb(211,94,96)',
        'rgb(128,133,133)',
        'rgb(144,103,167)',
        'rgb(171,104,87)',
        'rgb(114,147,203)',
        "springgreen",
        "lightseagreen",
        "maroon",
        "slategray",
        "lime",
        "magenta",
        "tomato",
        "crimson",
        "forestgreen",
        "deeppink",
        "indianred",
        "mediumorchid1",
        "olivedrab2",
        "seagreen2",
        "springgreen"
      )
      if (topic_nr > dim(private$phi)[1])
        return("falsches Topic ausgewählt")
      if (n > dim(private$phi)[2])
        return("mehr features ausgewählt
               als vorhanden")
      data <- (private$phi[topic_nr, (order(private$phi[topic_nr,], decreasing =
                                      T)[1:n])])
      data <- data.frame(cbind(names(data), data))
      colnames(data) <- c("word", "value")
      p <-
        plotly::plot_ly(
          data,
          labels = ~ word,
          values = ~ value,
          type =
            'pie',
          textposition = "inside",
          marker = list(
            colors = colors,
            line = list(color = '#FFFFFF', width = 1)
          ),
          textinfo = "label+percent",
          insidetextfont = list(color = '#FFFFFF')
        )
      plotly::layout(
        p,
        title = paste0("Top ", n, "
                       features for topic #: ", topic_nr),
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
        )
    }
    
    
    
    
    )
)



# A 'static' environment function to print out the registered function names
# [ToDo]: Perhaps a Description for each name?
#
tmodel$list_methods <- function() {
  print (names(register))
}

tmodel$list_parameters = function(method = ""){
  return (names(register[[ method ]]$private_fields$parameters))
}