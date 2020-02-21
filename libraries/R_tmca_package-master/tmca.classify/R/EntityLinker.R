# Andreas Niekler 2018
# NLP Group
# Leipzig University
library(R6)
library(SparseM)
library(tmca.util)
library(Matrix)
library(data.table)
library(dplyr)
library(magrittr)
#TODO Korpus nehmen, labels initialisieren, feature select, trainieren, cross_validation, train test, 

############################################################################################################################
#' An Abstraction 'base class' for  classification algorithms and active learning.
#'
#' @description It doesn't make sense to instantiate this class, however, no error will be thrown.
#' This is to imlement methods that apply to all algorithms only once (keep it DRY) and ensure the needed methods are implemented.
#' To implement a new Algorithm inherit from this class and implement the methods call(), input(), output() and inferTopics().
#'
#' @format R6Class
#' @usage No usage examples.
#' @field name  (private) Name of the classification function to be used
#' @field package (private) package that contains the algorithm
#' @section Methods:
#' \describe{
#'   \item{\code{call()}}{This should execute the algorithm and save its results to the internal variable (private$model)}
#'   \item{\code{input(dtm = NULL)}}{ Input the training DTM, transform to the algorithm specific format and save to private$internal_representation.}
#'   \item{\code{output()}}{Tranform the internal representation}
#'   \item{\code{labels()}}{The labels for the examples. Used in order to correct predictions and inputs by active learning}
#'   \item{\code{predict(new_text=NULL)}}{Predict the classes based on the current model}
#'   \item{\code{set_parameters(par_list)}}{This method uses a List of named Parameters to set the appropriate values of the implemented algorithm.
#'                                          It is implemented in this class. It checks whether the input parameters are available in the implementation.
#'                                          It only sets the parameters if there are no mismatches. }
#'    \item{\code{info()}}{Returns the package and algorithm name.}
#' }
#'
EntityLinkerClassifier_abstr <- R6Class(
  "EntityLinkerClassifier_abstr",
  private = list(
    name = "",
    package = "",
    internal_representation = NULL,
    internal_gold = NULL,
    parameters = NULL,
    model = NULL
  ),
  
  public = list(
    call = function() {
      "This function should implement the call to the classifier function"
      cat("Please imlpement this method")
      invisible(self)
    },
    set_input = function(input_corpus,features, gold) {
      
      if(!is.data.frame(corpus))
      {
        stop("Corpus must be a dataframe {ref_id = character(), text = character()}")
      }
      
      # sanity checks
      if (length(gold$gold) != nrow(corpus)) {
        stop("Length of labels and nrows of corpus do not match")
      }
      
      
      "This function should implement the transformation from input texts to the internal representation. This can be a Sequence or DTM"
      cat("Please implement a creation from the text input (d_id,text) to the internal format for the classifier")
      invisible(self)
    },
    set_gold = function(gold) {
      "This function should implement the assignment of gold labels to an instanciated classifier. No need to reimplement"
      private$internal_gold <- gold
      invisible(self)
    },
    info = function() {
      "This function returns the name an package of the used classification implementation. No need to re-implement in the child class. Just set the variables private$name and private$package"
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
      for (par_name in names(private$parameters)) {
        if (is.null(private$parameters [[par_name]])) {
          stop(paste(par_name, " not set", sep = ""))
        }
      }
      invisible(self)
    },
    
    get_parameters = function() {
      return (private$parameters)
    },
    
    get_available_parameters = function() {
      return (names(private$parameters))
    },
    get_input = function() {
      return (private$internal_representation)
    },
    get_gold = function() {
      return (private$internal_gold)
    }
      )
  )

####################################################################################
#' Implementation of LiblineaR:model algorithm
#' 
#' It is important to mention that the data to predict needs to be appended to the training data in order to produce correct tfidf values.
#'
#' @author Andreas Niekler
#' @description Implementation of the LiblineaR:model algorithm. Implements all of the baseclass methods.
#'              Parameters are the C Parameter, the type, the regularization and 
#' @format R6Class
#' @inherit classifier_abstr
#' @slot name Sets the name of the method/function (identifier for the class)
#' @slot package Sets the name of the package the method was imported from
#' @slot parameters Named list of available parameters: c, . This list determines which values CAN be set and also saves their value.
liblinear_model <- R6Class(
  "liblinear_model",
  inherit = classifier_abstr,
  private = list(
    name = "model",
    package = "liblinear",
    parameters = list(
      cost = 0.1, 
      epsilon = 0.01, 
      bias = 1,
      type = 0,
      cross = 0,
      tfidf = T,
      chi2selector =F,
      prune = F,
      min_word_length = T
    ),
    used_features = NULL,
    transform_input = function(input_matrix) {
        X.csc <- new("matrix.csc", ra = as.numeric(input_matrix@x),
                     ja = input_matrix@i + 1L,
                     ia = input_matrix@p + 1L,
                     dimension = input_matrix@Dim)
        return(as.matrix.csr(X.csc))
    }
  ),
  public = list(
    set_input = function(input_features, gold) {
      "This function processes the texts in the corpus just saves the sparseMatrix to the object. No need for transformation"
      
      #First the original dimensionality; then the reduction
      ids <- input_features$id
      ref_ids <- input_features$ref_id
      
     input_features %<>% 
        group_by(id,ref_id, feature) %>% 
        summarise(n = n()) %>%
        .[grepl(.$feature,pattern = "[a-zA-Z]"),]
      
      input_features$feature <- stringi::stri_replace_all_regex(str = input_features$feature, pattern = "[^_a-zA-Z]", replacement = "") 
      
      
      word_factor <- factor(input_features$feature)
     
      input_obj <- Matrix::sparseMatrix(
        i=input_features$id,
        j=as.integer(word_factor),
        x=input_features$n,
        dims = list(max(ids), max(as.integer(word_factor))),
        dimnames = list(unique(ref_ids),levels(word_factor)))
      #Create sparse M Format
      #private$internal_representation = private$transform_input(input_obj)
      #Transform if used
      
      if(private$parameters$tfidf){
        input_obj <- tmca.util::makeTFIDF(input_obj)
       
      }
      
      input_obj <- input_obj[,colSums(input_obj)>0]
      
      if(private$parameters$min_word_length)
      {
        input_obj <-  input_obj[,nchar(colnames(input_obj)) > 1]
      }
      
      private$internal_representation = input_obj
      
      
      if(is.null(gold$ref_id) || is.null(gold$gold))
        stop("In the labels there must be the list items ref_id,gold,dict,predicted")
      
      
      input_obj <-  input_obj[rowSums( input_obj) > 0,]
      
      private$internal_gold <- list(ref_id = gold$ref_id, gold = gold$gold, dict = NA, predicted = NA, set = gold$set)
      
      if(!is.null(gold$dict))
        private$internal_gold$dict <- gold$dict
      
      invisible(self)
    },
    train = function(ref_id = NULL, positive_class = NULL, from_dictionary_hits = F) {
      
      select_query <- private$internal_gold$ref_id %in% ref_id
      select_query_idx <- which(select_query)
      
      selected <- private$internal_gold$gold[select_query]
      
      already_tagged <- which(sapply(selected,is.data.frame))
      
      gold_table <- data.table::rbindlist(selected[already_tagged])
      
      labels <- rep(NA, length(selected))
      labels[already_tagged] <- as.character(gold_table$class)
        
      if(from_dictionary_hits)
        labels <- private$internal_gold$dict[select_query]
      
      
      names(labels) <- private$internal_gold$ref_id[select_query]
      
      if(!is.null(positive_class))
      {
        labels[labels != positive_class] <- "NEG"
        labels[is.na(labels)] <- "NEG"
      }
      
      validity_select <- !is.na(labels)
      
      labels <- labels[validity_select]
      
      #Discrepancies between MAtrix Format for training with lesser ref_ids...
      #internal counts vanish for subselected matrices
      internal <- private$internal_representation[names(labels), ]
      
      
      
      #SOME FEATURES ARE EMPTY AFTER REDUCING TO TRAINING SET
      internal  <- internal[,colSums(internal)>0]
      
      #Afterwards while training!!!!!!!!!!!
      #TFIDF transformation of input
       
      #Feature selector for input based on chi2
      if(private$parameters$chi2selector)
      {
        invisible(chisq.out <- apply(internal, 2, function(x) chisq.test(x, labels)$statistic))
        internal <- internal[,chisq.out > 6]
        print(paste("Chi2 selected", length(which(chisq.out > 6)), "features."))
      }
      
      #Pruning
      if(private$parameters$prune)
      {
        #Only for quanteda possible
        #internal %<>% dfm_trim(min_docfreq = nrow(.) *0.01,
        # 
        #check by 
        ratio <- colSums((internal > 0) + 0) / nrow(internal)
        thres <- !(ratio > 0.999 | ratio < 0.001) 
        internal <- internal[,thres]
      }
      
     
      #DELETE ALL EXAMPLES WITH LESS THAN N FEATURE
      internal <- 
        internal[rowSums(internal) >=1, ]
     
      labels <- labels[names(labels) %in% rownames(internal)]
      
      #first save vocabulary than transform to matrix.csr
      private$used_features <- colnames(internal)
      
      internal <- private$transform_input(internal)
      
      # get heuristic C-weights
      c_weights <- table(labels) / length(labels)
      c_weights <- abs(c_weights - 1)
      
      #SET WI PARAMETER OF LIBLINEAR
      #WIWIWI
      
      model <- LiblineaR::LiblineaR(
        data = internal, 
        target = labels, 
        cost = private$parameters$cost, 
        epsilon = private$parameters$epsilon, 
        # NO NEED IN CLASSIFICATION svr_eps = NULL, 
        bias = 1,
        wi = c_weights,
        type = private$parameters$type,
        cross = 0)
      
      private$model <- model
      
      
      invisible(self)
    },
    predict = function(ref_id = NULL){
      
      #TFIDF on statistics from training data
      #Must training separated DTM be used to infer statistic for tfidf
      docs <- rownames(private$internal_representation[rownames(private$internal_representation) %in% ref_id, ])
      
      internal <- private$internal_representation[docs, ]
      
     
      
      #make dtm's of equal vocabulary dimensions according to last features in the model
      features <- intersect(private$used_features, colnames(internal))
      target_matrix <- Matrix::Matrix(0,nrow = nrow(internal), ncol = length(private$used_features), dimnames = list(rownames(internal),private$used_features),sparse = T)
      
      #DELETE ALL EXAMPLES WITH LESS THAN N FEATURE
      internal <- 
        internal[rowSums(internal) >=1, ]
      
      target_matrix[rownames(internal), features] <- internal[,features]
      
      
      
      #transform to sparseM for LiblineaR
      internal <- private$transform_input(target_matrix)
      labels <- predict(private$model,internal,proba = T)
      
      names(labels$predictions) <- docs
      rownames(labels$probabilities) <- docs
      names(labels$predictions) <- docs
      
      private$internal_gold <- list(
        ref_id = private$internal_gold$ref_id,
        gold = private$internal_gold$gold,
        dict = private$internal_gold$dict,
        set = private$internal_gold$set,
        predicted = labels
      )
      
      invisible(self)
      
    },
    
    train_and_evaluate_cross = function(ref_id, folds, plot_graph = T, positive_class = NULL)
    {
      cParameterValues <- c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)
        
      fValues <- sapply(cParameterValues, function(x){
        
        print(paste("Testing C-value:", x))
        
        self$set_parameters(list(cost=x))
        
        eval <- c()
        
        for(fold in folds)
        {
          test <- ref_id[fold]
          train <- ref_id[!fold]
          
          self$train(ref_id = train, positive_class = positive_class)
          self$predict(test)
          
          gold_table <- data.table::rbindlist(private$internal_gold$gold[private$internal_gold$ref_id %in% test])
          
          test_labels <- gold_table$class
          
          if(!is.null(positive_class))
          {
            test_labels[test_labels != positive_class] <- "NEG"
            test_labels[is.na(test_labels)] <- "NEG"
            
          }
          
          fold_score <- tmca.util::F.measure(
            inPred = as.character(private$internal_gold$predicted$predictions),
            inLabels = test_labels
          )
          if(is.list(fold_score))
          {
            eval <- c(eval, fold_score$micro["F"])
          } else
          {
            eval <- c(eval, fold_score["F"])
          }
        }
        
        return(mean(eval))
        
      })
      
      private$model  <- NULL
      private$internal_gold$predicted <- NULL
      
      if (plot_graph) {
        plot(fValues, type="o", col="green", xaxt="n")
        axis(1,at=1:length(cParameterValues),labels=cParameterValues)
      }
      bestC <- cParameterValues[which.max(fValues)]
      print(paste0("Best C value: ", bestC, ", F1 = ", max(fValues)))
      
      return(fValues)
      
      #build cross chunks and pass to function train test
      #call testing function with cross sets from implementation
      
      #return result as string????
      
      #but how to test parameters from different classifiers within this setting????
      #just set up chunks here and test in classifier implementation
    }
  )
)



################################################################
# Register your new Algorthim here
#
################################################################

#source(classifier_abstraction)
classify.register = list("LiblineaR::LiblineaR" = liblinear_model
)

classify.register.classifier = function(name = stop("Name needed to identify the function"), func = stop("Please give a function to register")) {
  "Register a new implementation of a classifier."
  #[ToDo] Test func for correct inheritance and function implementation???
  register[[name]] = func
}

##############################################################
#' A classifier Class
#' @description An R6 class to execute, evaluate and refine a classifier independently of the specific underlying algorithm.
#'              This uses an implementation of an algorithm as specified in the documentation of classifier_abstr as engine.
#'              You can execute the methods to train a classifier, start an evaluation task and refine parameters, or predict unseen Text.
#'              The algorithm to be used can be set at initialization time. See usage examples.
#' @usage
#' #See available algorithms
#' tmodel$list_methods()
#' #Create a new Instance. Specify the algorithm by putting in one of the strings from the output of the previous command.
#' c <- cmodel$new(method = "LiblineaR::LiblineaR")
#' #Input a textcorpus. This should contain a column with the name "text". The rows represent the documents.
#' c$input( datasource = data)
#' #or
#' c$input_preprocessed(corpus = data, dtm = dtm )
#' # Now you can create a classifier based on the input data
#' c$create_model()
#' #After that you can infer the labels for unseen documents with:
#' $predict(reuters_test_data)
#' #Or you can try to evaluate the current topicmodel by starting:
#' c$evaluate()
#'
#' #Chaining is possible like:
#' c$input(datasource = testdata)$create_classifier()$evaluate()
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
#'  \item{\code{create_model()}}{
#'         Start estimating model parameters.
#'  }
#'  \item{\code{evaluate(measure = c("coherence", "topic_intrusion", "aligment", "word_intrusion"))}}{
#'        Start an evaluation task on the currently estimated model.
#'        Specifically this will measure the class match.
#'        
#'        "class match" non-interactive
#'        
#'  }
#'  \item{\code{predict(dtm = stop("Need dtm to infer topics"))}}{
#'        Infer a topic distribution with the current model for unseen text. new_text should be in the same format as \code{datasource} in \code {input()}
#'  }
#'  \item{\code{train_and_evaluate_cross(dtm = stop("Need dtm to infer topics"))}}{
#'        Infer a topic distribution with the current model for unseen text. new_text should be in the same format as \code{datasource} in \code {input()}
#'  }
#'  \item{\code{active_learn_dictionary_examples(dtm = stop("Need dtm to infer topics"))}}{
#'        Infer a topic distribution with the current model for unseen text. new_text should be in the same format as \code{datasource} in \code {input()}
#'  }
#'  \item{\code{active_learn_examples(dtm = stop("Need dtm to infer topics"), probability = 0.5)}}{
#'        Infer a topic distribution with the current model for unseen text. new_text should be in the same format as \code{datasource} in \code {input()}
#'  }
#'  \item{\code{train_and_evaluate_sets(dtm_train = stop("Need dtm to train"),dtm_test = stop("Need dtm to test"))}}{
#'        Infer a topic distribution with the current model for unseen text. new_text should be in the same format as \code{datasource} in \code {input()}
#'  }
#' }
cmodel <- R6Class(
  "cmodel",
  
  private = list(
    #Register of Functions
    register = classify.register,
    method = NULL,
    corpus = NULL,
    features    = NULL,
    classify_machine = NULL,
    silent = F,
    iteration = NULL,
    progress = NULL,
    progress_examples  = NULL,
    progress_validation  = NULL
  ),
  public = list(
    initialize = function(method = "") 
    {
      "Initialize the the classifier with a method"
      
      #Check if there's an implementation of the method registered
      if (!method %in% names(private$register))
        stop(paste("Method", method, "not known", sep = " "))
      # Instantiate the Classifier Algorithm
      private$method <- method
      private$classify_machine <- private$register[[method]]$new()
      
      invisible(self)
    },
    get_iteration = function() {return(private$iteration)},
    get_progress = function() {return(private$progress)},
    get_progress_examples = function() {return(private$progress_examples)},
    get_progress_validation = function() {return(private$progress_validation)},
    get_input = function() {
      "Return the input dtm"
      return(private$classify_machine$get_input())
    },
    get_gold = function() {
      "Return the classifier result and annoations"
      return(private$classify_machine$get_gold())
    },
    get_input_corpus = function() {
      "Return the input corpus"
      private$corpus
      invisible(self)
    },
    
    set_parameters = function(par_list) {
      "To set the parameters they will be handed down to the instantiated classifier method. The classifier abstraction class checks for valid parameters"
      private$classify_machine$set_parameters(par_list = par_list)
      invisible(self)
    },
    
    get_parameters = function() {
      "Return a list of current parameters"
      return(private$classify_machine$get_parameters)
    },
    
    get_available_parameters = function() {
      "Return a list of available parameters for the current Method"
      return(private$classify_machine$get_available_parameters())
    },
    set_gold = function(gold = stop("Need the gold labels for the feature sequence")){
      private$classify_machine$set_gold(gold = gold)
      invisible(self)
    },
    set_input = function(corpus = stop("Need a corpus"),features = stop("Need a processed feature sequence"), gold = stop("Need the gold labels for the feature sequence")) 
      {
      "Possibility to input text examples"
      if (is.null(corpus$ref_id) || is.null(features$ref_id)) {
        stop(
          "The corpus needs rownames to dentify correct labels"
        )
      }
      
      
      private$corpus = corpus
      private$features = features
      private$classify_machine$set_input(input_features = private$features, gold = gold)
      invisible(self)
    },
    
    train = function(ref_id = NULL, positive_class = NULL,from_dictionary_hits = F)
    {
      #if no ids for training data are give we just use all examples
      if(is.null(ref_id))
      {
        gold <- private$classify_machine$get_gold()
        ref_id <- gold$ref_id[!is.na(gold$gold)]
      }
      
      print(paste("Using",length(ref_id),"examples to train model"))
      private$classify_machine$train(ref_id,positive_class = positive_class,from_dictionary_hits = from_dictionary_hits)
    },
    
    predict = function(ref_id = NULL)
    {
      if(is.null(ref_id))
      {
        gold <- private$classify_machine$get_gold()
        ref_id <- gold$ref_id[is.na(gold$predicted)]
      }
      
      print(paste("Using",length(ref_id),"examples to predict"))
      private$classify_machine$predict(ref_id)
    },
    
    train_and_evaluate_cross = function(ref_id = NULL, k = 10, positive_class = NULL)
    {
      if(is.null(ref_id))
      {
        gold <- private$classify_machine$get_gold()
        ref_id <- gold$ref_id[!is.na(gold$gold)]
      }
      
      print(paste("Using",length(ref_id),"examples to cross validate model"))
      
      folds <- lapply(1:k,tmca.util::get_k_fold_logical_indexes, k = k, n = length(ref_id))
      
      result <- private$classify_machine$train_and_evaluate_cross(ref_id, folds,positive_class = positive_class)
      
      #Cross validate on exititng and train on optimized C
      
      #return optimized model
      
      #build cross chunks and pass to function train test
      #call testing function with cross sets from implementation
      
      #return result as string????
      
      #but how to test parameters from different classifiers within this setting????
      #just set up chunks here and test in classifier implementation
    },
    active_learn_dictionary_examples = function(dictionary = stop("Please give a quanteda dictionary"))
    {
      #DICTIONARY FORMAT dictionary <- data.table(id = integer(), word = character(), category = character())
      #tryCatch(file_in <- file(dictionary,encoding = "UTF-8"), error = function(e) stop(paste("Error Reading Dictionary:",e)))
      
      gold <- private$classify_machine$get_gold()
      
      for(entry in names(dictionary))
      {
        
        Short_dict <- list()
        Short_dict[[entry]] <- dictionary[[entry]]
        Short_dict <- quanteda::dictionary(Short_dict)
        toks <- quanteda::kwic(quanteda::corpus(private$corpus$text,docnames=private$corpus$ref_id),pattern = Short_dict,window = 5)
        gold$dict[gold$ref_id %in% toks$docname] <-  entry  
      }
      #PROCESS DICT AS CORPUS OR USE DICTIONARY MATCHER FROM QUANTEDA
     private$classify_machine$set_gold(gold)
     invisible(self)
    },
    active_learn_examples = function(strategy = "LC", batch_size = 25, positive_class = NULL, labels = NULL, verbose = T, coder = "DEFAULT")
    {
      
      gold <- private$classify_machine$get_gold()
      
      #labels <- factor(gold$gold)
      
      ref_id <- gold$ref_id[gold$set == "train" & !is.na(gold$gold)]
      
      #Add examples not belonging to class in positive_class
      other <- gold$ref_id[gold$set == "train" & is.na(gold$gold)]
      
      
      #determine if extra examples are needed from NA space
      tmp_labels <- data.table::rbindlist(gold$gold[gold$ref_id %in% ref_id])$class
      tmp_labels <- table(tmp_labels)
      
      ratio <- 0
      more_examples <-  0 - sum(tmp_labels)
      
      if(!is.na(tmp_labels[positive_class]))
      {
        ratio <- tmp_labels[positive_class]/sum(tmp_labels)
        more_examples <- 2*tmp_labels[positive_class] - sum(tmp_labels)
      }
      

      if(ratio > 0.5)
       ref_id <- c(ref_id, sample(other,more_examples))
      #else
      #  ref_id <- c(ref_id, other)
        
      # sanity checks
      if (is.null(positive_class)) {
        positive_class <- levels(labels)[2]
        message(paste0("No positive class label given. Assuming ", positive_class), immediate. = T)
      }
      
      self$train(ref_id = ref_id, positive_class = positive_class)
      
      #predict from the last train cycle
      self$predict(ref_id = gold$ref_id)
      
      #build cross chunks and pass to function train test
      
      predicted <- private$classify_machine$get_gold()$predicted
      
      if (strategy == "random") {
        # select random
        selected_queries <- sample(gold$ref_id[!is.na(predicted$predictions) & predicted$predictions == positive_class], batch_size)
        
      } else if (strategy == "LC") {
        # select least certain
        
        boundary_distances <- abs(predicted$probabilities[,positive_class] - 0.5)
        # boundary_distances <- abs(predicted_labels_u$decisionValues[, 1])
        uncertain_decisions <- order(boundary_distances)
        unset_labels <- which(is.na(gold$gold))
        uncertain_decisions <- intersect(uncertain_decisions,unset_labels)[1:batch_size]
        
        selected_queries <- gold$ref_id[uncertain_decisions]
        
      } else if (strategy == "MC") {
        # select most certain
        
        boundary_distances <- abs(predicted$probabilities[,positive_class] - 1)
        # boundary_distances <- abs(predicted_labels_u$decisionValues[, 1])
        certain_decisions <- order(boundary_distances)
        unset_labels <- which(is.na(gold$gold))
        certain_decisions <- intersect(certain_decisions,unset_labels)[1:batch_size]
        selected_queries <- gold$ref_id[certain_decisions]
        
      } else if (strategy == "DIFF") {
        # difference to last run
      }  else if (strategy == "LCB") {
        
        pp <- length(which(gold$gold == positive_class)) / length(which(!is.na(gold$gold)))#ONLY GOLD SECURE
        pmax <- mean(c(0.5, 1 - pp))
        #predicted_labels_u <- predict(model, u_dfm, proba = T)
        prob_positive <- predicted$probabilities[, positive_class]#OK
        lidx <- prob_positive < pmax
        uncertain_decisions <- rep(0, length(predicted$predicted))
        uncertain_decisions[lidx] <- prob_positive[lidx] / pmax
        uncertain_decisions[!lidx] <- (1 - prob_positive[!lidx]) / (1 - pmax)
        
        # order and select
        #uncertain_decisions <- order(uncertain_decisions, decreasing = T)[1:batch_size]
        
        uncertain_decisions <- order(uncertain_decisions, decreasing = T)
        unset_labels <- which(is.na(gold$gold))
        uncertain_decisions <- intersect(uncertain_decisions,unset_labels)[1:batch_size]
        
        selected_queries <- gold$ref_id[uncertain_decisions]
        
      # } else if (strategy == "LCBMC") {
      #   # browser()
      #   pp <- sum(gold$gold == positive_class) / length(which(!is.na(gold$gold)))#ONLY GOLD SECURE
      #   pmax <- mean(c(0.5, 1 - pp))
      #   #predicted_labels_u <- predict(model, u_dfm, proba = T)
      #   prob_positive <- predicted$probabilities[, positive_class]#OK
      #   lidx <- prob_positive < pmax
      #   current_uncertain_decisions <- rep(0, length(predicted$predicted))
      #   current_uncertain_decisions[lidx] <- prob_positive[lidx] / pmax
      #   current_uncertain_decisions[!lidx] <- (1 - prob_positive[!lidx]) / (1 - pmax)
      #   
      #   w0 <- 1 / length(which(!is.na(gold$gold)))
      #   uncertain_decisions <- current_uncertain_decisions - w0 * last_AL_uncertainty
      #   last_AL_uncertainty <<- current_uncertain_decisions
      #   
      #   # order and select
      #   uncertain_decisions <- order(uncertain_decisions, decreasing = T)[1:batch_size]
      #   selected_queries <- gold$ref_id[uncertain_decisions]
      #   
      } 
      else {
        stop("Unknown query selection strategy")
      }
      
      #Istead present all labels for other choices
      #labels <- as.character(private$classify_machine$get_gold()$predicted$predictions[selected_queries])
      # oracle
      oracle_decisions <- self$query_oracle_human(selected_queries, labels, positive_class, coder)
      
      if (verbose > 1) {
        cat("Selected queries:\n")
        cat(paste(oracle_decisions, "-", private$corpus[selected_queries]$text, "\n"))
      }
      
      return(list(
        selected_queries = selected_queries,
        oracle_decisions = oracle_decisions
      ))
    },
    query_oracle_human = function(ref_id, labels, positive_class, coder = "DEFAULT") {
      
      viewer <- getOption("viewer")
      
      if(!("not_relevant" %in% labels))
        labels <- c(labels,"not_relevant")
      
      decisions <- list()
      
      d <- 1
      
      while(d <= length(ref_id))
      {
        doc_id <- ref_id[d]
       
        doc_id_idx <- which(private$corpus$ref_id == doc_id)
        
        if (!is.null(viewer)) {
          
          doc_html <- paste0("<b>", as.character(private$corpus$text[doc_id_idx]),"</b>")
          
          if(length(private$corpus$text) > doc_id_idx)
          {
            doc_html <- paste0(doc_html, "<br/><br/>",as.character(private$corpus$text[doc_id_idx + 1]))
          }
          
          if(doc_id_idx > 1)
          {
            doc_html <- paste0(as.character(private$corpus$text[doc_id_idx - 1]),"<br/><br/>",doc_html)
          }
          
          
          doc_html <- gsub("\n", "<br/>", doc_html)
          
          final_html <- ""
          
          final_html <- paste0(final_html, "<p>", doc_html , "</p>")
          
          htmltools::html_print(htmltools::HTML(paste0("<h2>ID ", doc_id, "</h2>", final_html)))
        } else {
          cat("ID", doc_id, "\n", "-----------------", as.character(private$corpus$text[doc_id_idx]), "\n")
        }
        #REDO IF SELECTION IS BACKWARD FUNCTION TODO
        
        my_decision <- self$read_human_decision(labels = labels,positive_class = positive_class)
        
        if(my_decision == 999)
        {
          #Only reset of as least 1 element has past
          if(d > 1)
            d <- d - 1
        } else
        {
          decisions[[d]] <- data.table(class = labels[my_decision], coder = coder, timestamp = Sys.time())
          d <- d + 1
        }
        
      }
      
      
      return(decisions)
    },
    read_human_decision = function(show_label_legend = T, labels, positive_class) {
      
      if (show_label_legend) {
        cat("\nInsert 999 for Undo to last example\n")
        for (i in 1:length(labels)) {
          if(labels[i] == positive_class)
            cat(paste0("   [", i, "] ", "!",labels[i],"! - Class to train", "\n"))
          else
            cat(paste0("   [", i, "] ", labels[i], "\n"))
        }
      }
      
      n <- readline(prompt="Label for document: ")
      
      n <- as.integer(n)
      if (!is.na(n) & (n == 999)) {
        print("Undo")
        return(n)
        break()
      }
      
      if (is.na(n) | !is.na(n) & (n < 1 | n > length(labels))) {
        abort <- readline(prompt = "Could not interpret selection. Abort? [yes|no]: ")
        if (tolower(substr(abort, 0, 1)) == "y") {
          stop("Aborted\n")
        } else {
          n <- self$read_human_decision(labels = levels(labels), positive_class = positive_class)
        }
      }
      
      return(n)
    },
    query_oracle_experiment = function(idx) {
      return(gold_labels[idx])
    },
    stop_criterion_matches = function(v, window = 2, threshold = 0.99) {
      "Stopping criterion for active learning: Stability (see Bloodgood; Vijay-Shanker 2009)"
      b <- v[!is.na(v)] > threshold
      if (length(b) < window) return(0)
      r <- sapply(1:(length(b) - window + 1), FUN = function(x) {
        if (all(b[x:(x + window - 1)])) {
          T
        } else {
          F
        }
      })
      if (!any(r)) {
        return(0)
      }
      return(which(r)[1] + window - 1)
    },
    active_learning = function(
      # corpus,
      # dfm,
      # labels,
      batch_size = 25L,
      strategy = "LC",
      max_iterations = 200,
      tune_C = FALSE,
      cross = NULL,
      stop_threshold = 0.99,
      stop_window = 3,
      type = 7,
      verbose = TRUE,
      labels = NULL,
      positive_class = NULL,
      from_dictionary_hits = F,
      validation_class = NULL,
      coder = "DEFAULT"
    ) {
      "Active learning for classification. If gold labels are present,
      experiment mode is conducted. Otherwise, the user oracle is asked
      to decide on selected examples."
      
      #DEtermine how to initialize
      if(is.null(private$iteration))
      {
        private$iteration <- setNames(replicate(length(labels),1,simplify = F),labels)
      }
      
      if(is.null(private$progress))
      {
        d <- data.frame(row.names = F, stringsAsFactors = F)
        private$progress <- setNames(replicate(length(labels), d, simplify = FALSE),labels)
      }
      
      if(is.null(private$progress))
      {
        d <- list()
        private$progress_examples <- setNames(replicate(length(labels), d, simplify = FALSE),labels)
      } 
      
      if(is.null(private$progress))
      {
        d <- data.frame(col.names = F, row.names = F, stringsAsFactors = F)
        private$progress_validation <- setNames(replicate(length(labels), d, simplify = FALSE),labels)
        
      } 
      
      if(batch_size < 25)
      {
        print("Batch size less than 25. Setting batch size to 25.")
        batch_size <- 25
      }
      
      gold <- private$classify_machine$get_gold()
      
      do_again <- T
      
      if(from_dictionary_hits)
      {
        while(do_again)
        {
          ref_id <- gold$ref_id[gold$dict == positive_class & !is.na(gold$dict)]
          selected_queries <- sample(ref_id,batch_size)
          #labels <- gold$dict[selected_queries]
          
          oracle_decisions <- self$query_oracle_human(selected_queries, labels, positive_class, coder)
          gold$gold[match(selected_queries,gold$ref_id)] <- oracle_decisions
          
          
          abort <- readline(prompt = "Evaluate more pattern based examples? [yes|no]: ")
          if (tolower(substr(abort, 0, 1)) != "y") {
            
            do_again <- F
            print("Pattern evaluation aborted!")
            

            
          }
          
         
          #Check if an example was selected for positive_class
          #If not need to start over or select another class
          if(sum(rbindlist(oracle_decisions)$class == positive_class) < 1)
          {
            do_again <- T
            print("There is no example for the positive_class in the gold standard. Please repeat pattern match to find some or abort!")
          }
          #reset do again
        }
        
      }
      
      private$classify_machine$set_gold(gold)
      
      #previous_predictions <- factor(rep(levels(labels)[1], length(labels)), levels = levels(labels))
      #repolaced by prediction and gold column If prediction ist equal to gold - we are fine
      
      stop_active_learning <- FALSE
      
     
      
      while (!stop_active_learning) {
        
        #Train on gold, predict on unknown
        
        #FORCE TO FALSE AFTER ONE INTERATION?
        oracle <- self$active_learn_examples(positive_class = positive_class, strategy = strategy, labels = labels, batch_size = batch_size, coder = coder)
        
        #CREATE THE LABELS FROM ACTIVE LEARNING
        gold <- private$classify_machine$get_gold()
        gold$gold[match(oracle$selected_queries,gold$ref_id)] <- oracle$oracle_decisions
        private$classify_machine$set_gold(gold)
        gold <- private$classify_machine$get_gold()
        #SET GOLD
        
        #predict from the last train cycle ALL ID WITH EXISTING MODEL FROM AL TRAINING
        # Still in memory from active learning prediction
        #private$classify_machine$predict(ref_id = gold$ref_id)
        
        #Get LAst Prediction
        predicted_labels_su <- gold$predicted$predictions
        
        # simple statistics
        n_pos <- sum(predicted_labels_su == positive_class, na.rm = T)
        n_neg <- sum(predicted_labels_su != positive_class, na.rm = T)
        
        eval_metrics <- data.frame(private$iteration[[positive_class]], n_pos, n_neg, stringsAsFactors = F)
        
        # Stopping
        if (private$iteration[[positive_class]] == 1) {
          stabililty <- 0
        } else {
          stabililty <- irr::kappa2(cbind(predicted_labels_su, previous_predictions))$value
        }
        
        previous_predictions <- predicted_labels_su
        
        eval_metrics <- cbind(eval_metrics, stabililty)
        
        #SElect all no null labels from gold
        
        # evaluation metrics (A, P, R, S, kappa, alpha)
        
        if (is.numeric(cross)) {
          #KORRIGIEREN MIT EIGENER DATENSTRUKTUR
          c_ref_id <- gold$ref_id[!is.na(gold$gold) & gold$set == "train"]
          
          cv_res <- self$train_and_evaluate_cross(k = cross,ref_id = c_ref_id, positive_class = positive_class)
          
          #cv_res <- cross_validation(s_dfm, s_labels, n_folds = cross, cost = optimal_c, type = type, positive_class = positive_class)
          cv_res <- as.data.frame(cv_res)
          eval_metrics <- cbind(eval_metrics, t(cv_res))
        }
        
        private$progress[[positive_class]] <- rbind(private$progress[[positive_class]], eval_metrics)
        rownames(private$progress[[positive_class]]) <- NULL
        
        private$progress_examples[[positive_class]] <- c(private$progress_examples[[positive_class]], list(oracle$selected_queries))
        
        # validation set evaluation
        #TODO: IMPLEMENT DATASTRUCTURE FOR VALIDATION
        if (!is.null(validation_class)) {
          
          idx <- gold$ref_id[gold$set == validation_class]
          
          
          gold_table <- data.table::rbindlist(gold$gold[idx])
          validation_labels <- gold_table$class
          
          if(!is.null(positive_class))
          {
            validation_labels[validation_labels != positive_class] <- "NEG"
            validation_labels[is.na(validation_labels)] <- "NEG"
            
          }
          
          # GET IDX FOR VALIDATION SET
          #Train again
          #UMBAU auf 2 Klassen????
          self$train(ref_id = gold$ref_id[!is.na(gold$gold) & gold$set == "train"], positive_class = positive_class, from_dictionary_hits = F)
          self$predict(ref_id = idx)
          
          
          v_predicted <- as.character(private$classify_machine$get_gold()$predicted$prediction)
          v_result <- tmca.util::F.measure(v_predicted, validation_labels, positiveClassName = positive_class)
          private$progress_validation[[positive_class]] <- rbind(private$progress_validation[[positive_class]], v_result)
          colnames(private$progress_validation[[positive_class]]) <- names(v_result)
          
          if (verbose) {
            print(tail(private$progress[[positive_class]], 1))
            if (length(validation_labels) > 0) {
              print(tail(private$progress_validation[[positive_class]], 1))
            }
          }
          
        } 
        
        # bind together on eval metrics
        # 
        # plot progress
        # 
        # !!!!!!!!!Choose Validation Set; Set test set
        # Same population, different population
        
       
        
        
        if (self$stop_criterion_matches(private$progress[[positive_class]]$stabililty, threshold = stop_threshold, window = stop_window)) {
          message("Stability criterion reached. Stopping active learning.")
          stop_active_learning <- TRUE
        }
        
        if (private$iteration[[positive_class]] >= max_iterations) {
          message("Max iterations reached. Stopping active learning.")
          stop_active_learning <- TRUE
        }
        
        if (sum(!is.na(gold$gold)) == length(gold$gold)) {
          message("No new examples to learn. Stopping active learning.")
          stop_active_learning <- TRUE
        }
        
        private$iteration[[positive_class]] <- private$iteration[[positive_class]] + 1
        
      }
    },
    train_and_evaluate_sets = function()
    {
      
    }
    
    )
      )



# A 'static' environment function to print out the registered function names
# [ToDo]: Perhaps a Description for each name?
#
cmodel$list_methods <- function() {
  print (names(register))
}

cmodel$list_parameters = function(method = ""){
  return (names(register[[ method ]]$private_fields$parameters))
}