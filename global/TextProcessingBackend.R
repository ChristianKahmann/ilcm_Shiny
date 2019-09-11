####################################################################################
#' Implementation of Textprocessing Backend. Abstraction Class.
#'
#' @author Janos Borst
#' @description Abstraction Class for Backends. Defines the functions that must be implemented
#' Base class around different abtractions of textprocessing and analysis libraries like quanteda, or spacy
#' @format R6Class
#' @slot name Slot to set for the implemented backend.
#' @export

TextProcessingBackend = R6::R6Class(
  "TextProcessingBackend",
  private = list(
    name = "abstraction",
    logfile = NULL,
    
    printlog = function(message) {
      "Function that either prints out the message or logs to file"
      if (is.null(private$logfile)) {
        print(message)
      } 
      
      else if(private$logfile=="silent"){
        #Do nothing
        return()
      }
      else{
        # if logfile is anything else than silent or NULL then try to write to that file
        message <- paste(Sys.time(), message, sep = ": ")
        write(message, file = private$logfile, append = T)
      }
    }
  ),
  public = list(
    get_name = function() {
      "Returns the name of the current backend"
      print(private$name)
      return(private$name)
    },
    logging = function(o = NULL) {
      private$logfile = o
    },
    validControlArgument = function(control) {
      " Sanity Checks for the control argument. Should be Named List."
      if (!is.list(control)) {
        stop("Control ist not seem to be a list.")
      }
      
      if (!length(names(control)) == length(control)) {
        stop("Control does not seem to be a named list?")
      }
      
      invisible(control)
      
    },
    validOptionsForBackend = function(control) {
      "Function checks for the general input control"
      if (length(setdiff (names(control), names(private$available_options))) != 0) {
        stop(
          paste0(
            "Control has unavailable arguments for ",
            private$name,
            " Backend: \n Available options: ",
            paste(names(private$available_options), collapse = ", "),
            "\n No option: " ,
            paste(setdiff (
              names(control), names(private$available_options)
            ), collapse = ", ")
          )
        )
      }
      
      invisible(control)
    },
    
    
    createPipeline = function(control = list()) {
      "This function updates the Parameters of the functions to be executes with the parameters from the control element, thus fixing the pipeline."
      private$available_options = modifyList(private$available_options, control)
    },
    executePipeline = function() {
      cat("Executes the the previously created pipeline. Implemented in Backend")
    },
    input = function() {
      cat(
        "Takes a Tibble input and converts it to the preffered format. Sould be implemented for specific Backend."
      )
    },
    get_options = function() {
      "Returns the available options for the backend."
      return(private$available_options)
    }
    
  )
)



#' ####################################################################################
#' #' Implementation of Quanteda Backend
#' #'
#' #' @author Janos Borst
#' #' @description Implementation Class for Quanteda Backend. Defines the functions that must be implemented
#' #' @format R6Class
#' #' @slot name Slot to set for the implemented backend.
#' #' @export
#'
#'
QuantedaBackend = R6::R6Class(
  "QuantedaBackend",
  inherit = TextProcessingBackend,
  private = list(
    name = "Quanteda",
    available_options =   list(
      tokenize = NULL,
      sentence_as_documents = F,
      language = "english",
      ngrams = NULL,
      stem = F,
      remove_stopwords = F,
      remove_numbers = F,
      remove_punctuation = F,
      remove_hyphenation = F,
      remove_custom = NULL,
      tolower = F,
      prune = list(
        min_termfreq= NULL,
        max_termfreq= NULL,
        min_docfreq=NULL, 
        max_docfreq=NULL,
        termfreq_type=c("count","prop", "rank", "quantile"),
        docfreq_type=c("count","prop", "rank", "quantile")
      ),
      tfidf=F,
      char_length = NULL,
      vocabularysize=NULL
    ),
    tokenize = function(x) {
      if (quanteda::is.tokens(x)) {
        private$printlog("Already tokenized")
        private$available_options$tokenize = NULL
        return(x)
      }
      
      
      if (!is.null(private$available_options$tokenize)) {
        private$printlog("Tokenizing")
        quanteda::tokens(
          x,
          what = private$available_options$tokenize,
          remove_symbols = TRUE,
          remove_numbers = private$available_options$remove_numbers,
          remove_punct = private$available_options$remove_punctuation,
          remove_hyphens = private$available_options$remove_hyphenation,
          include_docvars = T
        )
      } else{
        return(x)
      }
    },
    
    sentence_as_documents = function(x) {
      if (private$available_options$sentence_as_documents) {
        n1 = quanteda::ndoc(x)
        private$printlog("(quanteda) Reshaping to sentences")
        x %<>% quanteda::corpus_reshape(to = "sentence")
        n2 = quanteda::ndoc(x)
        
        private$printlog(paste(" ---> (Docs:",n1, "to sentences: ",n2,")"))
        return()
      } else{
        return(x)
      }
      
      
    },
    
    remove_stopwords = function(x) {
      if (private$available_options$remove_stopwords) {
        private$printlog(paste("(quanteda) Removing Stopwords"))
        n1 = sum(quanteda::ntoken(x))
        x = quanteda::tokens_remove(x,quanteda::stopwords(private$available_options$language))
        n2 = sum(quanteda::ntoken(x))
        private$printlog(paste(" ---> Removed", n1-n2, "tokens"))
        private$printlog(paste(" ---> Docs:",quanteda::ndoc(x),", Features: ",length(quanteda::types(x))))
        return(x)
      } else{
        return(x)
      }
    },
    remove_special_symbols = function(x) {
      # If tokenize was executed the removal of number, punctuation already taken care of
      
      if (!is.null(private$available_options$tokenize)) {
        return(x)
      }
      
      regex <- c()
      printstring = c()
      if (private$available_options$remove_numbers){
        # private$printlog("Removing numbers")
        regex <- c(regex, "^[\\p{N}]+$")
        printstring= c("numbers",printstring)
      }
      if (private$available_options$remove_punctuation){
        # private$printlog("Removing punctuation")
        regex <- c(regex, "^[\\p{P}\\p{S}]+$")
        printstring= c("punctuation",printstring)
      }
      if (private$available_options$remove_hyphenation){
        # private$printlog("Removing hyphenation")
        regex <- c(regex, "^-$")
        printstring= c("hyphenation",printstring)
      }
      
      private$printlog(paste0("(quanteda) Removing",paste(printstring,collapse=", ")))
      if (length(regex)) {
        n1 = sum(quanteda::ntoken(x))
        x = x %>% quanteda::tokens_remove(
          paste(regex, collapse = '|'),
          valuetype = 'regex',
          padding = FALSE
        )
        n2 = sum(quanteda::ntoken(x))
        private$printlog(paste(" ---> Removed", n1-n2, "tokens"))
        private$printlog(paste(" ---> Docs:",quanteda::ndoc(x),", Features: ",length(quanteda::types(x))))
        return(x)
      }
      else{
        return(x)
      }
      
      
    },
    
    char_length = function(x) {
      if (!is.null(private$available_options$char_length)) {
        private$printlog("(quanteda) Constraining token length")
        if (is.numeric(private$available_options$char_length) &
            length(private$available_options$char_length) == 2) {
          n1 = length(quanteda::types(x))
          x =
            x %>% quanteda::tokens_remove(
              min_nchar = private$available_options$char_length[1],
              max_nchar = private$available_options$char_length[2]
            )
          n2 = length(quanteda::types(x))
          private$printlog(paste(" ---> Removed", n1 - n2, "tokens"))
          private$printlog(paste(" ---> Docs:",quanteda::ndoc(x),", Features: ",length(quanteda::types(x))))
          return(x)
          
        } else{
          warning("char_length option not understood.")
          return(x)
        }
      } else{
        return(x)
      }
      
    },
    
    prune = function(x) {
      "Wrapper for quanteda pruning"
      # browser()
      
      
      if (length(unlist(private$available_options$prune)) > 0) {
        
        if (!quanteda::is.dfm(x)) {
          private$printlog("Pruning option was set so format is coeerced to quanteda::dfm")
          x = quanteda::dfm(x)
        }
        private$printlog(paste("(quanteda) Pruning: (Docs: ", quanteda::ndoc(x) ,", Features: ",quanteda::nfeat(x), ")"))
        parameters_pruning<-private$available_options$prune
        parameters_pruning[["x"]]<-x
        x = do.call(quanteda::dfm_trim, parameters_pruning)
        private$printlog(paste(" ---> (Docs:", quanteda::ndoc(x) ,", Features: ",quanteda::nfeat(x), ")"))

        
        return(x)
      }
      else{
        return(x)
      }
      
      
      
      
    }
    ,
    ngrams = function(x) {
      if (!is.null(private$available_options$ngrams)) {
        private$printlog("(quanteda) Building ngrams")
        n1 = length(quanteda::types(x))
        x = quanteda::tokens_ngrams(
          x,
          n = private$available_options$ngrams,
          skip = 0L,
          concatenator = "_"
        )
        n2 = length(quanteda::types(x))
        private$printlog(paste(" ---> Created",n2-n1, "new features by ngrams"))
        return(x)
      } else{
        return(x)
      }
      
    },
    
    stem = function(x) {
      if (private$available_options$stem) {
        if (quanteda::is.tokens(x)) {
          private$printlog("(quanteda) Stemming tokens object")
          quanteda::tokens_wordstem(x, language = private$available_options$language)
        }
        else if (quanteda::is.dfm(x)) {
          private$printlog("(quanteda) Stemming dfm object")
          quanteda::dfm_wordstem(x, language = private$available_options$language)
        } else{
          warning("Format not recognized for stemming")
        }
      } else{
        return(x)
      }
      
    },
    casing = function(x) {
      if (private$available_options$tolower) {
        private$printlog("(quanteda) To lower case")
        if (quanteda::is.tokens(x)) {
          return(x %>% quanteda::tokens_tolower())
        }
        else if (quanteda::is.dfm(x)) {
          return(x %>% quanteda::dfm_tolower())
        }
        else{
          return(x)
        }
      } else{
        return(x)
      }
    },
    remove_custom = function(x) {
      if (!is.null(private$available_options$remove_custom)) {
        private$printlog("(quanteda) Removing custom list")
        # browser()
        if (is.character(private$available_options$remove_custom) |
            is.list(private$available_options$remove_custom)) {
          x = x %>% quanteda::tokens_remove(private$available_options$remove_custom)
          private$printlog(paste(" ---> Features:",length(quanteda::types(x))))
          return(x)
        }
      } else{
        return(x)
      }
    },
  tfidf = function(x){
    if (private$available_options$tfidf) {
      private$printlog("(quanteda) Weighting by tfidf")
      if (!quanteda::is.dfm(x)) {
        private$printlog("tfidf option was set so format is coeerced to quanteda::dfm")
        x = quanteda::dfm(x)
      }
      return(quanteda::tfidf(x))
    }else{
      return(x)
    }
  }, 
  set_vocabularysize = function(x) {
    "Wrapper for quanteda pruning"
    # browser()
    
    
    if (!is.null(private$available_options$vocabularysize)) {
      private$printlog("(quanteda) Constraining vocabularysize")
      if (!quanteda::is.dfm(x)) {
        private$printlog("Vocabularysize was set format is coeerced to quanteda::dfm")
        x = quanteda::dfm(x)
      }
      # browser()
      x = head(quanteda::dfm_sort(x, decreasing = TRUE, margin = c("features")),n = quanteda::ndoc(x), nf=2)
      private$printlog(paste(" ---> Features:",length(quanteda::types(x))))
      return(x)
      
      
    }
    else{
      return(x)
    }
    }
    
  ),
  public = list(
    input = function(x) {
      
      if ("spacyr_parsed" %in% class(x)) {
        private$printlog("Transforming spacy_parsed object to quanteda tokens")
        if (tibble::has_name(x, "document")) {
          tmp = quanteda::as.tokens(x)
          quanteda::docvars(tmp) = tibble::as.tibble(unique(x[, which(names(x) %in% c("doc_id", "document"))]))
          return(tmp)
        } else{
          return(quanteda::as.tokens(x))
        }
        
        
      }
      else{
        return(x)
      }
    },
    executePipeline = function(x) {
      # browser()
      x %>%
        private$sentence_as_documents() %>%
        private$tokenize() %>%
        private$casing() %>%
        private$remove_special_symbols() %>%
        private$remove_custom() %>%
        private$ngrams() %>%
        private$remove_stopwords() %>%
        private$char_length() %>%
        private$stem() %>%
        private$prune() %>%
        private$tfidf() %>%
        private$set_vocabularysize()
    }
  )
  
)




####################################################################################
#' Implementation of Spacy Backend
#'
#' @author Janos Borst
#' @description Implementation Class for Tidytext Backend. Implements the pipline functions
#' @format R6Class
#' @slot name Slot to set for the implemented backend.
#' @export


SpacyrBackend = R6::R6Class(
  "SpacyrBackend",
  inherit = TextProcessingBackend,
  private = list(
    name = "spacyr",
    available_options =   list(
      language = "english",
      lemma = F,
      entity = F,
      pos = F,
      sentence_as_documents = F
    ),
    parse = function(x){
      # browser()
      if(!("spacyr_parsed" %in% class(x))){
        private$printlog("(spacyr) Spacy parsing")
        return(
          x %>% spacyr::spacy_parse(
            pos = private$available_options$pos,
            lemma = private$available_options$lemma,
            entity = private$available_options$entity
          )
        )
      }else{
        return(x)
      }
      
    },
    
    
    sentences_as_documents = function(x) {
      if (private$available_options$sentence_as_documents) {
        private$printlog("(spacyr) Spacy reshaping to sentences")
        # browser()
        ndocs = length(unique(x$doc_id))
        x$document = x$doc_id
        x$doc_id = paste(x$doc_id, x$sentence_id, sep = ".")
        nsent  = length(unique(x$doc_id))
        private$printlog(paste(" ---> (Docs: ",ndocs, "to sentences: ",nsent,")"))
        return(x)
      } else{
        return(x)
      }
    }
  ),
  public = list(
    get_name = function() {
      print(private$name)
      return(private$name)
    },
    input = function(x) {
      x
    },
    
    
    executePipeline = function(x) {
      # spacyr::spacy_initialize()
      private$printlog("Executing spacyr_parse")
      s =   x%>% private$parse() %>% private$sentences_as_documents()
      
      # spacyr::spacy_finalize()
      s
    }
    
  )
)
