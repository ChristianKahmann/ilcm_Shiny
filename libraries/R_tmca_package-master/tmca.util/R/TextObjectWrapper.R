# Janos Borst 2017
# NLP Group
# Leipzig University

####################################################################################
#' Implementation of a Wrapper around Quanteda and Spacy backend. You can manage different options for preprocessing and use the advantages of both backends.
#'
#' @author Janos Borst
#' @description Class to manage a Textobject and in and output various data formats. Also for text processing.
#'  Wrapper around different abtractions of textprocessing and analysis libraries like quanteda, or spacyr
#'
#' @usage TextObjectWrapper$new() # for a new instance, then use any of the methods described below.
#'
#' @examples # Example definition of a Text input with metadata and two control inputs
#' text_df <- dplyr::data_frame(
#'    text = c("These are 4 example sentences.",
#'    "Nothing special is happening in this example sentences",
#'    "Every example sentence has a different date.",
#'    "The date is an example for meta information that will be kept.") ,
#'    date=(c("2018-04-09","2018-04-06","2018-04-07","2018-04-08")))
#' control1 = list() # Empty control for spacy will at least tokenize
#' control2= list(
#'    ngrams = c(1),
#'    remove_stopwords = T,
#'    tolower = T,
#'    remove_numbers  = T,
#'    char_length = c(5,10))
#'
#' # You should initialize spacy to the version you want to use
#'    spacyr::spacy_initialize()
#' # Also you need to load dplyr style piping
#'    library(dplyr)
#'
#' # You can execute these control elements with the process method
#'
#'    tow = TextObjectWrapper$new()
#' # You can specify if any output will be printed
#'    tow$logging(NULL) # Will print to console (default). If "silent" nothing will be printed, any other string will interpreted as location of a logging file
#'
#'
#'    tow$input(text_df)
#'
#' # x will contain the raw return value. spacyr_parsed in the first example quanteda::tokens in the second
#'    x = tow$process(control1, "spacyr")
#'    x = tow$process(control2, "quanteda")
#'
#' # If you need other formats you can use the output method like:
#' dfm1 = tow$output(x, format = "dfm")
#'
#' # To reset the current processing use
#' tow$reset()
#'
#' # Alternatively (if you dont need the interim results) you can define the whole process in one control element like:
#' fullcontrol = list(
#'   spacyr = control1,
#'   quanteda= control2)
#' dfm2 = tow$fullprocess(fullcontrol) %>% tow$output(format = "dfm")
#'
#' # The output dfms are equivalent ( but not necessarily in the same order inside the dfm ))
#' all.equal(sort(quanteda::featnames(dfm1)),sort(quanteda::featnames(dfm2)))
#' all.equal(sum(quanteda::ntoken(dfm1)),sum(quanteda::ntoken(dfm2)))
#'
#' # And the contain the original docvars
#' quanteda::docvars(dfm1)
#' quanteda::docvars(dfm2)
#'
#' spacyr::spacy_finalize()
#'
#'
#'
#'
#' @format R6Class
#' @slot backend_object (private) Currently used backend.
#' @slot internal_representation (private) Data in the internal format which is the current return of the backend(maybe spacyr_parsed or one of the quanteda formats
#' ).
#' @slot input_untouched (private) The object always carries a copy of the  original input data. If you use the `reset()` method your progress will be reset to this.
#'
#' @slot spacy_was_used (private) Boolean indicating whether spacyr was used. If you try to use the output method to converse into your desired format and spacy was used beforehand, then the meta information of the input needs to be merged back again.
#' @slot logfile (private) Slot indicating whether to print, do nothing, or append to a logfile.
#' @include TextProcessingBackend.R
#' @section Methods:
#' \describe{
#'   \item{\code{input()}}{Input of the raw text. This can be a data frame or a tibble or a quanteda corpus. If it is a data.frame or a qibble it should contain the douments in a column calls "text". Additional coluns will be kept as meta information.}
#'   \item{\code{logging(o = NULL)}}{If NULL (default)information will be printed to console. If "silent" then nothing will be printed. If anything other than silent or NULL, a logfile of this name is searched for and appended to. }
#'   \item{\code{get_options(name)}}{Gives you the available preprocessing options of a backend (spacyr or quanteda)}
#'   \item{\code{process(control, backend)}}{Executes a control element in combination with the choice of backend. Additionally returns the raw return value of the backend. (Can be directly piped into the 'output'- method).}
#'   \item{\code{fullprocess(control)}}{Wrapper for the process method. Allows you to define just one control element that contains control elements for both backends.}
#'    \item{\code{output()}}{Converts the current State of processing to the desired output format. At the moment possible options include:
#'     - spacyr_parsed
#'     - dfm
#'     - sparseMatrix
#'    }
#'    \item{\code{get_original_documents()}}{Get a data.frame with the concatenated documents from input. (Before processing)}
#'    \item{\code{get_meta_data()}}{If you used input_spacyr_object, You can use this to retrieve the meta data after preprocessing.(Only Current version if you called output between preocess and get_meta_data)}
#'    \item{\code{input_spacyr_object(x, meta_data=NULL)}}{Input a Spacyobject, possibly with meta_data. meta_data should be a data.frame with a column doc_id.}
#'
#' }
#'

#' @export
TextObjectWrapper <- R6::R6Class(
  "TextObjectWrapper",
  private = list(
    register = list("quanteda" = QuantedaBackend,
                    "spacyr" = SpacyrBackend),
    backend_object = NULL,
    input_untouched = NULL,
    spacy_was_used = F,
    internal_representation = NULL,
    persistent = T,
    meta_data = NULL,
    meta_data_untouched = NULL,
    available_outputs = c("dfm", "sparseMatrix", "tibble", "quanteda", "spacyr_parsed", "documents"),
    logfile = NULL,
    printlog = function(message) {
      "Function that either prints out the message or logs to file"

      if (is.null(private$logfile)) {
        print(message)
      }
      else if(private$logfile=="silent"){
        #Do nothing
        return()
      }else{
        # if logfile is anything else than silent or NULL then try to write to that file
        message <- paste(Sys.time(), message, sep = ": ")
        write(message, file = private$logfile, append = T)
      }
    }
    ),

  public = list(
    initialize = function() {
      "Nothing special to be done here."
    },
    logging = function(o = NULL) {
      "Setting the direction for the progress information."
      private$logfile = o
    },
    input = function(x, colname = "text") {
      "This function takes Raw Text Data frame input and turns into the internal representation"
      if("spacyr_parsed" %in% class(x)){
        private$input_untouched = x
      }
      else if ( is.data.frame(x) | tibble::is.tibble(x) ) {
        private$input_untouched =  quanteda::corpus(x, text_field=colname)
      }
      else if(quanteda::is.corpus(x)){
        private$input_untouched = x
      }

      else{
        warning("So far only dataframes, tibbles and quanteda::corpus supported.")
      }

      self$reset()
      private$internal_representation = private$input_untouched


    },
    input_spacyr_object = function(x, meta_data=NULL){
      private$printlog("Directly inputting a spacy object")
      private$input_untouched = x
      private$meta_data_untouched = meta_data
      self$reset()


      private$meta_data_untouched = private$meta_data
      private$internal_representation = private$input_untouched
    },
    reset = function(){
      "Reset progress to the point where you gave the input. (Lose all current progress)"
      private$backend_object=NULL
      private$spacy_was_used=F
      private$internal_representation = NULL
      private$meta_data = NULL
      gc()
      private$meta_data = private$meta_data_untouched
      private$internal_representation = private$input_untouched

    },

    get_options = function(name){
      "Return the available options for the backend `name`"
      tmp = private$register[[name]]$new()
      o = tmp$get_options()
      rm(tmp)
      return(o)
    },
    extract_whitelist = function(){
        return(saved_words)
    },
    
    process = function(control, backend) {
      "Process a control element for the specified backend. Also some sanity checks (options, backends availability..)"

      if (backend %in% c("spacyr")) {
        private$spacy_was_used = T
      }

      private$backend_object = private$register[[backend]]$new()
      private$backend_object$logging(private$logfile)
      private$backend_object$validControlArgument(control)
      private$backend_object$validOptionsForBackend(control)
      private$backend_object$createPipeline(control)

      if (private$persistent){
        private$internal_representation = private$backend_object$input(private$internal_representation) %>%
          private$backend_object$executePipeline()
        return(private$internal_representation)
      }
      else{
        return(private$backend_object$input(private$internal_representation) %>%
                 private$backend_object$executePipeline())
      }


    },

    fullprocess = function(control){
      "Wrapper around the process-method to define the whole process."
      if (private$persistent){
        for (name in names(control)){
          self$process(control[[name]], name)
        }
        return(private$internal_representation)
      }else{
        private$persistent = T
        for (name in names(control)){
          self$process(control[[name]], name)
        }
        tmp = private$internal_representation
        self$reset()
        private$persistent = F
        return(tmp)
      }
    },
    get_output_formats = function(){
      return(private$available_outputs)
    },
    get_meta_data = function(){
      return(private$meta_data)
    },
    output = function(x = private$internal_representation, format = "dfm") {
      "Most Important Methods. Converts All returns from the Backend to the desired Output format."


      if (!format %in% private$available_outputs) {
        private$printlog(paste0(
          "Outputformat not available. Choose from ",
          available_outputs
        ))
      }

      ###################################
      # Output to spacyr_parsed only from spacyr_parsed

      if(format == "spacyr_parsed"){
        if ("spacyr_parsed" %in% class(x)){
          return(x)
        }
      }




      ##############################
      #If input was spacy format already, see if there's meta_data to be merged
      # browser()
      if ("spacyr_parsed" %in% class(private$input_untouched)){
        if(!is.null(private$meta_data)){
          if (quanteda::is.tokens(x) | quanteda::is.dfm(x) | quanteda::is.corpus(x)) {


            # Check if  level of interest was set to sentences then we have to merge by another variable
            if (tibble::has_name(quanteda::docvars(x), "document")) {
              tmp = private$meta_data_untouched
              tmp$document = tmp$doc_id
              tmp$doc_id=NULL
              private$meta_data = tibble::as.tibble(merge(quanteda::docvars(x), tmp,  "document"))
              quanteda::docvars(x) =  private$meta_data
            } else{
              #if not the docvars should have the same form as the input docvars
              quanteda::docvars(x) <- private$meta_data_untouched### Questionable for now !
            }

          }
        }
      }

      # If output format not spacy but spacy was used in the process we need to merge back the docvar meta information.
      else if (private$spacy_was_used) {
        private$printlog("Spacy was used. Merging back docvars to output.")

        # if quanteda backend was used after spacy we already have a quanteda object but we have to merge back the docvars anyways.
        if (quanteda::is.tokens(x) | quanteda::is.dfm(x) | quanteda::is.corpus(x)) {


          # Check if  level of interest was set to sentences then we have to merge by another variable
          if (tibble::has_name(quanteda::docvars(x), "document")) {
            tmp = quanteda::docvars(private$input_untouched)
            tmp$document = row.names(quanteda::docvars(private$input_untouched))
            quanteda::docvars(x) =  tibble::as.tibble(merge(quanteda::docvars(x), tmp,  "document"))
          } else{
            #if not the docvars should have the same form as the input docvars
            quanteda::docvars(x) <-
              quanteda::docvars(private$input_untouched) ### Questionable for now !
          }

        }
      }

      ###################################
      # Output to dfm
      if (format == "dfm") {
        ### One of the quanteda formats
        if (quanteda::is.corpus(x) | quanteda::is.tokens(x)) {
          return(quanteda::dfm(x))
        }

        if (quanteda::is.dfm(x)) {
          return(x)
        }

        ### Spacy return
        if ("spacyr_parsed" %in% class(x)) {
          # browser()
          tmp = x %>% quanteda::as.tokens()
          quanteda::docvars(tmp) = tibble::as.tibble(unique(x[, which(names(x) %in% c("doc_id", "document"))]))


        if (tibble::has_name(quanteda::docvars(tmp), "document")) {
              tmp_docvars = quanteda::docvars(private$input_untouched)
              tmp_docvars$document = row.names(quanteda::docvars(private$input_untouched))
              quanteda::docvars(tmp) =  tibble::as.tibble(merge(quanteda::docvars(tmp), tmp_docvars,  by="document"))
            } else{
              #if not the docvars should have the same form as the input docvars
              quanteda::docvars(tmp) <-
                quanteda::docvars(private$input_untouched) ### Questionable for now !
            }
            return(tmp %>% quanteda::dfm())
          }

      }
      ###################################


      ###################################
      # Output to sparseMatrix
      if (format == "sparseMatrix") {
        if (quanteda::is.corpus(x) | quanteda::is.tokens(x)) {
          return(
            x %>% quanteda::dfm() %>% methods::as("dgCMatrix")
          )
        }
        if (quanteda::is.dfm(x) ) {
          return(
            x %>%  methods::as("dgCMatrix")
          )
        }

        if ("spacyr_parsed" %in% class(x)) {
          return(
            x %>% quanteda::as.tokens() %>% quanteda::dfm() %>% methods::as("dgCMatrix")
          )
        }

      }
      ###################################


      ###################################
      # Output to tibble
      if (format == "tibble") {
        if ("spacyr_parsed" %in% class(x)) {
          return(x %>% tibble::as.tibble())
        }
        if (quanteda::is.tokens(x)){
          return(tibble::tibble(doc_id =unlist(lapply(names(x),function(n){rep(n,length(x[[n]]))})), token = unlist(x) ))
        }
        if(quanteda::is.dfm(x)){
          return(tidytext::tidy(x))
          }

      }

      ###################################

      ###################################
      # Output to documents
      if (format == "documents") {
        if ("spacyr_parsed" %in% class(x)) {
          tmp = aggregate(token ~ doc_id, x, paste, collapse=" ")

        }
        else if(quanteda::is.corpus(x)){
          tmp = tibble::tibble(doc_id = quanteda::docnames(quanteda::corpus(tib)),text=quanteda::texts(quanteda::corpus(x)))
        }
        else if (quanteda::is.tokens(x)){
          tmp = aggregate(term ~ document,tidytext::tidy(quanteda::dfm(x)), paste, collapse=" " )

        }
        else if(quanteda::is.dfm(x)){
          tmp = aggregate(term ~ document,tidytext::tidy(x), paste, collapse=" " )

        }
        colnames(tmp )  <- c("doc_id","text")
        return(tmp)
      }

      ###################################
      warning(
        paste0(
          "Combination of backend ",
          private$backend,
          " and output format ",
          format,
          " not supported"
        )
      )

    },
    get_original_documents = function(x = private$input_untouched){

      if ("spacyr_parsed" %in% class(x)) {
        return(aggregate(token ~ doc_id, x, paste, collapse=" "))
      }

      if(quanteda::is.corpus(x)){
        return( tibble::tibble(doc_id = quanteda::docnames(quanteda::corpus(tib)),text=quanteda::texts(quanteda::corpus(x))) )
      }
      #For now only spacyr_parsed and corpus possible as input_untouched, but if you use the function external input you can also put dtms, dfms ans tokens through
      if (quanteda::is.tokens(x)){
        return(aggregate(term ~ document,tidytext::tidy(quanteda::dfm(x)), paste, collapse=" " ))
      }
      if(quanteda::is.dfm(x)){
        return( aggregate(term ~ document,tidytext::tidy(x), paste, collapse=" " ))
      }
      if(is.element("dgCMatrix", class(x))){
        return( aggregate(term ~ document,tidytext::tidy(x), paste, collapse=" " ))
      }
    }

  )
)
