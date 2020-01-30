# Andreas Niekler 2018
# NLP Group
# Leipzig University

##########################################################################################
#
# Test the registered algorithms that are part of the package for defined behaviour including:
#  * correct inheritance
#  * correct parameter_sets
#  * correct input and output format
#  not testing for content (hoping the used packages do that, and no idea how to test the content ;))
#
# Tests could also be run manually after a new algorithm was registered. it checks all the elements of "register"
##########################################################################################
testthat::test_that("registered_algorithms inheritance", {
  # Test if all the registered algorithm class fullfill the basic necesseties
  for (funcname in names(tmca.classify::classify.register)){
    x = tmca.classify::register[[funcname]]
    testthat::expect_match( x$get_inherit()$classname, "classifier_abstr", info=funcname)
  }
  
})

testthat::test_that("input corpus Liblinear algorithm", {
  
  library(tmca.util)
  # If the parameter_set method exposes the expected behaviour for all registered algorithms.
  #x = tmca.unsupervised::register[["LiblineaR::LiblineaR"]]$new()
  x <- cmodel$new(method = "LiblineaR::LiblineaR")
  x$.__enclos_env__$private$silent = T
  testthat::expect_failure(testthat::expect_null(x$.__enclos_env__$private$tm_machine))
  
  #insert dtm
  path_to_test_data = paste(system.file(".",package="tmca.classify"),"/test/testthat/class_trec_test.rds",sep = "")
  test_data = readRDS(path_to_test_data)
  
  #PRoduce token sequence
  test_processed = process_text_sequence(data = test_data,ngram = 1:2)
  colnames(test_processed) <- c("id","feature")
  test_processed <- test_processed[!(test_processed$feature %in% deleteSpecialCharacters(c(tm::stopwords("en"),quanteda::stopwords()))),]
  
  colnames(test_processed) <- c("id","feature")
  test_processed$ref_id = test_processed$id
  
  num_documents = length(unique(test_processed$id))
  num_words = length(levels(factor(test_processed$feature)))
  
  #check internal format
  my_gold <- data.table::data.table(ref_id = unique(test_processed$ref_id))
  
  my_gold$gold <- sapply(stringi::stri_split(test_data$category,regex=":"),function(x) x[1])
  my_gold$dict <- sapply(stringi::stri_split(test_data$category,regex=":"),function(x) x[1])
  my_gold$predicted <- sapply(stringi::stri_split(test_data$category,regex=":"),function(x) x[1])
  my_gold$set <- sapply(stringi::stri_split(as.character(test_data$source),regex="\\."),function(x) x[2])
  my_gold$set[my_gold$set == "test"] <- "validation"
  
  x$set_input(corpus = data.table(ref_id = unique(test_processed$ref_id), text = test_data),features = test_processed, gold = my_gold)
  processed_dtm <- x$get_input()
  
  testthat::expect_equal(dim(processed_dtm)[1], num_documents)
  testthat::expect_equal(dim(processed_dtm)[2], num_words)
  
  #Seting TFIDF afterwars must be possible after input -- Store full matrix process when running!!!!!!!!!!!!!!!!!!!!
  x$set_parameters(list(tfidf=F,chi2selector=F, prune =F,type=0))
  
  result <- x$train_and_evaluate_cross(k = 10)
  
})

testthat::test_that("input corpus Liblinear algorithm", {
  
  library(tmca.util)
  # If the parameter_set method exposes the expected behaviour for all registered algorithms.
  #x = tmca.unsupervised::register[["LiblineaR::LiblineaR"]]$new()
  x <- cmodel$new(method = "LiblineaR::LiblineaR")
  x$.__enclos_env__$private$silent = T
  
  #check internal format
  my_gold <- list() #data.table::data.table(ref_id = unique(test_processed$ref_id))
  
  my_gold$ref_id <- unique(test_processed$ref_id)
  
  my_gold$gold <- sapply(stringi::stri_split(test_data$category,regex=":"),function(x) x[1])
  my_gold$dict <- rep(NA,length(my_gold$ref_id))
  my_gold$predicted <- rep(NA,length(my_gold$ref_id))
  my_gold$set <- sapply(stringi::stri_split(as.character(test_data$source),regex="\\."),function(x) x[2])
  my_gold$set[my_gold$set == "test"] <- "validation"
  
  my_gold$gold[my_gold$set != "validation"] <- NA
  
  x$set_input(corpus = data.table(ref_id = unique(test_processed$ref_id), text = test_data$text),features = test_processed, gold = my_gold)
  
  dictionary <- quanteda::dictionary(list(DESC = c('how* can* i*'),
                                          HUM = c('who**'),
                                          NUM = c('how* many*'),
                                ENTY = c('what*')))
  
  x$active_learn_dictionary_examples(dictionary = dictionary)
  
  #TEST FOR DICT DICTIONARY PENDING
  
  
  x$active_learning(
    strategy = "LCB", 
    batch_size = 10, 
    positive_class = "HUM", 
    from_dictionary_hits = T,
    labels = unique(sapply(stringi::stri_split(test_data$category,regex=":"),function(x) x[1])),
    validation_class = "validation",
    cross = 10
  )
  
  
})

testthat::test_that("parameter_set function", {
  # If the parameter_set method exposes the expected behaviour for all registered algorithms.
  for (funcname in names(tmca.unsupervised::register)){
    #Get an instance of that algorithm
    x = tmca.unsupervised::register[[ funcname ]]$new()
    #Get the available parameter_set for the registered algorithm
    par_names = x$get_available_parameters()

    #assign a parameter set
    parset = list()
    for(par in par_names){
      parset[[ par ]] = 1
    }

    # set all parameters (no error)
    testthat::expect_identical(parset,x$set_parameters(parset)$.__enclos_env__$private$parameters)
    testthat::expect_error(x$set_parameters(parset), NA)
    testthat::expect_warning(x$set_parameters(parset), NA)
    
    # set subset (Warn about missing parameters but no error)
    subset = parset [ par_names[1] ]
    testthat::expect_warning(x$set_parameters(subset))
    testthat::expect_error(x$set_parameters(parset), NA)
    
    # set wrong parameter (error)
    parset[[ "i_hope_this_never_will_be_a_parametername" ]] = 1
    expect_error(x$set_parameters(parset))

  }
  
})

# Ensure the basic functionality of an implemented algorithm. 
# The four implemented methods should set certain variales and have spcific output format.

testthat::test_that("functionality, variable names and input_output formats of registered algorithms",{
  path_to_test_data = paste(system.file(".",package="tmca.unsupervised"),"/tests/testthat/unsupervised_20_docs_test.rds",sep = "")

  test_data = readRDS(path_to_test_data)
  test_dtm = tmca.util::deleteStopwordsFromDTM(tmca.util::process_dtm(data=test_data))
  num_documents = dim(test_dtm)[1]
  num_words = dim(test_dtm)[2]
  
  for (funcname in names(tmca.unsupervised::register)){
    # Get an instance of that algorithm
    x = tmca.unsupervised::register[[ funcname ]]$new()
    
    #this should set an internal representation
    x$input(input_matrix=test_dtm)
    testthat::expect_failure(testthat::expect_null(x$.__enclos_env__$private$internal_representation))

    #this should set an internal model
    x$call()
    testthat::expect_failure(testthat::expect_null(x$.__enclos_env__$private$model))
    
    
    #Test the output format
    l = x$output()

    
    #Test the output format for dimensionality and name
    testthat::expect_equal(names(l),c("theta","phi"))
    testthat::expect_equal(dim(l$theta)[1], num_documents)
    testthat::expect_equal(dim(l$phi)[2], num_words)
    testthat::expect_equal(dim(l$phi)[1], dim(l$theta)[2])
    

    testthat::expect_equal( unname(rowSums(l$phi))  , rep(1,dim(l$phi)[1]) )
    testthat::expect_equal(unname(rowSums(l$theta)), rep(1,num_documents ) )
    
    # Formats for inferred on previously unseen text 
    inferred_topics = x$infer_topics(dtm = test_dtm) 
    testthat::expect_equal(dim(inferred_topics)[1], num_documents)
    testthat::expect_equal(dim(inferred_topics)[2], dim(l$theta)[2])
  }
})


##########################################################################################
#
# Test the tmodel class for expected behaviour (with any of the registered algorithms?):
#  * Think of good tests. The class mainly wraps the methods of the above tested implementations...
##########################################################################################


testthat::test_that("Test tmodel workflow",{
  
  path_to_test_data = paste(system.file(".",package="tmca.unsupervised"),"/tests/testthat/unsupervised_20_docs_test.rds",sep = "")
  
  test_data = readRDS(path_to_test_data)
  test_dtm = tmca.util::deleteStopwordsFromDTM(tmca.util::process_dtm(data=test_data))
  num_documents = dim(test_dtm)[1]
  num_words = dim(test_dtm)[2]
  for (funcname in names(tmca.unsupervised::register)){
    # browser()
    t <- tmodel$new(method = funcname)
    t$.__enclos_env__$private$silent = T
    testthat::expect_failure(testthat::expect_null(t$.__enclos_env__$private$tm_machine))
    l = t$input_preprocessed(corpus = NULL, dtm = test_dtm)$create_tm()$get_model()
    #Test the output format for dimensionality and name
    testthat::expect_equal(names(l),c("theta","phi"))
    testthat::expect_equal(dim(l$theta)[1], num_documents)
    testthat::expect_equal(dim(l$phi)[2], num_words)
    testthat::expect_equal(dim(l$phi)[1], dim(l$theta)[2])
    
    
    testthat::expect_equal( unname(rowSums(l$phi))  , rep(1,dim(l$phi)[1]) )
    testthat::expect_equal(unname(rowSums(l$theta)), rep(1,num_documents ) )
    
    # Formats for inferred on previously unseen text 
    inferred_topics = t$infer_topics(dtm = test_dtm) 
    testthat::expect_equal(dim(inferred_topics)[1], num_documents)
    testthat::expect_equal(dim(inferred_topics)[2], dim(l$theta)[2])

  } 

})
