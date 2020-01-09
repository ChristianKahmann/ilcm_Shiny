# Janos Borst 2017
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
  for (funcname in names(tmca.unsupervised::register)){
    x = tmca.unsupervised::register[[funcname]]
    testthat::expect_match( x$get_inherit()$classname, "tm_abstr", info=funcname)
  }
  
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
