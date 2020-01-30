context("TextObjectWrapper")

testthat::test_that("TextObjectWrapper Technical",{
  test_object = TextObjectWrapper$new()
    text <- c("123",
              "1","2","3")

    df = data.frame(text = text,stringsAsFactors=FALSE)


    #test that various input formats are converted to corpus in internal representation
    test_object$input(df)
    testthat::expect_true(quanteda::is.corpus(test_object$.__enclos_env__$private$internal_representation))

    test_object$input(tibble::as.tibble(df))
    testthat::expect_true(quanteda::is.corpus(test_object$.__enclos_env__$private$internal_representation))


    test_object$input(quanteda::corpus(tibble::as.tibble(df)))
    testthat::expect_true(quanteda::is.corpus(test_object$.__enclos_env__$private$internal_representation))


    #check for the default execution of
    for( backend in names(test_object$.__enclos_env__$private$register)){
      o = test_object$get_options(backend)
      l = list()
      l[[backend]] = o
      # print(l)

      testthat::expect_error(x = testthat::expect_error( test_object$process(l)))

      testthat::expect_true(quanteda::is.dfm(x %>% test_object$output(format = "dfm")))

      test_object$reset()
      testthat::expect_equal(test_object$.__enclos_env__$private$internal_representation,
                             test_object$.__enclos_env__$private$input_untouched)

    }






})

testthat::test_that("Docvars Propagation",{
  text <- c("test 123 Because I could not stop for Death. Because I could not stop for Death. test 123 Because I could not stop for Death.",
            "test2",
            "test3",
            "test4")
  text_df <- dplyr::data_frame( text = text ,
                                date=(c("2018-04-09","2018-04-06","2018-04-07","2018-04-08")),
                                source = (c("Zeit","Spiegel","Guardian","FAZ"))
  )

  docvars = dplyr::data_frame(
                               date=(c("2018-04-09","2018-04-06","2018-04-07","2018-04-08")),
                               source = (c("Zeit","Spiegel","Guardian","FAZ"))
  )

  TOW =  TextObjectWrapper$new()
  TOW$logging("silent")
  TOW$input(text_df)


  control= list(
    quanteda = list(
      tokenize = "word",
      remove_stopwords = T,
      sentence_as_documents = T
      )
  )

  x = TOW$process(control)
  testthat::expect_equal(
    unique(tibble::as.tibble(quanteda::docvars(x)[,which(colnames(quanteda::docvars(x)) %in% c("date", "source"))])),
    docvars

  )

  TOW$reset()
  control= list(
    quanteda = list(
      tokenize = "word",
      remove_stopwords = T,
      sentence_as_documents = F
    )
  )
  x = TOW$process(control)
  testthat::expect_equal(
    tibble::as.tibble(quanteda::docvars(x)[,which(colnames(quanteda::docvars(x)) %in% c("date", "source"))]),
    docvars

  )


  TOW$reset()
  control= list(
    spacyr = list(
      sentence_as_documents = F
    )
  )
  x = TOW$process(control) %>% TOW$output(format = "dfm")
  testthat::expect_equal(
    tibble::as.tibble(quanteda::docvars(x)[,which(colnames(quanteda::docvars(x)) %in% c("date", "source"))]),
    docvars

  )
  TOW$reset()
  control= list(
    spacyr = list(
      sentence_as_documents = T
    )
  )
  x = TOW$process(control) %>% TOW$output(format = "dfm")
  testthat::expect_equal(
    unique(tibble::as.tibble(quanteda::docvars(x)[,which(colnames(quanteda::docvars(x)) %in% c("date", "source"))])),
    docvars

  )


  quanteda::docvars(x)
  })


testthat::test_that("QuantedaBackend",{
 # Test the single functions of a backend
  test_object = QuantedaBackend$new()
  text <- c("123",
            "1","2","3")
  text_df <- dplyr::data_frame( text = text ,
                                date=(c("2018-04-09","2018-04-06","2018-04-07","2018-04-08")),
                                source = (c("Zeit","Spiegel","Guardian","FAZ"))
  )
  tib = tibble::as.tibble(text_df)

  Quanteda


 })
