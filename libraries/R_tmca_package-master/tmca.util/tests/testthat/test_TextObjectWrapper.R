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


    #check for the error free execution of the default parameters for every backend.
    for( backend in names(test_object$.__enclos_env__$private$register)){
      o = test_object$get_options(backend)
      l = list()
      l[[backend]] = o
      #print(l)

      testthat::expect_error(x = test_object$process(l))

      x = test_object$fullprocess(l)

      testthat::expect_true(quanteda::is.dfm(x %>% test_object$output(format = "dfm")))

      test_object$reset()
      testthat::expect_equal(test_object$.__enclos_env__$private$internal_representation,
                             test_object$.__enclos_env__$private$input_untouched)

    }

})

testthat::test_that("Docvars Propagation",{
  "This Function is testing the correct propagation of Docvars. it tests for 4 combinations of sentence and document level tokenisation using spacy or quanteda."



  # First sentence contains more than one sentence and shuold consecutively be split up by sentence level tokenization.
  text <- c("test 123 Because I could not stop for Death. Because I could not stop for Death. test 123 Because I could not stop for Death.",
            "test2",
            "test3",
            "test4")

  # Adding some docvars
  text_df <- dplyr::data_frame( text = text ,
                                date=(c("2018-04-09","2018-04-06","2018-04-07","2018-04-08")),
                                source = (c("Zeit","Spiegel","Guardian","FAZ"))
  )

  # Keeping the original docvars for an comparison at the end.
  docvars = dplyr::data_frame(
                               date=(c("2018-04-09","2018-04-06","2018-04-07","2018-04-08")),
                               source = (c("Zeit","Spiegel","Guardian","FAZ"))
  )

  # Testing sentence segmentation with quanteda
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

  x = TOW$fullprocess(control)
  testthat::expect_equal(
    unique(tibble::as.tibble(quanteda::docvars(x)[,which(colnames(quanteda::docvars(x)) %in% c("date", "source"))])),
    docvars

  )

  # Testing documentlevel segmentation with spacy and output as quanteda format. Checking the right merging of the docvars
  TOW$reset()
  control= list(
    spacyr = list(
      sentence_as_documents = F
    )
  )
  x = TOW$fullprocess(control) %>% TOW$output(format = "dfm")
  testthat::expect_equal(
    tibble::as.tibble(quanteda::docvars(x)[,which(colnames(quanteda::docvars(x)) %in% c("date", "source"))]),
    docvars

  )
  # Testing sentence level segmentation with spacy and output as quanteda format. Checking the right merging of the docvars
  TOW$reset()
  control= list(
    spacyr = list(
      sentence_as_documents = T
    )
  )
  x = TOW$fullprocess(control) %>% TOW$output(format = "dfm")
  testthat::expect_equal(
    unique(tibble::as.tibble(quanteda::docvars(x)[,which(colnames(quanteda::docvars(x)) %in% c("date", "source"))])),
    docvars

  )


  quanteda::docvars(x)
  })


testthat::test_that("QuantedaBackend",{
 # Test the single functions of a backend
  test_object = QuantedaBackend$new()
  test_object$logging("silent")
  text <- c("123",
            "-",
            ".",
            "special")
  text_df <- dplyr::data_frame( text = text )
  tib = tibble::as.tibble(text_df)
    control = list(
      tokenize = "word",
      remove_numbers = T,
      remove_all_numbers=T,
      remove_hyphenation = T,
      remove_punctuation = T,
      remove_custom = list("special")

  )
  test_object$createPipeline(control)
  #execute and count tokens
  s = sum(quanteda::ntoken(quanteda::corpus(text_df) %>%
                         test_object$input() %>%
                         test_object$executePipeline()
  ))
  testthat::expect_true(s==0)

  test_object = QuantedaBackend$new()
  test_object$logging("silent")
  text <- c("1 2 3 4",
            "1 2 3",
            "1 2",
            "1")
  text_df <- dplyr::data_frame( text = text )

  control = list(
    tokenize = "word",
    prune = list(min_docfreq = 0.30,
                 max_docfreq = 0.75,
                 min_count = NULL,
                 max_count = NULL)

  )
  test_object$createPipeline(control)
  #execute and count tokens
  s = sum(quanteda::ntoken(quanteda::corpus(text_df) %>%
                             test_object$input() %>%
                             test_object$executePipeline()
  ))

  testthat::expect_true(s==0)

  control = list(
    tokenize = "word",
    prune = list(min_docfreq = 0.50,
                 max_docfreq = 0.80,
                 min_termfreq = 2,
                 max_termfreq = 2)

  )
  test_object$createPipeline(control)
  #execute and count tokens
  s = sum(quanteda::ntoken(quanteda::corpus(text_df) %>%
                             test_object$input() %>%
                             test_object$executePipeline()
  ))

  testthat::expect_true(s==s)

  # Full test
  test_object = QuantedaBackend$new()
  test_object$logging("silent")
  text <- c("1 2 3 4  hello world and success! test 1 2 3 4 hello world success success",
            "1 2 3 testing hello The Successes !",
            "1 2 testing hello success - . ?",
            "1 testing hello")
  text_df <- dplyr::data_frame( text = text )

  control =  list(
    tokenize = "word",
    sentence_as_documents = T,
    language = "english",
    ngrams = c(1:3),
    stem = T,
    remove_stopwords = T,
    remove_numbers = T,
    remove_all_numbers=T,
    remove_punctuation = T,
    remove_hyphenation = T,
    remove_custom = list("hello","world"),
    tolower = T,
    prune = list(
      min_docfreq = 0.2,
      min_termfreq = 3,
      max_docfreq = 0.75,
      max_termfreq = 5
    ),
    tfidf=T,
    char_length = c(4,8)

  )
  test_object$createPipeline(control)
  x = quanteda::corpus(text_df) %>%
    test_object$input() %>%
    test_object$executePipeline()

  print(x)

  num_tokens = quanteda::nfeat(x)
  token = quanteda::featnames(x)

  testthat::expect_equal(num_tokens,0)
  #testthat::expect_equal(token, "success")

 })
