context("Context-Volatility")

# Ahmad Dawar Hakimi 2018
# NLP Group
# University Leipzig

################################################################################################
#
# Test the registered algorithms that are part of the package for defined behaviour including:
#   
#   *  correct input and output format
#   *  correct computation of context volatlity
#   *  correct computation by help functions
#   *  correct logging
#
################################################################################################

 library(tm)
 library(tmca.cooccurrence)

text = c("These are 4 example sentences.",
             "Nothing special is happening in this example sentences.",
             "Every example sentence has a different date.",
             "The date is an example for meta information that will be kept.",
             "I need 2 more example sentences.",
             "So my example works.")
             
text = tm::removePunctuation(text)

dates =(c("2018-01-09","2018-01-06","2018-04-06","2018-04-09", "2018-02-09", "2018-02-06"))
         
 
corpus <- Corpus(VectorSource(text))
binDTM <- as(as.matrix(DocumentTermMatrix(corpus)), 'dgCMatrix')
errorDTM <- as.matrix(DocumentTermMatrix(corpus))
row.names(binDTM) <- dates
row.names(errorDTM) <- dates
errorTerms <- c("Not", "available", "vocabulary")
errorDates <- c()
notEnoughDates <-(c("2018-01-09","2018-01-06","2018-04-06","2018-04-09", "2018-02-09"))

testthat::test_that("context volatility significance", {
  
  volaSigSimple = testthat::expect_warning(calculate_context_volatility(binDTM = binDTM, dates = dates, memory = 1, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "sig_simple", terms = c('example', 'sentence', 'date'), logfile = NULL))
  
  # test that output has correct dimensions
  testthat::expect_equal(dim(volaSigSimple), c(3,2))
  
  # Check if NA's are produced
  testthat::expect_false(any(is.na(volaSigSimple)))  
  
  # DTM must be dgCMatrix of package Matrix(normal matrix throws error)
  testthat::expect_error(calculate_context_volatility(errorDTM, dates = dates, memory = 1, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "sig_simple", terms = c('example', 'sentence', 'date'), logfile = NULL))
  
  # dates must be given for document term matrix(throws error because dates vector is empty)
  testthat::expect_error(calculate_context_volatility(binDTM, dates = errorDates, memory = 1, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "sig_simple", terms = c('example', 'sentence', 'date'), logfile = NULL))
  
  # number of rows in document term matrix must be same length than length of dates(throws error because one date is missing)
  testthat::expect_error(calculate_context_volatility(binDTM, dates = notEnoughDates, memory = 1, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "sig_simple", terms = c('example', 'sentence', 'date'), logfile = NULL))
  
  # at least one term needs to be in the vocabulary(throws error because no term is in the vocabulary)
  testthat::expect_error(calculate_context_volatility(binDTM, dates = dates, memory = 1, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "sig_simple", errorTerms, logfile = NULL))
  
  
  volaSigCosine = testthat::expect_warning(calculate_context_volatility(binDTM = binDTM, dates = dates, memory = 1, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "sig_cosine", terms = c('example', 'sentence', 'date'), logfile = NULL))
  
  # test that output has correct dimensions
  testthat::expect_equal(dim(volaSigCosine), c(3,2))

  # test if calculated values are probabilities
  testthat::expect_gte(min(volaSigCosine, na.rm = T), expected = 0)
  testthat::expect_lte(max(volaSigCosine, na.rm = T), expected = 1)

  })

testthat::test_that("context volatility significance window",{

  VarCoefVector=c(1:9)
  
  # test if Coefficient of Variation is computed correct
  testthat::expect_equal(VarCoef(VarCoefVector), sd(as.double(VarCoefVector))/mean(as.double(VarCoefVector)))
  
  volaSigVCWindow = testthat::expect_warning(calculate_context_volatility(binDTM = binDTM, dates = dates, memory = 2, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "sig_vc_window", terms = c('example', 'sentence', 'date'), logfile = NULL))
  
  # test that output has correct dimensions
  testthat::expect_equal(dim(volaSigVCWindow), c(3,2))
  
  
  volaSigSDWindow = testthat::expect_warning(calculate_context_volatility(binDTM = binDTM, dates = dates, memory = 2, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "sig_sd_window", terms = c('example', 'sentence', 'date'), logfile = NULL))

  # test that output has correct dimensions
  testthat::expect_equal(dim(volaSigSDWindow), c(3,2))
  })

 testthat::test_that("context volatility rank", {

  volaRankNoZero = calculate_context_volatility(binDTM, dates = dates, memory = 2, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "rank_no_zero", terms = c('example', 'sentence', 'date'), logfile = NULL)
  
  # test that output has correct dimensions
  testthat::expect_equal(dim(volaRankNoZero), c(3,2))
  
  #volaRankMaxRank = calculate_context_volatility(binDTM, dates = dates, memory = 2, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "rank_max_rank", terms = c('example', 'sentence', 'date'), logfile = NULL)
  volaRankRecommender = calculate_context_volatility(binDTM, dates = dates, memory = 2, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "rank_recommender", terms = c('example', 'sentence', 'date'), logfile = NULL)
  
  # test that output has correct dimensions
  testthat::expect_equal(dim(volaRankRecommender), c(3,2))
  
  volaRankMinMax = calculate_context_volatility(binDTM, dates = dates, memory = 2, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "rank_minmax", terms = c('example', 'sentence', 'date'), logfile = NULL)

  # test that output has correct dimensions
  testthat::expect_equal(dim(volaRankMinMax), c(3,2))
  })




testthat::test_that("help functions",{
    testVector <- c(-2:3,NA)
    emptyVector <- c(NA)
    testVector2 <- c(-2:3)
    
    # test that help functions compute correct values
    testthat::expect_equal(min_no_na(testVector), -2)
    testthat::expect_warning(min_no_na(emptyVector))
    testthat::expect_equal(max_no_na(testVector), 3)
    testthat::expect_warning(max_no_na(emptyVector))
    testthat::expect_equal(mean_no_na(testVector), 0.5)
    testthat::expect_equal(sum_no_na(testVector), 3)
    testthat::expect_equal(abs_sum(testVector2), 9)
    testthat::expect_equal(abs_mean(testVector2), 1.5)
    testthat::expect_equal(abs_mean_TI(testVector2), 1.8)
})

testthat::test_that("printlog",{
  # test that logging is working
  testthat::expect_output(printlog("It's working!", NULL),"It's working!")
  printlog("It's working!", "logs.txt")
  # test that log file is created
  testthat::expect_true(file.exists(file.path("logs.txt")))
  testthat::expect_silent(printlog("It's silent!", logfile="silent"))
  file.remove("logs.txt")
  testthat::expect_false(file.exists(file.path("logs.txt")))
  
})