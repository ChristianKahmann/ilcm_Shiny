context("Cooccurrence-Calculation")

# Ahmad Dawar Hakimi 2018
# NLP Group
# University Leipzig

################################################################################################
#
# Test the registered algorithms that are part of the package for defined behaviour including:
#   * correct inputs
#   * correct calculation of significance measures
#
################################################################################################




library(tm)
library(Matrix)
library(memoise)
library(testthat)
#source("../../R/FastCooc.R")

# read in test data
data <-
  read.csv(
    file = "test.txt",
    sep = "\t",
    encoding = "UTF-8",
    header = F,
    stringsAsFactors = F
  )

# create sparse binary DocumentTermMatrix of read in test data
help <- apply(data, 1, paste, collapse = " ")
tt <- Corpus(VectorSource(help))
binDTM <- as.matrix(DocumentTermMatrix(tt))
sparseDTM <- as(binDTM, "dgCMatrix")
coocCounts <- t(sparseDTM) %*% sparseDTM
tmp <- Matrix::summary(coocCounts)

#delete vocab whith no coocs
tmp[tmp[, "x"] < Coocc$minCoocFreq, "x"] <- 0
tmp[tmp[, "x"] > Coocc$maxCoocFreq, "x"] <- 0

#set diagonals to 0's
tmp[tmp[, 1] == tmp[, 2], "x"] <- 0

# create sparsematrix with vocabs that have coocs
coocCounts <-
  Matrix::sparseMatrix(
    i = tmp[, 1],
    j = tmp[, 2],
    x = tmp[, 3],
    dimnames = dimnames(coocCounts),
    dims = dim(coocCounts)
  )

# create Coocc obejct
coocs <- Coocc$new(coocCounts)


# read in empty file (needed for test)
emptyFile <-
  read.csv(
    file = "emptyFile.txt",
    sep = "\t",
    encoding = "UTF-8",
    header = F,
    stringsAsFactors = F
  )

# create sparse binary DocumentTermMatrix
helpE <- apply(emptyFile, 1, paste, collapse = " ")
ttE <- Corpus(VectorSource(helpE))
binDTME <- as.matrix(DocumentTermMatrix(ttE))
sparseDTME <- as(binDTME, "dgCMatrix")
coocCountsE <- t(sparseDTME) %*% sparseDTME
tmpE <- Matrix::summary(coocCountsE)

# create sparseMatrix
coocCountsE <-
  Matrix::sparseMatrix(
    i = tmpE[, 1],
    j = tmpE[, 2],
    x = tmpE[, 3],
    dimnames = dimnames(coocCountsE),
    dims = dim(coocCountsE)
  )

# create Coocc object
coocsE <- Coocc$new(coocCountsE)

testthat::test_that("DICE", {
  # Test if calculated results are probabilities and are not NA's
  coocs$set_measure("DICE")
  calcDICE <- coocs$ccoocs()
  testthat::expect_gte(min(calcDICE), expected = 0)
  testthat::expect_lte(max(calcDICE), expected = 1)
  testthat::expect_false(any(is.na(calcDICE)))
})

testthat::test_that("LOGLIK", {
  # Test if calculated results are not NA'S
  coocs$set_measure("LOGLIK")
  calcLOGLIK <- coocs$ccoocs()
  testthat::expect_false(any(is.na(calcLOGLIK)))
})

testthat::test_that("MI", {
  # Test if calculated results are not NA'S
  coocs$set_measure("MI")
  calcMI <- coocs$ccoocs()
  testthat::expect_false(any(is.na(calcMI)))
})

testthat::test_that("COUNT", {
  # Test if calculated results are not NA'S
  coocs$set_measure("COUNT")
  calcCOUNT <- coocs$ccoocs()
  testthat::expect_false(any(is.na(calcCOUNT)))

})

testthat::test_that("Empty file", {
  # Test if empty file throws error
  expect_error(calc <- coocsE$ccoocs())
})
