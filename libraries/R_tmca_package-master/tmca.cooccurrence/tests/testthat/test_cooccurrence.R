context("Coocc-old")

source("../../R/cooccurrence.R")

testthat::test_that("Cooccurrence-Calculation",{
  testDTM<-Matrix::Matrix(matrix(c(1),10,3),sparse = T)
  colnames(testDTM)<-c("a","b","c")
  testCooc<-coocs.dgCMatrix(binDTM = testDTM,significanceThreshold = 0)

  testthat::expect_equal(dim(testCooc),c(dim(testDTM)[2],dim(testDTM)[2]))
  testthat::expect_that(as.numeric(Matrix::rowSums(testCooc)),equals(as.numeric(rowSums(matrix(c(0,1,1,1,0,1,1,1,0),3,3))),tolerance=0.01))
  testthat::expect_that(as.numeric(Matrix::colSums(testCooc)),equals(as.numeric(colSums(matrix(c(0,1,1,1,0,1,1,1,0),3,3))),tolerance=0.01))

})

testthat::test_that("CooccurrenceCount-Calculation",{
  testDTM_count<-Matrix::Matrix(matrix(1:30,10,3),sparse = T)
  colnames(testDTM_count)<-c("a","b","c")

  testthat::expect_equal(calcCoocCounts(testDTM_count,minCoocFreq = 0)[[1]],dim(testDTM_count)[1])
  testthat::expect_equal(as.numeric(calcCoocCounts(testDTM_count,minCoocFreq = 0)[[2]]),c(55,155,255))
  testthat::expect_true(all(as.matrix(calcCoocCounts(testDTM_count,minCoocFreq = 0)[[3]])==matrix(c(0,935,1485,935,0,4035,1485,4035,0),3,3))==TRUE)
  testthat::expect_true(all(calcCoocCounts(testDTM_count,minCoocFreq = 0)[[4]] %in% colnames(testDTM_count)==TRUE))
})

testthat::test_that("CooccurrenceSignificance-Calculation",{
  testDTM_sig<-Matrix::Matrix(c(1,1,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,1,0,0,1,0,1,0,0,1,1,0,1,1,1,0,0),8,5,byrow = T)
  colnames(testDTM_sig)<-c("a","b","c","d","e")
  testSig_Dice<-calcSignificanceValues(k=8,kj=setNames(c(3,3,4,3,3), c("a","b","c","d","e")),coocCounts = Matrix::Matrix(c(0,3,0,0,0,3,0,0,0,0,0,0,0,2,2,0,0,2,0,2,0,0,2,2,0),5,5,byrow = T,sparse=T,dimnames = list(c("a","b","c","d","e"),c("a","b","c","d","e"))),finalSig=Matrix::Matrix(c(0,3,0,0,0,3,0,0,0,0,0,0,0,2,2,0,0,2,0,2,0,0,2,2,0),5,5,byrow = T,sparse=T,dimnames = list(c("a","b","c","d","e"),c("a","b","c","d","e"))),relWords = c("a","b","c","d","e"),measure = "DICE",significanceThreshold = 0)
  testSig_LogLik<-calcSignificanceValues(k=8,kj=setNames(c(3,3,4,3,3), c("a","b","c","d","e")),coocCounts = Matrix::Matrix(c(0,3,0,0,0,3,0,0,0,0,0,0,0,2,2,0,0,2,0,2,0,0,2,2,0),5,5,byrow = T,sparse=T,dimnames = list(c("a","b","c","d","e"),c("a","b","c","d","e"))),finalSig=Matrix::Matrix(c(0,3,0,0,0,3,0,0,0,0,0,0,0,2,2,0,0,2,0,2,0,0,2,2,0),5,5,byrow = T,sparse=T,dimnames = list(c("a","b","c","d","e"),c("a","b","c","d","e"))),relWords = c("a","b","c","d","e"),measure = "LOGLIK",significanceThreshold = 0)
  testSig_MI<-calcSignificanceValues(k=8,kj=setNames(c(3,3,4,3,3), c("a","b","c","d","e")),coocCounts = Matrix::Matrix(c(0,3,0,0,0,3,0,0,0,0,0,0,0,2,2,0,0,2,0,2,0,0,2,2,0),5,5,byrow = T,sparse=T,dimnames = list(c("a","b","c","d","e"),c("a","b","c","d","e"))),finalSig=Matrix::Matrix(c(0,3,0,0,0,3,0,0,0,0,0,0,0,2,2,0,0,2,0,2,0,0,2,2,0),5,5,byrow = T,sparse=T,dimnames = list(c("a","b","c","d","e"),c("a","b","c","d","e"))),relWords = c("a","b","c","d","e"),measure = "MI",significanceThreshold = 0)
  testSig_Count<-calcSignificanceValues(k=8,kj=setNames(c(3,3,4,3,3), c("a","b","c","d","e")),coocCounts = Matrix::Matrix(c(0,3,0,0,0,3,0,0,0,0,0,0,0,2,2,0,0,2,0,2,0,0,2,2,0),5,5,byrow = T,sparse=T,dimnames = list(c("a","b","c","d","e"),c("a","b","c","d","e"))),finalSig=Matrix::Matrix(c(0,3,0,0,0,3,0,0,0,0,0,0,0,2,2,0,0,2,0,2,0,0,2,2,0),5,5,byrow = T,sparse=T,dimnames = list(c("a","b","c","d","e"),c("a","b","c","d","e"))),relWords = c("a","b","c","d","e"),measure = "COUNT",significanceThreshold = 0)
  testthat::expect_lte(max(testSig_Dice),expected = 1)
  testthat::expect_gte(min(testSig_Dice),expected = 0)

})
