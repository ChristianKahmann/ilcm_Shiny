context("Util")

testthat::test_that("Prunce-function",{
  testDTM<-Matrix::Matrix(matrix(sample(x = c(0,1,2),size = 30,replace = T),10,3),sparse = T)
  testDTM<-cbind(testDTM,c(0,0,0,0,0,0,0,0,0,0))
  pruned_testDTM_abs<-tmca.util::prune_absolute(testDTM)
  pruned_testDTM_rel<-tmca.util::prune_relative(testDTM)
  testthat::expect_equal(class(testDTM),class(pruned_testDTM_abs))
  testthat::expect_equal(class(testDTM),class(pruned_testDTM_rel))
  testthat::expect_lte(object = dim(pruned_testDTM_abs)[2],expected = dim(testDTM)[2])
  testthat::expect_lte(object = dim(pruned_testDTM_rel)[2],expected = dim(testDTM)[2])
})


testthat::test_that("make_binary-function",{
  testDTM<-Matrix::Matrix(matrix((sample(c(0,1,10,4,5),size = 30,replace=T)),10,3),sparse = T)
  binary_testDTM<-tmca.util::make_binary(testDTM)

  testthat::expect_equal(dim(testDTM),dim(binary_testDTM))
  testthat::expect_that(max(binary_testDTM),equals(1))
  testthat::expect_that(sum(binary_testDTM),equals(length(Matrix::which(testDTM>0))))

})


testthat::test_that("makeTFIDF-function",{
  testDTM<-Matrix::Matrix(matrix((sample(c(0,1,10),size = 30,replace=T)),10,3),sparse = T)
  tfidf_testDTM<-tmca.util::makeTFIDF(testDTM)

  testthat::expect_equal(dim(testDTM),dim(tfidf_testDTM))

})
