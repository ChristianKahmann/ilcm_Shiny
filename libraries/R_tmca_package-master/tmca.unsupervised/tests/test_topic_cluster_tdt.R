testthat::test_that("update_document_frequency function", {
  v1 <- c(1,1,1,1,1,1,1,1,1,1)
  v2 <- c(1,1,1,1,1,1,1,1,1,1)
  names(v1) <- c("a","b","c","d","e","f","g","h","i","j")
  names(v2) <- c("k","l","m","n","o","a","b","c","d","p")
  
  #Target should be
  #a=7,b=10,c=12,d=13,e=1,f=1,g=1,h=1,i=1,j=1,k=1,l=1,m=1,n=1,o=1,p=1
  
  v3 <- tmca.unsupervised::update_document_frequency(v1,v2)
  v_test <- c(2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1)
  names(v_test) <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p")
  testthat::expect_equal(v_test, v3[names(v_test)])
  
})

testthat::test_that("create_documents function", {
  df <- rep(1,3003)
  names(df) <- as.character(1:3003)
  
  new_dtm <- Matrix(0,ncol=3003,nrow=3,sparse = T)
  colnames(new_dtm) <- as.character(1:3003)
  
  new_dtm[1,1:1001] <- 1
  new_dtm[2,1002:2002] <- 1
  new_dtm[3,2003:3003] <- 1
  
  #new_dtm %*% diag(log((0.5+N)/df)/log(1.0 + N))
  #based on df=1,N=1,freq=1 all components must be 0.5849625
  
  weight <- 0.5849625
  docs <- tmca.unsupervised::create_documents(new_dtm,df,1)
  
  reference <-  new_dtm <- Matrix(0,ncol=3003,nrow=3,sparse = T)
  colnames(reference) <- as.character(1:3003)
  reference[1,1:1000] <- weight
  reference[2,1002:2001] <- weight
  reference[3,2003:3002] <- weight
  
  testthat::expect_equal(as.matrix(reference), as.matrix(docs))
  
})

testthat::test_that("compare_documentsnearest function", {
  
  v1 <- Matrix::Matrix(0,ncol=1000,nrow=1)
  v1[1,] <- 0.58
  
  v2 <- Matrix::Matrix(0,ncol=1000,nrow=1)
  v2[1,] <- 0.58
  v2[1,1000] <- 0.50
  
  v3 <- Matrix::Matrix(0,ncol=1000,nrow=1)
  v3[1,] <- 0.5
  
  match <- tmca.unsupervised::compare_documentsnearest(v1, Matrix::rBind(v2,v3))
  testthat::expect_equal(match,as.numeric(1))
  
})

testthat::test_that("sim_ab function", {
  
  v1 <- c(1,1,1)
  v2 <- c(2,2,2)
  v3 <- c(-1,-1,-1)
  v4 <- c(-1,1,1)
  
  testthat::expect_equal(tmca.unsupervised::sim_ab(v1,v2),1)
  testthat::expect_equal(tmca.unsupervised::sim_ab(v1,v3),-1)
  testthat::expect_equal(tmca.unsupervised::sim_ab(v1,v4),0.3333333,tolerance=1e-2)
  
})

testthat::test_that("Tracker functionality", {
  
  v1 <- c(1,2,3,4,5,6,7,8,9,10)
  v2 <- c(1,2,3,4,5,6,7,8,9,10)
  v3 <- c(0,2,0,2,0,0,0,0,0,0)
  
  names(v1) <- c("a","b","c","d","e","f","g","h","i","j")
  names(v2) <- c("a","b","c","d","e","f","g","h","i","j")
  names(v3) <- c("a","b","c","d","e","f","g","h","i","j")
  
  
  tmp_dtm <- Matrix(0,nrow=3,ncol=10,sparse=T) 
  
  tmp_dtm[1,] <- v1
  tmp_dtm[2,] <- v2
  tmp_dtm[3,] <- v3
  
  colnames(tmp_dtm) <- c("a","b","c","d","e","f","g","h","i","j")
  rownames(tmp_dtm) <- c("1","2","3")
  
  t2 <- new("tdt_tracker")
  t2 <- track(t2, tmp_dtm)
  
  testthat::expect_equal(length(table(t2@clusters)),2)
  
})
