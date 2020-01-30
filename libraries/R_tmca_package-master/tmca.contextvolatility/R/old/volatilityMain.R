library(snow)
library(parallel)
library(lsa)
library(tmca)
library(Matrix)
library(tm)





cv <- function(binaryDTM, ...)
  UseMethod("cv")

cv.dgCMatrix <-
  function(binaryDTM,
           cores = parallel::detectCores() - 1 ,
           dates = NULL,
           intervall = "DAYS",
           span = Inf,
           significanceThreshold = 0,
           measure = "DICE",
           volatility_measure=NULL,
           minCoocFreq = 1,
           terms = NULL)
  {
    if (class(binaryDTM)[1] != "dgCMatrix")
      stop("DTM must be \"dsCMatrix\" of package \"Matrix\".")
    if (is.null(dates))
      stop("No dates given for document-term-matrix")
    if (nrow(binaryDTM) != length(dates))
      stop("Number of rows in document-term-matrix must be same length than length of dates.")
    binDTM<<-binaryDTM
    if(volatility_measure=="global"||volatility_measure=="globalsig")
    {
    #coocs function from tmca package
    write("Calculating global co-occurrences.", stdout())
    
    globalCoocs <-
      coocs(
        binaryDTM,
        measure = measure,
        significanceThreshold = significanceThreshold,
        minCoocFreq = minCoocFreq
      )
    }
    #For Inf the method returns a global volatility
    write("Calculating timeslice co-occurrences.", stdout())
    
    datesTmp <- createStrippedDates(dates, aggr = intervall)
    distinctDates <- names(table(datesTmp))
    
    cl <- makeCluster(cores, outfile = "")
    #setDefaultCluster(cl)
    
    
    tmpLib <- .libPaths()[1]
    clusterExport(cl, c('getTimesliceCoocs', 'tmpLib'),envir=environment())
    clusterEvalQ(cl, {
      .libPaths(c(tmpLib))
      library(Matrix)
      library(tmca)
    })
    

    #clusterExport(cl, c("getTimesliceCoocs","tmpLib"))
    
    #clusterEvalQ(cl, {.libPaths(c(tmpLib))})
    #clusterEvalQ(cl, sessionInfo())
    
    sequ <- seq_along(distinctDates)
    if(volatility_measure=="global"||volatility_measure=="globalsig")
    {
    coocsYears <-
      parSapply(
        cl,
        sequ,
        getTimesliceCoocs,
        distinctDates = distinctDates,
        datesTmp = datesTmp,
        minSig = significanceThreshold,
        binDTM = binaryDTM,
        glob_sig = globalCoocs
      )
    stopCluster(cl)
    
    write("Calculating volatility", stdout())
    }
    else
    {
      coocsYears <-
        parSapply(
          cl,
          sequ,
          getTimesliceCoocs_no_replace,
          distinctDates = distinctDates,
          datesTmp = datesTmp,
          minSig = significanceThreshold,
          binDTM = binaryDTM,
          glob_sig = globalCoocs
        )
      stopCluster(cl)
      
      write("Calculating volatility wihtout replacing", stdout())
    }
   # return(coocsYears)
      sequ <- seq_along(coocsYears)
      vocab <- c()
      for (i in sequ)
      {
        if(is.null(terms))
        {
        terms <- union(terms, rownames(coocsYears[[i]]))
        }
        
        vocab <- union(vocab, rownames(coocsYears[[i]]))
      }
    print("sequ fertig")
    
    
    cl <- makeCluster(cores, outfile = "")
    #setDefaultCluster(cl)
    
    clusterExport(
      cl,
      c(
        'tmpLib',
        'vocab',
        'coocsYears',
        'nzmean',
        'nzrankQuantile',
        'rankQuantile',
        'nzvar'
      ),
      envir=environment()
    )
    
    clusterEvalQ(cl, {
      .libPaths(c(tmpLib))
      library(Matrix)
      library(contextvolatility)
    })
    
    voldata <- NULL
    
    if(is.infinite(span))
    {
      term_split <- split(terms, as.numeric(cut(seq_along(terms),cores)))
      names(term_split) <- c()
      voldata <-
        parLapplyLB(cl, term_split, getVolForTermLogVar)
    }
    else
    {
      clusterExport(
        cl,
        c(
          'binDTM',
          'getDiachronicFreqs',
          'datesTmp',
          'distinctDates'
        ),
        envir=environment()
      )
      
      term_split <- split(terms, as.numeric(cut(seq_along(terms),cores)))
      names(term_split) <- c()
      print("term split klappt")
      if(volatility_measure=="global")
      {  
        print("in if schleife")
        voldata <-parLapplyLB(cl, term_split, getVolForTermLogVarTS, span = span)
      } 
      else
      {
        #array<-array(c(0),dim=c(length(coocsYears),dim(coocsYears[[1]]),dim(coocsYears)[2]))
       # dimlist<-list(colnames(coocsYears[[1]]),colnames(coocsYears[[2]]),colnames(coocsYears[[3]]))
    
        voldata <- parLapply(cl,term_split,function(x){
          return(calculate_volat(coocsYears,h = span,method = volatility_measure,vocab,datesTmp,x))
        })
        
        #voldata<-calculate_volat(coocsYears,h = span,method = volatility_measure,vocab,datesTmp,terms)
        #return(coocsYears)
       
      }  
    }
    
    stopCluster(cl)
    return(voldata)
  }
