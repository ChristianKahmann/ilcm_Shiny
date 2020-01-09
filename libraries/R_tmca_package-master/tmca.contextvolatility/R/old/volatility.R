getVolForTermLogVar <- function(x)
{
  library(Matrix)
  
  termVolatility <- list()
  
  for(term in x)
  {
    volatilityRanks <- Matrix(0,nrow=length(vocab), ncol=length(coocsYears))
    rownames(volatilityRanks) <- vocab
    
    cat(term,"\n")
    
    for (i in 1:length(coocsYears)) {
      currentYearTTM <- coocsYears[[i]]
      if (term %in% rownames(currentYearTTM)) {
        #tmp <- log(currentYearTTM[term,])
        #tmp[is.infinite(tmp)] <- 0
        #volatilityRanks[names(currentYearTTM[term,]), i] <- tmp
        rankedCoocs <- names(which(sort(currentYearTTM[term,], decreasing=TRUE) > 0))
        if (length(rankedCoocs) > 0) {
          coocRanks <- seq(1:length(rankedCoocs))
          #volatilityRanks[, i] <- length(rankedCoocs)
          volatilityRanks[rankedCoocs, i] <- coocRanks
          
        }
      }
    }
    
    #volatilityRanks <- apply(gp,1,volatilityRanks,l=6)
    
    #volatilityRanks <- diff(t(volatilityRanks))
    #volatilityRanks <- t(volatilityRanks)
    #Experiment was beste Voli ist steht aus
    #No Overservation no meaning change
    #no var in cooc (0) no context
    # so nzmean, nzvar
    setCoocs <- (coocsYears[[1]])[1,] > 0
    
    termVolatility[[term]] <- mean(apply(volatilityRanks[setCoocs,], 1, rankQuantile))
    #termVolatility <- nzmean(apply(volatilityRanks[setCoocs,], 1, var))
    
  }
  
  gc()
  
  return(termVolatility)
}
getVolForTermLogVarTS <- function(x, span = Inf)
{
  termVolatility <- list()
  count<-0
  for(term in x)
  {
    count<-count+1
    print(count)
    if(!(term %in% vocab))
      next()
    
    volatilityTS <- Matrix(0, nrow=3, ncol=length(coocsYears))
    
    rownames(volatilityTS) <- rep(term,3)
    
    volatilityRanks <- Matrix(0,nrow=length(vocab), ncol=length(coocsYears))
    
    rownames(volatilityRanks) <- vocab
    
    write(term, stdout())
    
    for (i in 1:length(coocsYears)) {
      
      currentYearTTM <- coocsYears[[i]]
      if (term %in% rownames(currentYearTTM)) {
        #tmp <- log(currentYearTTM[term,])
        #tmp[is.infinite(tmp)] <- 0
        #volatilityRanks[names(currentYearTTM[term,]), i] <- tmp
        rankedCoocs <- names(which(sort(currentYearTTM[term,], decreasing=TRUE) > 0))
        if (length(rankedCoocs) > 0) {
          coocRanks <- seq(1:length(rankedCoocs))
          #volatilityRanks[, i] <- length(rankedCoocs)
          volatilityRanks[rankedCoocs, i] <- coocRanks
          
        }
      }
    }
    
    #volatilityRanks <- t(apply(volatilityRanks,1,gp,l=6))
    
    #Relative änderung = volatility ranks diff / v over t0
    #volatilityRanks <- diff(t(volatilityRanks))
    #volatilityRanks <- t(volatilityRanks)
    
    if(is.infinite(span)) span <- dim(volatilityRanks)
    
    setCoocs <- (coocsYears[[1]])[1,] > 0
    
    for(i in span:ncol(volatilityRanks))
    {
      tmp <- volatilityRanks[setCoocs,(i-(span-1)):i]
      #volatility <- nzmean(apply(tmp, 1, rankQuantile))#??nzvar?
      volatility <- mean(apply(tmp, 1, VarCoef))#??nzvar?
      #volatilität für spanne von 0 bis 1?!
      #+1 because diff table is differnt
      volatilityTS[1,i] <- volatility
    }
    
    
    freqData <- getDiachronicFreqs(datesTmp, binDTM, term = term)
    x<-freqData[,"TermCount"]
    
    normalized_f = (x-min(x))/(max(x)-min(x))
    normalized_v = (volatilityTS[1,]-min(volatilityTS[1,]))/(max(volatilityTS[1,])-min(volatilityTS[1,]))
    
    colnames(volatilityTS) <- paste(distinctDates,"-01",sep="")
    
    volatilityTS[2,] <- normalized_v
    volatilityTS[3,] <- normalized_f
    
    termVolatility[[term]] <- volatilityTS
    
  }
  gc()
  #names(termVolatility) <- x
  return(termVolatility)
}
getRankVolAllTS <- function(span = Inf, slicedCoocs = NULL, cores = detectCores() - 1)
{
  
  terms <- c()
  
  sequ <- seq_along(slicedCoocs)
  
  for(i in sequ)
  {
    terms <- union(terms, rownames(slicedCoocs[[i]]))
  }
  
  cl <- makeCluster(cores,outfile="volli.log")
  
  setDefaultCluster(cl)
  
  clusterExport(NULL, c("terms","slicedCoocs","nzmean","nzrankQuantile","rankQuantile","nzvar"))
  #clusterEvalQ(NULL, {.libPaths(c(tmpLib));library(Matrix);library(tmca)})
  #iterate over terms
  sequ <- seq_along(terms)
  
  res <- parLapply(cl, sequ, getVolForTermLogVarTS, span = span) 
  
  stopCluster(cl)
  
  return(res)
}
getRankVolAllGlobal <- function(cluster = makeCluster(2,methods = T,outfile=""))
{
  
  
  #iterate over terms
  sequ <- seq_along(completeCoocVocabTerms)
  
  res <- parSapply(cluster, sequ, getVolForTermLogVar) 
  
  return(res)
}
nzmean <- function(x) {
  zvals <- x==0
  if (all(zvals)) 0 else mean(x[!zvals])
}
nzvar <- function(x) {
  zvals <- x!=0
  if (sum(zvals, na.rm=TRUE) < 2) 0 
  else
    var(x[zvals])
}
nzrankQuantile <- function(x)
{
  zvals <- x==0
  
  if (all(zvals)) return(0) else tmp <- quantile(x[!zvals], type=8)
  
  #How to punish if not all point have a rank
  #n/Window?
  #1.Quantil - 2.Quantil
  span <- tmp[4] - tmp[2]
  factor <- length(x[!zvals])/length(x)
  
  #return(span * factor^2)
  return(span)
  
}
rankQuantile <- function(x)
{
  tmp <- quantile(x, type=8)
  #1.Quantil - 2.Quantil
  span <- tmp[4] - tmp[2]
  return(span)
}
#----------------------------------------chirstian's functions----------------------------------------------------
#preprocessing-TermTerm matritzen vorhanden
create_Matrix_Worte<-function(coocsYears,vocabIns)
{
  
  Matrix_Worte_Brexit<-matrix(c(0),length(vocabIns)[1],(length(coocsYears)))
  count<-1
  print("calc Matrix Worte")
  pb5<-txtProgressBar(min = 1,max=length(vocabIns),style = 3,width = 50)
  
  for (j in 1:(length(coocsYears)) ) {
    tmpCoocs <- coocsYears[[j]]
    coocs <- table(which(tmpCoocs[vocabIns,]!=0,arr.ind = T)[,1])
    
    
    Matrix_Worte_Brexit[as.integer(names(coocs)),rep(j,length(names(coocs)))]<-coocs
    setTxtProgressBar(pb5,j)
  }
  
  close(pb5)
  return(Matrix_Worte_Brexit)
}
create_Liste_Worte<-function(coocsYears,vocabIns)
{
  Liste_Worte<-vector("list",length(vocabIns))
  print(length(Liste_Worte))
  count<-1
  print("calc Liste Worte")
  pb4<-txtProgressBar(min = 1,max=length(coocsYears),style = 3,width = 50)
  
  for (j in 2:length(coocsYears) ) {
    
    tmpCoocs <- coocsYears[[j]]
    setTxtProgressBar(pb4,j)
    count <-1
    apply(tmpCoocs,1,function(x){
      kooks <- names(x)[which(x > 0)]
      Liste_Worte[[count]] <<- union(Liste_Worte[[count]],kooks)
      count <<- count + 1
    })
  }
  
  close(pb4)
  print(length(Liste_Worte))
  return(Liste_Worte)
}
calculate_volat<-function(coocsYears,h,method,vocabIns,datesTmp,terms)
{
  vocabIns<<-vocabIns
  print(length(vocabIns))
  print(length(coocsYears))
  print(method)
 
 if(method=="globalsig"||method=="sig")
  {
    volatility_data<-matrix(c(0),length(terms),(length(coocsYears)-h+1))
    c<-0
    for(term in terms)
    {
      print(term)
      c<-c+1
      matrix_term<-matrix(c(0),length(coocsYears),dim(coocsYears[[1]])[1])
      for (k in 1:length(coocsYears))
      {
        matrix_term[k,]<-log(coocsYears[[k]][term,])
      }
      for (i in 1:((length(coocsYears)-h+1)))
      {
        
        coocs_sig<-unique(which(matrix_term[(i:(i+h-1)),]>0,arr.ind = T)[,2])
       # coocs_sig<-unique(coocs_sig%%dim(matrix_term)[2])

        if(length(coocs_sig)==0){volatility_data[c,i]<-NaN}
        if(length(coocs_sig)==1){volatility_data[c,i]<-VarCoef(matrix_term[i:(i+h-1),coocs_sig])}
        else{
        distances<-apply(matrix_term[i:(i+h-1),coocs_sig],MARGIN = 2,VarCoef)
        volatility_data[c,i]<-mean(distances)
        }
      }
      
    }
    rownames(volatility_data)<-terms  
    return(volatility_data)
  }
  # Array_Term_Term_Matrices<-array(c(0),dim=c(length(coocsYears),dim(coocsYears[[1]]),dim(coocsYears)[2]))
  #for ( i in 1:length(coocsYears))
  #{  
  #  Array_Term_Term_Matrices[i,,]<-as.matrix(coocsYears[[i]])
  # }  
  Matrix_Worte_Brexit<<-create_Matrix_Worte(coocsYears,vocabIns)
  max_Matrix_Worte_Brexit<-apply(Matrix_Worte_Brexit,1,max)
  max_Matrix_Worte_Brexit<<-data.frame(max_Matrix_Worte_Brexit)
  
  mean_Matrix_Worte_Brexit<-apply(Matrix_Worte_Brexit,1,mean)
  mean_Matrix_Worte_Brexit<<-data.frame(mean_Matrix_Worte_Brexit)
  
  Liste_Worte<<-create_Liste_Worte(coocsYears,vocabIns)
  
  print(length(Liste_Worte))

  
  
  
  volatility_data<-matrix(c(0),length(terms),(length(coocsYears)-h+1))
  for (i in 1:((length(coocsYears)-h+1)))
  { count<-i
  cat("Iteration: ",i)
  print("calc indices")
  indices<-getIndices(coocsYears[i:(i+h-1)])
  if(method=="abs_min_abs_max")
  {
    #data<-array(c(0),dim = c(h,dim(coocsYears[[2]])[1],dim(coocsYears[[2]])[1]))
   # data_ranked<-array(c(0),dim = c(2*h,dim(coocsYears[[2]])[1],dim(coocsYears[[2]])[1]))
    data<-list()
    data_ranked<-list()
    for (sp in 1:h)
    {
      data[sp]<-Matrix(c(0),dim(coocsYears[[2]])[1],dim(coocsYears[[2]])[1])
      data_ranked[sp]<-Matrix(c(0),dim(coocsYears[[2]])[1],dim(coocsYears[[2]])[1])
      data_ranked[(sp+h)]<-Matrix(c(0),dim(coocsYears[[2]])[1],dim(coocsYears[[2]])[1])
    }
    
  }
  else
  { 
    data<-array(c(0),dim = c(h,dim(coocsYears[[2]])[1],dim(coocsYears[[2]])[1]))
    data_ranked<-data
  }
  for (j in 1:h)
  {
    if(method=="abs_min_abs_max"){data[j]<-Matrix((coocsYears[[(j+i-1)]]))}
    else{data[j,,]<-as.matrix(coocsYears[[(j+i-1)]])}
    print("apply Ranks")
    count<-count-1
    index_max<-i:(i+h-1)
    # if(j<h&&count>0){data_ranked[j,,]<-data_ranked[j+1,,]}
    # else
    # {
    switch(method,
           no_zero = data_ranked[j,,]<-apply_Rank(as.matrix(data[j,,]),method,indices,terms,index_max),            
           max_rank = data_ranked[j,,]<-apply_Rank(as.matrix(data[j,,]),method,indices,terms,index_max), 
           recommender= data_ranked[j,,]<-apply_Rank(as.matrix(data[j,,]),method,indices,terms,index_max),
           abs_min_abs_max={ ergeb<-apply_Rank((data[[j]]),method,indices,terms,index_max)
           data_ranked[(2*j-1)]<-ergeb[[1]]
           data_ranked[(2*j)]<-ergeb[[2]] }
    )
    #}
  }
  if(method=="abs_min_abs_max")
  {
    print("calc distance abs_min_abs_max")
    cost_Matrix<-berechneAbstand(data_ranked,terms,indices)
    print("calc mean")
    mean_volat<-matrix(c(0),length(terms),1)
    k=0
    for (term in terms)
    {
      k<-k+1
      mean_volat[k,1]<-mean(cost_Matrix[k,indices[[which(vocabIns==term)]]],na.rm = TRUE)
    } 
    mean_volat<-mean_volat/(h-1)
  }
  else
  {  
    print("calc IQR")
    print(max(data_ranked,na.rm=TRUE))
    #---umstellen iqr varcoef---
    IQR_Matrix<-getIQR(data_ranked,terms)
    #IQR_Matrix<-getVarCoef(data_ranked,terms)
    print("calc mean")
    mean_volat<-matrix(c(0),(length(terms)),1)
    k=0
    print(dim(IQR_Matrix))
    print(max(IQR_Matrix))

    for (term in terms)
    {
      k<-k+1
      mean_volat[k,1]<-mean(IQR_Matrix[k,indices[[which(vocabIns==term)]]],na.rm = TRUE)
    }
  }
  
  volatility_data[,i]<-mean_volat  
  }
  rownames(volatility_data)<-terms
  
  # print("readjust volat")
  #test
  #termVolatility <- list()
  #volatilityTS <- Matrix(0, nrow=3, ncol=(length(coocsYears)-h+1))
  # distinctDates <- names(table(datesTmp))
  #for (term in vocabIns)
  # {
  #freqData <- getDiachronicFreqs(datesTmp, binDTM, term = term)
  #x<-freqData[,"TermCount"]
  # print(term)
  #print(volatility_data[term,])
  #print(x)
  # normalized_f = (x-min(x))/(max(x)-min(x))
  #volatilityTS[1,]<-volatility_data[term,]
  #normalized_v = (volatilityTS[1,]-min(volatilityTS[1,]))/(max(volatilityTS[1,])-min(volatilityTS[1,]))
  
  # colnames(volatilityTS) <- distinctDates[1:(length(coocsYears)-h+1)]
  
  #volatilityTS[2,] <- normalized_v
  #normalized_f<-normalized_v
  # for(l in 1:(length(coocsYears)-h+1))
  #  {
  #   normalized_f[l]<-mean(x[l:(l+h-1)],na.rm=TRUE)
  # }  
  # normalized_f = (normalized_f-min(normalized_f))/(max(normalized_f)-min(normalized_f))
  #volatilityTS[3,] <- normalized_f
  
  
  # termVolatility[[term]] <- volatilityTS
  #test
  
  #
  #}
  
  
  
  
  
  
  return(volatility_data)
}  
#------------------------------------------------------------------------------------------------------------  
#calculate IQR of given array  
getIQR<-function(array,terms)
{
  pb<-txtProgressBar(min = 1,max=length(terms),style = 3,width = 50,title = "get IQR")
  IQRM<-matrix(c(0),length(terms),dim(array)[2])
  count=0
  for (term in terms)
  {
    count<-count+1
    IQRM[count,]<-apply(array[,which(vocabIns==term),],MARGIN=2,IQRNA)
    setTxtProgressBar(pb,count)
  }
  close(pb)
  return(IQRM)
}
#calculate varcoef
getVarCoef<-function(array,terms)
{
  pb<-txtProgressBar(min = 1,max=length(terms),style = 3,width = 50,title = "get IQR")
  VCM<-matrix(c(0),length(terms),dim(array)[2])
  count=0
  for (term in terms)
  {
    count<-count+1
    VCM[count,]<-apply(array[,which(vocabIns==term),],MARGIN=2,VarCoef)
    setTxtProgressBar(pb,count)
  }
  close(pb)
  return(VCM)
}
VarCoef<-function(x)
{
  return(sd(x)/mean(x))
}
#------------------------------------------------------------------------------------------------------------   
#function to calculate IQR and rooming NA-Values
IQRNA<-function(x)
{
  return(IQR(x,na.rm = TRUE))
}  
#------------------------------------------------------------------------------------------------------------   
#for every wordvector, get indices of those words that were apparent in the given time h   
getIndices<-function(data)
{  
  indices<-vector("list",dim(data[[1]])[2])  
  for (i in 1:dim(data[[1]])[2])
  {
    ind<-which(data[[1]][i,]>0)
    for (j in 2:length(data))
    {  
      ind<-union(ind,which(data[[j]][i,]>0))
    }  
    indices[[i]]<-ind
  }
  return(indices)
}
#------------------------------------------------------------------------------------------------------------   
#apply rank for 1 wordvector beginning by 1. for the biggest value, keeping na's  
rangDecr<-function(wordvector)
{
  return(rank(-wordvector,ties.method = "min",na.last = "keep"))
}
#------------------------------------------------------------------------------------------------------------ 
#applying ranks depending on used method
apply_Rank<-function(Term_Term_Matrix,method,indices,terms,index_max)
{
  switch(method,
         no_zero = {Rank_Matrix<-Term_Term_Matrix
         print("using no zero information")
         Term_Term_Matrix[which(Term_Term_Matrix==0)]<-NA
         for (i in which(vocabIns %in% terms))
         {
           Rank_Matrix[i,]<-rangDecr(Term_Term_Matrix[i,])
         }
         return(Rank_Matrix)
         },
         max_rank = {Rank_Matrix<-Term_Term_Matrix
         print("using max Rank")
         countermax=0
         Term_Term_Matrix[which(Term_Term_Matrix==0)]<-NA
         pb_max<-txtProgressBar(min = 1,max= length(which(vocabIns %in% terms)),style = 3,width = 50,title = "max_Rank")
         for (i in which(vocabIns %in% terms))
         {
           Rank_Matrix[i,]<-rangDecr(Term_Term_Matrix[i,])
         }
         RTT_ad<-Rank_Matrix
         for (i in  which(vocabIns %in% terms))
         { countermax<-countermax+1
           setTxtProgressBar(pb_max,countermax)
           if(length(Liste_Worte[[i]])>0)
           {
             for (j in 1: length(Liste_Worte[[i]]))
             {
               if(is.na(Rank_Matrix[i,j]))
               {
                 if(max(Rank_Matrix[i,],na.rm = TRUE)>0)
                 {
                   RTT_ad[i,j]<-max(Rank_Matrix[i,],na.rm = TRUE)
                 }
               }
             }
           }
         }
         close(pb_max)
         RTT_ad[which(RTT_ad==-Inf)]<-NA
         return(RTT_ad)
         },
         recommender = {rownames(Term_Term_Matrix)<-vocabIns
         print("using recommender system")
         colnames(Term_Term_Matrix)<-vocabIns
         Term_Term_Matrix<-adjust_Matrix(Term_Term_Matrix,indices,terms)
         Rank_Matrix<-Term_Term_Matrix
         Term_Term_Matrix[which(Term_Term_Matrix==0)]<-NA
         for (i in which(vocabIns %in% terms))
         {
           Rank_Matrix[i,]<-rangDecr(Term_Term_Matrix[i,])
         }
         return(Rank_Matrix)
         },
         abs_min_abs_max={
           pb_absmin<-txtProgressBar(min = 1,max=2*(length(terms)),style = 3,width = 50,title = "max_Rank")
           counterabs<-0
           Rank_Matrices<-list()
           print("using abs_min_abs_max")
           RankM<-Matrix(Term_Term_Matrix)
           for (i in which(vocabIns %in% terms)){
             counterabs<-counterabs+1
             vector<-xtfrm(RankM[i,])
             setTxtProgressBar(pb_absmin,counterabs)
             maxAnzahl<-max_Matrix_Worte_Brexit[i,1]
             #maxAnzahl<-max(Matrix_Worte_Brexit[i,index_max])
             vector<-rank(vector,ties.method = "min")
             help<-vector
             help[which(help==1)]<-0
             help[which(help>1)]<-help[which(help>1)]-(max(help[which(help>1)])-maxAnzahl)
             RankM[i,]<-help
             if(max(RankM[i,])>1)
             {
               RankM[i,]<-RankM[i,]/maxAnzahl
             }
           }
           Rank_Matrices[1]<-RankM
           
           RankM2<-Matrix(Term_Term_Matrix)
           RankM2[which(RankM2==0)]<-NA
           for (i in which(vocabIns %in% terms)){
             counterabs<-counterabs+1
             setTxtProgressBar(pb_absmin,counterabs)
             vector<-xtfrm(RankM2[i,])
             maxAnzahl<-max_Matrix_Worte_Brexit[i,1]
             #maxAnzahl<-max(Matrix_Worte_Brexit[i,index_max])
             vector<-rank(vector,ties.method = "max",na.last="keep")
             help<-vector
             help[is.na(help)]<-0
             RankM2[i,]<-help
             if(max(RankM2[i,])>=1)
             {
               RankM2[i,]<-RankM2[i,]/maxAnzahl
             }
           }
           Rank_Matrices[2]<-RankM2
           close(pb_absmin)
           return(Rank_Matrices)
         }
  )
}

adjust_wordvector<-function(TT_matrix,word,indices)
{
  kooks_word<-unlist(Liste_Worte[which(colnames(TT_matrix)==word)])
  distance_matrix<-matrix(c(0),1,length(kooks_word))
  colnames(distance_matrix)<-kooks_word
  wordvector_adjusted<-TT_matrix[word,]
  if(length(kooks_word)>0)
  {
    for (i in 1:length(kooks_word))
    {
      distance_matrix[i]<-cosine(TT_matrix[word,],TT_matrix[kooks_word[i],])
    }
    
    distance_matrix<-t(distance_matrix)
    max_Indices<-matrix(c(0),1,min(20,length(which(distance_matrix>0))))
    
    if(max(distance_matrix,na.rm=TRUE)>0)
    {
      for (j in 1:min(20,length(which(distance_matrix>0))))
      {
        max_Indices[j]<-which.max(distance_matrix)
        #distance_matrix<-distance_matrix[-max_Indices[j]]
        distance_matrix[max_Indices[j]]<-0
      }
      approx_matrix<-matrix(TT_matrix[kooks_word[max_Indices],],ncol=dim(TT_matrix)[2])
      colnames(approx_matrix)<-colnames(TT_matrix)
      #  print(rownames(approx_matrix))
      for ( ki in 1:length(kooks_word))
      {
        
        if(TT_matrix[word,kooks_word[ki]]==0)
        {
          wordvector_adjusted[kooks_word[ki]]<-mean(approx_matrix[,kooks_word[ki]])
        }
      }
    }
  }
  wordvector_adjusted[-unlist(indices)]<-0
  return(wordvector_adjusted)
  
}
adjust_Matrix<-function(Rank_Matrix,indices,terms)
{
  Matrix_adjusted<-Rank_Matrix
  counterrecom=0
  pb2<-txtProgressBar(min = 1,max=length(terms),style = 3,width = 50)
  for (i in which(vocabIns %in% terms))
  {
    counterrecom<-counterrecom+1
    Matrix_adjusted[,i]<-adjust_wordvector(Rank_Matrix,vocabIns[i],indices[i])
    setTxtProgressBar(pb2,counterrecom)
  }
  close(pb2)
  return(Matrix_adjusted)
}


berechneAbstand<-function(testarray,terms,indices)
{
  pb3<-txtProgressBar(min = 1,max=length(terms),style = 3,width = 50)
  Abstand<-0
  counter=0
  c=0
  #AbstandsMatrix<-array(c(0),dim=c(3,length(terms),dim(testarray[[1]])[2]))
  AbstandsMatrix<-list()
  AbstandsMatrix[1]<-Matrix(c(0),length(terms),dim(testarray[[1]])[2])
  AbstandsMatrix[2]<-Matrix(c(0),length(terms),dim(testarray[[1]])[2])
  AbstandsMatrix[3]<-Matrix(c(0),length(terms),dim(testarray[[1]])[2])
  for (term in terms)
  {
    index_term<-which(vocabIns==term)
      c<-c+1
      term_matrix<-matrix(c(0),length(testarray),dim(testarray[[1]])[1])
      for( k in 1:length(testarray))
      {
        term_matrix[k,]<-testarray[[k]][index_term,]
      }
    for(j in (indices[[index_term]]))
    {
    Werte<-term_matrix[,j]
      #Werte<-c(testarray[1,i,j],testarray[2,i,j],testarray[3,i,j],testarray[4,i,j],testarray[5,i,j],testarray[6,i,j]) 
  
      Abstand<-0
      for(ind in seq(1,(length(Werte)-3),2))
      {
        
        if(Werte[ind]!=0)
        {
          if(Werte[ind+2]!=0)
          {
            Abstand<-Abstand+diff(sort(c(dbeta(Werte[ind],2.5,1),dbeta(Werte[ind+2],2.5,1))))
            AbstandsMatrix[[2]][c,j]<-AbstandsMatrix[[2]][c,j]+diff(sort(c(dbeta(Werte[ind],2.5,1),dbeta(Werte[ind+2],2.5,1))))
          }
          else
          {
            Abstand<-Abstand+diff(sort(c(dbeta(Werte[ind+1],2.5,1),dbeta(Werte[ind+3],2.5,1)))) 
            AbstandsMatrix[[3]][c,j]<-AbstandsMatrix[[3]][c,j]+diff(sort(c(dbeta(Werte[ind+1],2.5,1),dbeta(Werte[ind+3],2.5,1)))) 
          }
        }
        else
        {
          Abstand<-Abstand+diff(sort(c(dbeta(Werte[ind+1],2.5,1),dbeta(Werte[ind+3],2.5,1)))) 
          AbstandsMatrix[[3]][c,j]<-AbstandsMatrix[[3]][c,j]+diff(sort(c(dbeta(Werte[ind+1],2.5,1),dbeta(Werte[ind+3],2.5,1)))) 
        }
        ind<-ind+2
      }
       AbstandsMatrix[[1]][c,j]<-Abstand
    }
    counter=counter+1
    setTxtProgressBar(pb3,counter) 
  }
  close(pb3)
  return(AbstandsMatrix[[1]])
  
} 