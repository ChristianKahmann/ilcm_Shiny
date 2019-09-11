chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 


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
  if(method=="globalno_zero"){method<-"no_zero"}
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
      matrix_term<-Matrix(c(0),length(coocsYears),dim(coocsYears[[1]])[1])
      for (k in 1:length(coocsYears))
      {
        matrix_term[k,]<-coocsYears[[k]][term,]
      }
      for (i in 1:((length(coocsYears)-h+1)))
      {
        
        coocs_sig<-unique(which(matrix_term[(i:(i+h-1)),]>0,arr.ind = T)[,2])
        # coocs_sig<-unique(coocs_sig%%dim(matrix_term)[2])
        
        if(length(coocs_sig)==0){volatility_data[c,i]<-NaN}
        if(length(coocs_sig)==1){volatility_data[c,i]<-sd(matrix_term[i:(i+h-1),coocs_sig])}
        else{
          distances<-apply(matrix_term[i:(i+h-1),coocs_sig],MARGIN = 2,sd)
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
  {
    gc()
    count<-i
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
      #data<-array(c(0),dim = c(h,dim(coocsYears[[2]])[1],dim(coocsYears[[2]])[1]))
      #data_ranked<-data
      data<-list()
      data_ranked<-list()
      for (sp in 1:h)
      {
        data[sp]<-Matrix(c(0),dim(coocsYears[[2]])[1],dim(coocsYears[[2]])[1])
        data_ranked[sp]<-Matrix(c(0),dim(coocsYears[[2]])[1],dim(coocsYears[[2]])[1])
        #data_ranked[(sp+h)]<-Matrix(c(0),dim(coocsYears[[2]])[1],dim(coocsYears[[2]])[1])
      }
    }
    for (j in 1:h)
    {
      if(method=="abs_min_abs_max"){data[[j]]<-Matrix((coocsYears[[(j+i-1)]]))}
      else{data[[j]]<-Matrix(coocsYears[[(j+i-1)]])}
      print("apply Ranks")
      
      index_max<-i:(i+h-1)
      # if(j<h&&count>0){data_ranked[j,,]<-data_ranked[j+1,,]}
      # else
      # {
      switch(method,
             no_zero = data_ranked[[j]]<-apply_Rank(Matrix(data[[j]]),method,indices,terms,index_max),            
             max_rank = data_ranked[j,,]<-apply_Rank(as.matrix(data[j,,]),method,indices,terms,index_max), 
             recommender= data_ranked[j,,]<-apply_Rank(as.matrix(data[j,,]),method,indices,terms,index_max),
             abs_min_abs_max={ ergeb<-apply_Rank((data[[j]]),method,indices,terms,index_max)
             data_ranked[[(2*j-1)]]<-ergeb[[1]]
             data_ranked[[(2*j)]]<-ergeb[[2]] }
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
        #mean_volat[k,1]<-cost_Matrix[k]
      } 
      #mean_volat<-mean_volat/(h-1)
    }
    else
    {  
      print("calc IQR")
      print(max(data_ranked,na.rm=TRUE))
      IQR_Matrix<-getIQR(data_ranked,terms)
      # IQR_Matrix<-getVarCoef(data_ranked,terms)
      #IQR_Matrix<-getSD(data_ranked,terms)
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
  IQRM<-Matrix(c(0),length(terms),dim(array)[2])
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
  pb<-txtProgressBar(min = 1,max=length(terms),style = 3,width = 50,title = "get VarCoef")
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
getSD<-function(array,terms)
{
  pb<-txtProgressBar(min = 1,max=length(terms),style = 3,width = 50,title = "get SD")
  VCM<-matrix(c(0),length(terms),dim(array)[2])
  count=0
  for (term in terms)
  {
    count<-count+1
    VCM[count,]<-apply(array[,which(vocabIns==term),],MARGIN=2,sd)
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
           
           RankM<-as.matrix(Term_Term_Matrix)
           for (i in which(vocabIns %in% terms)){
             counterabs<-counterabs+1
             vector<-(RankM[i,])
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
           Rank_Matrices[[1]]<-RankM
           
           RankM2<-as.matrix(Term_Term_Matrix)
           RankM2[which(RankM2==0)]<-NA
           for (i in which(vocabIns %in% terms)){
             counterabs<-counterabs+1
             setTxtProgressBar(pb_absmin,counterabs)
             vector<-(RankM2[i,1])
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
           Rank_Matrices[[2]]<-RankM2
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
  count_null<-matrix(c(0),5,1)
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
            #AbstandsMatrix[[2]][c,j]<-AbstandsMatrix[[2]][c,j]+diff(sort(c(dbeta(Werte[ind],2.5,1),dbeta(Werte[ind+2],2.5,1))))
            
          }
          else
          {
            Abstand<-Abstand+diff(sort(c(dbeta(Werte[ind+1],2.5,1),dbeta(Werte[ind+3],2.5,1)))) 
            # AbstandsMatrix[[3]][c,j]<-AbstandsMatrix[[3]][c,j]+diff(sort(c(dbeta(Werte[ind+1],2.5,1),dbeta(Werte[ind+3],2.5,1))))
            count_null[c]<-count_null[c]+1
          }
        }
        else
        {
          Abstand<-Abstand+diff(sort(c(dbeta(Werte[ind+1],2.5,1),dbeta(Werte[ind+3],2.5,1)))) 
          # AbstandsMatrix[[3]][c,j]<-AbstandsMatrix[[3]][c,j]+diff(sort(c(dbeta(Werte[ind+1],2.5,1),dbeta(Werte[ind+3],2.5,1)))) 
          
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



help_function<-function(x)
{
  i=x
  print("x")
  print(i)
  sentences<-as.matrix(split_by_sentence(as.character(data[i,3])))
  #print(sentences[1])
  # apply(sentences,1,function(x){sentence<-cbind(data_all[i,3],x);sentence_matrix_zeit<<-rbind(sentence_matrix_zeit,sentence);print(dim(sentence_matrix_zeit))})  
  sentences<-cbind(sentences,rep(data[i,2],dim(sentences)[1]))
  return(sentences)
}






split_by_sentence <- function (text) {
  
  # split based on periods, exclams or question marks
  result <- unlist (strsplit (text, split = "[\\.!?]+"))
  
  # do not return empty strings
  result <- stri_trim_both (result)
  result <- result [nchar (result) > 0]
  
  # ensure that something is always returned
  if (length (result) == 0)
    result <- ""
  
  return (result)
}



preprocess <- function(text, lang="en")
{
  test<-as.character(text)
  text <- tm::removePunctuation(text, preserve_intra_word_dashes = F)
  text <- tm::removeWords(text, stopwords(kind="de"))
  text <- tm::removeNumbers(text)
  #text <- tm::stemDocument(text,language=lang)
  text <- tolower(text)
  #text <- dehyphenation(text)
  text <- deleteSpecialCharacters(text)
  text <- tm::stripWhitespace(text)
  
  return(text)
}

dehyphenation <- function(text){
  # very simple dehyphenation heuristic
  text <- stringr::str_replace_all(text,"(\\p{Ll}+)[-??????][\\r\\n](\\p{Ll}+)", "\\1\\2\n")
  return(text)
}


deleteSpecialCharacters <- function(text){
  # very simple dehyphenation heuristic
  text <- stringr::str_replace_all(text,"[^\\p{L}\\p{Nd}-\\s]+", "")
  return(text)
}




calc_volat_model<-function(terms,history,GlobalCoocs,global,method,wf="linear"){
  #initialize history for each term with its global mean values for the cooccurrences
  #  history_sig<<-list()
 
  if(is.null(GlobalCoocs)){
    init<-do.call(cbind,CoocYears[1:history])
    history_sig<<-lapply(1:length(terms),FUN = function(i){return(matrix(init[terms[i],],history,dim(CoocYears[[1]])[2],byrow = T))})  
  }
  else{
    history_sig<<-lapply(1:length(terms),FUN = function(i){return(matrix(rep(GlobalCoocs[terms[i],],history),history,dim(GlobalCoocs)[2],byrow = T))})   
  }  
  
   
  #print("history initiiert")
  #create weightfactor
  if(wf=="linear"){
    weightfactor<<-1/((history*history+history)/2)
    for(f in 2:history){
      weightfactor<<-c(weightfactor,f/((history*history+history)/2))
    }
  }
  if(wf=="exp"){
    weightfactor<<-exp(1)
    for(f in 2:history){
      weightfactor<<-c(weightfactor,exp(f))
    }
    weightfactor<<-weightfactor/sum(weightfactor)
  }
  
  #get list of coocs for every term
  #browser()
  idx<<-lapply(X=1:length(terms),function(x){return(which(global[terms[x],]>0))})
  #idx<<-lapply(X=1:length(terms),function(x){return(which(global[terms[x],]>=0))})
  
  #iterate over every timeslice
  #for (j in 1:length(CoocYears))
  #t3<-Sys.time()
  res<-do.call(cbind,lapply(1:length(CoocYears),FUN = function(j){
    #print("zeit pro schleife")
    #print(Sys.time()-t3)
    #t3<-Sys.time()
    #get coocurrence information
    #print(j)
    local_sig<-CoocYears[[j]][terms,]
    #iterate over every term
    #for(l in 1:dim(local_sig)[1])
    #{
    #  res[l,j]<-abs_mean(calc_distance_dist_mult(local_sig[l,],l,j,history))
    #}
    #res[,j]<-
    
    return(do.call(rbind,new_func(local_sig,j,history,idx,method)))
    
    
    #return(do.call(rbind,lapply(1:dim(local_sig)[1],FUN = function(x,j){return(abs_mean(calc_distance_dist_mult(local_sig[x,],x,j,history)))},j)))
    # if(j%%50==0){gc()}
  }))
  rownames(res)<-terms
  return(res)
}




new_func<-function(local,j,history,idx,method){
  
  if(j<(history+1)){
    
    result_matrix<-matrix(c(0),dim(local)[1],dim(local)[2])
    for(k in 1:dim(local)[1]){
      if(length(which(local[k,]>0))>1){
        ind<-idx[[k]]
        result_matrix[k,ind]<-local[k,ind]-colSums(history_sig[[k]][,ind]*weightfactor)
      }
    }
    
    for(b in 1:dim(local)[1]){
      for(h in 1:(history-1)){
        history_sig[[b]][h,]<<-history_sig[[b]][(h+1),]
      }
      history_sig[[b]][history,]<<-local[b,]
    }
    return(lapply(1:dim(result_matrix)[1],FUN = function(x){
      ind2<-which(local[x,]>0)
      vec<-result_matrix[x,]
      vec[-ind2]<-0
      vec<-vec[idx[[x]]]
      return(abs_mean_TI(vec))
    }))
  }
  else{
    t1<-Sys.time()
    res<-list()
    
    #t1<-Sys.time()
    #memory<-do.call(rbind,lapply(history_sig,FUN = function(x){return(colSums(x*weightfactor))}))
    result_matrix<-local-do.call(rbind,lapply(history_sig,FUN = function(x){return(colSums(x*weightfactor))}))
    #####
    if(method=="cosine"){
      result_matrix<-matrix(c(0),dim(local)[1],1)
      for(i in 1:dim(local)[1]){
        result_matrix[i,1]<-lsa::cosine(local[i,],colSums(history_sig[[i]]*weightfactor)) 
        
      }
    }
    
    #####
    #t2<-Sys.time()-t1
    
    for(b in 1:dim(local)[1]){
      for(h in 1:(history-1)){
        history_sig[[b]][h,]<<-history_sig[[b]][(h+1),]
      }
      history_sig[[b]][history,]<<-local[b,]
      
      if(method=="standard"){
        #res[[b]]<-mvtnorm::dmvnorm(x=local[b,],mean = memory[b,],log = T)
        res[[b]]<-abs_mean(result_matrix[b,idx[[b]]])
      }
      if(method=="just_pos"){
        res[[b]]<-abs_mean(result_matrix[b,which(result_matrix[b,]>0)])
      }
      if(method=="just_neg"){
        res[[b]]<-abs_mean(result_matrix[b,which(result_matrix[b,]<0)])
      }
      if(method=="pos_vs_neg"){
        res[[b]]<-mean(result_matrix[b,idx[[b]]])
      }
      if(method=="cosine"){
        res[[b]]<-(1-result_matrix[b,1])
      }
    }
    
    return(res)
  }
}




varKoef<-function(vector,local){
  vector<-abs(vector)
  return((sd(vector)/mean(local)))
}

absSumMean<-function(vector){
  vector<-abs(vector)
  return(sum(vector)*mean(vector[which(vector!=0)]))
}
euklid<-function(vector){
  vector<-abs(vector)
  return(sqrt(sum(vector)))
}
miin<-function(vector){return(min(vector,na.rm=T))}
maax<-function(vector){return(max(vector,na.rm=T))}
meean<-function(vector){return(mean(vector,na.rm=T))}
suum<-function(vector){return(sum(vector,na.rm=T))}
abs_sum<-function(vector){vector<-abs(vector);return(sum(vector))}
abs_mean<-function(vector){vector<-abs(vector);return(mean(vector))}
abs_mean_log<-function(vector){vector<-abs(vector);return((mean(vector)*log(length(vector))))}
abs_mean_TI<-function(vector){vector<-abs(vector);vector<-vector[which(vector!=0)];return(mean(vector))}

sd_b<-function(vector){return((1000*sd(vector)))}

#t1<-Sys.time()
#a<-calc_volat_model(CoocYears = CY_WC_WS_W3,terms =colnames(CY_WC_WS_W3[[1]])[1:500],history = 3,GlobalCoocs = globalCoocs)
#t2<-Sys.time()-t1
#print(t2)



library(Matrix)
library(stringi)
#functions
split_by_sentence <- function (text) {
  
  # split based on periods, exclams or question marks
  result <- unlist (strsplit (text, split = "[\\.!?]+"))
  
  # do not return empty strings
  result <- stri_trim_both (result)
  result <- result [nchar (result) > 0]
  
  # ensure that something is always returned
  if (length (result) == 0)
    result <- ""
  
  return (result)
}


ccoocs <- function(binDTM, measure = "DICE", significanceThreshold = 1.0, minCoocFreq = 1,cores=3) {

  #if(class(binDTM)[1] != "dgCMatrix") stop("DTM must be \"dsCMatrix\" of package \"Matrix\".")
  
  # Ensure binary DTM
  if (any(binDTM > 1)) {
    binDTM <- binDTM >= 1 + 0
  }
  
  # calculate cooccurrence counts
  coocCounts <- t(binDTM) %*% binDTM
  
  #DELETE NA'S, Much faster on table object
  tmp <- Matrix::summary(coocCounts)
  
  #delete vocab whith no coocs

  tmp[tmp[,"x"] < minCoocFreq,"x"] <- 0
  
  #set diagonals to 0's
  tmp[tmp[,1] == tmp[,2],"x"] <- 0
  
  coocCounts <-Matrix::sparseMatrix(i=tmp[,1], j=tmp[,2], x=tmp[,3],
                            dimnames=dimnames(coocCounts),dims = dim(coocCounts))
  
  finalSig <-  Matrix::Matrix(0, nrow = nrow(coocCounts), ncol = ncol(coocCounts),sparse = T,dimnames = dimnames(coocCounts))
  
  k <- nrow(binDTM)
  kj <- colSums(binDTM)
  names(kj) <- colnames(binDTM)
  tmp_sig <- vector(mode="numeric", length=length(kj))
  names(tmp_sig) <- colnames(binDTM)
  
  relWords <- colnames(binDTM)
  # relWords <- intersect(names(which(kj > 0)),colnames(coocCounts)[which(rowSums(coocCounts) > 0)])
  
  # for (coocTerm in relWords)
  # {
  #  help_function(coocTerm)
  # }
  
  #t3<-Sys.time()
  #finalSig<-Matrix(unlist(mclapply(relWords,help_function)),ncol = length(relWords))
  #t4<-Sys.time()-t3
  #print(t4)
  switch(measure,
        DICE={    
           
           cuts<-max((dim(binDTM)[2]%/%200),cores)
           
           split <- split(1:dim(binDTM)[2], as.numeric(cut(seq_along(1:dim(binDTM)[2]),cuts)))
           dims<-dim(binDTM)[2]
           #cl <- parallel::makeCluster(cores,type="FORK",outfile="log.txt")
           #cl<-parallel::makeForkCluster(nnodes = 3,outfile='log.txt',homogeneous=TRUE,port=11001) 
           
           #parallel::clusterCall(cl, function(x) .libPaths(x), .libPaths())
           #parallel::setDefaultCluster(cl)
           #parallel::clusterExport(NULL, c('kj','dims','split',"coocCounts"),envir=environment())
           #parallel::clusterEvalQ(cl, library(Matrix))
           
           finalSig<-do.call(rbind,parallel::mclapply(mc.preschedule = T,mc.cleanup = T,mc.silent = T,mc.cores = cores,X = 1:cuts,FUN = function(f){
            
             # print(length(split[[f]]))
             # print(dim(binDTM)[2])
             help<-matrix(c(0),length(split[[f]]),dims)
             
             count=0
             for(i in split[[f]]){
               count=count+1
               help[count,]<-((kj+kj[i]+0.002))
             }
             
             return(Matrix((2*as.matrix(coocCounts[split[[f]],]))/(help),sparse = T))
             
             
           }
           
           )
           )
           #parallel::stopCluster(cl)
           colnames(finalSig)<-colnames(binDTM)
           rownames(finalSig)<-colnames(binDTM)
           return((finalSig))
         },
         MI={
           help<-matrix(c(0),dim(binDTM)[2],dim(binDTM)[2])
           for(i in 1:dim(binDTM)[2]){
             help[i,]<-((kj+0.001)*(kj[i]+0.001))
           }
           browser()
           finalSig<-log((k*as.matrix(coocCounts))/(help))
           rm(help)
           
           colnames(finalSig)<-colnames(binDTM)
           rownames(finalSig)<-colnames(binDTM)
           gc()
           return(finalSig)
           
         },
         COUNT = {
           finalSig <- coocCounts/sum(coocCounts)
           colnames(finalSig)<-colnames(binDTM)
           rownames(finalSig)<-colnames(binDTM)
           gc()
           return(Matrix(finalSig))
         }
  )
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}