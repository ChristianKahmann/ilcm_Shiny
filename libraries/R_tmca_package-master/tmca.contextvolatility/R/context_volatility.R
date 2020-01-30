# Christian Kahmann 2018
# NLP Group
# Leipzig University

####################################################################################
#' Implementation of the measure context volatility
#'
#' @author Christian Kahmann, Ahmad Dawar Hakimi
#' @title Context Volatility
#' @description Calculate context volatility based on a dtm and a corresponding date vector.
#'  Different approaches for calculating context volatility are integrated
#'
#' @usage 
#' 
#' calculate_context_volatility(binDTM = dtm,dates = dates,memory = 6,intervall = "month",cooc_measure = "DICE", significanceThreshold = 0,minCoocFreq = 1,measure = "rank_no_zero",terms)
#'
#' @examples 
#' 
#' library(tm)
#' 
#' text = c("These are 4 example sentences.",
#'    "Nothing special is happening in this example sentences",
#'    "Every example sentence has a different date.",
#'    "The date is an example for meta information that will be kept.",
#'    "I need 2 more example sentences.",
#'    "So my example works.")
#'    
#' text = tm::removePunctuation(text)
#'    
#' dates =(c("2018-01-09","2018-01-06","2018-04-07","2018-04-08", "2018-02-04", "2018-02-05"))
#' 
#' corpus <- Corpus(VectorSource(text))
#' dtm <- as(as.matrix(DocumentTermMatrix(corpus)), 'dgCMatrix')
#' row.names(dtm) <- dates
#'  
#'
#' vola = calculate_context_volatility(binDTM = dtm, dates = dates, memory = 1, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure = "sig_simple", terms = c('example', 'sentence', 'date'), logfile = NULL)
#' 
#' @section context_volatility-methods:
#' \describe{
#'    \item{\code{calculate_context_volatility(binDTM, dates, memory = 3, intervall = 'month', cooc_measure = 'DICE', significanceThreshold = 0, minCoocFreq = 1, maxCoocFreq = 1000000, measure, wf = 'linear', terms = NULL, logfile = NULL)}}{Computes the context volatility significance, context volatility significance window or the context volatility rank based on the used measure('sig_simple', 'sig_cosine', 'sig_vc_window', 'rank_minmax', 'rank_no_zero', 'rank_max_rank', 'rank_recommender')
#'    
#'    sig_simple: Calls the method calculate_context_volatility_sig with the parameter method='standard'.
#'    
#'    sig_cosine: Calls the method calculate_context_volatility_sig with the parameter method='cosine'.
#'    
#'    sig_vc_window: Calls the method calculate_context_volatility_sig_window with the parameter method='sig_vc'.
#'    
#'    sig_sd_window: Calls the method calculate_context_volatility_sig_window with the parameter method='sig_sd'.
#'    
#'    rank_minmax: Calls the method calculate_context_volatility_rank with the parameter method='minmax'.
#'    
#'    rank_no_zero: Calls the method calculate_context_volatility_rank with the parameter method='no_zero'.
#'    
#'    rank_max_rank: Calls the method calculate_context_volatility_rank with the parameter method='max_rank'.
#'    
#'    rank_recommender: Calls the method calculate_context_volatility_rank with the parameter method='recommender'.
#'    
#'    }
#'    
#'}
#'
#' @section context_volatility_sig-methods:
#' \describe{
#'    \item{\code{calculate_context_volatility_sig(Coocs_TimeSlices,global,memory,terms=NULL,wf="linear",method)}}{Computes the distance of the significance values of every term in a context to the previous contexts of every term for all time slices with a weightfactor which is created beforehand. The weightfactors are either 'linear' or 'exp'}
#'    \item{\code{calc_distance_sig(Coocs_TimeSlices,time,memory,context,method,weightfactor,terms)}}{Computes the distance of the significance values of the time slices depending on the used method(standard, cosine)
#'    
#'    standard: Subtracts from the coocurrence matrix of the time slice time all coocurrence matrices of all previous time slices. Afterwards it computes the absmean of all significance values for each term.
#'    
#'    cosine: Computes the cosine similarity of the local context of a term and the previous contexts of the term. Afterwards it computes and saves the cosine distance in a list.
#'}  
#'    \item{\code{rbind_contexts(Coocs_TimeSlices,term,time,memory)}}{Rbinds to a coocurrence matrix of an initial time slice which consist of all coocurrent words of the terms all other coocurrence matrices of all following time slices up to a given 'time'.}
#' }
#'    
#' @section context_volatility_sig_window-methods:
#' \describe{
#'    \item{\code{calculate_context_volatility_sig_window(Coocs_TimeSlices,memory,terms,method)}}{Computes the Coefficient of Variation or the Standard deviation of all significance values of all coocurrent words for each term in each time slice based on used method 'sig_vc' or sig_sd'.}
#'    \item{\code{VarCoef(x)}}{Computes the Coefficient of Variation}
#' } 
#' 
#' @section context_volatility_ranks-methods:
#' \describe{
#'    \item{\code{calculate_context_volatility_rank(Coocs_TimeSlices,memory,method,terms,global)}}{Computes the mean distance or mean IQR of the ranks of the coocurrent words of each term in each time slice. Computes the mean distance when the method is 'minmax' else it computes the mean IQR.}
#'    \item{\code{create_matrix_words(coocsYears,vocabIns)}}{Creates a matrix in which each column represents a single date and the row the amount of a coocurrent words from the vocabulary .}
#'    \item{\code{create_wordList(coocsYears,vocabIns,global,terms)}}{Creates a wordlist of the coocurrent words of each term which appear in the vocabulary  of the timeslice.}
#'    \item{\code{getIndices(data,terms)}}{Returns a vector with the indices of the coocurrent words of the terms in the data.}
#'    \item{\code{apply_Rank(Term_Term_Matrix,method,indices,terms,index_max)}}{Applying rank depending on the used method('no_zero', 'max_rank', 'recommender', 'minmax')
#'    
#'    no_zero: Not applying ranks to not observable cooccurrences.
#'    
#'    max_rank: Set non observable coocurrences to max rank.
#'    
#'    recommender: Adjusts the matrix with the function adjust_Matrix() before applying ranks.
#'    
#'    minmax: Creates a list with two sublist which each contain a rank matrix the first one is ranked with ties.method='min' and the second one with ties.method='max'
#'    
#'    }
#'    \item{\code{adjust_wordvector(Term_Term_Matrix,word,indices,terms)}}{It adjusts a wordvector of a Term_Term_Matrix through setting the terms significance to 0 and replace the other 0-significance with an approximate significance.}
#'    \item{\code{adjust_Matrix(Term_Term_Matrix,indices,terms)}}{It adjusts the Term_Term_Matrix through setting the terms significance to 0 and replace the other 0-significance with an approximate significance. Calls the adjusts_wordvector function. }
#'    \item{\code{calcDistance(data_ranked,terms,indices)}}{Computes the distances of the ranks of coocurrent words in different time slices for each term. Weights higher ranks more than lower ranks.}
#'    \item{\code{rangDecr(wordvector)}}{Apply rank for 1 wordvector beginning by 1. for the biggest value, keeping na's}
#'    \item{\code{getIQR(array,terms)}}{Computes the inter quartile range(IQR) of all ranks that the cooccurents of a term take for all time slices in a given history.}
#'    \item{\code{getVarCoef(array,terms)}}{Computes the Coefficient of Variation of all ranks that the cooccurents of a term take for all time slices in a given history.}
#'    \item{\code{getSD(array,terms)}}{Computes the standard deviation of all ranks that the cooccurents of a term take for all time slices in a given history.}
#'    \item{\code{IQRNA(x)}}{Computes IQR and removes NA-Values.}
#' }           
#' 
#' @section help functions:
#' \describe{
#'    \item{\code{min_no_na(vector)}}{Returns the smallest value of the vector.}
#'    \item{\code{max_no_na(vector)}}{Returns the biggest value of the vector.}
#'    \item{\code{mean_no_na(vector)}}{Returns the mean of the numbers of the vector.}
#'    \item{\code{sum_no_na(vector)}}{Returns the sum of the numbers of the vector.}
#'    \item{\code{abs_sum(vector)}}{Returns the absolute sum of the numbers of the vector.}
#'    \item{\code{abs_mean(vector)}}{Returns the absolute mean of the numbers of the vector.}
#'    \item{\code{abs_mean_TI(vector)}}{Removes all zeros of the vector and returns the absolute mean of the numbers of the vector.}
#'    \item{\code{printlog(message, logfile = NULL)}}{If logfile NULL it prints to console and if logfile is given it prints to the log file.}
#' }
#' 
#' 
#' @param binDTM a DocumentTermMatrix in the form of a Matrix::dgCMatrix
#' @param dates a vector of strings, each string represents a date
#' @param memory an integer used for Defaults to 3.
#' @param intervall a string specifying the time intervall('day','week',month','year'). Default to month.
#' @param cooc_measure a string specifying the coocurrence measure('DICE', 'LOGLIK', 'MI', 'COUNT') Default to DICE.
#' @param terms a vector of strings for which the context volatility should be computed. Defaults to NULL.
#' @param significanceThreshold a number which determinds a threshold. Significances below this threshold will be set to zero.
#' @param minCoocFreq a number specifying the minimal times a cooocurrence has to appear, for it not beeing set to 0. Defaults to 1.
#' @param maxCoocFreq a number specifying the maximal times a cooccurrence can appear, for it not beeing set to 0
#' @param meassure a string specifying if the context volatility significance, context volatility significance window or the context volatility rank should be computed. Default to rank_no_zero.
#' @param wf a string specifying the weightfactor. Defaults to linear.
#' @param logfile a string specifying the name of the logfile if NULL then it just prints out the message to the console. Defaults to NULL.
#' @param Coocs_TimeSlices a list with a sublist for each unique date containing a coocurrence matrix for the whole vocabulary
#' @param global a large dgCMatrix containing a coocurrence matrix for the whole vocabulary
#' @param method a string specifying which method should be used. (method options for computing context volatility significance are 'standard' or 'cosine')(method options for computing context volatility significance windows are 'sig_vc' or 'sig_sd')(method options for computing context volatility rank are 'no_zero', 'max_rank', 'recommender', 'minmax')
#' @param weightfactor a number specifying the computed weightfactor
#' @param time a number specifying a counter which is used to determine which TimeSlice should be used for the computation
#' @param context a list containing the coocurrent words of each term
#' @param term a string out of vector of the terms
#' @param coocsYear a copy of Coocs_TimeSlices
#' @param vocabIns a vector of strings containing the vocabulary
#' @param data a list containing a sublist of Coocs_TimeSlices
#' @param Term_Term_Matrix a list containing multiple sublists of Coocs_TimeSlices. The amount of sublists is set by the parameter memory.
#' @param indices a list containing a sublist for each term. Those sublist contain the indices of the coocurrent words to the term.
#' @param index_max a number specifying the maximum index
#' @param word a string specifying a word out of the vocabulary
#' @param data_ranked a list containing multiple sublists. Each sublist contains a matrix which holds the ranks of the coocurrent word to the terms. The amount of sublists is set by the parameter memory.
#' @param wordvector a row of a Term_Term_Matrix of a single term.
#' @param array a multidimensional array
#' @param message a string specifying what should be printed out or printed into a logfile
#' @param x a vector containing numbers
#' @param vector a vector containing numbers
#'
#'
#' @export

#library("tmca.cooccurrence")


calculate_context_volatility<-function(binDTM,dates, memory=3, intervall="month",Coocs_TimeSlices=NULL,global=NULL,un_dates=NULL,
                                       cooc_measure="DICE", terms=NULL, significanceThreshold=0, minCoocFreq=1, maxCoocFreq=1000000, measure,wf="linear", logfile = NULL){
  if(is.null(Coocs_TimeSlices) | is.null(global)){
    coocsCalc <- Coocc$new(binDTM)
    
    if (class(binDTM)[1] != "dgCMatrix")
      stop("DTM must be \"dgCMatrix\" of package \"Matrix\".")
    if (is.null(dates))
      stop("No dates given for document-term-matrix")
    if (nrow(binDTM) != length(dates))
      stop("Number of rows in document-term-matrix must be same length than length of dates.")
    if (any(binDTM > 1)) {
      binDTM <- binDTM >= 1 + 0
    }
    if(is.null(terms)){
      terms<-colnames(binDTM)
    }
    if(!all(terms%in%colnames(binDTM))){
      stop("at least one word is not included in the vocabulary!")
    }
    #transform dates to given intervall
    if(intervall=="month"){
      dates<-substring(as.character(dates),1,7)
    }
    if(intervall=="year"){
      dates<-substring(as.character(dates),1,4)
    }
    if(intervall=="week"){
      dates<-strftime(as.character(dates),format = "%Y-%V")
    }
    if(intervall=="day"){
      dates<-substring(as.character(dates),1,10)
    }
    
    #calculate cooccurrences for time-slices
    un_dates<-as.matrix(unique(dates))
    un_dates<-un_dates[order(as.numeric(un_dates),decreasing = F)]
    freq<-matrix(c(0),dim(binDTM)[2],length(un_dates))
    rownames(freq)<-colnames(binDTM)
    Coocs_TimeSlices<-list()
    count<-0
    
    coocsCalc$set_measure(cooc_measure)
    coocsCalc$set_significanceThreshold(significanceThreshold)
    coocsCalc$set_minCoocFreq(minCoocFreq)
    coocsCalc$set_maxCoocFreq(maxCoocFreq)
    
    for(d in un_dates){
      count<-count+1
      idx<-which(dates==d)
      coocsCalc$set_binDTM(binDTM[idx,])
      Coocs_TimeSlices[[count]]<- coocsCalc$ccoocs()
      freq[,count]<-colSums(binDTM[idx,])
    }
    printlog("Co-occurrence matrix for time slices calculated")
    #global<-Matrix(c(0),dim(binDTM)[2],dim(binDTM)[2])
    #for(i in 1:length(Coocs_TimeSlices)){
    #  global<-global+Coocs_TimeSlices[[i]]
    #  print(i)
    #  if(i%%10==0){gc()}
    #}
    coocsCalc$set_measure(cooc_measure)
    coocsCalc$set_significanceThreshold(significanceThreshold)
    coocsCalc$set_minCoocFreq(minCoocFreq)
    coocsCalc$set_maxCoocFreq(maxCoocFreq)
    coocsCalc$set_binDTM(binDTM)
    
    global<-coocsCalc$ccoocs()
    printlog("global Co-occurrence matrix calculated")
    gc()
  }
  #call CV-function based on given measure
  if(measure=="sig_simple"){
    result<-calculate_context_volatility_sig(Coocs_TimeSlices,global,memory,terms,wf=wf,method="standard")
    colnames(result)<-un_dates[(memory+1):length(un_dates)]
  }
  if(measure=="sig_cosine"){
    result<-calculate_context_volatility_sig(Coocs_TimeSlices,global,memory,terms,wf=wf,method="cosine")
    colnames(result)<-un_dates[(memory+1):length(un_dates)]
  }
  if(measure=="sig_vc_window"){
    result<-calculate_context_volatility_sig_window(Coocs_TimeSlices,memory,terms,method="sig_vc")
    colnames(result)<-un_dates[1:(length(un_dates)-memory+1)]
  }
  if(measure=="sig_sd_window"){
    result<-calculate_context_volatility_sig_window(Coocs_TimeSlices,memory,terms,method="sig_sd")
    colnames(result)<-un_dates[1:(length(un_dates)-memory+1)]
  }
  if(measure=="rank_minmax"){
    result<-calculate_context_volatility_rank(Coocs_TimeSlices,memory,terms,method="minmax",global)
    # colnames(result)<-un_dates[1:(length(un_dates)-memory+1)]
  }
  if(measure=="rank_no_zero"){
    result<-calculate_context_volatility_rank(Coocs_TimeSlices,memory,terms,method="no_zero",global)
    # colnames(result)<-un_dates[1:(length(un_dates)-memory+1)]
  }
  if(measure=="rank_max_rank"){
    result<-calculate_context_volatility_rank(Coocs_TimeSlices,memory,terms,method="max_rank",global)
    # colnames(result)<-un_dates[1:(length(un_dates)-memory+1)]
  }
  if(measure=="rank_recommender"){
    result<-calculate_context_volatility_rank(Coocs_TimeSlices,memory,terms,method="recommender",global)
    # colnames(result)<-un_dates[1:(length(un_dates)-memory+1)]
  }
  printlog("Context Volatility calculated")
  return(result)
}

###sig########################################################################################
calculate_context_volatility_sig<-function(Coocs_TimeSlices,global,memory,terms=NULL,wf="linear",method){
  #create weightfactor
  if(wf=="linear"){
    weightfactor<-1/((memory*memory+memory)/2)
    for(f in 2:memory){
      weightfactor<-c(weightfactor,f/((memory*memory+memory)/2))
    }
  }
  if(wf=="exp"){
    weightfactor<-exp(1)
    for(f in 2:memory){
      weightfactor<-c(weightfactor,exp(f))
    }
    weightfactor<-weightfactor/sum(weightfactor)
  }
  
  #get list of coocs for every term
  context_index<-which(global[terms,]>0,arr.ind = T)
  tmp<-Matrix::summary(global[terms,])
  tmp<-tmp[which(tmp[,3]>0),1:2]
  context<-list()
  for(i in unique(tmp[,1])){
    context[[i]]<-tmp[which(tmp[,1]==i),2]
  }
  #iterate over every timeslice
  loghelper<-floor(seq((memory+1),length(Coocs_TimeSlices),length.out = 11))[2:11]
  names(loghelper)<-c(10,20,30,40,50,60,70,80,90,100)
  res<-do.call(cbind,lapply((memory+1):length(Coocs_TimeSlices),FUN = function(j){
    print(paste0("finished volatility-calculation for ",j-memory, " out of ", length(Coocs_TimeSlices)-memory," timeslices"))
    if(j %in% loghelper){
      log_to_file(message = paste0("&emsp; ",names(which(loghelper==j)),"% of points in time processed"),logfile)
    }
    return(do.call(rbind,calc_distance_sig(Coocs_TimeSlices,j,memory,context,method,weightfactor,terms,context_index)))
  }))
  gc()
  rownames(res)<-terms
  return(res)
}


calc_distance_sig<-function(Coocs_TimeSlices,time,memory,context,method,weightfactor,terms,context_index){
  res<-list()
  local<-Coocs_TimeSlices[[time]][terms,]
  #history<-do.call(
  #  rbind,
  #  lapply(1:length(terms),FUN = function(x){
  #    return(t(Matrix(colSums(rbind_contexts(Coocs_TimeSlices,terms[x],time,memory)*weightfactor))))}
  #  )
  #)
  coocs_loc<-Coocs_TimeSlices[(time-memory):(time-1)]
  for(i in 1:length(coocs_loc)){
    coocs_loc[[i]]<-coocs_loc[[i]]*weightfactor[i]
  }
  history<-coocs_loc[[1]]
  if(length(coocs_loc)>1){
    for(i in 2:length(coocs_loc)){
      history<-history+coocs_loc[[i]]
    }
  }
  history<-history[terms,]
  
  if(method=="standard"){
    
    result_matrix<-local-history
    # result_matrixT<-t(result_matrix)
    #  
    #  context2<-NULL
    #  for(i in 1:length(context)){
    #    local_context<-((i-1)*dim(result_matrix)[2])+context[[i]]
    #    context2<-c(context2,local_context)
    #  }
    # result_matrixT[-context2]<-NA
    m<-cbind(context_index,result_matrix[context_index])
    res<-as.list(aggregate(m[,3],by=list(m[,1]),FUN=function(x){return(abs_mean(x))})[,2])
    return(res)
    
  }
  if(method=="cosine"){
    
    m<-cbind(context_index,paste(local[context_index],history[context_index],sep="_"))
    
    result_matrix<-aggregate(m[,3],by=list(rownames(m)),FUN=function(x){
      y<-as.matrix(stringr::str_split(string = as.character(x),pattern = "_",simplify = T))
      return(1-lsa::cosine(as.numeric(y[,1]),as.numeric(y[,2])))})
    rownames(result_matrix)<-result_matrix[,1]
    result_matrix<-result_matrix[rownames(local),2]
    
    res<-as.list(result_matrix)
   # for(i in 1:dim(local)[1]){
  #   print(i)
  #   result_matrix[i,1]<-lsa::cosine(local[i,context[[i]]],history[i,context[[i]]])
  #  }
  }
 # for(b in 1:dim(local)[1]){
    #if(method=="standard"){
    #  res[[b]]<-abs_mean(result_matrix[b,context[[b]]])
    #}
  #  if(method=="cosine"){
    #  res[[b]]<-(1-result_matrix[b,1])
   # }
  #}
  return(res)
}

#rbind_contexts<-function(Coocs_TimeSlices,term,time,memory){
#  result<-Coocs_TimeSlices[[(time-memory)]][term,]
#  for(i in (time-memory+1):(time-1)){
#    result<-rbind(result,Coocs_TimeSlices[[i]][term,])
#  }
#  return(result)
#}

###sigs-window########################################################################

calculate_context_volatility_sig_window<-function(Coocs_TimeSlices,memory,terms,method){
  volatility_data<-matrix(c(0),length(terms),(length(Coocs_TimeSlices)-memory+1))
  c<-0
  for(term in terms)
  {
    printlog(term)
    c<-c+1
    matrix_term<-Matrix(c(0),length(Coocs_TimeSlices),dim(Coocs_TimeSlices[[1]])[1])
    for (k in 1:length(Coocs_TimeSlices))
    {
      print(k)
      matrix_term[k,]<-Coocs_TimeSlices[[k]][term,]
    }
    for (i in 1:((length(Coocs_TimeSlices)-memory+1)))
    {
      coocs_sig<-unique(which(matrix_term[(i:(i+memory-1)),]>0,arr.ind = T)[,2])
      if(length(coocs_sig)==0){
        volatility_data[c,i]<-NaN
      }
      if(length(coocs_sig)==1){
        if(method=="sig_vc"){
          volatility_data[c,i]<-VarCoef(matrix_term[i:(i+memory-1),coocs_sig])
        }
        if(method=="sig_sd"){
          volatility_data[c,i]<-sd(matrix_term[i:(i+memory-1),coocs_sig])}
      }
      else{
        if(method=="sig_vc"){
          distances<-apply(matrix_term[i:(i+memory-1),coocs_sig],MARGIN = 2,VarCoef)
        }
        if(method=="sig_sd"){
          distances<-apply(matrix_term[i:(i+memory-1),coocs_sig],MARGIN = 2,sd)
        }
        volatility_data[c,i]<-mean(distances)
      }
    }
  }
  rownames(volatility_data)<-terms
  return(volatility_data)
}

VarCoef<-function(x)
{
  return(sd(x)/mean(x))
}

###ranks##############################################################################
calculate_context_volatility_rank<-function(Coocs_TimeSlices,memory,method,terms,global)
{
  #browser()
  vocabIns<<-colnames(Coocs_TimeSlices[[1]])
  matrix_words_brexit<<-create_matrix_words(Coocs_TimeSlices,vocabIns)
  max_matrix_words_brexit<-apply(matrix_words_brexit,1,max)
  max_matrix_words_brexit<<-data.frame(max_matrix_words_brexit)
  mean_matrix_words_brexit<-apply(matrix_words_brexit,1,mean)
  mean_matrix_words_brexit<<-data.frame(mean_matrix_words_brexit)
  
  wordList<<-create_wordList(vocabIns,global,terms)
  
  printlog(length(wordList))
  volatility_data<-matrix(c(0),length(terms),(length(Coocs_TimeSlices)-memory+1))
  for (i in 1:((length(Coocs_TimeSlices)-memory+1)))
  {
    gc()
    count<-i
    cat("Iteration: ",i)
    printlog("calc indices")
    indices<-getIndices(Coocs_TimeSlices[i:(i+memory-1)],terms)
    if(method=="minmax")
    {
      data<-list()
      data_ranked<-list()
      for (sp in 1:memory)
      {
        data[[sp]]<-Matrix(c(0),dim(Coocs_TimeSlices[[2]])[1],dim(Coocs_TimeSlices[[2]])[1])
        data_ranked[[sp]]<-Matrix(c(0),dim(Coocs_TimeSlices[[2]])[1],dim(Coocs_TimeSlices[[2]])[1])
        data_ranked[[(sp+memory)]]<-Matrix(c(0),dim(Coocs_TimeSlices[[2]])[1],dim(Coocs_TimeSlices[[2]])[1])
      }
      
    }
    else
    {
      data<-list()
      data_ranked<-list()
      for (sp in 1:memory)
      {
        data[sp]<-Matrix(c(0),dim(Coocs_TimeSlices[[2]])[1],dim(Coocs_TimeSlices[[2]])[1])
        data_ranked[sp]<-Matrix(c(0),dim(Coocs_TimeSlices[[2]])[1],dim(Coocs_TimeSlices[[2]])[1])
      }
    }
    for (j in 1:memory)
    {
      if(method=="minmax"){data[[j]]<- (Coocs_TimeSlices[[(j+i-1)]])}
      else{
        data[[j]]<-(Coocs_TimeSlices[[(j+i-1)]])
        #data[[j]][which(data[[j]]==0)]<-NA
      }
      printlog("apply Ranks")
      index_max<-i:(i+memory-1)
      switch(method,
             no_zero = data_ranked[[j]]<-apply_Rank(data[[j]],method,indices,terms,index_max),
             max_rank = data_ranked[[j]]<-apply_Rank((data[[j]]),method,indices,terms,index_max),
             recommender= data_ranked[[j]]<-apply_Rank((data[[j]]),method,indices,terms,index_max),
             minmax={ ergeb<-apply_Rank((data[[j]]),method,indices,terms,index_max)
             data_ranked[[(2*j-1)]]<-ergeb[[1]]
             data_ranked[[(2*j)]]<-ergeb[[2]] }
      )
      #}
    }
    if(method=="minmax")
    {
      printlog("calc distance minmax")
      cost_Matrix<-calcDistance(data_ranked,terms,indices)
      printlog("calc mean")
      mean_volat<-matrix(c(0),length(terms),1)
      k=0
      
      for (term in terms)
      {
        k<-k+1
        mean_volat[k,1]<-mean(cost_Matrix[k,indices[[k]]],na.rm = TRUE)
        #mean_volat[k,1]<-cost_Matrix[k]
      }
      #mean_volat<-mean_volat/(memory-1)
    }
    else
    {
      printlog("calc IQR")
      data_ranked<-array(do.call(cbind,data_ranked),dim=c(nrow(data_ranked[[1]]),ncol(data_ranked[[1]]),length(data_ranked)))
      printlog(max(data_ranked,na.rm=TRUE))
      IQR_Matrix<-getIQR(data_ranked,terms)
      #IQR_Matrix<-getVarCoef(data_ranked,terms)
      #IQR_Matrix<-getSD(data_ranked,terms)
      printlog("calc mean")
      mean_volat<-matrix(c(0),(length(terms)),1)
      k=0
      printlog(dim(IQR_Matrix))
      printlog(max(IQR_Matrix[1,],na.rm=T))
      printlog(length(indices[[1]]))
      
      for (term in terms)
      {
        k<-k+1
        mean_volat[k,1]<-mean(IQR_Matrix[k,indices[[k]]],na.rm = TRUE)
      }
    }
    volatility_data[,i]<-mean_volat
  }
  rownames(volatility_data)<-terms
  rm(wordList)
  rm(max_matrix_words_brexit)
  rm(mean_matrix_words_brexit)
  rm(vocabIns)
  return(volatility_data)
}

create_matrix_words<-function(coocsYears,vocabIns)
{
  matrix_words_brexit<-matrix(c(0),length(vocabIns)[1],(length(coocsYears)))
  pb<-txtProgressBar(min = 1,max=length(coocsYears),style = 3,width = 50)
  for (j in 1:(length(coocsYears))) {
    tmpCoocs <- coocsYears[[j]]
    coocs <- table(which(tmpCoocs[vocabIns,]!=0,arr.ind = T)[,1])
    matrix_words_brexit[as.integer(names(coocs)),rep(j,length(names(coocs)))]<-coocs
    setTxtProgressBar(pb,j)
  }
  close(pb)
  return(matrix_words_brexit)
}

create_wordList<-function(vocabIns,global,terms)
{
  wordList<-vector("list",length(terms))
  pb4<-txtProgressBar(min = 1,max=length(terms),style = 3,width = 50)
  
  for (j in 1:length(terms)) {
    wordList[[j]]<-vocabIns[which(global[terms[j],]>0)]
    setTxtProgressBar(pb4,j)
  }
  close(pb4)
  return(wordList)
}

getIndices<-function(data,terms)
{
  indices<-vector("list",length(terms))
  for (i in 1:length(terms))
  {
    ind<-which(data[[1]][terms[i],]>0)
    for (j in 2:length(data))
    {
      ind<-union(ind,which(data[[j]][terms[i],]>0))
    }
    indices[[i]]<-ind
  }
  return(indices)
}

#applying ranks depending on used method
apply_Rank<-function(Term_Term_Matrix,method,indices,terms,index_max)
{
  switch(method,
         no_zero = {
           #browser()
           Rank_Matrix<-Term_Term_Matrix[terms,]
           Rank_Matrix[which(Rank_Matrix==0)]<-NA
           printlog("using no zero information")
           #Term_Term_Matrix[which(Term_Term_Matrix==0)]<-NA
           for (i in 1:length(terms))
           {
             Rank_Matrix[i,]<-rangDecr(Rank_Matrix[i,])
           }
           return(Rank_Matrix)
         },
         max_rank = {
           Rank_Matrix<-Term_Term_Matrix[terms,]
           Rank_Matrix[which(Rank_Matrix==0)]<-NA
           printlog("using max Rank")
           countermax=0
           #Term_Term_Matrix[which(Term_Term_Matrix==0)]<-NA
           pb_max<-txtProgressBar(min = 1,max= length(which(vocabIns %in% terms)),style = 3,width = 50,title = "max_Rank")
           for (i in 1:length(terms))
           {
             Rank_Matrix[i,]<-rangDecr(Rank_Matrix[i,])
           }
           RTT_ad<-Rank_Matrix
           for (i in  1:length(terms))
           {
             countermax<-countermax+1
             setTxtProgressBar(pb_max,countermax)
             if(length(wordList[[i]])>0)
             {
               if(max(Rank_Matrix[i,],na.rm = TRUE)>0)
               {
                 j=which(is.na(Rank_Matrix[i,wordList[[i]]]))
                 RTT_ad[i,j]<-max(Rank_Matrix[i,],na.rm = TRUE)
               }
             }
           }
           close(pb_max)
           RTT_ad[which(RTT_ad==-Inf)]<-NA
           return(RTT_ad)
         },
         recommender = {
           rownames(Term_Term_Matrix)<-vocabIns
           printlog("using recommender system")
           colnames(Term_Term_Matrix)<-vocabIns
           Term_Term_Matrix<-adjust_Matrix(Term_Term_Matrix,indices,terms)
           Rank_Matrix<-Term_Term_Matrix[terms,]
           Rank_Matrix[which(Rank_Matrix==0)]<-NA
           #Term_Term_Matrix[which(Term_Term_Matrix==0)]<-NA
           for (i in 1:length(terms))
           {
             Rank_Matrix[i,]<-rangDecr(Rank_Matrix[i,])
           }
           return(Rank_Matrix)
         },
         minmax={
           pb_absmin<-txtProgressBar(min = 1,max=2*(length(terms)),style = 3,width = 50,title = "max_Rank")
           counterabs<-0
           Rank_Matrices<-list()
           printlog("using minmax")
           
           RankM<-as.matrix(Term_Term_Matrix[terms,])
           for (i in 1:length(terms)){
             counterabs<-counterabs+1
             vector<-(RankM[i,])
             setTxtProgressBar(pb_absmin,counterabs)
             #maxCount<-max_matrix_words_brexit[i,1]
             maxCount<-max(matrix_words_brexit[which(vocabIns==terms[i]),index_max])
             vector<-rank(vector,ties.method = "min")
             help<-vector
             help[which(help==1)]<-0
             help[which(help>1)]<-help[which(help>1)]-(max(help[which(help>1)])-maxCount)
             RankM[i,]<-help
             if(max(RankM[i,])>1)
             {
               RankM[i,]<-RankM[i,]/maxCount
             }
           }
           Rank_Matrices[[1]]<-RankM
           
           RankM2<-as.matrix(Term_Term_Matrix[terms,])
           RankM2[which(RankM2==0)]<-NA
           for (i in 1:length(terms)){
             counterabs<-counterabs+1
             setTxtProgressBar(pb_absmin,counterabs)
             vector<-(RankM2[i,])
             #maxCount<-max_matrix_words_brexit[i,1]
             maxCount<-max(matrix_words_brexit[which(vocabIns==terms[i]),index_max])
             vector<-rank(vector,ties.method = "max",na.last="keep")
             help<-vector
             help[is.na(help)]<-0
             RankM2[i,]<-help
             if(max(RankM2[i,])>=1)
             {
               RankM2[i,]<-RankM2[i,]/maxCount
             }
           }
           Rank_Matrices[[2]]<-RankM2
           close(pb_absmin)
           return(Rank_Matrices)
         }
  )
}


adjust_wordvector<-function(Term_Term_Matrix,word,indices,terms)
{
  #browser()
  kooks_word<-unlist(wordList[[which(terms==word)]])
  distance_matrix<-matrix(c(0),1,length(kooks_word))
  colnames(distance_matrix)<-kooks_word
  wordvector_adjusted<-Term_Term_Matrix[word,]
  if(length(kooks_word)>0)
  {
    for (i in 1:length(kooks_word))
    {
      distance_matrix[i]<-lsa::cosine(wordvector_adjusted,Term_Term_Matrix[kooks_word[i],])
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
      approx_matrix<-matrix(Term_Term_Matrix[kooks_word[max_Indices],],ncol=dim(Term_Term_Matrix)[2])
      colnames(approx_matrix)<-colnames(Term_Term_Matrix)
      #  print(rownames(approx_matrix))
      for ( ki in 1:length(kooks_word))
      {
        if(Term_Term_Matrix[word,kooks_word[ki]]==0)
        {
          wordvector_adjusted[kooks_word[ki]]<-mean(approx_matrix[,kooks_word[ki]])
        }
      }
    }
  }
  wordvector_adjusted[-unlist(indices)]<-0
  return(wordvector_adjusted)
  
}
adjust_Matrix<-function(Term_Term_Matrix,indices,terms)
{
  Matrix_adjusted<-Term_Term_Matrix
  counterrecom=0
  pb2<-txtProgressBar(min = 1,max=length(terms),style = 3,width = 50)
  for (i in which(vocabIns %in% terms))
  {
    counterrecom<-counterrecom+1
    Matrix_adjusted[,i]<-adjust_wordvector(Term_Term_Matrix,vocabIns[i],indices[which(terms==vocabIns[i])],terms)
    setTxtProgressBar(pb2,counterrecom)
  }
  close(pb2)
  return(Matrix_adjusted)
}

calcDistance<-function(data_ranked,terms,indices)
{
  pb3<-txtProgressBar(min = 1,max=length(terms),style = 3,width = 50)
  distance<-0
  counter=0
  c=0
  distanceMatrix<-Matrix(c(0),length(terms),dim(data_ranked[[1]])[2])
  count_null<-matrix(c(0),5,1)
  for (term in terms)
  {
    #index_term<-which(vocabIns==term)
    c<-c+1
    term_matrix<-matrix(c(0),length(data_ranked),dim(data_ranked[[1]])[2])
    for( k in 1:length(data_ranked))
    {
      term_matrix[k,]<-data_ranked[[k]][c,]
    }
    for(j in (indices[[c]]))
    {
      values<-term_matrix[,j]
      #values<-c(data_ranked[1,i,j],data_ranked[2,i,j],data_ranked[3,i,j],data_ranked[4,i,j],data_ranked[5,i,j],data_ranked[6,i,j])
      distance<-0
      for(ind in seq(1,(length(values)-3),2))
      {
        if(values[ind]!=0)
        {
          if(values[ind+2]!=0)
          {
            distance<-distance+diff(sort(c(dbeta(values[ind],2.5,1),dbeta(values[ind+2],2.5,1))))
            #distanceMatrix[[2]][c,j]<-distanceMatrix[[2]][c,j]+diff(sort(c(dbeta(values[ind],2.5,1),dbeta(values[ind+2],2.5,1))))
          }
          else
          {
            distance<-distance+diff(sort(c(dbeta(values[ind+1],2.5,1),dbeta(values[ind+3],2.5,1))))
            # distanceMatrix[[3]][c,j]<-distanceMatrix[[3]][c,j]+diff(sort(c(dbeta(values[ind+1],2.5,1),dbeta(values[ind+3],2.5,1))))
            count_null[c]<-count_null[c]+1
          }
        }
        else
        {
          distance<-distance+diff(sort(c(dbeta(values[ind+1],2.5,1),dbeta(values[ind+3],2.5,1))))
          # distanceMatrix[[3]][c,j]<-distanceMatrix[[3]][c,j]+diff(sort(c(dbeta(values[ind+1],2.5,1),dbeta(values[ind+3],2.5,1))))
        }
        ind<-ind+2
      }
      distanceMatrix[c,j]<-distance
    }
    #counter=counter+1
    #setTxtProgressBar(pb3,counter)
  }
  close(pb3)
  return(distanceMatrix)
  
}

rangDecr<-function(wordvector)
{
  return(rank(-wordvector,ties.method = "min",na.last = "keep"))
}

#calculate IQR of given array
getIQR<-function(array,terms)
{
  pb<-txtProgressBar(min = 1,max=length(terms),style = 3,width = 50,title = "get IQR")
  IQRM<-Matrix(c(0),length(terms),dim(array)[2])
  count=0
  for (term in terms)
  {
    count<-count+1
    IQRM[count,]<-apply(array[count,,],MARGIN=1,IQRNA)
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
    VCM[count,]<-apply(array[count,,],MARGIN=1,VarCoef)
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
    VCM[count,]<-apply(array[count,,],MARGIN=1,sd)
    setTxtProgressBar(pb,count)
  }
  close(pb)
  return(VCM)
}
VarCoef<-function(x)
{
  return(sd(x)/mean(x))
}

#function to calculate IQR and rooming NA-Values
IQRNA<-function(x)
{
  return(IQR(x,na.rm = TRUE))
}

##########################help functions#########################################################

min_no_na<-function(vector){return(min(vector,na.rm=T))}
max_no_na<-function(vector){return(max(vector,na.rm=T))}
mean_no_na<-function(vector){return(mean(vector,na.rm=T))}
sum_no_na<-function(vector){return(sum(vector,na.rm=T))}
abs_sum<-function(vector){vector<-abs(vector);return(sum(vector))}
abs_mean<-function(vector){vector<-abs(vector);return(mean(vector))}
abs_mean_TI<-function(vector){vector<-abs(vector);vector<-vector[which(vector!=0)];return(mean(vector))}


##########################################logging###############################################
printlog = function(message, logfile = NULL) {
  "Function that prints out the message or logs to file"
  
  if (is.null(logfile)) {
    print(message)
  }  
  else if(logfile=="silent"){
    #Do nothing
    return()
  }else{
    # if logfile is anything else than silent or NULL then try to write to that file
    message<-paste(Sys.time(),message,sep = ": ")
    write(message,file = file,append = T)
  }
}

