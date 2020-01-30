preprocess <- function(text, lang="german",sw = "de")
{
  
  text <- removePunctuation(text, preserve_intra_word_dashes = TRUE)
  text <- removeWords(text, stopwords(kind=sw))
  text <- removeNumbers(text)
  text <- stemDocument(text,language=lang)
  text <- tolower(text)
  text <- stripWhitespace(text)
  
  
  return(text)
}

tokenize_to_words<-function(Text,SentModel,WordModel)
{
  text <- NLP::as.String(Text)
  sentenceBoundaries <- NLP::annotate(text, SentModel)
  #sentences <- text[sentenceBoundaries]
  
  wordBoundaries <- NLP::annotate(text,WordModel,sentenceBoundaries)
  words <- text[wordBoundaries[wordBoundaries$type == "word"]]
  #tokenized_vector<-rbind(tokenized_vector,words)
  
  return(words)
}

processToDTM <- function(cl,processIds,lang="german",sw = "de", data=NULL){
  
  
  result <- list()
  
  if(is.null(processIds))
    stop("Please provide a list of Ids to process from sources")
  
  ######CLUSTER_PAR############################
  
 result <-
    parLapplyLB(cl, processIds,function(x,data){
      
      if(x%%100==0)
        write(paste0("Processing Document: ",x),stdout())
      
      text <- data[x,"text"]
      text <- preprocess(text=text,lang=lang,sw = sw)
      
      if(nchar(text)<3)
        return()
      
      #tokens <- tokenize_to_words(text,SentModel,WordModel)
      
      tokens <- unlist(strsplit(as.character(text), "[[:space:]]+"))
      wordCount <- table(tokens)
      return(data.table(i=rep(x,length(wordCount)),j=names(wordCount),x=as.vector(wordCount)))
      #result <<- rbindlist(result,data.table(i=rep(x,length(wordCount)),j=names(wordCount),x=as.vector(wordCount)),use.names = T,fill = F)
      
    },data)
  

  
  ######CLUSTER_PAR############################
  
  #sapply(processIds,function(x){
    
    #if(x%%100==0)
    #  write(paste0("Processing Document: ",x),stdout())
    
    #text <- data[x,"text"]
    #text <- preprocess(text,lang=lang,sw = sw)
    
    #if(nchar(text)<3)
    #  return()
    
    #tokens <- tokenize_to_words(text,SentModel,WordModel)
    
    #tokens <- unlist(strsplit(as.character(text), "[[:space:]]+"))
    #wordCount <- table(tokens)
    #result[[x]] <<- data.table(i=rep(x,length(wordCount)),j=names(wordCount),x=as.vector(wordCount))
    #result <<- rbindlist(result,data.table(i=rep(x,length(wordCount)),j=names(wordCount),x=as.vector(wordCount)),use.names = T,fill = F)
  
  #})
  result <- rbindlist(result)
  WordFactor <- factor(result$j)
  WordLevels <- levels(WordFactor)
  
  
  #delete vocab whith no coocs
  #result[result[,"x"] < minCoocFreq,"x"] <- 0
  
  myNames <- list(processIds,WordLevels)
  
  correctedIds <- as.integer(result$i) - min(as.integer(result$i)) + 1

  
  dtm <-sparseMatrix(i=correctedIds, j=as.integer(WordFactor), x=as.integer(result$x),
                            dimnames=myNames,dims = c(length(myNames[[1]]),length(myNames[[2]])))
  
  return(dtm)
  #replace levels with level id
  #make matrix and then spare matrix
  #return
  
}

