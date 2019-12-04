preprocess <- function(text, lang="en",SW=FALSE)
{

  text <- tm::removePunctuation(text, preserve_intra_word_dashes = F)
  if(SW==TRUE){
    text <- tm::removeWords(text, tm::stopwords(lang))
    }
  text <- tm::removeNumbers(text)
  #text <- tm::stemDocument(text,language=lang)
  text <- tolower(text)
  #text <- dehyphenation(text)
  text <- deleteSpecialCharacters(text)
  text <- tm::stripWhitespace(text)

  return(text)
}

deleteStopwordsFromDTM <- function(dtm, lang="en")
{
  sw <- tm::stopwords(lang)
  dtm <- dtm[,!(colnames(dtm) %in% sw)]
  return(dtm)
}

dehyphenation <- function(text){
  # very simple dehyphenation heuristic
  text <- stringr::str_replace_all(text,"(\\p{Ll}+)[-–—][\\r\\n](\\p{Ll}+)", "\\1\\2\n")
  return(text)
}


deleteSpecialCharacters <- function(text){
  # very simple dehyphenation heuristic
  text <- stringr::str_replace_all(text,"[^\\p{L}\\p{Nd}-\\s]+", "")
  return(text)
}

process_text_list <- function(id, data, ngram, mode, concatenator = "_", type="count"){
  if(id%%100==0)
    write(paste0("Processing Document: ",id),stdout())

  text <- data[id,"text"]
  #text <- preprocess(text=text,lang=lang,sw = sw)

  if(mode=="character")
    text <- gsub("\\s", "_", text)

  tokens <- quanteda::tokens(as.character(text),ngrams = ngram,what=mode, concatenator=concatenator)
  tokens <- unlist(tokens)

  switch(type,
         "count" = {
           word_count <- table(tokens)
           if(length(word_count) > 0)
             return(data.table::data.table(i=rep(id,length(word_count)),j=names(word_count),x=as.vector(word_count)))
           else
             return(data.table::data.table(i=integer(0),j=character(0),x=integer(0)))
           },
         "sequence" = {
           if(length(tokens) > 0)
             return(data.table::data.table(i=rep(id,length(tokens)),j=tokens,stringsAsFactors = F))
           else
             return(data.table::data.table(i=integer(0),j=character(0)))

           })

  #tokens <- tokenize_to_words(text,SentModel,WordModel)

  #tokens <- unlist(strsplit(as.character(text), "[[:space:]]+"))

  }

produce_token_datatable <- function(lang="en", data=NULL, process_ids=NULL, mode="word",ngram=1,concatenator="_", use_cores = 1, SW=FALSE, type = "count"){

  result <- list()

  #NO deleteion of Stopwords in the firstplace
  data[,"text"] <- preprocess(data[,"text"], lang, SW=F)

  if(use_cores < 2)
  {
    result <-
      lapply(process_ids,process_text_list,data,ngram,mode,concatenator,type)
  }
  else
  {

    cl <- parallel::makeCluster(use_cores,output="")

    parallel::clusterCall(cl, function(x) .libPaths(x), .libPaths())

    result <-
      parallel::parLapplyLB(cl,process_ids,process_text_list,data,ngram,mode,concatenator,type)
    parallel::stopCluster(cl)
  }

  return(result)

}

process_text_sequence <- function(lang="en", data=NULL, mode="word",ngram=1,concatenator="_", use_cores = 1, SW=FALSE){
  process_ids <- 1:nrow(data)
  #NO deleteion of Stopwords in the firstplace
  result <- produce_token_datatable(lang=lang,
                                    data=data,
                                    process_ids=process_ids,
                                    mode=mode,
                                    ngram=ngram,
                                    concatenator=concatenator,
                                    use_cores = use_cores, SW=F,
                                  type="sequence")

  result <- data.table::rbindlist(result)
  
  if(SW)
    result <- result[result$j %in% tm::stopwords(lang),]
  
  colnames(test_processed) <- c("id","feature","count")
  
  result$j<- factor(result$j)

  return(result)
}

# process_dtm
#
# This function converts a text data.source to a document term matrix
# It expects a data.frame containing a column "text"
process_dtm <- function(lang="en", data=NULL, mode="word",ngram=1,concatenator="_", use_cores = 1, SW=FALSE){
  process_ids <- 1:nrow(data)
  result <- produce_token_datatable(lang=lang, data=data,process_ids=process_ids, mode=mode,ngram=ngram,concatenator=concatenator, use_cores = use_cores, SW=SW)



  result <- data.table::rbindlist(result)
  word_factor<- factor(result$j)
  word_levels <- levels(word_factor)

  my_names <- list(process_ids,word_levels)

  corrected_ids <- result$i#as.integer(result$i) - min(as.integer(result$i)) + 1


  dtm <-Matrix::sparseMatrix(i=corrected_ids, j=as.integer(word_factor), x=as.integer(result$x),
                     dimnames=my_names,dims = c(length(my_names[[1]]),length(my_names[[2]])))

  if(SW)
  dtm <- deleteStopwordsFromDTM(dtm)
  
  return(dtm)
}
