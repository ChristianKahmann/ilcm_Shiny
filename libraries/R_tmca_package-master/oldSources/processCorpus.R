processCorpus <- function(x)
{
  jobIds <- which(data[,"source"] %in% jobs[x,1])
  
  write(paste0("Working on:",jobs[x,1]), stdout())
  
  m <- list(ID = "ID", content = "text",source="source",category="category")
  myReader <- readTabular(mapping = m)
  corpus <- Corpus(DataframeSource(data[jobIds,]), readerControl = list(reader = myReader))
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE, mc.cores=1)
  corpus <- tm_map(corpus, removeNumbers, mc.cores=1)
  corpus <- tm_map(corpus, stemDocument, language = jobs[x,2], mc.cores=1)
  corpus <- tm_map(corpus, content_transformer(tolower), mc.cores=1)

  #corpus <- tm_map(corpus, function(x) removeWords(x, germanStopwords))
  corpus <- tm_map(corpus, stripWhitespace, mc.cores=1)
  
  write(paste0("Done:",jobs[x,1]), stdout())
  
  return(corpus)
}