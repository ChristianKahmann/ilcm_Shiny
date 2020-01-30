#Process data with tika
# TIKA Skript
.libPaths(c("C:/_TOOLS/R_LIB"))

tikaExtractTextFromFile <- function(file, sourceFolder, targetFolder){
  
  # Korrekter Aufruf von TIKA
  # java -jar tika-app-1.13.jar
  if(file.exists(paste0(targetFolder, file, ".txt")))
    return()
  cat("Extracting from ", file, "...\n")
  command <- paste0("java -jar tika-app-1.13.jar --text ", sourceFolder, file)
  
  output <- system(command, intern = T)
  output <- iconv(output, to = "UTF-8")
  
  # Korrektur
  dir.create(paste0(targetFolder, unlist(strsplit(file[1],split = "/"))[1]),recursive = T)
  fileConn<-file(paste0(targetFolder, file, ".txt"), encoding = "UTF-8")
  writeLines(output, fileConn)
  close(fileConn)
  
}


sourceFolder <- "./data/"
myFiles <- list.files(path = sourceFolder, pattern = NULL, 
                      full.names = FALSE, recursive = T,
                      include.dirs = FALSE)

targetFolder <- "./data_txt/"

# Iteriere über Dateien, extrahiere Text und lege in targetFolder ab
library(parallel)

cl <- makeCluster(detectCores()-1,outfile="./process.log")
clusterExport(cl,c('tikaExtractTextFromFile'))

parSapply(cl,myFiles,tikaExtractTextFromFile,sourceFolder = sourceFolder, targetFolder = targetFolder)

stopCluster(cl)

for (filename in myFiles) {
  cat("Extracting from ", filename, "...\n")
  tikaExtractTextFromFile(filename, sourceFolder = sourceFolder, targetFolder = targetFolder)
}





# ------------------------------------------
# TEIL II: extrahierten Text bereinigen

dehyphenation <- function(text){
  # very simple dehyphenation heuristic
  text <- gsub("(\\p{Ll}+)[-–—][\\r\\n](\\p{Ll}+)", "\\1\\2\n", text, perl = T)
  return(text)
}

removeBlankLines <- function(text){
  text <- gsub("[\\r\\n]\\s*[\\r\\n]", "\n", text, perl = T)
  return(text)
}

concatLines <- function(text) {
  text <- paste0(text, collapse = "\n")
  return(text)
}

removeLinebreaksInSentences <- function(text){
  text <- gsub("(\\p{Ll}|\\s+)[\\r\\n](\\s+|\\p{L})", "\\1 \\2", text, perl = T)
  return(text)
}

library(tm) #load text mining library

# DirSource specifies the exact folder where my text file(s) is for analysis with tm.
targetFolder <- "./data_X_txt/"
corpus  <- Corpus(DirSource(targetFolder, mode = "text", encoding = "UTF-8"), readerControl = list(language="de")) 

docToInspect <- 13

summary(corpus)  # check what we have read
print(as.character(corpus[[docToInspect]])) # single lines are stored in character vector

corpus <- tm_map(corpus, content_transformer(concatLines))
cat(as.character(corpus[[docToInspect]])) # complete document as one character string

corpus <- tm_map(corpus, content_transformer(removeBlankLines))
cat(as.character(corpus[[docToInspect]])) # blank lines removed

corpus <- tm_map(corpus, content_transformer(dehyphenation))
cat(as.character(corpus[[docToInspect]])) # dehyphenated

corpus <- tm_map(corpus, content_transformer(removeLinebreaksInSentences))
cat(as.character(corpus[[docToInspect]])) # less line breaks


# REGULAR PREPROCESSING
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english")) # this stopword file is at C:\Users\[username]\Documents\R\win-library\2.13\tm\stopwords 
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus <- tm_map(corpus, removeWords, stopwords("german")) # this stopword file is at C:\Users\[username]\Documents\R\win-library\2.13\tm\stopwords 
corpus <- tm_map(corpus, stemDocument, language = "german")
corpus <- tm_map(corpus, stripWhitespace)

as.character(corpus[[14]])
summary(corpus)

DTM <- DocumentTermMatrix(corpus)

Terms(DTM)
