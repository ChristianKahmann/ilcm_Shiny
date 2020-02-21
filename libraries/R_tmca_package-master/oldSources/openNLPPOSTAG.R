#POS TAG with openNLP
#Pre-processing

library(openNLP)
library(NLP)
options(java.parameters = "-Xmx4g" )
#Tokenisierung mit openNLP
# Function to convert a document in a vector of sentences
convert_text_to_sentences_postag <- function(text, lang = "de", SentModel = NULL, TokModel = NULL, POSModel = NULL, taggers = NULL) {
  
  # Convert to NLP:String Object
  text <- NLP::as.String(text)
  
  # Annotate sentence boundaries
  sentenceBoundaries <- NLP::annotate(text, SentModel)
  
  # Select the sentences out of the original text object using the annotations
  sentences <- text[sentenceBoundaries]
  
  #Pass sentences to POS annotator
  sentences <- sapply(sentences, processPos, TokModel=TokModel, POSModel=POSModel,taggers=taggers)
  
  #MErge by %S% for external purpose
  document <- paste(sentences["POStagged",], collapse = " %s% ")
  
  
  # return the documents
  return(list(DOC_PLAIN = document, SENTENCES = sentences))
}

processPos <- function(text, lang = "de", TokModel = NULL, POSModel = NULL,taggers=NULL) {
  
  text <-  NLP::as.String(text)
  
  a2 <- Annotation(1L, "sentence", 1L, nchar(text))
  
  a2 <- NLP::annotate(text, TokModel, a2)
  
  a3 <- NLP::annotate(text, POSModel, a2)
  
  for(tagger in taggers)
  {
    a3 <- NLP::annotate(text, tagger, a3)
  }
  
  a3w <- a3[a3$type == "word"]
  a3e <- a3[a3$type == "entity"]
  
  tmp <- as.list(a3w)
  
  a3final <- Annotation()
  
  lapply(a3e,function(x){
    
    firstTok <- which(a3w$start == x$start)
    lastTok <- which(a3w$end == x$end)
    
    for(i in firstTok:lastTok)
      tmp[[i]]$features[[1]]$POS <<- paste0("I-",toupper(substr(x$features[[1]]$kind,stop = 3,start=0)))  
    
  })
  
  lapply(tmp,function(x)a3final<<-merge(a3final,as.Annotation(x)))
  
  
  POStags <- unlist(lapply(a3final$features, `[[`, "POS"))
  
  POStagged <- paste(sprintf("%s/%s", text[a3final], POStags), collapse = " ")
  
  list(POStagged = POStagged, POStags = POStags, WORDS = a3w, entities = a3e)
}

#Function to convert a vector of documents into a vector of POS-Tagged documents
postag_text_source <- function(text, ...) {
  
  WTA <- Maxent_Word_Token_Annotator(model = "resources/en-token.bin")
  STA <- Maxent_Sent_Token_Annotator(model = "resources/en-sent.bin")
  PTA <- Maxent_POS_Tag_Annotator(model = "resources/en-pos-maxent.bin")
  NFP <- Maxent_Entity_Annotator(kind = "per", model = "resources/en-ner-person.bin")
  NFO <- Maxent_Entity_Annotator(kind = "org", model = "resources/en-ner-organization.bin")
  NFL <- Maxent_Entity_Annotator(kind = "loc", model = "resources/en-ner-location.bin")
  
  taggers <- list(NFP,NFO,NFL)
  
  
  pb <- txtProgressBar(min=0, max=length(text),title = "POS TAG PROGRESS",width = 40,char = "|",style = 3)
  i <- 0
  docs <- sapply(text, FUN=function(x){
    i <<- i + 1
    setTxtProgressBar(pb, i)
    convert_text_to_sentences_postag(x,SentModel = STA,TokModel = WTA, POSModel = PTA, taggers = taggers)
  }, ...)
  close(pb)
  
  return(docs)
}

#Tokenisieren mit openNLP

#function(x) x ist text return(tokenized vector)
#satze erst trennen -> neue Datenquelle
#Tokenisierer auf Sätzen aufrufen
# Achtung nicht jedes mal das Modell neu laden, sondern mit an die Funktion  übergeben.