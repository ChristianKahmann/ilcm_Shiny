# this function reads exported collections from epol exports

readEpolCorpusExport <- function(DATADIRECTORY, contextUnit = "documents", readFulltext = FALSE) {
  fileNameCorpus <- paste0(DATADIRECTORY,"/", contextUnit, "/docTermMatrix.csv")
  fileNameExternalIds <- paste0(DATADIRECTORY,"/", contextUnit, "/externalIds.csv")
  fileNameVocabulary <- paste0(DATADIRECTORY,"/", contextUnit, "/vocabulary.csv")
  fileNameDatesPerDoc <- paste0(DATADIRECTORY,"/", contextUnit, "/datesPerDoc.csv")
  fileNameFullText <- paste0(DATADIRECTORY,"/", contextUnit, "/fulltext.csv")

  # vocabulary
  vocabularyCount <- read.csv(fileNameVocabulary, head=FALSE, sep ="\t", stringsAsFactor=FALSE, enc="UTF-8", quote="")
  vocabularyDocuments <- as.character(vocabularyCount[,1])

  # external document Ids
  documentIds <- read.csv(fileNameExternalIds, head=FALSE, stringsAsFactor=FALSE)
  documentIds <- as.character(documentIds[,1])

  # document term matrix
  corpusRaw <- read.csv(fileNameCorpus, head=FALSE)
  documentsDTM <- sparseMatrix(i=corpusRaw[,1], j=corpusRaw[,2], x=corpusRaw[,3], dimnames=list(documentIds, vocabularyDocuments))

  # years per doc
  datesPerDoc <- read.csv(fileNameDatesPerDoc, head=FALSE)
  datesPerDoc <- datesPerDoc[,1]
  names(datesPerDoc) <- documentIds

  # paragraph / sentence Ids
  if (contextUnit != "documents") {
    fileNameCUDocumentIds <- paste0(DATADIRECTORY,"/", contextUnit, "/externalIds_documents.csv")
    cuDocumentIds <- read.csv(fileNameCUDocumentIds, head=FALSE, stringsAsFactor=FALSE)
    cuDocumentIds <- cuDocumentIds[,1]
  } else {
    cuDocumentIds = NULL
  }

  # full text
  fullText <- NA
  if (readFulltext) {
    fullText <- readLines(fileNameFullText, encoding = "UTF-8")
    names(fullText) <- documentIds
  }

  corpus <- list(
    dtm = documentsDTM,
    vocabulary = vocabularyDocuments,
    documentIds = documentIds,
    datesPerDoc = datesPerDoc,
    cuDocumentIds = cuDocumentIds,
    fullText = fullText)
  return (corpus)
}


filterCorpusByKeyword <- function(corpusObject, keywords) {
  filterIdx <- corpusObject$dtm[, keywords] > 0
  filteredCorpus <- list(
    dtm = corpusObject$dtm[filterIdx, ],
    vocabulary = corpusObject$vocabulary,
    documentIds = corpusObject$documentIds[filterIdx],
    datesPerDoc = corpusObject$datesPerDoc[filterIdx],
    cuDocumentIds = corpusObject$cuDocumentIds[filterIdx],
    fullText = corpusObject$fullText[filterIdx])
  return(filteredCorpus)
}
