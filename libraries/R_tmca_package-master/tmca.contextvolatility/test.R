#TEST VOlatility
library(Matrix)

source("readEpolCorpus.R")

myCorpus <- readEpolCorpusExport("./data/topic/", contextUnit = "sentences")
choose <- as.vector(which(colSums(myCorpus$dtm) > 3))
binDTM <- myCorpus$dtm[,choose]
dates <- as.vector(myCorpus$datesPerDoc)

##########TEST_VOLLI Paket

volli <- cv(binDTM,dates = dates)
volli <- cv(binDTM,dates = dates,intervall="MONTH",span=6,terms=c("kredit","anleger","immobilie","haus","schulden","lehman brothers"))