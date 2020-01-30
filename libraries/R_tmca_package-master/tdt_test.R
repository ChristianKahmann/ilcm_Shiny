# test tdt
options(stringsAsFactors = F)
textdata <- read.csv("../_R_GESIS/Tutorials/data/corpus.csv", header = TRUE, sep = "\t", encoding = "UTF-8")


install("./tmca.util")
library(tmca.util)

colnames(textdata)[4] <- "text"
dtm <- tmca.util::process_dtm(data = textdata,use_cores = 1)
dtm <- tmca.util::deleteStopwordsFromDTM(dtm, lang="de")
colnames(dtm)
#monthly batches
textdata$year <- substr(textdata$DATE, 0, 4)
textdata$decade <- paste0(substr(textdata$DATE, 0, 3), "0")


t1 <- new("tdt_tracker")

for(level in levels(factor(textdata$year))[1:50])
{
  tmp_dtm <- dtm[textdata$year==level,,drop=FALSE]
  tmp_dtm <-  tmp_dtm[, colSums(tmp_dtm) > 0, drop=F]
  tmp_dtm <-  rBind(tmp_dtm, tmp_dtm[sample(1:nrow(tmp_dtm),size = 1),])
  #oder irgendein datenobjekt
  t1 <- track(t1, tmp_dtm)
}
  