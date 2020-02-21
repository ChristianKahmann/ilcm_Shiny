#REad Conll

#NLP
library(NLP)
con <- scan("C:\\_TOOLS\\workspace_asv\\ASVUIMA\\tiger_release_aug07.corrected.16012013.conll09")
object1 = CoNLLTextDocument(scan("./conllu.test"), encoding = "utf-8", meta = list())

#cleanNLP
devtools::install_github("statsmaths/cleanNLP")
library(cleanNLP)
object2 = from_CoNLL("./conllu.test")
