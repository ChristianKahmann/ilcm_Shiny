library(udpipe)
dl <- udpipe_download_model(language = "german")
str(dl)

model<-udpipe_load_model(file="german-ud-2.0-170801.udpipe")
txt<-"Peter schießt Sebastian ins Bein"
x <- udpipe_annotate(model, x = txt)
x <- as.data.frame(x)
str(x)





options( java.parameters = "-Xmx2g" ) 
library(NLP)
library(coreNLP)
#initCoreNLP() # change this if downloaded to non-standard location
initCoreNLP(annotators = "tokenize,ssplit,pos,lemma,parse")
## Some text.
s <- c("A rare black squirrel has become a regular visitor to a suburban garden.")
s <- as.String(s)


anno<-annotateString(s)
parse_tree <- getParse(anno)
parse_tree