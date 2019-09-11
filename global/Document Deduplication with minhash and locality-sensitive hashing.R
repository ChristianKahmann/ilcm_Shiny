library(textreuse)
minhash <- minhash_generator(n = 240, seed = 3552)

minhash("das ist ein toller Satz2")


dokumente<-lapply(unique(token[,2]),FUN = function(x){
  paste(token[which(token[,2]==x),5],collapse=" ")
}
)

dokumente<-do.call(rbind,dokumente)

dokumente_red<-dokumente[1:1000]
dokumente_red<-c(dokumente_red,paste(dokumente_red[1],"das ist extra dazu gekommen."))
head(minhash(dokumente[1,1]))
length(minhash(dokumente[1,1]))

lsh_threshold(h = 200, b = 50)

corpus <- TextReuseCorpus(text=dokumente_red, tokenizer = tokenize_ngrams, n = 5,
                          minhash_func = minhash, keep_tokens = TRUE,
                          progress = FALSE)

buckets <- lsh(corpus, bands = 80, progress = TRUE)

candidates <- lsh_candidates(buckets)

lsh_compare(candidates, corpus, jaccard_similarity, progress = FALSE)
