# problem, was wenn ein window = 0 
## wenn wir 0 abfangen, dann wird trotzdem 0.te iteration berechnet und das sind die ersten nachbarn
cooccurrence_both_directions <- function(x,group=rep(1,length(x)),order = TRUE, ..., relevant = rep(TRUE, length(x)), skipgram_front = 0, skipgram_back = 0){
  stopifnot(all(max(skipgram_front,skipgram_back) >= 0))
  cooc <- term1 <- term2 <- NULL
  is_front_zero<-FALSE
  is_back_zero<-FALSE
  ## skipdistances if it is only 1 value, it is considered the maximum skip distance between words, compute all skip n-grams between 0 and skipgram
  ## if there are several values, consider them as such
  skipdistances_front <- as.integer(skipgram_front)
  if(length(skipdistances_front) == 1 & skipdistances_front !=0){
    skipdistances_front <- seq(0, skipdistances_front-1, by = 1)
  }else if(skipdistances_front == 0){
    is_front_zero<-TRUE
    skipdistances_front <- union(0L, skipdistances_front)
  }
  else{
    skipdistances_front <- union(0L, skipdistances_front)
  }
  skipdistances_back <- as.integer(skipgram_back)
  if(length(skipdistances_back) == 1 & skipdistances_back!=0){
    skipdistances_back <- seq(0, skipdistances_back-1, by = 1)
  }else{
    is_back_zero<-TRUE
    skipdistances_back <- union(0L, skipdistances_back)
  }
  # look which word are followed with the next word, 
  # look which word is followed by the 2nd next word, 3rd next word andsoforth
  # but if the data is not considered relevant, do not use it
  irrelevant <- !relevant
  if(is_back_zero!= TRUE){
    result_back <- lapply(skipdistances_back, FUN=function(n){
      result_backward<-result_back<-data.table(term1 = c(x),
                                               term2 = c(txt_previous(x, n = n + 1L)), 
                                               cooc = 1L,
                                               group1=c(group),
                                               group2=c(rep(NA,(n+1L)),group[1:(length(group)-(n+1L))]))
      #result<-rbind(result_forward,result_backward)
      # not_relevant <- txt_next(relevant, n = n + 1L)
      # not_relevant <- irrelevant | (not_relevant %in% FALSE)
      # if(sum(not_relevant) > 0){
      #   result[not_relevant, term1 := NA_character_] 
      #   result[not_relevant, term2 := NA_character_]  
      # }
      result_back <- subset(result_back, !is.na(term1) & !is.na(term2))
      result_back <- subset(result_back, result_back$group1==result_back$group2)
      result_back <- result_back[, list(cooc = sum(cooc)), by = list(term1, term2)]
      result_back
    })
  }else{
    result_back<-vector(mode = "list", length = 2)
  }
  if(is_front_zero != TRUE){
    result <- lapply(skipdistances_front, FUN=function(n){
      result_forward<-result <-  data.table(term1 = c(x),
                                            term2 = c(txt_next(x, n = n + 1L)), 
                                            cooc = 1L,
                                            group1=c(group),
                                            group2=c(group[(n + 2L):length(group)],rep(NA,(n+1L))))
      
      result <- subset(result, !is.na(term1) & !is.na(term2))
      result <- subset(result, result$group1==result$group2)
      result <- result[, list(cooc = sum(cooc)), by = list(term1, term2)]
      result
    })
  }else{
    result<-vector(mode = "list", length = 2)
  }
  browser()
  links<-data.table::rbindlist(result_back, fill = TRUE)
  rechts<-data.table::rbindlist(result, fill = TRUE)
  result_final<-rbind(links,rechts)
  #result_final<-rbindlist(result_final)
  #browser()
  #result_final <- subset(result_final, !is.na(term1) & !is.na(term2))
  result_final <- result_final[, list(cooc = sum(cooc)), by = list(term1, term2)]
  #result_all<-rbind(result_all,result)
  
  
  if(order){
    setorder(result_final, -cooc)  
  }
  class(result_final) <- c("cooccurrence", "data.frame", "data.table")
  result_final
}
######## Test #######################
my_data<-data.frame(words=c("Das","ist","ein","schöner", "test","der","der","super","viel","spaß","macht",
                            "Das","ist","ein","kleines", "beispiel","das","der","super","viel","freude","bereitet"),
                    id = c(1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2))
my_data<-data.frame(words=c("Das","ist","ist","ein"),
                    id = c(1,1,1,1))
my_data<-data.frame(words=c("Das","ist","ein","schöner", "test","der","der","super","viel","spaß","macht"),id = c(1,1,1,1,1,1,1,1,1,1,1))
ergebnis<-cooccurrence_both_directions(my_data$words,group=my_data$id,order = T,skipgram_front= 3,skipgram_back = 3)
### FEHLER ANALYSE ####
# Probleme bei der Diagonale
# wenn ich das skipgram fenster auf 3 einstelle werden mir aber 4 Stellen berechnet

############################################################################################## CLASSIC VERSION:
cooccurrence_both_directions <- function(x,group=rep(1,length(x)),order = TRUE, ..., relevant = rep(TRUE, length(x)), skipgram = 0){
  stopifnot(all(skipgram >= 0))
  cooc <- term1 <- term2 <- NULL
  
  ## skipdistances if it is only 1 value, it is considered the maximum skip distance between words, compute all skip n-grams between 0 and skipgram
  ## if there are several values, consider them as such
  skipdistances <- as.integer(skipgram)
  if(length(skipdistances) == 1){
    skipdistances <- seq(0, skipdistances, by = 1)
  }else{
    skipdistances <- union(0L, skipdistances)
  }
  
  # look which word are followed with the next word, 
  # look which word is followed by the 2nd next word, 3rd next word andsoforth
  # but if the data is not considered relevant, do not use it
  irrelevant <- !relevant
  
  result <- lapply(skipdistances, FUN=function(n){
    result_forward<-result <- data.table(term1 = c(x),
                                         term2 = c(txt_next(x, n = n + 1L)), 
                                         cooc = 1L,
                                         group1=c(group),
                                         group2=c(group[(n + 2L):length(group)],rep(NA,(n+1L))))
    
    
    result_backward<-data.table(term1 = c(x),
                                term2 = c(txt_previous(x, n = n + 1L)), 
                                cooc = 1L,
                                group1=c(group),
                                group2=c(rep(NA,(n+1L)),group[1:(length(group)-(n+1L))]))
    result<-rbind(result_forward,result_backward)
    # not_relevant <- txt_next(relevant, n = n + 1L)
    # not_relevant <- irrelevant | (not_relevant %in% FALSE)
    # if(sum(not_relevant) > 0){
    #   result[not_relevant, term1 := NA_character_] 
    #   result[not_relevant, term2 := NA_character_]  
    # }
    result <- subset(result, !is.na(term1) & !is.na(term2))
    result <- subset(result, result$group1==result$group2)
    result <- result[, list(cooc = sum(cooc)), by = list(term1, term2)]
    result
  })
  result <- rbindlist(result)
  result <- result[, list(cooc = sum(cooc)), by = list(term1, term2)]
  #result_all<-rbind(result_all,result)
  
  
  if(order){
    setorder(result, -cooc)  
  }
  class(result) <- c("cooccurrence", "data.frame", "data.table")
  result
}

###################################################################################################################
observeEvent(input$coocs_kwic_document,{
  selected_row<-as.numeric(stringr::str_split(string = input$coocs_kwic_document,pattern = "_",simplify = T)[1,6])
  validate(
    need(selected_row>0,message=F)
  )
  dataset<- stringr::str_split(string=values$coocs_examples_document_ids[selected_row],pattern = "_",simplify = T)[1,1]
  doc_id<- stringr::str_split(string=values$coocs_examples_document_ids[selected_row],pattern = "_",simplify = T)[1,2]
  token<-get_token_from_db(dataset = dataset,doc_ids = doc_id,host=values$host,port=values$port)
  id_targets<-union(which(tolower(token[,"word"])%in%input$coocs_examples_words),which(tolower(token[,"lemma"])%in%input$coocs_examples_words))
  
  token[id_targets,"word"]<-paste0(' <b style="color:',"black",'">',token[id_targets,"word"],'</b> ')
  ## edit: if skipgram selected show skigram windows back and front
  text<-paste(token[,"word"],collapse=" ")
  showModal(
    modalDialog(title = paste0("Document: ",dataset,"_",doc_id),easyClose = T,
                tags$div(HTML(text))
    ))
  isolate(shinyjs::runjs('Shiny.onInputChange(\"coocs_kwic_document\",  "coocs_kwic_show_doc_button_0")'))
})

#####################################################################################################################
sample_sentence<-data.frame(words = c("Did","you","read","this","book","?","It","'s","the","latest","work","by","Harris",".","You","can","read","the","latest","book","of","Harris","to","improve","your","work","."),
                            doc_id =c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2),
                      sentence_id = c(1,1,1,1,1,1,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1))
# test 2
sample_sentence<-data.frame(words=c("Did","you","read","the","book","from","Harris","Harris","is","the","best"),
                            doc_id=c(1,1,1,1,1,1,1,1,1,1,1),
                            sentence_id = c(1,1,1,1,1,1,1,1,1,1,1))
sample_sentence$words<-tolower(sample_sentence$words)
skipgram_cooc<-cooccurrence_both_directions(sample_sentence$words,group=paste0(sample_sentence$doc_id,"_",sample_sentence$sentence_id),order = T,skipgram_front = 2, skipgram_back = 2)

#start.time <- Sys.time()
library(tm)
#myCorpus <- Corpus(VectorSource(c("did you read this book ? it 's the latest work by harris .","you can read the latest book of harris to improve your work .")))
myCorpus <- Corpus(VectorSource(c("did you read the book from harris harris is the best")))
my_dtm<- DocumentTermMatrix(myCorpus)
#keep <-c("book","did","harris","latest","read","the","this","work","you","can","improve","your")
keep<-c( "best","book","did","from","harris","read","the","you")
skipgram_cooc<-skipgram_cooc[which(skipgram_cooc$term1 %in% keep),]
skipgram_cooc<-skipgram_cooc[which(skipgram_cooc$term2 %in% keep),]
skipgram_cooc_matrix<- Matrix::sparseMatrix(i = as.numeric(factor(skipgram_cooc$term1,levels = colnames(my_dtm))),
                                            j = as.numeric(factor(skipgram_cooc$term2,levels = colnames(my_dtm))),
                                            x = skipgram_cooc$cooc,
                                            dims = c(ncol(my_dtm),ncol(my_dtm)))

colnames(skipgram_cooc_matrix)<-my_dtm$dimnames$Terms
rownames(skipgram_cooc_matrix)<-my_dtm$dimnames$Terms
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished calculating Matrix within",time.taken),file = logfile)

coocsCalc <- Skip_cooc$new(skipgram_cooc_matrix)

coocsCalc$set_maxCoocFreq(10000000)
coocsCalc$set_minCoocFreq(2)
#log_to_file(message = "&emsp; Calculating coocs with Dice-Significance measure",logfile)
coocsCalc$set_measure("DICE")
coocs_matrix_dice<-coocsCalc$skip_ccoocs()
#log_to_file(message = "&emsp;  ✔ ",logfile)
gc()
###################### harry potter
doc<-data.frame(words=c("this","is","the","test","test","this","for","a","while","nerver","test","the","test","without","testing"),
                doc_id=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2),
                sentence_id=c(1,1,1,1,2,2,2,2,2,1,1,1,1,1,1))
doc$words<-tolower(doc$words)
skipgram_cooc<-cooccurrence_both_directions(doc$words,group=paste0(doc$doc_id,"_",doc$sentence_id),order = T,skipgram_front = 2, skipgram_back = 2)

#start.time <- Sys.time()
library(tm)

myCorpus <- Corpus(VectorSource(c("this is the test . test this for a while . never test the test without testing .")))
my_dtm<- DocumentTermMatrix(myCorpus)
keep<-c(  "for", "never" ,  "test"  ,  "testing", "the" ,    "this"   , "while",   "without")
skipgram_cooc<-skipgram_cooc[which(skipgram_cooc$term1 %in% keep),]
skipgram_cooc<-skipgram_cooc[which(skipgram_cooc$term2 %in% keep),]
skipgram_cooc_matrix<- Matrix::sparseMatrix(i = as.numeric(factor(skipgram_cooc$term1,levels = colnames(my_dtm))),
                                            j = as.numeric(factor(skipgram_cooc$term2,levels = colnames(my_dtm))),
                                            x = skipgram_cooc$cooc,
                                            dims = c(ncol(my_dtm),ncol(my_dtm)))

colnames(skipgram_cooc_matrix)<-my_dtm$dimnames$Terms
rownames(skipgram_cooc_matrix)<-my_dtm$dimnames$Terms
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished calculating Matrix within",time.taken),file = logfile)

coocsCalc <- Skip_cooc$new(skipgram_cooc_matrix)

coocsCalc$set_maxCoocFreq(10000000)
coocsCalc$set_minCoocFreq(2)
coocsCalc$set_measure("DICE")
coocs_matrix_dice<-coocsCalc$skip_ccoocs()
coocsCalc$set_measure("MI")
coocs_matrix_mi<-coocsCalc$skip_ccoocs()
gc()

##### test ob dice wirklich nur für binär geeignet
doc<-data.frame(words=c("this", "is", "the","this", "is", "the", "the", "this", "is", "bunny", "in","the","sun","this", "is", "the","this", "is", "the","this", "is", "the"),
                doc_id=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                sentence_id=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
doc$words<-tolower(doc$words)
skipgram_cooc<-cooccurrence_both_directions(doc$words,group=paste0(doc$doc_id,"_",doc$sentence_id),order = T,skipgram_front = 2, skipgram_back = 1)

#start.time <- Sys.time()
library(tm)

myCorpus <- Corpus(VectorSource(c("this is the this is the the this is bunny in the sun this is the this is the this is the")))
my_dtm<- DocumentTermMatrix(myCorpus)
keep<-c(  "this" ,  "the" ,  "bunny", "sun")
skipgram_cooc<-skipgram_cooc[which(skipgram_cooc$term1 %in% keep),]
skipgram_cooc<-skipgram_cooc[which(skipgram_cooc$term2 %in% keep),]
skipgram_cooc_matrix<- Matrix::sparseMatrix(i = as.numeric(factor(skipgram_cooc$term1,levels = colnames(my_dtm))),
                                            j = as.numeric(factor(skipgram_cooc$term2,levels = colnames(my_dtm))),
                                            x = skipgram_cooc$cooc,
                                            dims = c(ncol(my_dtm),ncol(my_dtm)))

colnames(skipgram_cooc_matrix)<-my_dtm$dimnames$Terms
rownames(skipgram_cooc_matrix)<-my_dtm$dimnames$Terms
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished calculating Matrix within",time.taken),file = logfile)

coocsCalc <- Skip_cooc$new(skipgram_cooc_matrix)

coocsCalc$set_maxCoocFreq(10000000)
coocsCalc$set_minCoocFreq(3)
coocsCalc$set_measure("DICE")
coocs_matrix_dice<-coocsCalc$skip_ccoocs()
