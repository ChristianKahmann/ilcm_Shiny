#Bitte diese DAtei nur bis Zeile 140 ausführen und sourcen. Der Rest danach kann weggelassen erden wenn sich nichts geändert hat.

#.libPaths(c("C:/_TOOLS/R_LIB"))
library(dplyr)
library(magrittr)
library(SparseM)
library(LiblineaR)
library(quanteda)
library(data.table)


source("classification_model.R")
source("TextProcessingBackend.R")
source("TextObjectWrapper.R")

readData <- function(){
  
  if (!file.exists("sources.Rdata")) {
    
  
  load(file="tokens_lemmatisiert_giz_and_kfw.RData")
  
  for(i in 1:length(tokens_lemmatisiert_giz))
  {
    tokens_lemmatisiert_giz[[i]]$doc_id <- rep(i+length(tokens_lemmatisiert_kfw), length(tokens_lemmatisiert_giz[[i]]$doc_id))
    
    tmp <- (tokens_lemmatisiert_giz[[i]])
    class(tmp)<-c("spacyr_parsed","data.frame")
    tokens_lemmatisiert_giz[[i]] <- spacyr::entity_consolidate(tmp)
  }
  
  for(i in 1:length(tokens_lemmatisiert_kfw))
  {
    tmp <- (tokens_lemmatisiert_kfw[[i]])
    class(tmp)<-c("spacyr_parsed","data.frame")
    tokens_lemmatisiert_kfw[[i]] <- spacyr::entity_consolidate(tmp)
  }
  
  data_giz <- data.table::rbindlist(tokens_lemmatisiert_giz)
  data_kfw <- data.table::rbindlist(tokens_lemmatisiert_kfw)
  
  data <- rbind(data_giz, data_kfw)
  
  
  Encoding(data$lemma) <- "UTF-8"
  Encoding(data$token) <- "UTF-8"
  head(data,n=20)
  
  save(data,file="sources.Rdata")
  #Source Important Classes NO TMCA BECAUSE ALL FOR THOMAS
  }
  #Letze Sources Version 07.05.2018
  load("sources.Rdata")
  
  
  print("Extracting text preparing Object")
  #TRANSFORMATION TO RIGHT FORMAT
  data %<>% rename(doc_id_global = doc_id)
  data %<>% mutate(doc_id = paste(doc_id_global,sentence_id,sep = "_"))
  data %<>% mutate(text = token)
  
  test <- data[,c("lemma","doc_id","tag")]
  test$shift <- NA
  test$shift[1:(nrow(test)-1)] <- test$lemma[2:nrow(test)]
  
  test$shift_id <- NA
  test$shift_id[1:(nrow(test)-1)] <- test$doc_id[2:nrow(test)]
  
  test$shift_pos <- NA
  test$shift_pos[1:(nrow(test)-1)] <- test$tag[2:nrow(test)]
  
  test %<>% mutate(bigram = paste(test$lemma,test$shift,sep="_"))
  
  test %<>% filter(doc_id == shift_id)
  
  tail(test)
  
  feature_list <- data.frame(doc_id = c(data$doc_id,test$shift_id), token = c(data$lemma,test$bigram),stringsAsFactors = F)
  
  colnames(feature_list) <- c("doc_id","token")
  
  feature_list <- feature_list[order(feature_list$doc_id),]
  head(feature_list)
  
  
  print("Getting text content.")
  TOW = TextObjectWrapper$new()
  
  class(data)<-c("spacyr_parsed","data.frame")
  TOW$input(data)
  
  #control2 = list(remove_stopwords = F,tolower = T,remove_numbers  = T, char_length = c(2,99),remove_punctuation = T)
  
  #x = TOW$process(control2, "quanteda")  %>% TOW$output(format="tibble") 
  
  original_text <- TOW$get_original_documents()
  feature_list$token <- tolower(feature_list$token)
  
  #id_muss durchlaufend sein assizionne id mit ref_id
  colnames(feature_list) <- c("ref_id","feature")
  feature_list$id <- as.integer(factor(feature_list$ref_id))
  
  save(feature_list, file = "feature_list.Rdata")
  save(original_text, file = "original_text.Rdata")
  
}

#' We init the classifier with this function
#'
#' @return
#' @export
#'
#' @examples
initClassifier <- function(){
  # If the parameter_set method exposes the expected behaviour for all registered algorithms.
  #x = tmca.unsupervised::register[["LiblineaR::LiblineaR"]]$new()
  source("dict.R")
  
  print("Initializing Classifier")
  classifier <- cmodel$new(method = "LiblineaR::LiblineaR")
  classifier$.__enclos_env__$private$silent = T
  
  
  
  #num_documents = length(unique(feature_list$id))
  #num_words = length(levels(factor(feature_list$feature)))
  
  #check internal format
  my_gold <- list() #data.table::data.table(ref_id = unique(test_processed$ref_id))
  my_gold$ref_id <- unique(feature_list$ref_id)
  
  my_gold$gold <- rep(NA,length(my_gold$ref_id))
  my_gold$dict <- rep(NA,length(my_gold$ref_id))
  my_gold$predicted <- rep(NA,length(my_gold$ref_id))
  my_gold$set <- rep("train",length(my_gold$ref_id))
  #my_gold$set[my_gold$set == "test"] <- "validation"
  #my_gold$gold[my_gold$set != "validation"] <- NA
  
  print("Initializing Data Objects")
  classifier$set_input(corpus = data.table(ref_id = original_text$doc_id, text = original_text$token),features = feature_list, gold = my_gold)
  
  print("Dictionary Lookup")
  
  classifier$active_learn_dictionary_examples(dictionary = deval_dict)
  
  return(classifier)
}

#Only first time #NO RUN WENN NICHTS GEÄNDERT
readData()

#second run with prepared objects #NO RUN WENN NICHTS GEÄNDERT
load(file = "feature_list.Rdata")
load(file = "original_text.Rdata")

#Initialize clean model #NO RUN WENN NICHTS GEÄNDERT
classifier <- initClassifier()
save(classifier, file="classifier.Rdata")


# Hallo, ihr müsst eingetlich nur die folgenden Zeile ausführen und die obigen Pakete und Sourcen laden.
# Dann könnte ihr mit Zeile 158 in der KAtegorie "frag" Beispiele lernen (basiert auf Dictionary)
# Die Batch Size bestimmt, wie viele Beispiel pro durchlauf gelernt werden sollen (25 ist Standard)
# positive class bestimmt nach welcher Klasse bzw. welchen Beispielen gesucht wird
# Möglich Klassen sind general, frag, auth, capacity,  legitimiicy --> siehe dict.R 
# wenn ihr dann umstellen wollt könnt ihr from_dictionary_hits = F setzen und dann wird nur noch mit Klassifikator generlnt und vorgeschlagen
load(file="classifier.Rdata")

classifier$active_learning(
  strategy = "LCB", 
  batch_size = 10, 
  positive_class = "legitimiicy", 
  from_dictionary_hits = T,
  labels = names(deval_dict),
  cross = 10,
  coder = "AN"
)
