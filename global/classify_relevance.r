#Bitte diese DAtei nur bis Zeile 140 ausführen und sourcen. Der Rest danach kann weggelassen erden wenn sich nichts geändert hat.

#.libPaths(c("C:/_TOOLS/R_LIB"))
library(dplyr)
library(magrittr)
library(SparseM)
library(LiblineaR)
library(quanteda)
library(data.table)


source("global/classification_model.R")
source("global/TextProcessingBackend.R")
source("global/TextObjectWrapper.R")

#Diese Funktion liest Daten ein und konvertiert diese in ein Tibble basiertes Darstellungsformat
readData <- function() {
  if (!file.exists("sources.Rdata")) {
    
    #Bereits verarbeitete Daten im RData Forat
    load(file = "tokens_lemmatisiert_giz_and_kfw.RData")
    
    #2 Schleifen zum konsolidieren der Entitäten Frankfurt,am,Main wird "Frankfurt am Main"
    # Erster Durchlauf für KIZ Daten
    for (i in 1:length(tokens_lemmatisiert_giz))
    {
      tokens_lemmatisiert_giz[[i]]$doc_id <-
        rep(
          i + length(tokens_lemmatisiert_kfw),
          length(tokens_lemmatisiert_giz[[i]]$doc_id)
        )
      
      tmp <- (tokens_lemmatisiert_giz[[i]])
      class(tmp) <- c("spacyr_parsed", "data.frame")
      tokens_lemmatisiert_giz[[i]] <- spacyr::entity_consolidate(tmp)
    }
    
    
    #Wiederholung für KFW Dokumente
    for (i in 1:length(tokens_lemmatisiert_kfw))
    {
      tmp <- (tokens_lemmatisiert_kfw[[i]])
      class(tmp) <- c("spacyr_parsed", "data.frame")
      tokens_lemmatisiert_kfw[[i]] <- spacyr::entity_consolidate(tmp)
    }
    
    #Verbinden der Dokumente, die eineln als Liste geführt werden, zu Data.frames für die beiden Quellen
    data_giz <- data.table::rbindlist(tokens_lemmatisiert_giz)
    data_kfw <- data.table::rbindlist(tokens_lemmatisiert_kfw)
    
    #Verbinden der Dataframes zu einer Datenquelle mit allen Daten
    data <- rbind(data_giz, data_kfw)
    
    #Festlegen eines einheitlichen Encodings
    Encoding(data$lemma) <- "UTF-8"
    Encoding(data$token) <- "UTF-8"
    head(data, n = 20)
    
    #Speichern der verabreiteten Objekte für später
    save(data, file = "sources.Rdata")
    #Source Important Classes NO TMCA BECAUSE ALL FOR THOMAS
  }
  
  #Letze Sources Version 07.05.2018
  load("sources.Rdata")
  
  
  print("Extracting text preparing Object")
  #TRANSFORMATION TO RIGHT FORMAT
  #Umbenennen der Spalte doc_id zu doc_id_global
  data %<>% rename(doc_id_global = doc_id)
  #Kopieren der doc_id_global und sentence_id als neue ID "234_456"
  #Anlegen einer neuen spalte doc_id mit den neuen IDS
  data %<>% mutate(doc_id = paste(doc_id_global, sentence_id, sep = "_"))
  #Kopieren der Spalte token und die neue Spalte text
  data %<>% mutate(text = token)
  
  #Erzeugen von ngram Inforamtionen 
  test <- data[, c("lemma", "doc_id", "pos")]
  
  #Neue Spalte Shift
  test$shift <- NA
  #Verschieben der Tokens aus der lemma Spalte um 1 Position
  test$shift[1:(nrow(test) - 1)] <- test$lemma[2:nrow(test)]
  
  #Neue Spalte shift_id
  test$shift_id <- NA
  #Verscieben der Wort Ids um 1 Position
  test$shift_id[1:(nrow(test) - 1)] <- test$doc_id[2:nrow(test)]
  
  #Neue Spalte shift_pos
  test$shift_pos <- NA
  #Verschieben der POS Tags um 1 Position
  test$shift_pos[1:(nrow(test) - 1)] <- test$tag[2:nrow(test)]
  
  #Erstellen einer neuen Spalte Bigram aus lemma und shift
  test %<>% mutate(bigram = paste(test$lemma, test$shift, sep = "_"))
  
  #herausfiltern wo doc_id und shift_id gleich sind !!!! Satzgrenze!!! Kein echtes bigram
  test %<>% filter(doc_id == shift_id)
  
  tail(test)
  
  #Neue Liste von Wörtern aus lemma und bigram Inforamtionen
  feature_list <-
    data.frame(
      doc_id = c(data$doc_id, test$shift_id),
      token = c(data$lemma, test$bigram),
      stringsAsFactors = F
    )
  
  #Umbenennen der Spalten und as korrekte Format
  colnames(feature_list) <- c("doc_id", "token")
  
  #Order by document id
  feature_list <- feature_list[order(feature_list$doc_id), ]
  head(feature_list)
  
  
  print("Getting text content.")
  
  #Create new TextObjectWrapper to rebuild original documents
  TOW = TextObjectWrapper$new()
  
  class(data) <- c("spacyr_parsed", "data.frame")
  TOW$input(data)
  
  #control2 = list(remove_stopwords = F,tolower = T,remove_numbers  = T, char_length = c(2,99),remove_punctuation = T)
  
  #x = TOW$process(control2, "quanteda")  %>% TOW$output(format="tibble")
  
  #Get original Text from text object wrapper
  original_text <- TOW$get_original_documents()
  
  #Process all fetures (lemma,bigrams) to lowercase 
  feature_list$token <- tolower(feature_list$token)
  
  #id_muss durchlaufend sein assizionne id mit ref_id
  colnames(feature_list) <- c("ref_id", "feature")
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
initClassifier <- function() {
  # If the parameter_set method exposes the expected behaviour for all registered algorithms.
  #x = tmca.unsupervised::register[["LiblineaR::LiblineaR"]]$new()
  source("dict.R")
  
  print("Initializing Classifier")
  #Create new classifier Object
  classifier <- cmodel$new(method = "LiblineaR::LiblineaR")
  classifier$.__enclos_env__$private$silent = T
  
  
  
  #num_documents = length(unique(feature_list$id))
  #num_words = length(levels(factor(feature_list$feature)))
  
  #TWEAK FOR FASTER TESTING
  #Example: ref_id_cut <- unique(feature_list$ref_id)[1:20000]
  ref_id_cut <- unique(feature_list$ref_id)
  
  #check internal format; A list containing named vactors that carry the information
  my_gold <-
    list() #data.table::data.table(ref_id = unique(test_processed$ref_id))
  my_gold$ref_id <- ref_id_cut
  
  #Create empty representations for all possible fields
  my_gold$gold <- rep(NA, length(my_gold$ref_id))
  my_gold$dict <- rep(NA, length(my_gold$ref_id))
  my_gold$predicted <- rep(NA, length(my_gold$ref_id))
  my_gold$set <- rep("train", length(my_gold$ref_id))
  
  
  # Die Festlegung eines Validation Set kann ebenfalls über dieses Datenobjekt erfolgen
  # Dafür muss an den Datenpunkten, wo bereits annotierte Daten existieren, in der Spalte "SET" ein Eintrag "validation" erfolgen.
  #my_gold$set[my_gold$ref_id %in% itemsToSet] <- "validation"
  #my_gold$gold[my_gold$set != "validation"] <- "test"
  
  print("Initializing Data Objects")
  classifier$set_input(
    corpus = data.table(ref_id = original_text$doc_id[original_text$doc_id %in% ref_id_cut], text = original_text$token[original_text$doc_id %in% ref_id_cut]),
    features = feature_list[feature_list$ref_id %in% ref_id_cut, ],
    gold = my_gold
  )
  
  print("Dictionary Lookup")
  
  classifier$active_learn_dictionary_examples(dictionary = dict)
  
  return(classifier)
}

#Only first time #NO RUN WENN NICHTS GEÄNDERT
readData()

#second run with prepared objects #NO RUN WENN NICHTS GEÄNDERT
load(file = "feature_list.Rdata")
load(file = "original_text.Rdata")

#Initialize clean model #NO RUN WENN NICHTS GEÄNDERT
classifier <- initClassifier()
save(classifier, file = "classifier.Rdata")


# Hallo, ihr müsst eingetlich nur die folgenden Zeile ausführen und die obigen Pakete und Sourcen laden.
# Dann könnte ihr mit Zeile 158 in der KAtegorie "frag" Beispiele lernen (basiert auf Dictionary)
# Die Batch Size bestimmt, wie viele Beispiel pro durchlauf gelernt werden sollen (25 ist Standard)
# positive class bestimmt nach welcher Klasse bzw. welchen Beispielen gesucht wird
# Möglich Klassen sind general, frag, auth, capacity,  legitimiicy --> siehe dict.R
# wenn ihr dann umstellen wollt könnt ihr from_dictionary_hits = F setzen und dann wird nur noch mit Klassifikator generlnt und vorgeschlagen
load(file = "classifier.Rdata")

classifier$active_learning(
  strategy = "LCB",
  batch_size = 10,
  positive_class = "pro",
  from_dictionary_hits = T,
  labels = names(dict),
  cross = 10,
  coder = "AN"
)

#Der Zugriff auf die Gold Labels, Coder und Zeitstempel kann wie folgt geschenen
my_gold <- classifier$get_gold()

#Get all gold datasets which are not NA
gold_decisions <- rbindlist(my_gold$gold[!is.na(my_gold$gold)])
ids <- my_gold$ref_id[!is.na(my_gold$gold)]

#Find timestamp or coder
coder_idx <- which(gold_decisions$coder == "AN")

# Search for in between date not exact date
my_timestamp <- Sys.time() - hrs(2)

#Function to add or subtract hours
hrs <- function(u) {
  x <- u * 3600
  return(x)
}

#Function to add or subtract minutes
mns <- function(m) {
  x <- m * 60
  return(x)
}

timestamp_idx <-
  which((gold_decisions$timestamp > my_timestamp) &
          (gold_decisions$timestamp < my_timestamp + hrs(1)))

#coder timestamp combination
coder_timestamp_idx <- intersect(coder_idx, timestamp_idx)


#DELETE THOSE ANNOTATIONS
my_gold$gold[(my_gold$ref_id %in% ids[coder_timestamp_idx])] <- NA

#Die Label Daten ohne die gelöschten Gold-Annotationen werden wieder zurück in den Klassifizierer geschrieben.
classifier$set_gold(my_gold)
