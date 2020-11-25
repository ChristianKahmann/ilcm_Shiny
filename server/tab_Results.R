source(file.path("server","tab_Results_Coocs.R"),local = T)$value
source(file.path("server","tab_Results_Classification.R"),local = T)$value
source(file.path("server","tab_Results_Dictionary_Extraction.R"),local = T)$value
source(file.path("server","tab_Results_Frequency_Extraction.R"),local = T)$value
source(file.path("server","tab_Results_Sentiment_Analysis.R"),local = T)$value
source(file.path("server","tab_Results_Topic_Model.R"),local = T)$value
source(file.path("server","tab_Results_Volatility.R"),local = T)$value
#source(file.path("server","tab_Results_Facto.R"),local = T)$value
source(file.path("server","tab_Results_Vecto.R"),local = T)$value
source(file.path("server","tab_Results_Deduplication.R"),local = T)$value
source(file.path("server","tab_Results_Keyword_Extraction.R"),local = T)$value
source(file.path("server","tab_Results_Syntactic_Parsing.R"),local = T)$value

#' reload results if reload button is clicked
#' depends on:
#'   input$Results_Reset: reset results
#'   values$reload_volat_result: reload volatility analysis result list
#'   values$reload_coocs_result: reload cooccurrence analysis result list
#'   values$reload_topic_result: reload topic model result list
#'   values$reload_de_result: reload dictionary extraction result list
#'   values$reload_fe_result: reload frequency extraction reuslt list
#'   values$reload_classification_result: reload classification result list
#'   values$reload_senti_result: reload sentiment analysis result list
#'   values$reload_vector_result: reload vector representation result list
#'   values$reload_deduplication_result: reload document deduplication result list
#'   values$reload_keyword_result: reload keyword extraction result list
#'   values$reload_parsing_result: reload syntactic parsing
observeEvent(input$Results_Reset,{
  values$reload_volat_result<-TRUE 
  values$reload_coocs_result<-TRUE 
  values$reload_topic_result<-TRUE 
  values$reload_de_result<-TRUE 
  values$reload_fe_result<-TRUE
  values$reload_classification_result<-TRUE
  values$reload_senti_result<-TRUE
 # values$reload_facto_result<-TRUE
  values$reload_vector_result<-TRUE
  values$reload_deduplication_result<-TRUE
  values$reload_keyword_result<-TRUE
  values$reload_parsing_result<-TRUE
}
)



#' reset details to null when results tab is at the top
#' depends on:
#'   input$coll: document collection
#'   values$Details_Analysis: details of analysis method
#'   values$fe_vocab: frequency extraction vocabulary list
#'   values$coocs_terms: Terms for cooccurrence analysis
#'   values$va_words: words from volatility analysis
#'   values$VS_selectList: selected list from vector space representation
#'   values$Det_SP_annotations: detailed vector space annotations
observe({
  validate(
    need(!is.null(input$coll),message=F)
  )
  if(input$coll=="Results"){
    values$Details_Analysis<-NULL
    values$fe_vocab<-NULL
    values$coocs_terms<-NULL
    values$va_words<-NULL
    values$VS_selectList<-NULL
    values$Det_SP_annotations<-NULL
  }
}
)