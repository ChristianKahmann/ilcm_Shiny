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

#reload results if reload button is clicked
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



#reset details to null lwhen results tab is oben
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