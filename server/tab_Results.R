source(file.path("server","tab_Results_Coocs.R"),local = T)$value
source(file.path("server","tab_Results_Classification.R"),local = T)$value
source(file.path("server","tab_Results_Dictionary_Extraction.R"),local = T)$value
source(file.path("server","tab_Results_Frequency_Extraction.R"),local = T)$value
source(file.path("server","tab_Results_Sentiment_Analysis.R"),local = T)$value
source(file.path("server","tab_Results_Topic_Model.R"),local = T)$value
source(file.path("server","tab_Results_Volatility.R"),local = T)$value
#source(file.path("server","tab_Results_Facto.R"),local = T)$value
source(file.path("server","tab_Results_Vecto.R"),local = T)$value

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
}
)


