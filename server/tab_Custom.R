values$custom<-FALSE
observeEvent(input$custom_action,{
  #check wheater solr query is valid and get solq query back
  values$custom_inputtext<-stringr::str_replace_all(string =input$custom_inputtext,pattern = " ",replacement = "%20" )
  if(stringr::str_detect(string = values$custom_inputtext,pattern = "rows=")){
    shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "Wrong rows-argument",text = "Please dont specify rows argument. Use Slider below the Search results instead.")
  }
  else{
    response<-NULL
    try({
      response<-solr_custom(url = values$custom_inputtext)
    },silent = T)
    #alert user when solr query was wrong
    if(is.null(response)){
      shinyWidgets::sendSweetAlert(type = "error",session = session,title = "Wrong syntax in Solr query",text = "Please check your query.")
    }
    #stop here response is NULL
    validate(
      need(!is.null(response),"wrong syntax")
    )
    values$custom<-TRUE
    values$sort<-""
    values$start=0
    values$url<-url
    values$q<-"custom"
    values$fq<-"custom"
    values$fq_init<-"custom"
    updateTextInput(session = session,inputId = "SR_row_sel",value = 1)
    
    values$numFound<-as.integer(response$response[1])
    values$start=1
    values$search<-values$search+1
    values$solr_query<-input$custom_inputtext
    values$delete_documents<-NULL
    updateTabsetPanel(session = session,inputId = "expl",selected = "Search Results")
  }
}
)