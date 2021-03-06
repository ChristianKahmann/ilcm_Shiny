values$Doc_custom<-F

#' render customed input text
#' depends on: 
#'   values$Doc_url: document url
output$custom_inputtext_Sub_UI<-renderUI({
textAreaInput(inputId = "custom_inputtext_Sub",label = "custom input",value =  values$Doc_url,rows=3)
})


#' observe customed actopn für subcollections
#' depends on:
#'   input$custom_action_Sub: customed action for subcollection
#'   values$custom_inputtext_Sub: customed inputtext for subcollection
#'    values$Doc_custom: is there a customed document
#'    values$Doc_sort: sort documents
#'    values$Doc_start: start document
#'    values$Doc_url: url for document
#'    values$Doc_q: selected character/ word from document
#'    values$Doc_fq: time stamp of document
#'    values$Doc_fq_init: initial time the requests are made
#'    values$numFound_Sub: number of found subcollections
#'    values$Doc_search: search for documents
#'    values$Doc_solr_query: solr query of documents
#'    input$custom_inputtext: cutomed inputtext
#'    values$Doc_delete_documents: deleted documents
#'    values$Sub_search: search for subcollection
observeEvent(input$custom_action_Sub,{
  #check wheater solr query is valid and get solq query back
  values$custom_inputtext_Sub<-stringr::str_replace_all(string =input$custom_inputtext_Sub,pattern = " ",replacement = "%20" )
  if(stringr::str_detect(string = values$custom_inputtext_Sub,pattern = "rows=")){
    shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "Wrong rows-argument",text = "Please dont specify rows argument. Use Slider below the Search results instead.")
  }
  else{
    response<-NULL
    try({
      response<-solr_custom(url = values$custom_inputtext_Sub)
    },silent = T)
    #alert user when solr query was wrong
    if(is.null(response)){
      shinyWidgets::sendSweetAlert(type = "error",session = session,title = "Wrong syntax in Solr query",text = "Please check your query.")
    }
    #stop here response is NULL
    validate(
      need(!is.null(response),"wrong syntax")
    )
    values$Doc_custom<-TRUE
    values$Doc_sort<-""
    values$Doc_start=0
    values$Doc_url<-url
    values$Doc_q<-"custom"
    values$Doc_fq<-"custom"
    values$Doc_fq_init<-"custom"
    updateTextInput(session = session,inputId = "Doc_row_sel",value = 1)
    
    values$numFound_Sub<-as.integer(response$response[1])
    values$Doc_start=1
    values$Doc_search<-values$search+1
    values$Doc_solr_query<-input$custom_inputtext
    values$Doc_delete_documents<-NULL
  }
  values$Sub_search<-T
  shiny::removeModal()
}
)