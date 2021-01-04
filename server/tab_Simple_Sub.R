#' check wheather Search button was clicked
#' depends on:
#'   input$simple_action_Sub: input of search button
#'   input$collections_rows_selected: selected rows of a certain collection
#'   values$solr_url: url tp solr 
#'   input$simple_inputtext_Sub: input text 
#'   values$Doc_sort: sorted documents
#'   values$Doc_custom: custemed documents 
#'   values$Doc_start: start document
#'   values$Doc_url: url of documents
#'   values$Doc_q: selected character / phrase from dropdown menu
#'   values$Doc_fq: time stamp of document request
#'   values$Doc_fq_init: initial time the request was made
#'   values$numFound_Sub: found numbers of documents in stack
#'   values$Doc_start: start document search
#'   values$Doc_search: documents to search
#'   values$search: search command
#'   values$Doc_solr_query: solr document query
#'   values$Doc_delete_documents: should documents be deleted?
#'   values$Sub_search: search values
observeEvent(input$simple_action_Sub,{
  #for simple search fq is just defined by the chosen dataset
  validate(
    need(length(input$collections_rows_selected)>0,"no Collection specified")
  )
  withBusyIndicatorServer("simple_action_Sub", {
    load(list.files("collections/collections/", full.names = T)[[input$collections_rows_selected]])
    Doc_q<-info[[8]]
    fq<-paste0('(collections:"',info[[5]],'")')
    
    
    url<-values$solr_url
    
    #get inputstring
    s<-isolate(input$simple_inputtext_Sub)
    if(s==""){
      s="*"
    }
    #transform input to Solr Query
    s<-stringr::str_replace_all(string = s,pattern = '\\+(?=((?:[^"]*"){2})*[^"]*$)',replacement = " AND ")
    s<-stringr::str_replace_all(string = s,pattern = '\\#(?=((?:[^"]*"){2})*[^"]*$)',replacement = " OR ")
    s<-stringr::str_replace_all(string = s,pattern = '\\-(?=((?:[^"]*"){2})*[^"]*$)',replacement = " NOT ")
    
    if(nchar(s)>0){
      q<-s
    }
    
    #check wheater solr query is valid and get solq query back
    response<-NULL
    try({
      response<-as.character(solr_search_advanced(base = url,q = q,fl="id",fq=fq,rows="1",raw=T))
    },silent = T)
    #alert user when solr query was wrong
    if(is.null(response)){
      shinyWidgets::sendSweetAlert(type = "error",session = session,title = "Wrong Syntax in Solr Query")
    }
    #stop here response is NULL
    validate(
      need(!is.null(response),"wrong syntax")
    )
    #set solr query parameters for calls in search results and timseries
    values$Doc_sort<-""
    values$Doc_custom<-FALSE
    values$Doc_start=0
    values$Doc_url<-url
    values$Doc_q<-q
    values$Doc_fq<-fq
    values$Doc_fq_init<-fq
    updateTextInput(session = session,inputId = "Doc_row_sel",value = 1)
    values$numFound_Sub<-as.integer(str_replace_all(string = str_extract(string =response[1],pattern = "numFound\\\":[0-9]+,"),pattern = "\\D",replacement = ""))
    values$Doc_start=1
    values$Doc_search<-values$search+1
    values$Doc_solr_query<-stringr::str_replace(string=stringr::str_replace(string=response[2],pattern = "&wt=(.)+",replacement = ""),pattern = "start=[0-9]+&rows=[0-9]&",replacement="")
    values$Doc_delete_documents<-NULL
    values$Sub_search<-T
    #switch to search results tab
    shiny::removeModal()
  })
})

#' create solr suggest suggestions for simple search
#' depends on:
#'   input$simple_inputtext_Sub: input text for search request (simple search selected)
#'   values$solr_url: url to solr query
observe({
  validate(
    need(!is.null(input$simple_inputtext_Sub),message=F)
  )
  x<-input$simple_inputtext_Sub
  praefix<-stringr::str_extract_all(string = x,pattern = "([a-zA-Z\\(\\\"\\+\\#\\-)]{1,100}[\\+\\-\\#]{1})*",simplify = T)[1,1]
  x<-stringr::str_replace(string = x,pattern = "([a-zA-Z\\(\\\"\\+\\#\\-)]{1,100}[\\+\\-\\#]{1})*",replacement="")
  praefix_x<-paste0(stringr::str_extract(string = x,pattern = '[\"\\(\\+\\#\\-]+'))
  if(praefix_x=="NA"){
    praefix_x<-""
  }
  x<-stringr::str_replace(string = x,pattern = '[\"\\(\\+\\#\\-]+',replacement="")
  if(nchar(x)==0){
    return()
  }
  else{
    #get suggestions from solr
    choices<-solr_suggest(base=stringr::str_replace(string = isolate(values$solr_url),pattern = "select/",replacement = "suggest/?"),q = x)
    if(!is.element(x,choices)){
      choices<-c(x,choices)
    }
    #append choices to praefix
    praefix<-paste0(praefix,praefix_x)
    choices<-paste0(praefix,choices)
    #update searchinput with new choices
    shinyTypeahead::updateTypeaheadInput(session = session,inputId = "simple_inputtext_Sub",choices = choices)
  }
})



