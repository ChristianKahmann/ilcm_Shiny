#check wheather Search button was clicked
observeEvent(input$simple_action,{
  withBusyIndicatorServer("simple_action", {
    #for simple search fq is just defined by the chosen dataset
    if(length(isolate(input$dataset))==0){
     # fq<-"dataset_s:(*:*)"
      shinyWidgets::sendSweetAlert(session=session,title = "No corpus specified",text = "Please choose at least one corpus in the top left corner.",type = "warning")
    }
    else{
      if(length(isolate(input$dataset))==1){
        fq<-paste("dataset_s:\"",isolate(input$dataset),"\"",sep="")
      }
      if(length(isolate(input$dataset))>1){
        fq<-paste0("dataset_s:(\"",isolate(input$dataset)[1],"\"")
        for(i in 2:length(isolate(input$dataset))){
          fq<-paste0(fq," OR \"",isolate(input$dataset)[i],"\"")
        }
        fq<-paste0(fq,")")
      } 
      url<-values$solr_url
      q<-"*"
      #get inputstring
      s<-isolate(input$simple_inputtext)
      
      #transform input to Solr Query
      s<-stringr::str_replace_all(string = s,pattern = "\\+",replacement = " AND ")
      s<-stringr::str_replace_all(string = s,pattern = "\\#",replacement = " OR ")
      s<-stringr::str_replace_all(string = s,pattern = "\\-",replacement = " NOT ")
      
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
      values$sort<-""
      values$custom<-FALSE
      values$start=0
      values$url<-url
      values$q<-q
      values$fq<-fq
      values$fq_init<-fq
      updateTextInput(session = session,inputId = "SR_row_sel",value = 1)
      values$numFound<-as.integer(str_replace_all(string = str_extract(string =response[1],pattern = "numFound\\\":[0-9]+,"),pattern = "\\D",replacement = ""))
      values$start=1
      values$search<-values$search+1
      values$solr_query<-stringr::str_replace(string=stringr::str_replace(string=response[2],pattern = "&wt=(.)+",replacement = ""),pattern = "start=[0-9]+&rows=[0-9]&",replacement="")
      values$delete_documents<-NULL
      #switch to search results tab
      updateTabsetPanel(session = session,inputId = "expl",selected = "Search Results")
    }
  })
})

#create solr suggest suggestions for simple search
observe({
  validate(
    need(!is.null(input$simple_inputtext),message=F)
  )
  x<-input$simple_inputtext
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
    shinyTypeahead::updateTypeaheadInput(session = session,inputId = "simple_inputtext",choices = choices)
  }
})



