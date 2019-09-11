observeEvent(input$Det_action_Sub,{
  validate(
    need(length(input$collections_rows_selected)>0,"no Collection specified")
  )
  withBusyIndicatorServer("Det_action_Sub", {
    load(list.files("collections/collections/", full.names = T)[[input$collections_rows_selected]])
    Doc_q<-info[[8]]
    fq<-paste0('collections:"',info[[5]],'"')
    url<-values$solr_url
    #get inputstring
    s<-isolate(input$Det_inputtext_Sub)
    if(s==""){
      s="*"
    }
    #transform input to Solr Query
    s<-stringr::str_replace_all(string = s,pattern = "\\+",replacement = " AND ")
    s<-stringr::str_replace_all(string = s,pattern = "\\#",replacement = " OR ")
    s<-stringr::str_replace_all(string = s,pattern = "\\-",replacement = " NOT ")
    
    if(nchar(s)>0){
      q<-s
    }
    fq<-paste("(",fq,") AND ",sep="")
    #add date to solr fq field
    if(length(input$Det_vonin_Sub)>0 & length(input$Det_bisin_Sub)>0){
      fq<-paste0(fq,'date_dt:[',input$Det_vonin_Sub,'T00:00:00Z TO ',input$Det_bisin_Sub,'T00:00:00Z]')
    } 
    
    
    
    #add chosen section to fq
    if(input$Det_Section_Sub!="all"){
      fq=paste(fq,' AND section_ss:','"',input$Det_Section_Sub,'"',sep="")
    }
    
    #add chosen publisher to fq
    if(input$Det_Pub_Sub!="all"){
      fq=paste(fq," AND publisher_s:",'"',input$Det_Pub_Sub,'"',sep="")
    }
    
    #add chosen author to fq
    #if(input$Det_Author!="all"){
    #  fq=paste(fq,' AND author_txt:','"',input$Det_Author,'"',sep="")
    #}
    
    #add chosen type to fq
    if(input$Det_Type_Sub!="all"){
      fq=paste(fq," AND type_s:",'"',input$Det_Type_Sub,'"',sep="")
    }
    
    #add chosen tokenrange to 
    fq=paste(fq," AND token_i:[",input$Det_Token_Sub[1],"TO",input$Det_Token_Sub[2],"]")
    
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
    values$Doc_start=0
    values$Doc_custom<-FALSE
    values$Doc_url<-url
    values$Doc_q<-q
    values$Doc_fq<-fq
    values$Doc_fq_init<-fq
    values$numFound_Sub<-as.integer(str_replace_all(string = str_extract(string =response[1],pattern = "numFound\\\":[0-9]+,"),pattern = "\\D",replacement = ""))
    updateTextInput(session = session,inputId = "Doc_row_sel",value = 1)
    values$Doc_start=1
    values$Doc_search<-values$search+1
    values$Doc_solr_query<-stringr::str_replace(string=stringr::str_replace(string=response[2],pattern = "&wt=(.)+",replacement = ""),pattern = "start=[0-9]+&rows=[0-9]&",replacement="")
    values$Sub_search<-T
    shiny::removeModal()
  })
})



#render input fields for detailed search depending on queries to maria db
output$Det_publication_Sub<-renderUI({
  validate(
    need(length(values$Doc_dataset)>0,message=FALSE)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  res<-selectInput(inputId = "Det_Pub_Sub",label = "Publisher:",choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select publisher from meta_publisher where dataset IN (",paste("'",values$Doc_dataset,"'",collapse=" ,",sep=""),");",collapse = "")),"all")),selected = "all")
  RMariaDB::dbDisconnect(mydb)
  return(res)
})

output$Det_type_Sub<-renderUI({
  validate(
    need(length(values$Doc_dataset)>0,message=FALSE)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  res<-selectInput(inputId = "Det_Type_Sub",label = "Type:",choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select type from meta_type where dataset IN (",paste("'",values$Doc_dataset,"'",collapse=" ,",sep=""),");",collapse = "")),"all")),selected = "all")
  RMariaDB::dbDisconnect(mydb)
  return(res)
})

output$Det_section_Sub<-renderUI({
  validate(
    need(length(values$Doc_dataset)>0,message=FALSE)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  res<-selectInput(inputId = "Det_Section_Sub",label = "Section:",choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select section from meta_section where dataset IN (",paste("'",values$Doc_dataset,"'",collapse=" ,",sep=""),");",collapse = "")),"all")),selected = "all")
  RMariaDB::dbDisconnect(mydb)
  return(res)
})

output$Det_author_Sub<-renderUI({
  validate(
    need(length(values$Doc_dataset)>0,message=FALSE)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  res<-selectInput(inputId = "Det_Author_Sub",label = "Author:",choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select author from meta_author where dataset IN (",paste("'",values$Doc_dataset,"'",collapse=" ,",sep=""),");",collapse = "")),"all")),selected = "all")
  RMariaDB::dbDisconnect(mydb)
  return(res)
})


output$Det_token_Sub<-renderUI({
  validate(
    need(length(values$Doc_dataset)>0,message="Please choose at least one dataset")
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  min=min(RMariaDB::dbGetQuery(mydb, paste("select token from meta_token where dataset IN (",paste("'",values$Doc_dataset,"'",collapse=" ,",sep=""),");",collapse = "")))
  max=max(RMariaDB::dbGetQuery(mydb, paste("select token from meta_token where dataset IN (",paste("'",values$Doc_dataset,"'",collapse=" ,",sep=""),");",collapse = "")))
  RMariaDB::dbDisconnect(mydb)
  sliderInput(inputId = "Det_Token_Sub",label = "Number of Token:",min=min,max=max,value = c(min,max),step = 10)
})

output$Det_von_Sub<-renderUI({
  validate(
    need(length(values$Doc_dataset)>0,message=FALSE)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  dates<-RMariaDB::dbGetQuery(mydb, paste("select date from meta_date where dataset IN (",paste("'",values$Doc_dataset,"'",collapse=" ,",sep=""),");",collapse = ""))
  remove<-which(dates=="0000-00-00")
  if(length(remove)>0){
    dates<-dates[-remove,]
  }
  dates2<-dates
  
  for(i in 1:length(dates)){
    dates2[[i]]<-as.Date(dates[[i]])
  }
  min=min(dates2[[1]])
  RMariaDB::dbDisconnect(mydb)
  dateInput(inputId = "Det_vonin_Sub",label = "Date von:",language = "de",value = min)
})

output$Det_zu_Sub<-renderUI({
  validate(
    need(length(values$Doc_dataset)>0,message=FALSE)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  dates<-RMariaDB::dbGetQuery(mydb, paste("select date from meta_date where dataset IN (",paste("'",values$Doc_dataset,"'",collapse=" ,",sep=""),");",collapse = ""))
  remove<-which(dates=="0000-00-00")
  if(length(remove)>0){
    dates<-dates[-remove,]
  }
  dates2<-dates
  for(i in 1:length(dates)){
    dates2[[i]]<-as.Date(dates[[i]])
  }
  max=max(dates2[[1]])
  RMariaDB::dbDisconnect(mydb)
  dateInput(inputId = "Det_bisin_Sub",label = "Date bis:",language = "de",value = max)
})


#create solr suggest suggestions for detailed search
observe({
  validate(
    need(!is.null(input$Det_inputtext_Sub),message=F)
  )
  x<-input$Det_inputtext_Sub
  praefix<-stringr::str_extract_all(string = x,pattern = "([a-zA-Z\\(\\\"\\+\\#\\-)]{1,100}[\\+\\-\\#]{1})*",simplify = T)[1,1]
  x<-stringr::str_replace(string = x,pattern = "([a-zA-Z\\(\\\"\\+\\#\\-)]{1,100}[\\+\\-\\#]{1})*",replacement="")
  praefix_x<-paste0(stringr::str_extract_all(string = x,pattern = '[\"\\(\\+\\#\\-]+',simplify = T))
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
    shinyTypeahead::updateTypeaheadInput(session = session,inputId = "Det_inputtext_Sub",choices = choices)
  }
})

