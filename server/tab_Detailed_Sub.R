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
    
    
    
    #add chosen mde1 to fq
    if(!("all"%in%input$Det_mde1_Sub)){
      if(!is.null(input$Det_mde1_Sub)){
        fq=paste(fq,' AND mde1_ss:(','"',paste(input$Det_mde1_Sub,collapse='" "'),'")',sep="")
      }
    }
    
    #add chosen mde2 to fq
    if(!("all"%in%input$Det_mde2_Sub)){
      if(!is.null(input$Det_mde2_Sub)){
        fq=paste(fq,' AND mde2_ss:(','"',paste(input$Det_mde2_Sub,collapse='" "'),'")',sep="")
      }
    }
    
    
    #add chosen mde3 to fq
    if(!("all"%in%input$Det_mde3_Sub)){
      if(!is.null(input$Det_mde3_Sub)){
        fq=paste(fq,' AND mde3_ss:(','"',paste(input$Det_mde3_Sub,collapse='" "'),'")',sep="")
      }
    }
    
    #add chosen mde4 to fq
    if(!("all"%in%input$Det_mde4_Sub)){
      if(!is.null(input$Det_mde4_Sub)){
        fq=paste(fq,' AND mde4_ss:(','"',paste(input$Det_mde4_Sub,collapse='" "'),'")',sep="")
      }
    }
    
    #add chosen mde5 to fq
    if(!("all"%in%input$Det_mde5_Sub)){
      if(!is.null(input$Det_mde5_Sub)){
        fq=paste(fq,' AND mde5_ss:(','"',paste(input$Det_mde5_Sub,collapse='" "'),'")',sep="")
      }
    }
    
    #add chosen mde6 to fq
    if(!("all"%in%input$Det_mde6_Sub)){
      if(!is.null(input$Det_mde6_Sub)){
        fq=paste(fq,' AND mde6_ss:(','"',paste(input$Det_mde6_Sub,collapse='" "'),'")',sep="")
      }
    }
    
    #add chosen mde7 to fq
    if(!("all"%in%input$Det_mde7_Sub)){
      if(!is.null(input$Det_mde7_Sub)){
        fq=paste(fq,' AND mde7_ss:(','"',paste(input$Det_mde7_Sub,collapse='" "'),'")',sep="")
      }
    }
    
    #add chosen mde8 to fq
    if(!("all"%in%input$Det_mde8_Sub)){
      if(!is.null(input$Det_mde8_Sub)){
        fq=paste(fq,' AND mde8_ss:(','"',paste(input$Det_mde8_Sub,collapse='" "'),'")',sep="")
      }
    }
    
    #add chosen mde9 to fq
    if(!("all"%in%input$Det_mde9_Sub)){
      if(!is.null(input$Det_mde9_Sub)){
        fq=paste(fq,' AND mde9_ss:(','"',paste(input$Det_mde9_Sub,collapse='" "'),'")',sep="")
      }
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


#render input fields for detailed search depending on queries to maria db
#set choices for mde1 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde1_Sub")
  validate(
    need(input$navbar_search_Sub=="Detailed",message=F),
    need(!is.null(values$metadata_available_Sub),message=F),
    need(all(!is.na(values$metadata_available_Sub[,"mde1"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde1 from meta_mde1 where dataset IN (",paste0("\'",stringr::str_replace_all(string = values$dataset_Sub,pattern = "dataset_s:",replacement = ""),"\'"),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde1_Sub",label = paste(values$metadata_available_Sub[,"mde1"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde1_Sub")
  }
})




#set choices for mde2 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde2_Sub")
  validate(
    need(input$navbar_search_Sub=="Detailed",message=F),
    need(!is.null(values$metadata_available_Sub),message=F),
    need(all(!is.na(values$metadata_available_Sub[,"mde2"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde2 from meta_mde2 where dataset IN (",paste0("\'",stringr::str_replace_all(string = values$dataset_Sub,pattern = "dataset_s:",replacement = ""),"\'"),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde2_Sub",label = paste(values$metadata_available_Sub[,"mde2"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde2_Sub")
  }
})

#set choices for mde3 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde3_Sub")
  validate(
    need(input$navbar_search_Sub=="Detailed",message=F),
    need(!is.null(values$metadata_available_Sub),message=F),
    need(all(!is.na(values$metadata_available_Sub[,"mde3"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde3 from meta_mde3 where dataset IN (",paste0("\'",stringr::str_replace_all(string = values$dataset_Sub,pattern = "dataset_s:",replacement = ""),"\'"),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde3_Sub",label = paste(values$metadata_available_Sub[,"mde3"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde3_Sub")
  }
})

#set choices for mde4 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde4_Sub")
  validate(
    need(input$navbar_search_Sub=="Detailed",message=F),
    need(!is.null(values$metadata_available_Sub),message=F),
    need(all(!is.na(values$metadata_available_Sub[,"mde4"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde4 from meta_mde4 where dataset IN (",paste0("\'",stringr::str_replace_all(string = values$dataset_Sub,pattern = "dataset_s:",replacement = ""),"\'"),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde4_Sub",label = paste(values$metadata_available_Sub[,"mde4"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde4_Sub")
  }
})

#set choices for mde5 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde5_Sub")
  validate(
    need(input$navbar_search_Sub=="Detailed",message=F),
    need(!is.null(values$metadata_available_Sub),message=F),
    need(all(!is.na(values$metadata_available_Sub[,"mde5"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde5 from meta_mde5 where dataset IN (",paste0("\'",stringr::str_replace_all(string = values$dataset_Sub,pattern = "dataset_s:",replacement = ""),"\'"),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde5_Sub",label = paste(values$metadata_available_Sub[,"mde5"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde5_Sub")
  }
})


#set choices for mde6 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde6_Sub")
  validate(
    need(input$navbar_search_Sub=="Detailed",message=F),
    need(!is.null(values$metadata_available_Sub),message=F),
    need(all(!is.na(values$metadata_available_Sub[,"mde6"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde6 from meta_mde6 where dataset IN (",paste0("\'",stringr::str_replace_all(string = values$dataset_Sub,pattern = "dataset_s:",replacement = ""),"\'"),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde6_Sub",label = paste(values$metadata_available_Sub[,"mde6"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde6_Sub")
  }
})


#set choices for mde7 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde7_Sub")
  validate(
    need(input$navbar_search_Sub=="Detailed",message=F),
    need(!is.null(values$metadata_available_Sub),message=F),
    need(all(!is.na(values$metadata_available_Sub[,"mde7"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde7 from meta_mde7 where dataset IN (",paste0("\'",stringr::str_replace_all(string = values$dataset_Sub,pattern = "dataset_s:",replacement = ""),"\'"),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde7_Sub",label = paste(values$metadata_available_Sub[,"mde7"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde7_Sub")
  }
})


#set choices for mde8 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde8_Sub")
  validate(
    need(input$navbar_search_Sub=="Detailed",message=F),
    need(!is.null(values$metadata_available_Sub),message=F),
    need(all(!is.na(values$metadata_available_Sub[,"mde8"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde8 from meta_mde8 where dataset IN (",paste0("\'",stringr::str_replace_all(string = values$dataset_Sub,pattern = "dataset_s:",replacement = ""),"\'"),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde8_Sub",label = paste(values$metadata_available_Sub[,"mde8"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde8_Sub")
  }
})


#set choices for mde9 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde9_Sub")
  validate(
    need(input$navbar_search_Sub=="Detailed",message=F),
    need(!is.null(values$metadata_available_Sub),message=F),
    need(all(!is.na(values$metadata_available_Sub[,"mde9"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde9 from meta_mde9 where dataset IN (",paste0("\'",stringr::str_replace_all(string = values$dataset_Sub,pattern = "dataset_s:",replacement = ""),"\'"),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde9_Sub",label = paste(values$metadata_available_Sub[,"mde9"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde9_Sub")
  }
})










#get token information from database and render slider input with received min and max value
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


#get earliest date from database meta_data table to render DateInput Starting Point
output$Det_von_Sub<-renderUI({
  validate(
    need(length(values$Doc_dataset)>0,message=FALSE)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  dates<-RMariaDB::dbGetQuery(mydb, paste("select date from meta_date where dataset IN (",paste("'",values$Doc_dataset,"'",collapse=" ,",sep=""),");",collapse = ""))
  #check that the date object is not empty
  if(dim(dates)[1]==0){
    return("no dates found")
  }
  #remove wrong dates 
  remove<-which(dates=="0000-00-00")
  if(length(remove)>0){
    dates<-dates[-remove,]
  }
  dates2<-dates
  #if only years available add "-01-01" to make input work
  for(i in 1:length(dates)){
    if(nchar(dates2[1,1])==4){
      dates2<-paste(dates2[,1],"-01-01",sep="")
      dates2<-as.Date(dates2)
      min=min(dates2)
    }
    else{
      dates2[[i]]<-as.Date(dates[[i]])
      min=min(dates2[[1]])
    }
  }
  RMariaDB::dbDisconnect(mydb)
  dateInput(inputId = "Det_vonin_Sub",label = "Date von:",language = "de",value = min)
})


#get latest date from database meta_data table to render DateInput End Point
output$Det_zu_Sub<-renderUI({
  validate(
    need(length(values$Doc_dataset)>0,message=FALSE)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  dates<-RMariaDB::dbGetQuery(mydb, paste("select date from meta_date where dataset IN (",paste("'",values$Doc_dataset,"'",collapse=" ,",sep=""),");",collapse = ""))
  #check that the date object is not empty
  if(dim(dates)[1]==0){
    return("no dates found")
  }
  #remove wrong dates 
  remove<-which(dates=="0000-00-00")
  if(length(remove)>0){
    dates<-dates[-remove,]
  }
  dates2<-dates
  #if only years available add "-01-01" to make input work
  for(i in 1:length(dates)){
    if(nchar(dates2[1,1])==4){
      dates2<-paste(dates2[,1],"-01-01",sep="")
      dates2<-as.Date(dates2)
      max=max(dates2)
    }
    else{
      dates2[[i]]<-as.Date(dates[[i]])
      max=max(dates2[[1]])
    }
  }
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

