observeEvent(input$Det_action,{
  validate(
    need(length(input$dataset)>0,message=FALSE)
  )
  withBusyIndicatorServer("Det_action", {
    url<-values$solr_url
    q<-"*"
    #get inputstring
    s<-isolate(input$Det_inputtext)
    #transform input to Solr Query
    s<-stringr::str_replace_all(string = s,pattern = "\\+",replacement = " AND ")
    s<-stringr::str_replace_all(string = s,pattern = "\\#",replacement = " OR ")
    s<-stringr::str_replace_all(string = s,pattern = "\\-",replacement = " NOT ")
    
    if(nchar(s)>0){
      q<-s
    }
    #add date to solr fq field
    if(length(input$Det_vonin)>0 & length(input$Det_bisin)>0){
      fq<-paste0('date_dt:[',input$Det_vonin,'T00:00:00Z TO ',input$Det_bisin,'T00:00:00Z]')
    } 
    
    #add chosen dataset to fq
    if(length(isolate(input$dataset))==0){
      # fq<-"dataset_s:(*:*)"
      shinyWidgets::sendSweetAlert(session=session,title = "No corpus specified",text = "Please choose at least one corpus in the top left corner.",type = "warning")
    }
    else{
      if(length(isolate(input$dataset))==1){
        fq_dataset<-paste("dataset_s:\"",isolate(input$dataset),"\"",sep="")
      }
      if(length(isolate(input$dataset))>1){
        fq_dataset<-paste0("dataset_s:(\"",isolate(input$dataset)[1],"\"")
        for(i in 2:length(isolate(input$dataset))){
          fq_dataset<-paste0(fq_dataset," OR \"",isolate(input$dataset)[i],"\"")
        }
        fq_dataset<-paste0(fq_dataset,")")
      } 
      
      fq<-paste(fq," AND ",fq_dataset,sep="")
      
      #add chosen mde1 to fq
      if(!("all"%in%input$Det_mde1)){
        if(!is.null(input$Det_mde1)){
          fq=paste(fq,' AND mde1_ss:(','"',paste(input$Det_mde1,collapse='" "'),'")',sep="")
        }
      }
      
      #add chosen mde2 to fq
      if(!("all"%in%input$Det_mde2)){
        if(!is.null(input$Det_mde2)){
          fq=paste(fq,' AND mde2_ss:(','"',paste(input$Det_mde2,collapse='" "'),'")',sep="")
        }
      }
      
      
      #add chosen mde3 to fq
      if(!("all"%in%input$Det_mde3)){
        if(!is.null(input$Det_mde3)){
          fq=paste(fq,' AND mde3_ss:(','"',paste(input$Det_mde3,collapse='" "'),'")',sep="")
        }
      }
      
      #add chosen mde4 to fq
      if(!("all"%in%input$Det_mde4)){
        if(!is.null(input$Det_mde4)){
          fq=paste(fq,' AND mde4_ss:(','"',paste(input$Det_mde4,collapse='" "'),'")',sep="")
        }
      }
      
      #add chosen mde5 to fq
      if(!("all"%in%input$Det_mde5)){
        if(!is.null(input$Det_mde5)){
          fq=paste(fq,' AND mde5_ss:(','"',paste(input$Det_mde5,collapse='" "'),'")',sep="")
        }
      }
      
      #add chosen mde6 to fq
      if(!("all"%in%input$Det_mde6)){
        if(!is.null(input$Det_mde6)){
          fq=paste(fq,' AND mde6_ss:(','"',paste(input$Det_mde6,collapse='" "'),'")',sep="")
        }
      }
      
      #add chosen mde7 to fq
      if(!("all"%in%input$Det_mde7)){
        if(!is.null(input$Det_mde7)){
          fq=paste(fq,' AND mde7_ss:(','"',paste(input$Det_mde7,collapse='" "'),'")',sep="")
        }
      }
      
      #add chosen mde8 to fq
      if(!("all"%in%input$Det_mde8)){
        if(!is.null(input$Det_mde8)){
          fq=paste(fq,' AND mde8_ss:(','"',paste(input$Det_mde8,collapse='" "'),'")',sep="")
        }
      }
      
      #add chosen mde9 to fq
      if(!("all"%in%input$Det_mde9)){
        if(!is.null(input$Det_mde9)){
          fq=paste(fq,' AND mde9_ss:(','"',paste(input$Det_mde9,collapse='" "'),'")',sep="")
        }
      }
      #add chosen tokenrange to 
      fq=paste(fq," AND token_i:[",input$Det_Token[1],"TO",input$Det_Token[2],"]")
      
      #check wheater solr query is valid and get solq query back
      response<-NULL
      try({
        response<-as.character(solr_search_advanced(base = url,q = q,fl="id",fq=fq,rows="1",raw=T))
      },silent = T)
      #alert user when solr query was wrong
      if(is.null(response)){
        if(nchar(fq>8000)){
        shinyWidgets::sendSweetAlert(type = "error",session = session,title = "Wrong Syntax in Solr Query",text = "most likely your query is too long.")
        }
        else{
          shinyWidgets::sendSweetAlert(type = "error",session = session,title = "Wrong Syntax in Solr Query")
        }
      }
      #stop here response is NULL
      validate(
        need(!is.null(response),"wrong syntax")
      )
      
      #set solr query parameters for calls in search results and timseries
      values$start=0
      values$custom<-FALSE
      values$url<-url
      values$q<-q
      values$fq<-fq
      values$fq_init<-fq
      values$numFound<-as.integer(str_replace_all(string = str_extract(string =response[1],pattern = "numFound\\\":[0-9]+,"),pattern = "\\D",replacement = ""))
      updateTextInput(session = session,inputId = "SR_row_sel",value = 1)
      values$start=1
      values$search<-values$search+1
      values$solr_query<-stringr::str_replace(string=stringr::str_replace(string=response[2],pattern = "&wt=(.)+",replacement = ""),pattern = "start=[0-9]+&rows=[0-9]&",replacement="")
      #switch to search results tab
      updateTabsetPanel(session = session,inputId = "expl",selected = "Search Results")
    }
  })
})



#render input fields for detailed search depending on queries to maria db
#set choices for mde1 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde1")
  validate(
    need(input$tabBoxsearch=="Detailed",message=F),
    need(length(isolate(input$dataset))>0,message=FALSE),
    need(!is.null(values$metadata_available),message=F),
    need(all(!is.na(values$metadata_available[,"mde1"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde1 from meta_mde1 where dataset IN (",paste("'",isolate(input$dataset),"'",collapse=" ,",sep=""),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde1",label = paste(values$metadata_available[,"mde1"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde1")
  }
})

#set choices for mde2 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde2")
  validate(
    need(input$tabBoxsearch=="Detailed",message=F),
    need(length(isolate(input$dataset))>0,message=FALSE),
    need(!is.null(values$metadata_available),message=F),
    need(all(!is.na(values$metadata_available[,"mde2"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde2 from meta_mde2 where dataset IN (",paste("'",isolate(input$dataset),"'",collapse=" ,",sep=""),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde2",label = paste(values$metadata_available[,"mde2"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde2")
  }
})

#set choices for mde3 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde3")
  validate(
    need(input$tabBoxsearch=="Detailed",message=F),
    need(length(isolate(input$dataset))>0,message=FALSE),
    need(!is.null(values$metadata_available),message=F),
    need(all(!is.na(values$metadata_available[,"mde3"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde3 from meta_mde3 where dataset IN (",paste("'",isolate(input$dataset),"'",collapse=" ,",sep=""),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde3",label = paste(values$metadata_available[,"mde3"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde3")
  }
})

#set choices for mde4 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde4")
  validate(
    need(input$tabBoxsearch=="Detailed",message=F),
    need(length(isolate(input$dataset))>0,message=FALSE),
    need(!is.null(values$metadata_available),message=F),
    need(all(!is.na(values$metadata_available[,"mde4"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde4 from meta_mde4 where dataset IN (",paste("'",isolate(input$dataset),"'",collapse=" ,",sep=""),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde4",label = paste(values$metadata_available[,"mde4"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde4")
  }
})

#set choices for mde5 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde5")
  validate(
    need(input$tabBoxsearch=="Detailed",message=F),
    need(length(isolate(input$dataset))>0,message=FALSE),
    need(!is.null(values$metadata_available),message=F),
    need(all(!is.na(values$metadata_available[,"mde5"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde5 from meta_mde5 where dataset IN (",paste("'",isolate(input$dataset),"'",collapse=" ,",sep=""),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde5",label = paste(values$metadata_available[,"mde5"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde5")
  }
})

#set choices for mde6 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde6")
  validate(
    need(input$tabBoxsearch=="Detailed",message=F),
    need(length(isolate(input$dataset))>0,message=FALSE),
    need(!is.null(values$metadata_available),message=F),
    need(all(!is.na(values$metadata_available[,"mde6"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde6 from meta_mde6 where dataset IN (",paste("'",isolate(input$dataset),"'",collapse=" ,",sep=""),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde6",label = paste(values$metadata_available[,"mde6"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde6")
  }
})

#set choices for mde7 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde7")
  validate(
    need(input$tabBoxsearch=="Detailed",message=F),
    need(length(isolate(input$dataset))>0,message=FALSE),
    need(!is.null(values$metadata_available),message=F),
    need(all(!is.na(values$metadata_available[,"mde7"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde7 from meta_mde7 where dataset IN (",paste("'",isolate(input$dataset),"'",collapse=" ,",sep=""),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde7",label = paste(values$metadata_available[,"mde7"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde7")
  }
})

#set choices for mde8 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde8")
  validate(
    need(input$tabBoxsearch=="Detailed",message=F),
    need(length(isolate(input$dataset))>0,message=FALSE),
    need(!is.null(values$metadata_available),message=F),
    need(all(!is.na(values$metadata_available[,"mde8"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde8 from meta_mde8 where dataset IN (",paste("'",isolate(input$dataset),"'",collapse=" ,",sep=""),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde8",label = paste(values$metadata_available[,"mde8"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde8")
  }
})

#set choices for mde9 if avaiable
observe({
  shinyjs::hideElement(id = "Det_mde9")
  validate(
    need(input$tabBoxsearch=="Detailed",message=F),
    need(length(isolate(input$dataset))>0,message=FALSE),
    need(!is.null(values$metadata_available),message=F),
    need(all(!is.na(values$metadata_available[,"mde9"])),message=F)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=isolate(values$host),port=isolate(values$db_port))
  choices = unique(rbind(RMariaDB::dbGetQuery(mydb, paste("select mde9 from meta_mde9 where dataset IN (",paste("'",isolate(input$dataset),"'",collapse=" ,",sep=""),");",collapse = ""))))
  RMariaDB::dbDisconnect(mydb)
  if(dim(choices)[1]>0){
    updateSelectizeInput(session=session,inputId = "Det_mde9",label = paste(values$metadata_available[,"mde9"],collapse="/"),choices = choices[,1],server=T)
    shinyjs::showElement(id = "Det_mde9")
  }
})





output$Det_token<-renderUI({
  validate(
    need(length(input$dataset)>0,message="Please choose at least one dataset")
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  min=min(RMariaDB::dbGetQuery(mydb, paste("select token from meta_token where dataset IN (",paste("'",input$dataset,"'",collapse=" ,",sep=""),");",collapse = "")))
  max=max(RMariaDB::dbGetQuery(mydb, paste("select token from meta_token where dataset IN (",paste("'",input$dataset,"'",collapse=" ,",sep=""),");",collapse = "")))
  RMariaDB::dbDisconnect(mydb)
  sliderInput(inputId = "Det_Token",label = "Number of Token:",min=min,max=max,value = c(min,max),step = 10)
})

output$Det_von<-renderUI({
  validate(
    need(length(input$dataset)>0,message=FALSE)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  dates<-RMariaDB::dbGetQuery(mydb, paste("select date from meta_date where dataset IN (",paste("'",input$dataset,"'",collapse=" ,",sep=""),");",collapse = ""))
  if(dim(dates)[1]==0){
    return("no dates found")
  }
  remove<-which(dates=="0000-00-00")
  if(length(remove)>0){
    dates<-dates[-remove,]
  }
  dates2<-dates
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
  dateInput(inputId = "Det_vonin",label = "Date von:",language = "de",value = min)
})

output$Det_zu<-renderUI({
  validate(
    need(length(input$dataset)>0,message=FALSE)
  )
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
  dates<-RMariaDB::dbGetQuery(mydb, paste("select date from meta_date where dataset IN (",paste("'",input$dataset,"'",collapse=" ,",sep=""),");",collapse = ""))
  if(dim(dates)[1]==0){
    return("no dates found")
  }
  remove<-which(dates=="0000-00-00")
  if(length(remove)>0){
    dates<-dates[-remove,]
  }
  dates2<-dates
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
  dateInput(inputId = "Det_bisin",label = "Date bis:",language = "de",value = max)
})


#create solr suggest suggestions for detailed search
observe({
  validate(
    need(!is.null(input$Det_inputtext),message=F)
  )
  x<-input$Det_inputtext
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
    shinyTypeahead::updateTypeaheadInput(session = session,inputId = "Det_inputtext",choices = choices)
  }
})

