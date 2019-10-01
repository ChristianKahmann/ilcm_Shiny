output$datasets_avaiable<-renderUI({
    mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  RMariaDB::dbBegin(conn = mydb)
  RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  RMariaDB::dbSendStatement(mydb, 'SET SQL_SAFE_UPDATES = 0;')
  datasets=RMariaDB::dbGetQuery(mydb,"SELECT DISTINCT dataset FROM ilcm.metadata_names;")
  RMariaDB::dbCommit(mydb)
  RMariaDB::dbDisconnect(mydb)

  values$update_datasets_avaiable

  tags$div(
    selectizeInput(inputId = "dataset",label = tags$p("which corpus?",style="color:white;"),choices = datasets[,1],width = "100%",multiple=T,selected=datasets[1,1]),
           id="select_sidebar")
})


output$dropdown_info<-renderMenu({
  validate(
    need(!is.null(values$user),message=FALSE)
  )
  return(dropdownMenu(type = "notifications",badgeStatus = NULL,icon=icon("info"),
                      notificationItem(
                        text = paste("You are logged in as: ",values$user),
                        icon=icon("user")
                      ),
                      notificationItem(
                        text =  paste("Version:",version),
                        icon = icon("tachometer")
                      )
  )
  )
})

#options Modal
observeEvent(input$openOptionsModal, {
  showModal(
    modalDialog(title = tags$h2("Options"),footer = NULL,easyClose = T,size = "l",fade = T,
                tags$h3("Languages"),
                fluidRow(
                  column(3,
                         tags$span(icon("language"),tags$b("Available languages:"))
                  ),
                  column(4,
                         tags$em(paste(stringr::str_split(
                           stringr::str_replace_all(string = system(command = "python3 -m spacy info",intern = T)[8],pattern = "Models           ",replacement = "")
                           ,pattern = ", ",simplify = T)[1,],collapse = ", "))
                  ),
                  column(4,offset=1,
                         selectizeInput(inputId = "options_add_model_select",label = "Available models to be added",options=list(create=T),choices=setdiff(c("en","de","es","fr","it","nl","pt","el","xx"),stringr::str_split(
                           stringr::str_replace_all(string = system(command = "python3 -m spacy info",intern = T)[8],pattern = "Models           ",replacement = "")
                           ,pattern = ", ",simplify = T)[1,])),
                         withBusyIndicatorUI(
                           bsButton(inputId = "options_add_model",label = "Add model",icon = icon("plus"),style = "success")
                         )
                  )
                ),
                tags$hr(),
                tags$br(),
                tags$h3("Connections"),
                fluidRow(
                  column(5,
                         fluidRow(style="margin-left:0px;margin-right:0px;padding-right:0px;",
                                  icon("database"),
                                  tags$b("MariaDB")
                         ),
                         column(6,
                                textInput(inputId = "options_database_host",value = values$host,label = "Host:")
                         ),
                         column(6,
                                textInput(inputId = "options_database_port",value = values$db_port,label = "Port:")
                         ),
                         fluidRow(style="margin-left:0px;margin-right:0px;padding-right:0px;",
                                  column(6,
                                         withBusyIndicatorUI(
                                           bsButton(inputId = "options_database_change",label = "change connection",icon = icon("edit"),style = "warning")
                                         )
                                  ),
                                  column(6,
                                         uiOutput(outputId = "options_database_connected")
                                  )
                         )
                  ),
                  column(5,offset=2,
                         fluidRow(style="margin-left:0px;margin-right:0px;padding-right:0px;",
                                  icon("search"),
                                  tags$b("Solr")
                         ),
                         column(6,
                                textInput(inputId = "options_solr_host",value = values$update_solr_url,label = "Host:")
                         ),
                         column(6,
                                textInput(inputId = "options_solr_port",value = values$update_solr_port,label = "Port:")
                         ),
                         fluidRow(style="margin-left:0px;margin-right:0px;padding-right:0px;",
                                  column(6,
                                         withBusyIndicatorUI(
                                           bsButton(inputId = "options_solr_change",label = "change connection",icon = icon("edit"),style = "warning")
                                         )
                                  ),
                                  column(6,
                                         uiOutput(outputId = "options_solr_connected")
                                  )
                         )
                  )
                ),
                tags$hr(),
                tags$br(),
                tags$h3("Other"),
                fluidRow(
                  column(3,
                         tags$span(icon("upload"),tags$b("File Upload"))
                  ),
                  column(4,
                        numericInput(inputId = "options_max_size_import",label = "max upload size for data import in Mb",value = max_upload_file_size,min = 5,max = 1000,step = 1)
                         )
                  )
    )
  )
})


#change max file upload size
observeEvent(ignoreInit = T,input$options_max_size_import,{
  print(input$options_max_size_import)
  config<-readLines("config_file.R")
  config<-stringr::str_replace_all(string = config,pattern = "^max_upload_file_size=.{1,20}$",replacement = paste0("max_upload_file_size=",input$options_max_size_import))
  writeLines(config,con="config_file.R")
  options(shiny.maxRequestSize=input$options_max_size_import*1024^2) 
})



#change database connection in values object and config_file.R
observeEvent(ignoreInit = T,input$options_database_change,{
  withBusyIndicatorServer("options_database_change", {
    values$host<-input$options_database_host
    values$db_port<-input$options_database_port
    host<<-input$options_database_host
    db_port<<-input$options_database_port
    #change config_file.R
    config<-readLines("config_file.R")
    config<-stringr::str_replace_all(string = config,pattern = "^host<-.{4,20}$",replacement = paste0("host<-'",input$options_database_host,"'"))
    config<-stringr::str_replace_all(string = config,pattern = "^db_port=.{4,20}$",replacement = paste0("db_port='",input$options_database_port,"'"))
    writeLines(config,con="config_file.R")
  }
  )
})




#show icon if database connection is working
output$options_database_connected<-renderUI({
  db_valid=FALSE
  try({
    mydb <-RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
    db_valid<-RMariaDB::dbIsValid(dbObj = mydb)
    RMariaDB::dbDisconnect(mydb)
  })
  if(db_valid){
    return(tagList(tags$i(
      class = "fa fa-check-square", 
      style = "color: rgb(0,166,90)"
    ),tags$b("Connections works!")))
  }
  else{
    return(tagList(tags$i(
      class = "fa fa-times-circle", 
      style = "color: rgb(191,6,37)"
    ),tags$b("Connection does not work!")))
  }
  
})




#change solr connection in values object and config_file.R
observeEvent(ignoreInit = T,input$options_solr_change,{
  withBusyIndicatorServer("options_solr_change", {
    values$update_solr_url<-input$options_solr_host
    values$update_solr_port<-input$options_solr_port
    values$solr_url<-paste0("http://",input$options_solr_host,":",input$options_solr_port,"/solr/iLCM/select/")
    update_solr_url<<-input$options_solr_host
    update_solr_port<<-input$options_solr_port
    solr_url<<-paste0("http://",input$options_solr_host,":",input$options_solr_port,"/solr/iLCM/select/")
    #change config_file.R
    config<-readLines("config_file.R")
    config<-stringr::str_replace_all(string = config,pattern = "^update_solr_url<-.{4,20}$",replacement = paste0("update_solr_url<-'",input$options_solr_host,"'"))
    config<-stringr::str_replace_all(string = config,pattern = "^update_solr_port<-.{4,20}$",replacement = paste0("update_solr_port<-'",input$options_solr_port,"'"))
    config<-stringr::str_replace_all(string = config,pattern = "^url<-.{4,50}$",replacement = paste0("url<-'",solr_url,"'"))
    writeLines(config,con="config_file.R")
  }
  )
})




#show icon if solr connection is working
output$options_solr_connected<-renderUI({
  solr_valid=FALSE
  try({
    response<-as.character(httr::GET(url = values$solr_url))
    if(grepl(x = response,pattern = '\"numFound')){
      solr_valid=T
    }
  })
  if(solr_valid){
    return(tagList(tags$i(
      class = "fa fa-check-square", 
      style = "color: rgb(0,166,90)"
    ),tags$b("Connections works!")))
  }
  else{
    return(tagList(tags$i(
      class = "fa fa-times-circle", 
      style = "color: rgb(191,6,37)"
    ),tags$b("Connection does not work!")))
  }
  
})








#install new spacy model
observeEvent(ignoreInit = T,input$options_add_model,{
  withBusyIndicatorServer("options_add_model", {
    query<-paste0("python3 -m spacy download ",input$options_add_model_select)
    ret<-system(query,intern = T)
    if(any(grepl("You can now load the model",ret))){
      shinyalert::shinyalert(title = "Successfully installed new spaCy model",
                             text = paste0("Model: ",input$options_add_model_select," was added and is used if specified language is '",input$options_add_model_select,"'"),
                             type="success")
    }
    else{
      shinyalert::shinyalert(title = "there was an error",text = paste(ret,collapse = " "),type = "error")
    }
  })
})
