
output$datasets_avaiable<-renderUI({
  values$import_files_changed
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  RMariaDB::dbBegin(conn = mydb)
  RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
  RMariaDB::dbSendStatement(mydb, 'SET SQL_SAFE_UPDATES = 0;')
  datasets=RMariaDB::dbGetQuery(mydb,"SELECT DISTINCT dataset FROM ilcm.metadata_names;")
  RMariaDB::dbCommit(mydb)
  RMariaDB::dbDisconnect(mydb)
  
  values$update_datasets_avaiable
  values$datasets_available<-datasets[,1]
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
                fluidRow(style="margin-left:0px;margin-right:0px",
                         column(3,
                                tags$span(icon("language"),tags$b("Available languages in spaCy:"))
                         ),
                         column(4,
                                uiOutput("options_spacy_installed")
                         ),
                         column(4,offset=1,
                                uiOutput("options_add_model_select_UI"),
                                withBusyIndicatorUI(
                                  bsButton(inputId = "options_add_model",label = "Add model",icon = icon("plus"),style = "success")
                                )
                         )
                ),
                tags$hr(),
                tags$br(),
                tags$h3("Connections"),
                fluidRow(style="margin-left:0px;margin-right:0px",
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
                #Delete existing copora
                tags$hr(),
                tags$br(),
                tags$h3("Corpora"),
                fluidRow(style="margin-left:0px;margin-right:0px",
                         column(3,
                                tags$span(icon("trash"),tags$b("Remove Corpora"))
                         ),
                         column(3,
                                selectInput(inputId ="Options_delete_dataset_select",label = "Corpus:",choices = values$datasets_available,multiple = F)
                         ),
                         column(2,
                                checkboxInput(inputId = "options_delete_dataset_annotations",label = "Include Annotations",value = FALSE)
                         )
                ),
                fluidRow(style="margin-left:0px;margin-right:0px",
                         column(3,     
                                shinyBS::bsButton(inputId = "options_delete_dataset_action",label = "Delete chosen dataset",icon = icon("trash"),style = "danger")
                         ),
                         column(4,
                                withBusyIndicatorUI(
                                  shinyBS::bsButton(inputId = "options_update_solr",label = "Re-index solr",icon = icon("refresh"),style = "default")
                                )
                         )
                ),
                #add new users
                tags$hr(),
                tags$br(),
                tags$h3("User"),
                fluidRow(style="margin-left:0px;margin-right:0px",
                         column(3,
                                tags$span(icon("user"),tags$b("Add Users"))
                         ),
                         column(4,
                                textInput(inputId = "options_newuser_username",label = "Username",placeholder = "username",value = "")
                         ),
                         column(4,
                                passwordInput(inputId = "options_newuser_password",label = "Password",placeholder = "passwd",value = "")
                         )
                ),
                fluidRow(style="margin-left:0px;margin-right:0px",
                         shinyBS::bsButton(inputId = "option_create_newuser",label = "Add user",icon = icon("user-plus"),style = "success")   
                ),
                tags$hr(),
                tags$br(),
                tags$h3("Other"),
                fluidRow(style="margin-left:0px;margin-right:0px",
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

#render installed spacy
output$options_spacy_installed<-renderUI({
  values$reload_options_modal
  return(tags$em(paste(stringr::str_remove_all(string = stringr::str_split(
    stringr::str_replace_all(string = system(command = "python -m spacy info",intern = T)[8],pattern = "Models           ",replacement = "")
    ,pattern = ", ",simplify = T),pattern = " "),collapse = ", "))
  )
})

#render spacy languaes which can be installed
output$options_add_model_select_UI<-renderUI({
  values$reload_options_modal
  return(selectizeInput(inputId = "options_add_model_select",label = "Available models to be added",options=list(create=T),choices=setdiff(c("en","de","es","fr","it","nl","pt","el","xx"),stringr::str_remove_all(string=stringr::str_split(
    stringr::str_replace_all(string = system(command = "python -m spacy info",intern = T)[8],pattern = "Models           ",replacement = "")
    ,pattern = ", ",simplify = T),pattern = " ")))
  )
})



#reindex solr/full import
observeEvent(ignoreNULL = T,input$options_update_solr,{
  #import data to solr
  withBusyIndicatorServer("options_update_solr", {
    url<-stringr::str_replace(string = values$solr_url,pattern = "select/",replacement = "")
    z<-RCurl::getURL(
      paste0(url,"dataimport?command=full-import"),followlocation=TRUE
    )
    #initiate suggest
    z<-RCurl::getURL(
      paste0(url,"suggest?suggest.build=true"),followlocation=TRUE
    )
    shinyWidgets::sendSweetAlert(type = "success",session = session,title =  "successfully started solr full import and solr suggest")
  })
})


#ask for confirmation before deleting corpora
observeEvent(ignoreNULL = T,input$options_delete_dataset_action,{
  if(input$options_delete_dataset_annotations){
    text=paste0("Are you sure you want to delete corpus:",input$Options_delete_dataset_select," including all annotations?")
  }
  else{
    text=paste0("Are you sure you want to delete corpus:",input$Options_delete_dataset_select,"?")
  }
  shinyWidgets::confirmSweetAlert(session = session,inputId = "options_delete_dataset_action_confirm",title = "Caution!",
                                  text = text,
                                  type="warning",danger_mode = T,)
})


#if confirmed delete corpus
observeEvent(ignoreNULL = T,input$options_delete_dataset_action_confirm,{
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  RMariaDB::dbBegin(conn = mydb)
  RMariaDB::dbSendStatement(mydb, 'SET SQL_SAFE_UPDATES = 0;')
  n=14
  withProgress(message = paste0('Removing corpus:',input$Options_delete_dataset_select), value = 0, {
    incProgress(1/n, detail = "Deleting token information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.token where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting documents information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.documents where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting metadata names information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.metadata_names where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting meta date information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.meta_date where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting meta token information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.meta_token where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting meta mde1 information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.meta_mde1 where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting meta mde2 information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.meta_mde2 where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting meta mde3 information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.meta_mde3 where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting meta mde4 information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.meta_mde4 where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting meta mde5 information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.meta_mde5 where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting meta mde6 information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.meta_mde6 where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting meta mde7 information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.meta_mde7 where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting meta mde8 information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.meta_mde8 where dataset='",input$Options_delete_dataset_select,"';"))
    
    incProgress(1/n, detail = "Deleting meta mde9 information")
    RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.meta_mde9 where dataset='",input$Options_delete_dataset_select,"';"))
  })
  
  if(input$options_delete_dataset_annotations){
    n=2
    withProgress(message = paste0('Removing Annotations with corpus:',input$Options_delete_dataset_select), value = 0, {
      incProgress(1/n, detail = "Deleting user annotations")
      RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.Annotations where dataset='",input$Options_delete_dataset_select,"';"))
      
      incProgress(1/n, detail = "Deleting active learning annotations")
      RMariaDB::dbSendStatement(mydb,paste0("delete FROM ilcm.annotations_classification where dataset='",input$Options_delete_dataset_select,"';"))
    })
  }
  RMariaDB::dbCommit(mydb)
  RMariaDB::dbDisconnect(mydb)
  values$update_datasets_avaiable<-runif(1,0,1)
  updateSelectInput(session = session,inputId = "Options_delete_dataset_select",choices = setdiff(values$datasets_available,input$Options_delete_dataset_select))
  shinyWidgets::sendSweetAlert(session = session,title = "Success",text = HTML(paste0("Corpus ",tags$b(input$Options_delete_dataset_select)," deleted.")),type = "success",html = T)
}
)





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
    query<-paste0("python -m spacy download ",input$options_add_model_select)
    ret<-system(query,intern = T)
    if(any(grepl("You can now load the model",ret))){
      values$reload_options_modal<-runif(1,0,1)
      shinyWidgets::sendSweetAlert(session=session,title = "Successfully installed new spaCy model",
                                   text = paste0("Model: ",input$options_add_model_select," was added and is used if specified language is '",input$options_add_model_select,"'"),
                                   type="success")
    }
    else{
      shinyWidgets::sendSweetAlert(session=session,title = "there was an error" ,text = paste(ret,collapse = " "),type = "error")
    }
  })
})




#add users
observeEvent(input$option_create_newuser,{
  
  name<-input$options_newuser_username
  passwd<-input$options_newuser_password
  if(nchar(name)<1){
    shinyWidgets::sendSweetAlert(session = session,title = "No username specified",text = "Please give a username",type = "warning")
    return()
  }
  if(nchar(passwd)<1){
    shinyWidgets::sendSweetAlert(session = session,title = "No password specified",text = "Please give a password",type = "warning")
    return()
  }
  if(name%in%credentials$username_id){
    shinyWidgets::sendSweetAlert(session = session,title = "This username is already used",text = "Please give another username",type = "warning")
    return()
  }
  
  credentials<<-rbind(credentials,c(name,sodium::password_store(passwd),"basic"))
  Code_for_config_file<-paste0("credentials<-rbind(credentials,c('",name,"',sodium::password_store('",passwd,"'),'basic'))")
  write(Code_for_config_file,file = "config_file.R",append = T)
  shinyWidgets::sendSweetAlert(session = session,title = "New User created",text = "You can logout and then use thw created User to Login",type = "success")
})
