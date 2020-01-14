##########################################################################################################
#                                import csv                                                              #
##########################################################################################################
values$Import_csv_id_doc<-""
values$Import_csv_title<-""
values$Import_csv_date<-""
values$Import_csv_body<-""
values$Import_csv_mde1<-""
values$Import_csv_mde2<-""
values$Import_csv_mde3<-""
values$Import_csv_mde4<-""
values$Import_csv_mde5<-""
values$Import_csv_mde6<-""
values$Import_csv_mde7<-""
values$Import_csv_mde8<-""
values$Import_csv_mde9<-""
values$Import_csv_language<-""
values$Import_csv_token<-""
values$Import_csv_dataset<-""

output$UI_Import_csv_file<-renderUI({
  values$invalidate_csv_files
  validate(
    need(length(list.files("data_import/unprocessed_data/",pattern = ".csv"))>0,message="No CSV-Files found in directory: data_import/unprocessed_data")
  )
  return(
    tagList(
      
      shinyWidgets::prettyRadioButtons(inputId = "Import_csv_files",label = "CSV Files",
                                       choices = stringr::str_replace_all(string = list.files("data_import/unprocessed_data/",pattern = ".csv"),pattern = ".txt",replacement = ""),
                                       fill=T,animation = "pulse",selected = character(0))
    )
  )
})

observeEvent(input$Import_csv_new,ignoreInit = T,{
  validate(
    need(
      !is.null(input$Import_csv_new),message=F
    )
  )
  print(input$Import_csv_new)
  if(file.exists(paste0("data_import/unprocessed_data/",input$Import_csv_new$name))){
    shinyWidgets::sendSweetAlert(session=session,title = "Filename already used",text = "Please rename your csv file and then try to upload it again.",type = "warning")
  }
  else{
    file.copy(from = input$Import_csv_new$datapath,to = paste0("data_import/unprocessed_data/",input$Import_csv_new$name))
    values$invalidate_csv_files<-runif(1,0,1)
    shinyWidgets::sendSweetAlert(session=session,title = "File added",text = "You can now select it in the list of files above",type = "success")
  }
})




observeEvent(input$Import_load_csv,{
  withBusyIndicatorServer("Import_load_csv", {
    values$data_csv<-readr::read_delim(file = paste0("data_import/unprocessed_data/",input$Import_csv_files),col_names = input$Import_load_csv_header,
                                     delim = input$import_load_csv_seperator,na = character() )
    colnames(values$data_csv)<-stringr::str_replace_all(string = colnames(values$data_csv),pattern = "\\.",replacement = " ")
    if(dim(values$data_csv)[1]<2 | dim(values$data_csv)[2]<2){
      text<-paste0("The resulting input dimesions are: ",dim(values$data_csv)[1]," x ",dim(values$data_csv)[2],". Something went wrong during the input. Make sure to specify the csv input parameters correct.")
      shinyWidgets::sendSweetAlert(session=session,title = "Input failed!",text = text,type = "error")
    }
    else{
      values$header_csv<-c(colnames(values$data_csv))
      values$data_load_csv_success<-TRUE
    }
  })
})

observeEvent(input$Import_start_mapping,{
  values$start_mapping<-TRUE
})

output$data_load_csv_success<-reactive({
  values$data_load_csv_success
})

output$start_mapping<-reactive({
  values$start_mapping
})
outputOptions(output, "data_load_csv_success", suspendWhenHidden = FALSE)
outputOptions(output, "start_mapping", suspendWhenHidden = FALSE)

output$Import_head_csv<-DT::renderDataTable({
  data<-values$data_csv
  data<-data[1:min(5,dim(data)[1]),]
  data<-t(apply(data,1,FUN=function(i){apply(as.matrix(i),MARGIN = 1,FUN = function(x){if(is.na(x)){return(x)}else{if(nchar(x)>100){return(substr(x,1,100))}else{return(x)}}})}))
  datatable(data = data,options = list(lengthChange = FALSE,dom="t"),width = "100%")
})


observeEvent(input$Import_check_csv,{
  showModal(
    modalDialog(
      size = "l",
      title = "Imported CSV",easyClose = T,
      tags$div(style="overflow-x:auto; height:70vh;",
               dataTableOutput(outputId = "Import_head_csv")
      ) 
    )
  )
})

output$UI_Import_csv_id_doc<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_id_doc",label = "Map id_doc",
                                   choices = c("automatic",values$header_csv),
                                   fill=T,animation = "pulse",selected = "automatic")
})

output$UI_Import_csv_title<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_title",label = "Map title",
                                   choices = c("automatic",values$header_csv),
                                   fill=T,animation = "pulse",selected = "automatic")
})

output$UI_Import_csv_date<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_date",label = "Map date",
                                   choices = c("automatic",values$header_csv),
                                   fill=T,animation = "pulse",selected = "automatic")
})

output$UI_Import_csv_body<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_body",label = "Map body",
                                   choices = values$header_csv,
                                   fill=T,animation = "pulse")
})

output$UI_Import_csv_mde1<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_mde1", label = "Map mde1",
                                   choices = c("not required",values$header_csv),
                                   fill=T,animation = "pulse",selected = "not required")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_csv_mde1",label = paste0("Map ",input$UI_Import_name_mde1))
})

output$UI_Import_csv_mde2<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_mde2",label = "Map mde2",
                                   choices = c("not required",values$header_csv),
                                   fill=T,animation = "pulse",selected = "not required")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_csv_mde2",label = paste0("Map ",input$UI_Import_name_mde2))
})

output$UI_Import_csv_mde3<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_mde3", label = "Map mde3",
                                   choices = c("not required",values$header_csv),
                                   fill=T,animation = "pulse",selected = "not required")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_csv_mde3",label = paste0("Map ",input$UI_Import_name_mde3))
})

output$UI_Import_csv_mde4<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_mde4", label = "Map mde4",
                                   choices = c("not required",values$header_csv),
                                   fill=T,animation = "pulse",selected = "not required")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_csv_mde4",label = paste0("Map ",input$UI_Import_name_mde4))
})

output$UI_Import_csv_mde5<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_mde5",label = "Map mde5",
                                   choices = c("not required",values$header_csv),
                                   fill=T,animation = "pulse",selected = "not required")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_csv_mde5",label = paste0("Map ",input$UI_Import_name_mde5))
})

output$UI_Import_csv_mde6<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_mde6",label = "Map mde6",
                                   choices = c("not required",values$header_csv),
                                   fill=T,animation = "pulse",selected = "not required")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_csv_mde6",label = paste0("Map ",input$UI_Import_name_mde6))
})

output$UI_Import_csv_mde7<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_mde7", label = "Map mde7",
                                   choices = c("not required",values$header_csv),
                                   fill=T,animation = "pulse",selected = "not required")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_csv_mde7",label = paste0("Map ",input$UI_Import_name_mde7))
})

output$UI_Import_csv_mde8<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_mde8", label = "Map mde8",
                                   choices = c("not required",values$header_csv),
                                   fill=T,animation = "pulse",selected = "not required")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_csv_mde8",label = paste0("Map ",input$UI_Import_name_mde8))
})

output$UI_Import_csv_mde9<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_csv_mde9", label = "Map mde9",
                                   choices = c("not required",values$header_csv),
                                   fill=T,animation = "pulse",selected = "not required")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_csv_mde9",label = paste0("Map ",input$UI_Import_name_mde9))
})



#title
observeEvent(input$Import_script_title,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_title",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_title can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_title<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_title<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_title", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_title,{
  tryCatch({
    eval(parse(text=input$script_title))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_title,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_title",label = "Titles (one for all)",placeholder = "Please type in the title"),
                actionButton("Import_csv_save_man_save_title", "save")
    )
  )
}
)

observeEvent(input$Import_csv_save_man_save_title,{
  values$Import_csv_title<-rep(input$Import_csv_save_man_title,dim(values$data_csv)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_csv_title),message=FALSE),
    need(input$Import_csv_title%in%c(colnames(values$data_csv),"automatic"),message=FALSE)
  )
  if(input$Import_csv_title=="automatic"){
    values$Import_csv_title<-paste("document ",1:dim(values$data_csv)[1],sep="")
  }
  else{
    values$Import_csv_title<-as.vector(as.matrix(values$data_csv[,input$Import_csv_title]))
  }
})


#id_doc
observeEvent(input$Import_script_id_doc,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_id_doc",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_id_doc can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_id_doc<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_id_doc<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_id_doc", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_id_doc,{
  tryCatch({
    eval(parse(text=input$script_id_doc))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_id_doc,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_id_doc",label = "id_doc (one for all)",placeholder = "Please type in the id_doc"),
                actionButton("Import_csv_save_man_save_id_doc", "save")
    )
  )
}
)

observeEvent(input$Import_csv_save_man_save_id_doc,{
  values$Import_csv_id_doc<-rep(input$Import_csv_save_man_id_doc,dim(values$data_csv)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_csv_id_doc),message=FALSE),
    need(input$Import_csv_id_doc%in%c(colnames(values$data_csv),"automatic"),message=FALSE)
  )
  if(input$Import_csv_id_doc=="automatic"){
    #check max id_doc in database for specified dataset
    offset=NA
    if(input$Import_csv_dataset!=""){
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
      print(paste0("SELECT MAX(id_doc) FROM ilcm.documents where dataset=",input$Import_csv_dataset,";"))
      offset<-RMariaDB::dbGetQuery(mydb,paste0("SELECT MAX(id_doc) FROM ilcm.documents where dataset='",input$Import_csv_dataset,"';"))[1,1]
    }
    if(is.na(offset)){
      offset=0
    }
    values$Import_csv_id_doc<-(offset+1):(offset+dim(values$data_csv)[1])
  }
  else{
    values$Import_csv_id_doc<-as.vector(as.matrix(values$data_csv[,input$Import_csv_id_doc]))
  }
})

#body
observeEvent(input$Import_script_body,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_body",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_body can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_body<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_body<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_body", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_body,{
  tryCatch({
    eval(parse(text=input$script_body))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_body,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_body",label = "bodys (one for all)",placeholder = "Please type in the body"),
                actionButton("Import_csv_save_man_save_body", "save")
    )
  )
}
)

observe({
  validate(
    need(!is.null(input$Import_csv_body),message=FALSE),
    need(input$Import_csv_body%in%c(colnames(values$data_csv)),message=FALSE)
  )
  values$Import_csv_body<-as.vector(as.matrix(values$data_csv[,input$Import_csv_body]))
})


observeEvent(input$Import_csv_save_man_save_body,{
  values$Import_csv_body<-rep(input$Import_csv_save_man_body,dim(values$data_csv)[1])
  removeModal()
})


#mde3
observeEvent(input$Import_script_mde3,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde3",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_mde3 can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_mde3<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_mde3<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde3", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde3,{
  tryCatch({
    eval(parse(text=input$script_mde3))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde3,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_mde3",label = "mde3s (one for all)",placeholder = "Please type in the mde3"),
                actionButton("Import_csv_save_man_save_mde3", "save")
    )
  )
}
)

observeEvent(input$Import_csv_save_man_save_mde3,{
  values$Import_csv_mde3<-rep(input$Import_csv_save_man_mde3,dim(values$data_csv)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_csv_mde3),message=FALSE),
    need(input$Import_csv_mde3%in%c(colnames(values$data_csv),"not required"),message=FALSE)
  )
  if(input$Import_csv_mde3=="not required"){
    values$Import_csv_mde3<-NULL
  }
  else{
    values$Import_csv_mde3<-as.vector(as.matrix(values$data_csv[,input$Import_csv_mde3]))
  }
})


#mde1
observeEvent(input$Import_script_mde1,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde1",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_mde1 can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_mde1<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_mde1<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde1", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde1,{
  tryCatch({
    eval(parse(text=input$script_mde1))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde1,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_mde1",label = "mde1s (one for all)",placeholder = "Please type in the mde1"),
                actionButton("Import_csv_save_man_save_mde1", "save")
    )
  )
}
)

observeEvent(input$Import_csv_save_man_save_mde1,{
  values$Import_csv_mde1<-rep(input$Import_csv_save_man_mde1,dim(values$data_csv)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_csv_mde1),message=FALSE),
    need(input$Import_csv_mde1%in%c(colnames(values$data_csv),"not required"),message=FALSE)
  )
  if(input$Import_csv_mde1=="not required"){
    values$Import_csv_mde1<-NULL
  }
  else{
    values$Import_csv_mde1<-as.vector(as.matrix(values$data_csv[,input$Import_csv_mde1]))
  }
})

#date
observeEvent(input$Import_script_date,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_date",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_date can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_date<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_date<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_date", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_date,{
  tryCatch({
    eval(parse(text=input$script_date))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_date,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_date",label = "dates (one for all)",placeholder = "Please type in the date"),
                actionButton("Import_csv_save_man_save_date", "save")
    )
  )
}
)

observeEvent(input$Import_csv_save_man_save_date,{
  values$Import_csv_date<-rep(input$Import_csv_save_man_date,dim(values$data_csv)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_csv_date),message=FALSE),
    need(input$Import_csv_date%in%c(colnames(values$data_csv),"automatic"),message=FALSE)
  )
  if(input$Import_csv_date=="automatic"){
    values$Import_csv_date<-rep(as.character(Sys.Date()),dim(values$data_csv)[1])
  }
  else{
    values$Import_csv_date<-as.vector(as.matrix(values$data_csv[,input$Import_csv_date]))
  }
})

#mde4
observeEvent(input$Import_script_mde4,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde4",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_mde4 can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_mde4<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_mde4<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde4", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde4,{
  tryCatch({
    eval(parse(text=input$script_mde4))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde4,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_mde4",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_csv_save_man_save_mde4", "save")
    )
  )
}
)

observeEvent(input$Import_csv_save_man_save_mde4,{
  values$Import_csv_mde4<-rep(input$Import_csv_save_man_mde4,dim(values$data_csv)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_csv_mde4),message=FALSE),
    need(input$Import_csv_mde4%in%c(colnames(values$data_csv),"not required"),message=FALSE)
  )
  if(input$Import_csv_mde4=="not required"){
    values$Import_csv_mde4<-NULL
  }
  else{
    values$Import_csv_mde4<-as.vector(as.matrix(values$data_csv[,input$Import_csv_mde4]))
  }
})

#mde5
observeEvent(input$Import_script_mde5,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde5",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_mde5 can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_mde5<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_mde5<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde5", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde5,{
  tryCatch({
    eval(parse(text=input$script_mde5))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde5,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_mde5",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_csv_save_man_save_mde5", "save")
    )
  )
}
)

observeEvent(input$Import_csv_save_man_save_mde5,{
  values$Import_csv_mde5<-rep(input$Import_csv_save_man_mde5,dim(values$data_csv)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_csv_mde5),message=FALSE),
    need(input$Import_csv_mde5%in%c(colnames(values$data_csv),"not required"),message=FALSE)
  )
  if(input$Import_csv_mde5=="not required"){
    values$Import_csv_mde5<-NULL
  }
  else{
    values$Import_csv_mde5<-as.vector(as.matrix(values$data_csv[,input$Import_csv_mde5]))
  }
})

#mde6
observeEvent(input$Import_script_mde6,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde6",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_mde6 can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_mde6<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_mde6<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde6", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde6,{
  tryCatch({
    eval(parse(text=input$script_mde6))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde6,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_mde6",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_csv_save_man_save_mde6", "save")
    )
  )
}
)

observeEvent(input$Import_csv_save_man_save_mde6,{
  values$Import_csv_mde6<-rep(input$Import_csv_save_man_mde6,dim(values$data_csv)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_csv_mde6),message=FALSE),
    need(input$Import_csv_mde6%in%c(colnames(values$data_csv),"not required"),message=FALSE)
  )
  if(input$Import_csv_mde6=="not required"){
    values$Import_csv_mde6<-NULL
  }
  else{
    values$Import_csv_mde6<-as.vector(as.matrix(values$data_csv[,input$Import_csv_mde6]))
  }
})

#mde7
observeEvent(input$Import_script_mde7,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde7",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_mde7 can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_mde7<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_mde7<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde7", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde7,{
  tryCatch({
    eval(parse(text=input$script_mde7))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde7,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_mde7",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_csv_save_man_save_mde7", "save")
    )
  )
}
)

observeEvent(input$Import_csv_save_man_save_mde7,{
  values$Import_csv_mde7<-rep(input$Import_csv_save_man_mde7,dim(values$data_csv)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_csv_mde7),message=FALSE),
    need(input$Import_csv_mde7%in%c(colnames(values$data_csv),"not required"),message=FALSE)
  )
  if(input$Import_csv_mde7=="not required"){
    values$Import_csv_mde7<-NULL
  }
  else{
    values$Import_csv_mde7<-as.vector(as.matrix(values$data_csv[,input$Import_csv_mde7]))
  }
})

#mde8
observeEvent(input$Import_script_mde8,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde8",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_mde8 can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_mde8<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_mde8<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde8", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde8,{
  tryCatch({
    eval(parse(text=input$script_mde8))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde8,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_mde8",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_csv_save_man_save_mde8", "save")
    )
  )
}
)

observeEvent(input$Import_csv_save_man_save_mde8,{
  values$Import_csv_mde8<-rep(input$Import_csv_save_man_mde8,dim(values$data_csv)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_csv_mde8),message=FALSE),
    need(input$Import_csv_mde8%in%c(colnames(values$data_csv),"not required"),message=FALSE)
  )
  if(input$Import_csv_mde8=="not required"){
    values$Import_csv_mde8<-NULL
  }
  else{
    values$Import_csv_mde8<-as.vector(as.matrix(values$data_csv[,input$Import_csv_mde8]))
  }
})

#mde9
observeEvent(input$Import_script_mde9,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde9",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_mde9 can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_mde9<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_mde9<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde9", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde9,{
  tryCatch({
    eval(parse(text=input$script_mde9))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde9,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_mde9",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_csv_save_man_save_mde9", "save")
    )
  )
}
)

observeEvent(input$Import_csv_save_man_save_mde9,{
  values$Import_csv_mde9<-rep(input$Import_csv_save_man_mde9,dim(values$data_csv)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_csv_mde9),message=FALSE),
    need(input$Import_csv_mde9%in%c(colnames(values$data_csv),"not required"),message=FALSE)
  )
  if(input$Import_csv_mde9=="not required"){
    values$Import_csv_mde9<-NULL
  }
  else{
    values$Import_csv_mde9<-as.vector(as.matrix(values$data_csv[,input$Import_csv_mde9]))
  }
})




#mde2
observeEvent(input$Import_script_mde2,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde2",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_csv_mde2 can be specified in this script
                          #if you want to use data from the imported csv file, you can use values$data_csv
                          #example:
                          # values$Import_csv_mde2<-paste0("Vortrag Nummer:",as.matrix(values$data_csv[,5]))
                          #or
                          # values$Import_csv_mde2<-rep("unbekannter Titel",dim(values$data_csv)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde2", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde2,{
  tryCatch({
    eval(parse(text=input$script_mde2))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde2,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_csv_save_man_mde2",label = "mde2s (one for all)",placeholder = "Please mde2 in the mde2"),
                actionButton("Import_csv_save_man_save_mde2", "save")
    )
  )
}
)

observeEvent(input$Import_csv_save_man_save_mde2,{
  values$Import_csv_mde2<-rep(input$Import_csv_save_man_mde2,dim(values$data_csv)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_csv_mde2),message=FALSE),
    need(input$Import_csv_mde2%in%c(colnames(values$data_csv),"not required"),message=FALSE)
  )
  if(input$Import_csv_mde2=="not required"){
    values$Import_csv_mde2<-NULL
  }
  else{
    values$Import_csv_mde2<-as.vector(as.matrix(values$data_csv[,input$Import_csv_mde2]))
  } 
})



output$Import_csv_metadata<-DT::renderDataTable({
  if(values$start_mapping==T){
    dataset<-input$Import_csv_dataset
    id_doc<-values$Import_csv_id_doc
    
    title<-values$Import_csv_title
    date<-values$Import_csv_date
    body<-values$Import_csv_body
    token<-values$Import_csv_token
    language<-input$Import_csv_language
    
    mde1<-values$Import_csv_mde1
    mde2<-values$Import_csv_mde2
    mde3<-values$Import_csv_mde3
    mde4<-values$Import_csv_mde4
    mde5<-values$Import_csv_mde5
    mde6<-values$Import_csv_mde6
    mde7<-values$Import_csv_mde7
    mde8<-values$Import_csv_mde8
    mde9<-values$Import_csv_mde9
    
    if(is.null(input$Import_csv_dataset)){
      dataset<-""
    }
    
    max_length<-max(length(dataset),length(id_doc),length(mde1),length(title),length(date),length(body),length(mde2),length(token),length(mde3),length(mde4),length(language),length(mde5),length(mde6),length(mde7),length(mde8),length(mde9))
    data<-data.frame(dataset=rep(dataset,max_length))
    data$id_doc<-c(id_doc, rep("", nrow(data)-length(id_doc)))
    data$title<-c(title, rep("", nrow(data)-length(title)))
    data$body<-c(body, rep("", nrow(data)-length(body)))
    data$date<-c(date, rep("", nrow(data)-length(date)))
    data$token<-c(token, rep("", nrow(data)-length(token)))
    data$language<-rep(language,max_length)
    #free metadata
    data$mde1<-c(mde1, rep("", nrow(data)-length(mde1)))
    data$mde2<-c(mde2, rep("", nrow(data)-length(mde2)))
    data$mde3<-c(mde3, rep("", nrow(data)-length(mde3)))
    data$mde4<-c(mde4, rep("", nrow(data)-length(mde4)))
    data$mde5<-c(mde5, rep("", nrow(data)-length(mde5)))
    data$mde6<-c(mde6, rep("", nrow(data)-length(mde6)))
    data$mde7<-c(mde7, rep("", nrow(data)-length(mde7)))
    data$mde8<-c(mde8, rep("", nrow(data)-length(mde8)))
    data$mde9<-c(mde9, rep("", nrow(data)-length(mde9)))
    
    values$Import_csv_meta_complete<-data
    
    colnames(data)[8:16]<-c(input$UI_Import_name_mde1,input$UI_Import_name_mde2,input$UI_Import_name_mde3,input$UI_Import_name_mde4,input$UI_Import_name_mde5,input$UI_Import_name_mde6,
                            input$UI_Import_name_mde7,input$UI_Import_name_mde8,input$UI_Import_name_mde9)
    
    data<-data[1:min(5,dim(data)[1]),]
    data<-t(apply(data,1,FUN=function(i){apply(as.matrix(i),MARGIN = 1,FUN = function(x){if(is.na(x)){return(x)}else{if(nchar(x)>100){return(paste0(substr(x,1,100),"..."))}else{return(x)}}})}))
    
    datatable(data = data,options = list(dom="t",ordering=F),rownames = F)
  }
  else{
    return(NULL)
  }
})

observe({
  body<-values$Import_csv_body
  body<-stringr::str_remove_all(string = body,pattern = "\n")
  body<-stringr::str_squish(string = body)
  values$Import_csv_token<-unlist(lapply(X = body,FUN = function(x){
    length(stringr::str_split(string = x,pattern = " ",simplify = T))}
  ))
})


observeEvent(ignoreNULL = T,input$Import_csv_dataset,{
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  RMariaDB::dbBegin(conn = mydb)
  values$Import_csv_metadatafields<-RMariaDB::dbGetQuery(mydb,paste0("SELECT * from metadata_names where dataset='",input$Import_csv_dataset,"';"))
  RMariaDB::dbCommit(mydb)
  RMariaDB::dbDisconnect(mydb)
})

output$Import_csv_metadata_names_warning<-renderUI({
  validate(
    need(values$Import_csv_meta_complete[1,"dataset"]!="",message=F),
    #need(any(c(input$Import_csv_mde1,input$Import_csv_mde2,input$Import_csv_mde3,input$Import_csv_mde4,input$Import_csv_mde5,input$Import_csv_mde6,input$Import_csv_mde7,input$Import_csv_mde8,input$Import_csv_mde9)!="not required"),message=F),
    need(!is.null(values$Import_csv_metadatafields),message=F),
    need(dim(values$Import_csv_metadatafields)[1]>0,message = "This dataset is not used yes. Feel free to specify your metadata")
  )
  
  data_db<-values$Import_csv_metadatafields[1,which(!is.na(values$Import_csv_metadatafields))]
  data_db<-data_db[,-1,drop=F]
  data_import<-data.frame(t(c(input$UI_Import_name_mde1,input$UI_Import_name_mde2,input$UI_Import_name_mde3,input$UI_Import_name_mde4,input$UI_Import_name_mde5,input$UI_Import_name_mde6,input$UI_Import_name_mde7,input$UI_Import_name_mde8,input$UI_Import_name_mde9)))
  colnames(data_import)<-c("mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
  data_import<-data_import[1,which(c(input$Import_csv_mde1,input$Import_csv_mde2,input$Import_csv_mde3,input$Import_csv_mde4,input$Import_csv_mde5,input$Import_csv_mde6,input$Import_csv_mde7,input$Import_csv_mde8,input$Import_csv_mde9)!="not required"),drop=F]
  data<-rbind.fill(data_db,data_import)
  colors<-matrix(c(0),dim(data)[1],dim(data)[2])
  #get colors for matching mde's
  if(dim(data)[2]>0){
    for(i in 1:dim(data)[2]){
      mde_names<-data[,i]
      if(any(is.na(mde_names))){
        colors[,i]<-c(1,1)
        next
      }
      if(mde_names[1]== mde_names[2]){
        colors[,i]<-c(0,0)
        next
      }
      else{
        colors[,i]<-c(2,2)
      }
    }
  }
  data<-cbind(data,colors)
  rownames(data)<-c("known","new")
  values$Import_csv_metadatanames_data<-data
  Icon<-tags$p(icon(name = "exclamation","fa-2x"),tags$b("Your current settings do not match those already existing in the database. You can still import your data though."),style="color:#ff8080")
  if(dim(data)[2]==0){
    Icon<-tags$p(icon(name = "check","fa-2x"),tags$b("Your settings match those in the database!"),style="color:#80ff80")
  }
  if(all(data[,c(((ncol(data)/2)+1):ncol(data))]==0)){
    Icon<-tags$p(icon(name = "check","fa-2x"),tags$b("Your settings match those in the database!"),style="color:#80ff80")
  }
  return(tagList(
    tags$div(HTML(paste0("There is already a corpus existing with the abbreviation:",tags$b(isolate(input$Import_csv_dataset)),". If you like to add data to this corpus, be aware of the used mde's:"))
             ),
    DT::dataTableOutput(outputId = "Import_csv_metadatanames_table"),
    tags$br(),
    Icon
  ))
})


output$Import_csv_metadatanames_table<-DT::renderDataTable({
  data =values$Import_csv_metadatanames_data
  validate(
    need(dim(data)[2]>0,message="In the database aswell in the current setting no mde's are beeing used.")
  )
  table<-DT::datatable( data = data,class = 'cell-border stripe',
                        options=list(dom="t",selection="none",columnDefs=list(list(targets=c(((ncol(data)/2)+1):ncol(data)),visible=F))))%>%
    DT::formatStyle(
      c(1:(ncol(data)/2)), c(((ncol(data)/2)+1):ncol(data)),
      backgroundColor = styleEqual(c(0, 1,2), c('#80ff80', '#ffc04d','#ff8080'))
    )
  return(table)
})


observeEvent(input$Import_csv_start_preprocess,{
  #test if metadata is valid 
  data<-values$Import_csv_meta_complete
  if(length(unique(data[,"id_doc"]))!=dim(data)[1]){
    shinyWidgets::sendSweetAlert(session=session,title = "Document id not unique",text = "Please specify id_doc to be unique for every document!",type = "error")
  }
  else{
    if(!(is.numeric(as.numeric(data[,"id_doc"])))){
      shinyWidgets::sendSweetAlert(session=session,title = "Document id not a number",text = "Please specify id_doc to be an integer",type = "error")
    }
    else{
      if(nchar(as.character(data[1,"dataset"]))==0){
        shinyWidgets::sendSweetAlert(session=session,title = "dataset abbreviation not specified",text = "Please specify a abbreviation for the data",type = "error")
      }
      else{
        if(stringr::str_detect(as.character(data[1,"dataset"]),pattern = "_")){
          shinyWidgets::sendSweetAlert(session=session,title = "'_' not allowed",text = "Please specify a abbreviation without using '_'",type = "error")
        }
        else{
          if(any(inherits(try({as.Date(data[,"date"],input$Import_mtf_date_format)}),"Date")==F)){
            shinyWidgets::sendSweetAlert(session=session,title = "At least one given date can't be imported",text = "Please specify the date and the date format",type = "error")
          }
          else{
            if(any(nchar(data[,"body"])<1)){
              #shinyWidgets::sendSweetAlert(session=session,title = "Body is empty for at least one document",type = "warning")
              confirmSweetAlert(
                session = session,
                inputId = "confirm_empty_body_csv_no_db",
                title = NULL,
                type="warning",
                text = tags$b(
                  "There is at least one document with empty body"
                ),
                btn_labels = c("Cancel and change settings", "Continue anyway"),
                html = TRUE
              )
            }
            else{
              #create meta metadata vector
              meta_metadata<-data.frame(t(c(input$Import_csv_dataset,input$UI_Import_name_mde1,input$UI_Import_name_mde2,input$UI_Import_name_mde3,input$UI_Import_name_mde4,input$UI_Import_name_mde5,
                                            input$UI_Import_name_mde6,input$UI_Import_name_mde7,input$UI_Import_name_mde8,input$UI_Import_name_mde9)))
              colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
              if(input$Import_csv_mde1=="not required"){
                meta_metadata[,"mde1"]<-NULL
              }
              if(input$Import_csv_mde2=="not required"){
                meta_metadata[,"mde2"]<-NULL
              }
              if(input$Import_csv_mde3=="not required"){
                meta_metadata[,"mde3"]<-NULL
              }
              if(input$Import_csv_mde4=="not required"){
                meta_metadata[,"mde4"]<-NULL
              }
              if(input$Import_csv_mde5=="not required"){
                meta_metadata[,"mde5"]<-NULL
              }
              if(input$Import_csv_mde6=="not required"){
                meta_metadata[,"mde6"]<-NULL
              }
              if(input$Import_csv_mde7=="not required"){
                meta_metadata[,"mde7"]<-NULL
              }
              if(input$Import_csv_mde8=="not required"){
                meta_metadata[,"mde8"]<-NULL
              }
              if(input$Import_csv_mde9=="not required"){
                meta_metadata[,"mde9"]<-NULL
              }
              #save needed parameters
              parameters<-list(data,db=FALSE,lang=data[1,"language"],input$Import_csv_date_format,meta_metadata)
              #create process ID
              ID<-get_task_id_counter()+1
              set_task_id_counter(ID)
              #save metadata for process
              process_info<-list(ID,paste("New Data - ",input$Import_csv_dataset,sep=""),"Create import csv files",as.character(Sys.time()))
              #save logfile path
              logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
              #create logfile
              write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
              #save data needed in script execution 
              save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
              #start script
              system(paste('Rscript collections/scripts/Import_Script.R','&'))
              #show modal when process is started
              shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
            }
          }
        }
      }
    }
  }
})

# if confirm to continue with empty body is clicked run import script anyway
observeEvent(ignoreNULL = T,input$confirm_empty_body_csv_no_db,{
  if(input$confirm_empty_body_csv_no_db){
    data<-values$Import_csv_meta_complete
    #create meta metadata vector
    meta_metadata<-data.frame(t(c(input$Import_csv_dataset,input$UI_Import_name_mde1,input$UI_Import_name_mde2,input$UI_Import_name_mde3,input$UI_Import_name_mde4,input$UI_Import_name_mde5,
                                  input$UI_Import_name_mde6,input$UI_Import_name_mde7,input$UI_Import_name_mde8,input$UI_Import_name_mde9)))
    colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
    if(input$Import_csv_mde1=="not required"){
      meta_metadata[,"mde1"]<-NULL
    }
    if(input$Import_csv_mde2=="not required"){
      meta_metadata[,"mde2"]<-NULL
    }
    if(input$Import_csv_mde3=="not required"){
      meta_metadata[,"mde3"]<-NULL
    }
    if(input$Import_csv_mde4=="not required"){
      meta_metadata[,"mde4"]<-NULL
    }
    if(input$Import_csv_mde5=="not required"){
      meta_metadata[,"mde5"]<-NULL
    }
    if(input$Import_csv_mde6=="not required"){
      meta_metadata[,"mde6"]<-NULL
    }
    if(input$Import_csv_mde7=="not required"){
      meta_metadata[,"mde7"]<-NULL
    }
    if(input$Import_csv_mde8=="not required"){
      meta_metadata[,"mde8"]<-NULL
    }
    if(input$Import_csv_mde9=="not required"){
      meta_metadata[,"mde9"]<-NULL
    }
    #save needed parameters
    parameters<-list(data,db=FALSE,lang=data[1,"language"],input$Import_csv_date_format,meta_metadata)
    #create process ID
    ID<-get_task_id_counter()+1
    set_task_id_counter(ID)
    #save metadata for process
    process_info<-list(ID,paste("New Data - ",input$Import_csv_dataset,sep=""),"Create import csv files",as.character(Sys.time()))
    #save logfile path
    logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
    #create logfile
    write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
    #save data needed in script execution 
    save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
    #start script
    system(paste('Rscript collections/scripts/Import_Script.R','&'))
    #show modal when process is started
    shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
  }
})


observeEvent(input$Import_csv_start_preprocess_and_write,{
  #test if metadata is valid 
  data<-values$Import_csv_meta_complete
  if(length(unique(data[,"id_doc"]))!=dim(data)[1]){
    shinyWidgets::sendSweetAlert(session=session,title = "Document id not unique",text = "Please specify id_doc to be unique for every document!",type = "error")
  }
  else{
    if(!(is.numeric(as.numeric(data[,"id_doc"])))){
      shinyWidgets::sendSweetAlert(session=session,title = "Document id not a number",text = "Please specify id_doc to be an integer",type = "error")
    }
    else{
      if(nchar(as.character(data[1,"dataset"]))==0){
        shinyWidgets::sendSweetAlert(session=session,title = "dataset abbreviation not specified",text = "Please specify a abbreviation for the data",type = "error")
      }
      else{
        if(stringr::str_detect(as.character(data[1,"dataset"]),pattern = "_")){
          shinyWidgets::sendSweetAlert(session=session,title = "'_' not allowed",text = "Please specify a abbreviation without using '_'",type = "error")
        }
        else{
          if(any(inherits(try({as.Date(data[,"date"],input$Import_mtf_date_format)}),"Date")==F)){
            shinyWidgets::sendSweetAlert(session=session,title = "At least one given date can't be imported",text = "Please specify the date and the date format",type = "error")
          }
          else{
            if(any(nchar(data[,"body"])<1)){
              #shinyWidgets::sendSweetAlert(session=session,title = "Body is empty for at least one document",type = "warning")
              confirmSweetAlert(
                session = session,
                inputId = "confirm_empty_body_csv_db",
                title = NULL,
                type="warning",
                text = tags$b(
                  "There is at least one document with empty body"
                ),
                btn_labels = c("Cancel and change settings", "Continue anyway"),
                html = TRUE
              )
            }
            else{
              #create meta metadata vector
              meta_metadata<-data.frame(t(c(input$Import_csv_dataset,input$UI_Import_name_mde1,input$UI_Import_name_mde2,input$UI_Import_name_mde3,input$UI_Import_name_mde4,input$UI_Import_name_mde5,
                                            input$UI_Import_name_mde6,input$UI_Import_name_mde7,input$UI_Import_name_mde8,input$UI_Import_name_mde9)))
              colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
              if(input$Import_csv_mde1=="not required"){
                meta_metadata[,"mde1"]<-NULL
              }
              if(input$Import_csv_mde2=="not required"){
                meta_metadata[,"mde2"]<-NULL
              }
              if(input$Import_csv_mde3=="not required"){
                meta_metadata[,"mde3"]<-NULL
              }
              if(input$Import_csv_mde4=="not required"){
                meta_metadata[,"mde4"]<-NULL
              }
              if(input$Import_csv_mde5=="not required"){
                meta_metadata[,"mde5"]<-NULL
              }
              if(input$Import_csv_mde6=="not required"){
                meta_metadata[,"mde6"]<-NULL
              }
              if(input$Import_csv_mde7=="not required"){
                meta_metadata[,"mde7"]<-NULL
              }
              if(input$Import_csv_mde8=="not required"){
                meta_metadata[,"mde8"]<-NULL
              }
              if(input$Import_csv_mde9=="not required"){
                meta_metadata[,"mde9"]<-NULL
              }
              #save needed parameters
              parameters<-list(data,db=TRUE,lang=data[1,"language"],input$Import_csv_date_format,meta_metadata)
              #create process ID
              ID<-get_task_id_counter()+1
              set_task_id_counter(ID)
              #save metadata for process
              process_info<-list(ID,paste("New Data - ",input$Import_csv_dataset,sep=""),"Create import csv files",as.character(Sys.time()))
              #save logfile path
              logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
              #create logfile
              write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
              #save data needed in script execution 
              save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
              #start script
              system(paste('Rscript collections/scripts/Import_Script.R','&'))
              #show modal when process is started
              shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
            }
          }
        }
      }
    }
  }
})

# if confirm to continue with empty body is clicked run import script anyway
observeEvent(ignoreNULL = T,input$confirm_empty_body_csv_db,{
  if(input$confirm_empty_body_csv_db){
    data<-values$Import_csv_meta_complete
    #create meta metadata vector
    meta_metadata<-data.frame(t(c(input$Import_csv_dataset,input$UI_Import_name_mde1,input$UI_Import_name_mde2,input$UI_Import_name_mde3,input$UI_Import_name_mde4,input$UI_Import_name_mde5,
                                  input$UI_Import_name_mde6,input$UI_Import_name_mde7,input$UI_Import_name_mde8,input$UI_Import_name_mde9)))
    colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
    if(input$Import_csv_mde1=="not required"){
      meta_metadata[,"mde1"]<-NULL
    }
    if(input$Import_csv_mde2=="not required"){
      meta_metadata[,"mde2"]<-NULL
    }
    if(input$Import_csv_mde3=="not required"){
      meta_metadata[,"mde3"]<-NULL
    }
    if(input$Import_csv_mde4=="not required"){
      meta_metadata[,"mde4"]<-NULL
    }
    if(input$Import_csv_mde5=="not required"){
      meta_metadata[,"mde5"]<-NULL
    }
    if(input$Import_csv_mde6=="not required"){
      meta_metadata[,"mde6"]<-NULL
    }
    if(input$Import_csv_mde7=="not required"){
      meta_metadata[,"mde7"]<-NULL
    }
    if(input$Import_csv_mde8=="not required"){
      meta_metadata[,"mde8"]<-NULL
    }
    if(input$Import_csv_mde9=="not required"){
      meta_metadata[,"mde9"]<-NULL
    }
    #save needed parameters
    parameters<-list(data,db=TRUE,lang=data[1,"language"],input$Import_csv_date_format,meta_metadata)
    #create process ID
    ID<-get_task_id_counter()+1
    set_task_id_counter(ID)
    #save metadata for process
    process_info<-list(ID,paste("New Data - ",input$Import_csv_dataset,sep=""),"Create import csv files",as.character(Sys.time()))
    #save logfile path
    logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
    #create logfile
    write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
    #save data needed in script execution 
    save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
    #start script
    system(paste('Rscript collections/scripts/Import_Script.R','&'))
    #show modal when process is started
    shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
  }
})










##########################################################################################################
#                                import multiple text files   MTF                                        #
##########################################################################################################
values$Import_mtf_id_doc<-""
values$Import_mtf_title<-""
values$Import_mtf_date<-""
values$Import_mtf_body<-""
values$Import_mtf_mde1<-""
values$Import_mtf_mde2<-""
values$Import_mtf_mde3<-""
values$Import_mtf_mde4<-""
values$Import_mtf_mde5<-""
values$Import_mtf_mde6<-""
values$Import_mtf_mde7<-""
values$Import_mtf_mde8<-""
values$Import_mtf_mde9<-""
values$Import_mtf_language<-""
values$Import_mtf_token<-""
values$Import_mtf_dataset<-""


output$UI_Import_mtf_file<-renderUI({
  values$invalidate_mtf_files
  validate(
    need(length(list.dirs("data_import/unprocessed_data/"))>1,message="No directory with text files found in 'data_import/unprocessed_data'")
  )
  return(
    tagList(
      
      shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_files",label = "Directory",
                                       choices = stringr::str_replace(string=list.dirs("data_import/unprocessed_data/")[-1],pattern="data_import/unprocessed_data//",replacement=""),
                                       fill=T,animation = "pulse",selected = character(0))
    ))
  
})

observeEvent(input$Import_mtf_new,ignoreInit = T,{
  validate(
    need(
      !is.null(input$Import_mtf_new),message=F
    )
  )
  
  print(input$Import_mtf_new)
  dir_name<-uuid::UUIDgenerate(use.time = T)
  dir.create(recursive = T,path = paste0("data_import/unprocessed_data/",dir_name))
  values$Import_mtf_new_directory_name_uuid<- paste0("data_import/unprocessed_data/",dir_name)
  for(i in 1:dim(input$Import_mtf_new)[1]){
    file.copy(from = input$Import_mtf_new$datapath[i],to = paste0("data_import/unprocessed_data/",dir_name,"/",input$Import_mtf_new$name[i]))
  }
  showModal(modalDialog(size = "s",
                        textInput(inputId = "Import_mtf_new_name",label = "Name new directory",placeholder = "new directory name"),
                        bsButton(inputId = "Import_mtf_new_name_button",label = "Save",icon = icon("save"),style = "success")
  ))
})

observeEvent(ignoreInit = T,input$Import_mtf_new_name_button,{
  file.rename(from =values$Import_mtf_new_directory_name_uuid ,to =paste0("data_import/unprocessed_data/",input$Import_mtf_new_name))
  removeModal()
  shinyWidgets::sendSweetAlert(session=session,title = "File added",text = "You can now select it in the list of files above",type = "success")
  values$invalidate_mtf_files<-runif(1,0,1)
})



observeEvent(input$Import_load_mtf,{
  withBusyIndicatorServer("Import_load_mtf", {
    validate(
      need(length(list.dirs("data_import/unprocessed_data/"))>1,message=FALSE)
    )
    data<-data.frame(id_doc=1:length(list.files(paste0("data_import/unprocessed_data/",input$Import_mtf_files))))
    texte<-list()
    for(i in 1:dim(data)[1]){
      texte[[i]]<-readtext::readtext(file=list.files(paste0("data_import/unprocessed_data/",input$Import_mtf_files),full.names = T)[i])
    }
    data<-do.call(rbind,texte)
    data<-cbind(1:dim(data)[1],data)
    colnames(data)[1:3]<-c("id_doc","title","text")
    values$header_mtf<-colnames(data)
    values$data_mtf<-data
    values$data_load_mtf_success<-TRUE
  })
})
###meta data csv

observeEvent(input$Import_mtf_metadata_csv,{
  values$mtf_metadata<-read.csv(input$Import_mtf_metadata_csv$datapath,header=input$Import_mtf_metadata_csv_header)
  if(dim(values$mtf_metadata)[1]!=dim(isolate(values$data_mtf))[1]){
    shinyWidgets::sendSweetAlert(session=session,title = "Wrong dimensions",text = paste0("Metadata CSV does not match the number of files in the chosen directory. ",
                                                                                          isolate(dim(values$data_mtf)[1])," files were imported The metadata csv has ",isolate(dim(values$mtf_metadata)[1])," rows."),type = "warning")
  }
  else{
    isolate(values$header_mtf<-c(values$header_mtf,colnames(values$mtf_metadata)))
    isolate(values$data_mtf<-cbind(values$data_mtf,values$mtf_metadata))   
  }
})



observeEvent(input$Import_start_mapping_mtf,{
  values$start_mapping_mtf<-TRUE
})

output$data_load_mtf_success<-reactive({
  values$data_load_mtf_success
})

output$start_mapping_mtf<-reactive({
  values$start_mapping_mtf
})
outputOptions(output, "data_load_mtf_success", suspendWhenHidden = FALSE)
outputOptions(output, "start_mapping_mtf", suspendWhenHidden = FALSE)

output$Import_head_mtf<-DT::renderDataTable({
  data<-values$data_mtf
  data<-data[1:min(5,dim(data)[1]),]
  data<-t(apply(data,1,FUN=function(i){apply(as.matrix(i),MARGIN = 1,FUN = function(x){if(nchar(x)>100){return(substr(x,1,100))}else{return(x)}})}))
  datatable(data = data,options = list(lengthChange = FALSE,dom="t"),width = "90%")
})


observeEvent(input$Import_check_mtf,{
  showModal(
    modalDialog(
      size = "l",
      title = "Imported Directory",easyClose = T,
      tags$div(style="overflow-x:auto; height:70vh;",
               dataTableOutput(outputId = "Import_head_mtf")
      )
    ) 
  )
})



output$UI_Import_mtf_id_doc<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_id_doc",label = "Map id_doc",
                                   choices = c("automatic",values$header_mtf),
                                   fill=T,animation = "pulse",selected = "automatic")
  
})

output$UI_Import_mtf_title<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_title",label = "Map title",
                                   choices = c("automatic",values$header_mtf),
                                   fill=T,animation = "pulse",selected = "automatic")
})

output$UI_Import_mtf_date<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_date",label = "Map date",
                                   choices = c("automatic",values$header_mtf),
                                   fill=T,animation = "pulse",selected = "automatic")
})

output$UI_Import_mtf_body<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_body",label = "Map body",
                                   choices = values$header_mtf,
                                   fill=T,animation = "pulse")
})



output$UI_Import_mtf_mde1<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde1",label = "Map mde1",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde1",label = paste0("Map ",input$UI_Import_name_mde1_mtf))
})

output$UI_Import_mtf_mde2<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde2",label = "Map mde2",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde2",label = paste0("Map ",input$UI_Import_name_mde2_mtf))
})

output$UI_Import_mtf_mde3<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde3",label = "Map mde3",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde3",label = paste0("Map ",input$UI_Import_name_mde3_mtf))
})

output$UI_Import_mtf_mde4<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde4",label = "Map mde4",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde4",label = paste0("Map ",input$UI_Import_name_mde4_mtf))
})

output$UI_Import_mtf_mde5<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde5",label ="Map mde5",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde5",label = paste0("Map ",input$UI_Import_name_mde5_mtf))
})

output$UI_Import_mtf_mde6<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde6",label = "Map mde6",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde6",label = paste0("Map ",input$UI_Import_name_mde6_mtf))
})

output$UI_Import_mtf_mde7<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde7",label ="Map mde7",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde7",label = paste0("Map ",input$UI_Import_name_mde7_mtf))
})

output$UI_Import_mtf_mde8<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde8",label = "Map mde8",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde8",label = paste0("Map ",input$UI_Import_name_mde8_mtf))
})

output$UI_Import_mtf_mde9<-renderUI({
  shinyWidgets::prettyRadioButtons(inputId = "Import_mtf_mde9",label = "Map mde9",
                                   choices = c("not required",values$header_mtf),
                                   fill=T,animation = "pulse")
})

observe({
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "Import_mtf_mde9",label = paste0("Map ",input$UI_Import_name_mde9_mtf))
})

#title
observeEvent(input$Import_script_title_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_title_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
#the vector values$Import_mtf_title can be specified in this script
#if you want to use data from the imported mtf file, you can use values$data_mtf
#example:
# values$Import_mtf_title<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
#or
# values$Import_mtf_title<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_title_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_title_mtf,{
  tryCatch({
    eval(parse(text=input$script_title_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_title_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_title",label = "Titles (one for all)",placeholder = "Please type in the title"),
                actionButton("Import_mtf_save_man_save_title", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_title,{
  values$Import_mtf_title<-rep(input$Import_mtf_save_man_title,dim(values$data_mtf)[1])
  removeModal()
})


observe({
  validate(
    need(!is.null(input$Import_mtf_title),message=FALSE),
    need(input$Import_mtf_title%in%c(colnames(values$data_mtf),"automatic"),message=FALSE)
  )
  if(input$Import_mtf_title=="automatic"){
    values$Import_mtf_title<-paste("document ",1:dim(values$data_mtf)[1],sep="")
  }
  else{
    values$Import_mtf_title<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_title]))
  }
})



#id_doc
observeEvent(input$Import_script_id_doc_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_id_doc_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
#the vector values$Import_mtf_id_doc can be specified in this script
#if you want to use data from the imported mtf file, you can use values$data_mtf
#example:
# values$Import_mtf_id_doc<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
#or
# values$Import_mtf_id_doc<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_id_doc_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_id_doc_mtf,{
  tryCatch({
    eval(parse(text=input$script_id_doc_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_id_doc_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_id_doc",label = "id_doc (one for all)",placeholder = "Please type in the id_doc"),
                actionButton("Import_mtf_save_man_save_id_doc", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_id_doc,{
  values$Import_mtf_id_doc<-rep(input$Import_mtf_save_man_id_doc,dim(values$data_mtf)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_mtf_id_doc),message=FALSE),
    need(input$Import_mtf_id_doc%in%c(colnames(values$data_mtf),"automatic"),message=FALSE)
  )
  if(input$Import_mtf_id_doc=="automatic"){
    #check max id_doc in database for specified dataset
    offset=NA
    if(input$Import_mtf_dataset!=""){
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
      #print(paste0("SELECT MAX(id_doc) FROM ilcm.documents where dataset=",input$Import_mtf_dataset,";"))
      offset<-RMariaDB::dbGetQuery(mydb,paste0("SELECT MAX(id_doc) FROM ilcm.documents where dataset='",input$Import_mtf_dataset,"';"))[1,1]
    }
    if(is.na(offset)){
      offset=0
    }
    values$Import_mtf_id_doc<-(offset+1):(offset+dim(values$data_mtf)[1])
  }
  else{
    values$Import_mtf_id_doc<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_id_doc]))
  }
})

#body
observeEvent(input$Import_script_body_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_body_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
#the vector values$Import_mtf_body can be specified in this script
#if you want to use data from the imported mtf file, you can use values$data_mtf
#example:
# values$Import_mtf_body<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
#or
# values$Import_mtf_body<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_body_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_body_mtf,{
  tryCatch({
    eval(parse(text=input$script_body_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_body_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_body",label = "bodys (one for all)",placeholder = "Please type in the body"),
                actionButton("Import_mtf_save_man_save_body", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_body,{
  values$Import_mtf_body<-rep(input$Import_mtf_save_man_body,dim(values$data_mtf)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_mtf_body),message=FALSE),
    need(input$Import_mtf_body%in%colnames(values$data_mtf),message=FALSE)
  )
  values$Import_mtf_body<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_body]))
})

#date
observeEvent(input$Import_script_date_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_date_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
#the vector values$Import_mtf_date can be specified in this script
#if you want to use data from the imported mtf file, you can use values$data_mtf
#example:
# values$Import_mtf_date<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
#or
# values$Import_mtf_date<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_date_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_date_mtf,{
  tryCatch({
    eval(parse(text=input$script_date_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_date_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_date",label = "dates (one for all)",placeholder = "Please type in the date"),
                actionButton("Import_mtf_save_man_save_date", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_date,{
  values$Import_mtf_date<-rep(input$Import_mtf_save_man_date,dim(values$data_mtf)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_mtf_date),message=FALSE),
    need(input$Import_mtf_date%in%c(colnames(values$data_mtf),"automatic"),message=FALSE)
  )
  if(input$Import_mtf_date=="automatic"){
    values$Import_mtf_date<-rep(as.character(Sys.Date()),dim(values$data_mtf)[1])
  }
  else{
    values$Import_mtf_date<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_date]))
  }
})

#mde1
observeEvent(input$Import_script_mde1_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde1_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_mtf_mde1 can be specified in this script
                          #if you want to use data from the imported mtf file, you can use values$data_mtf
                          #example:
                          # values$Import_mtf_mde1<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
                          #or
                          # values$Import_mtf_mde1<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde1_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde1_mtf,{
  tryCatch({
    eval(parse(text=input$script_mde1_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde1_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_mde1",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_mtf_save_man_save_mde1", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_mde1,{
  values$Import_mtf_mde1<-rep(input$Import_mtf_save_man_mde1,dim(values$data_mtf)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_mtf_mde1),message=FALSE),
    need(input$Import_mtf_mde1%in%c(colnames(values$data_mtf),"not required"),message=FALSE)
  )
  if(input$Import_mtf_mde1=="not required"){
    values$Import_mtf_mde1<-NULL
  }
  else{
    values$Import_mtf_mde1<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_mde1]))
  }
})

#mde2
observeEvent(input$Import_script_mde2_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde2_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_mtf_mde2 can be specified in this script
                          #if you want to use data from the imported mtf file, you can use values$data_mtf
                          #example:
                          # values$Import_mtf_mde2<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
                          #or
                          # values$Import_mtf_mde2<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde2_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde2_mtf,{
  tryCatch({
    eval(parse(text=input$script_mde2_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde2_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_mde2",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_mtf_save_man_save_mde2", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_mde2,{
  values$Import_mtf_mde2<-rep(input$Import_mtf_save_man_mde2,dim(values$data_mtf)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_mtf_mde2),message=FALSE),
    need(input$Import_mtf_mde2%in%c(colnames(values$data_mtf),"not required"),message=FALSE)
  )
  if(input$Import_mtf_mde2=="not required"){
    values$Import_mtf_mde2<-NULL
  }
  else{
    values$Import_mtf_mde2<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_mde2]))
  }
})

#mde3
observeEvent(input$Import_script_mde3_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde3_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_mtf_mde3 can be specified in this script
                          #if you want to use data from the imported mtf file, you can use values$data_mtf
                          #example:
                          # values$Import_mtf_mde3<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
                          #or
                          # values$Import_mtf_mde3<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde3_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde3_mtf,{
  tryCatch({
    eval(parse(text=input$script_mde3_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde3_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_mde3",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_mtf_save_man_save_mde3", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_mde3,{
  values$Import_mtf_mde3<-rep(input$Import_mtf_save_man_mde3,dim(values$data_mtf)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_mtf_mde3),message=FALSE),
    need(input$Import_mtf_mde3%in%c(colnames(values$data_mtf),"not required"),message=FALSE)
  )
  if(input$Import_mtf_mde3=="not required"){
    values$Import_mtf_mde3<-NULL
  }
  else{
    values$Import_mtf_mde3<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_mde3]))
  }
})

#mde4
observeEvent(input$Import_script_mde4_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde4_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_mtf_mde4 can be specified in this script
                          #if you want to use data from the imported mtf file, you can use values$data_mtf
                          #example:
                          # values$Import_mtf_mde4<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
                          #or
                          # values$Import_mtf_mde4<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde4_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde4_mtf,{
  tryCatch({
    eval(parse(text=input$script_mde4_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde4_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_mde4",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_mtf_save_man_save_mde4", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_mde4,{
  values$Import_mtf_mde4<-rep(input$Import_mtf_save_man_mde4,dim(values$data_mtf)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_mtf_mde4),message=FALSE),
    need(input$Import_mtf_mde4%in%c(colnames(values$data_mtf),"not required"),message=FALSE)
  )
  if(input$Import_mtf_mde4=="not required"){
    values$Import_mtf_mde4<-NULL
  }
  else{
    values$Import_mtf_mde4<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_mde4]))
  }
})

#mde5
observeEvent(input$Import_script_mde5_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde5_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_mtf_mde5 can be specified in this script
                          #if you want to use data from the imported mtf file, you can use values$data_mtf
                          #example:
                          # values$Import_mtf_mde5<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
                          #or
                          # values$Import_mtf_mde5<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde5_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde5_mtf,{
  tryCatch({
    eval(parse(text=input$script_mde5_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde5_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_mde5",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_mtf_save_man_save_mde5", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_mde5,{
  values$Import_mtf_mde5<-rep(input$Import_mtf_save_man_mde5,dim(values$data_mtf)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_mtf_mde5),message=FALSE),
    need(input$Import_mtf_mde5%in%c(colnames(values$data_mtf),"not required"),message=FALSE)
  )
  if(input$Import_mtf_mde5=="not required"){
    values$Import_mtf_mde5<-NULL
  }
  else{
    values$Import_mtf_mde5<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_mde5]))
  }
})

#mde6
observeEvent(input$Import_script_mde6_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde6_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_mtf_mde6 can be specified in this script
                          #if you want to use data from the imported mtf file, you can use values$data_mtf
                          #example:
                          # values$Import_mtf_mde6<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
                          #or
                          # values$Import_mtf_mde6<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde6_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde6_mtf,{
  tryCatch({
    eval(parse(text=input$script_mde6_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde6_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_mde6",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_mtf_save_man_save_mde6", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_mde6,{
  values$Import_mtf_mde6<-rep(input$Import_mtf_save_man_mde6,dim(values$data_mtf)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_mtf_mde6),message=FALSE),
    need(input$Import_mtf_mde6%in%c(colnames(values$data_mtf),"not required"),message=FALSE)
  )
  if(input$Import_mtf_mde6=="not required"){
    values$Import_mtf_mde6<-NULL
  }
  else{
    values$Import_mtf_mde6<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_mde6]))
  }
})

#mde7
observeEvent(input$Import_script_mde7_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde7_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_mtf_mde7 can be specified in this script
                          #if you want to use data from the imported mtf file, you can use values$data_mtf
                          #example:
                          # values$Import_mtf_mde7<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
                          #or
                          # values$Import_mtf_mde7<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde7_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde7_mtf,{
  tryCatch({
    eval(parse(text=input$script_mde7_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde7_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_mde7",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_mtf_save_man_save_mde7", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_mde7,{
  values$Import_mtf_mde7<-rep(input$Import_mtf_save_man_mde7,dim(values$data_mtf)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_mtf_mde7),message=FALSE),
    need(input$Import_mtf_mde7%in%c(colnames(values$data_mtf),"not required"),message=FALSE)
  )
  if(input$Import_mtf_mde7=="not required"){
    values$Import_mtf_mde7<-NULL
  }
  else{
    values$Import_mtf_mde7<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_mde7]))
  }
})

#mde8
observeEvent(input$Import_script_mde8_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde8_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_mtf_mde8 can be specified in this script
                          #if you want to use data from the imported mtf file, you can use values$data_mtf
                          #example:
                          # values$Import_mtf_mde8<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
                          #or
                          # values$Import_mtf_mde8<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde8_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde8_mtf,{
  tryCatch({
    eval(parse(text=input$script_mde8_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde8_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_mde8",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_mtf_save_man_save_mde8", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_mde8,{
  values$Import_mtf_mde8<-rep(input$Import_mtf_save_man_mde8,dim(values$data_mtf)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_mtf_mde8),message=FALSE),
    need(input$Import_mtf_mde8%in%c(colnames(values$data_mtf),"not required"),message=FALSE)
  )
  if(input$Import_mtf_mde8=="not required"){
    values$Import_mtf_mde8<-NULL
  }
  else{
    values$Import_mtf_mde8<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_mde8]))
  }
})

#mde9
observeEvent(input$Import_script_mde9_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                aceEditor("script_mde9_mtf",theme ="chrome"  ,mode="r", fontSize = "15",showLineNumbers = T,highlightActiveLine = T,autoComplete = "live",value='
                          #the vector values$Import_mtf_mde9 can be specified in this script
                          #if you want to use data from the imported mtf file, you can use values$data_mtf
                          #example:
                          # values$Import_mtf_mde9<-paste0("Vortrag Nummer:",as.matrix(values$data_mtf[,5]))
                          #or
                          # values$Import_mtf_mde9<-rep("unbekannter Titel",dim(values$data_mtf)[1])
                          '),
                footer = tagList(
                  actionButton("save_Imp_mde9_mtf", "save")
                )
    )
  )
})
observeEvent(input$save_Imp_mde9_mtf,{
  tryCatch({
    eval(parse(text=input$script_mde9_mtf))
  },
  error=function(e){
    shinyWidgets::sendSweetAlert(session=session,title = "error in code",text = as.character(e),type = "error")
  }
  )
  removeModal()
})

observeEvent(input$Import_type_mde9_mtf,{
  showModal(
    modalDialog(size = "l",easyClose = T,fade = T,
                textInput(inputId = "Import_mtf_save_man_mde9",label = "types (one for all)",placeholder = "Please type in the type"),
                actionButton("Import_mtf_save_man_save_mde9", "save")
    )
  )
}
)

observeEvent(input$Import_mtf_save_man_save_mde9,{
  values$Import_mtf_mde9<-rep(input$Import_mtf_save_man_mde9,dim(values$data_mtf)[1])
  removeModal()
})

observe({
  validate(
    need(!is.null(input$Import_mtf_mde9),message=FALSE),
    need(input$Import_mtf_mde9%in%c(colnames(values$data_mtf),"not required"),message=FALSE)
  )
  if(input$Import_mtf_mde9=="not required"){
    values$Import_mtf_mde9<-NULL
  }
  else{
    values$Import_mtf_mde9<-as.vector(as.matrix(values$data_mtf[,input$Import_mtf_mde9]))
  }
})





output$Import_mtf_metadata<-DT::renderDataTable({
  if(values$start_mapping_mtf==T){
    dataset<-input$Import_mtf_dataset
    id_doc<-values$Import_mtf_id_doc
    title<-values$Import_mtf_title
    date<-values$Import_mtf_date
    body<-values$Import_mtf_body
    token<-values$Import_mtf_token
    language<-input$Import_mtf_language
    mde1<-values$Import_mtf_mde1
    mde2<-values$Import_mtf_mde2
    mde3<-values$Import_mtf_mde3
    mde4<-values$Import_mtf_mde4
    mde5<-values$Import_mtf_mde5
    mde6<-values$Import_mtf_mde6
    mde7<-values$Import_mtf_mde7
    mde8<-values$Import_mtf_mde8
    mde9<-values$Import_mtf_mde9
    
    if(is.null(input$Import_mtf_dataset)){
      dataset<-""
    }
    max_length<-max(length(dataset),length(id_doc),length(mde1),length(title),length(date),length(body),length(mde2),
                    length(token),length(mde3),length(mde4),length(language),length(mde5),length(mde6),length(mde7),length(mde8),length(mde9))
    data<-data.frame(dataset=rep(dataset,max_length))
    data$id_doc<-c(id_doc, rep("", nrow(data)-length(id_doc)))
    data$title<-c(title, rep("", nrow(data)-length(title)))
    data$body<-c(body, rep("", nrow(data)-length(body)))
    data$date<-c(date, rep("", nrow(data)-length(date)))
    data$token<-c(token, rep("", nrow(data)-length(token)))
    data$language<-rep(language,max_length)
    #free metadata
    data$mde1<-c(mde1, rep("", nrow(data)-length(mde1)))
    data$mde2<-c(mde2, rep("", nrow(data)-length(mde2)))
    data$mde3<-c(mde3, rep("", nrow(data)-length(mde3)))
    data$mde4<-c(mde4, rep("", nrow(data)-length(mde4)))
    data$mde5<-c(mde5, rep("", nrow(data)-length(mde5)))
    data$mde6<-c(mde6, rep("", nrow(data)-length(mde6)))
    data$mde7<-c(mde7, rep("", nrow(data)-length(mde7)))
    data$mde8<-c(mde8, rep("", nrow(data)-length(mde8)))
    data$mde9<-c(mde9, rep("", nrow(data)-length(mde9)))
    
    values$Import_mtf_meta_complete<-data
    
    colnames(data)[8:16]<-c(input$UI_Import_name_mde1_mtf,input$UI_Import_name_mde2_mtf,input$UI_Import_name_mde3_mtf,input$UI_Import_name_mde4_mtf,
                            input$UI_Import_name_mde5_mtf,input$UI_Import_name_mde6_mtf,
                            input$UI_Import_name_mde7_mtf,input$UI_Import_name_mde8_mtf,input$UI_Import_name_mde9_mtf)
    
    data<-data[1:min(5,dim(data)[1]),]
    data<-t(apply(data,1,FUN=function(i){apply(as.matrix(i),MARGIN = 1,FUN = function(x){if(nchar(x)>100){return(paste0(substr(x,1,100),"..."))}else{return(x)}})}))
    
    datatable(data = data,options = list(dom="t",ordering=F),rownames = F)
  }
  else{
    return(NULL)
  }
})

observe({
  body<-values$Import_mtf_body
  body<-stringr::str_remove_all(string = body,pattern = "\n")
  body<-stringr::str_squish(string = body)
  values$Import_mtf_token<-unlist(lapply(X = body,FUN = function(x){
    length(stringr::str_split(string = x,pattern = " ",simplify = T))}
  ))
})

observeEvent(ignoreNULL = T,input$Import_mtf_dataset,{
  mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=values$db_port)
  RMariaDB::dbBegin(conn = mydb)
  values$Import_mtf_metadatafields<-RMariaDB::dbGetQuery(mydb,paste0("SELECT * from metadata_names where dataset='",input$Import_mtf_dataset,"';"))
  RMariaDB::dbCommit(mydb)
  RMariaDB::dbDisconnect(mydb)
})

output$Import_mtf_metadata_names_warning<-renderUI({
  validate(
    need(values$Import_mtf_meta_complete[1,"dataset"]!="",message=F),
    #need(any(c(input$Import_csv_mde1,input$Import_csv_mde2,input$Import_csv_mde3,input$Import_csv_mde4,input$Import_csv_mde5,input$Import_csv_mde6,input$Import_csv_mde7,input$Import_csv_mde8,input$Import_csv_mde9)!="not required"),message=F),
    need(!is.null(values$Import_mtf_metadatafields),message=F),
    need(dim(values$Import_mtf_metadatafields)[1]>0,message = "This dataset is not used yes. Feel free to specify your metadata")
  )
  
  data_db<-values$Import_mtf_metadatafields[1,which(!is.na(values$Import_mtf_metadatafields))]
  data_db<-data_db[,-1,drop=F]
  data_import<-data.frame(t(c(input$UI_Import_name_mde1_mtf,input$UI_Import_name_mde2_mtf,input$UI_Import_name_mde3_mtf,input$UI_Import_name_mde4_mtf,input$UI_Import_name_mde5_mtf,input$UI_Import_name_mde6_mtf,input$UI_Import_name_mde7_mtf,input$UI_Import_name_mde8_mtf,input$UI_Import_name_mde9_mtf)))
  colnames(data_import)<-c("mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
  data_import<-data_import[1,which(c(input$Import_mtf_mde1,input$Import_mtf_mde2,input$Import_mtf_mde3,input$Import_mtf_mde4,input$Import_mtf_mde5,input$Import_mtf_mde6,input$Import_mtf_mde7,input$Import_mtf_mde8,input$Import_mtf_mde9)!="not required"),drop=F]
  data<-rbind.fill(data_db,data_import)
  colors<-matrix(c(0),dim(data)[1],dim(data)[2])
  #get colors for matching mde's
  if(dim(data)[2]>0){
    for(i in 1:dim(data)[2]){
      mde_names<-data[,i]
      if(any(is.na(mde_names))){
        colors[,i]<-c(1,1)
        next
      }
      if(mde_names[1]== mde_names[2]){
        colors[,i]<-c(0,0)
        next
      }
      else{
        colors[,i]<-c(2,2)
      }
    }
  }
  data<-cbind(data,colors)
  rownames(data)<-c("known","new")
  values$Import_mtf_metadatanames_data<-data
  Icon<-tags$p(icon(name = "exclamation","fa-2x"),tags$b("Your current settings do not match those already existing in the database. You can still import your data though."),style="color:#ff8080")
  if(dim(data)[2]==0){
    Icon<-tags$p(icon(name = "check","fa-2x"),tags$b("Your settings match those in the database!"),style="color:#80ff80")
  }
  if(all(data[,c(((ncol(data)/2)+1):ncol(data))]==0)){
    Icon<-tags$p(icon(name = "check","fa-2x"),tags$b("Your settings match those in the database!"),style="color:#80ff80")
  }
  return(tagList(
    tags$div(HTML(paste0("There is already a corpus existing with the abbreviation:",tags$b(isolate(input$Import_mtf_dataset)),". If you like to add data to this corpus, be aware of the used mde's:"))
    ),
    DT::dataTableOutput(outputId = "Import_mtf_metadatanames_table"),
    tags$br(),
    Icon
  ))
})

output$Import_mtf_metadatanames_table<-DT::renderDataTable({
  data =values$Import_mtf_metadatanames_data
  validate(
    need(dim(data)[2]>0,message="In the database aswell in the current setting no mde's are beeing used.")
  )
  table<-DT::datatable( data = data,class = 'cell-border stripe',
                        options=list(dom="t",selection="none",columnDefs=list(list(targets=c(((ncol(data)/2)+1):ncol(data)),visible=F))))%>%
    DT::formatStyle(
      c(1:(ncol(data)/2)), c(((ncol(data)/2)+1):ncol(data)),
      backgroundColor = styleEqual(c(0, 1,2), c('#80ff80', '#ffc04d','#ff8080'))
    )
  return(table)
})





observeEvent(input$Import_mtf_start_preprocess,{
  #test if metadata is valid 
  data<-values$Import_mtf_meta_complete
  if(length(unique(data[,"id_doc"]))!=dim(data)[1]){
    shinyWidgets::sendSweetAlert(session=session,title = "Document id not unique",text = "Please specify id_doc to be unique for every document!",type = "error")
  }
  else{
    if(!(is.numeric(as.numeric(data[,"id_doc"])))){
      shinyWidgets::sendSweetAlert(session=session,title = "Document id not a number",text = "Please specify id_doc to be an integer",type = "error")
    }
    else{
      if(nchar(as.character(data[1,"dataset"]))==0){
        shinyWidgets::sendSweetAlert(session=session,title = "dataset abbreviation not specified",text = "Please specify a abbreviation for the data",type = "error")
      }
      else{
        if(stringr::str_detect(as.character(data[1,"dataset"]),pattern = "_")){
          shinyWidgets::sendSweetAlert(session=session,title = "'_' not allowed",text = "Please specify a abbreviation without using '_'",type = "error")
        }
        else{
          if(nchar(data[1,"dataset"])>50){
            shinyWidgets::sendSweetAlert(session=session,title = "abbreviation too long",text = "Please specify a abbreviation with maximum 50 chars.",type = "error")
          }
          else{
            if(any(inherits(try({as.Date(data[,"date"],input$Import_mtf_date_format)}),"Date")==F)){
              shinyWidgets::sendSweetAlert(session=session,title = "At least one given date can't be imported",text = "Please specify the date and the date format",type = "error")
            }
            else{
              if(any(nchar(data[,"body"])<1)){
                #shinyWidgets::sendSweetAlert(session=session,title = "Body is empty for at least one document",type = "warning")
                confirmSweetAlert(
                  session = session,
                  inputId = "confirm_empty_body_mtf_no_db",
                  title = NULL,
                  type="warning",
                  text = tags$b(
                    "There is at least one document with empty body"
                  ),
                  btn_labels = c("Cancel and change settings", "Continue anyway"),
                  html = TRUE
                )
              }
              else{
                #create meta metadata vector
                meta_metadata<-data.frame(t(c(input$Import_mtf_dataset,input$UI_Import_name_mde1_mtf,input$UI_Import_name_mde2_mtf,input$UI_Import_name_mde3_mtf,input$UI_Import_name_mde4_mtf,input$UI_Import_name_mde5_mtf,
                                              input$UI_Import_name_mde6_mtf,input$UI_Import_name_mde7_mtf,input$UI_Import_name_mde8_mtf,input$UI_Import_name_mde9_mtf)))
                colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
                if(input$Import_mtf_mde1=="not required"){
                  meta_metadata[,"mde1"]<-NULL
                }
                if(input$Import_mtf_mde2=="not required"){
                  meta_metadata[,"mde2"]<-NULL
                }
                if(input$Import_mtf_mde3=="not required"){
                  meta_metadata[,"mde3"]<-NULL
                }
                if(input$Import_mtf_mde4=="not required"){
                  meta_metadata[,"mde4"]<-NULL
                }
                if(input$Import_mtf_mde5=="not required"){
                  meta_metadata[,"mde5"]<-NULL
                }
                if(input$Import_mtf_mde6=="not required"){
                  meta_metadata[,"mde6"]<-NULL
                }
                if(input$Import_mtf_mde7=="not required"){
                  meta_metadata[,"mde7"]<-NULL
                }
                if(input$Import_mtf_mde8=="not required"){
                  meta_metadata[,"mde8"]<-NULL
                }
                if(input$Import_mtf_mde9=="not required"){
                  meta_metadata[,"mde9"]<-NULL
                }
                #save needed parameters
                parameters<-list(data,db=FALSE,lang=data[1,"language"],input$Import_mtf_date_format,meta_metadata)
                #create process ID
                ID<-get_task_id_counter()+1
                set_task_id_counter(ID)
                #save metadata for process
                process_info<-list(ID,paste("New Data - ",input$Import_mtf_dataset,sep=""),"Create import csv files",as.character(Sys.time()))
                #save logfile path
                logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
                #create logfile
                write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
                #save data needed in script execution 
                save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
                #start script
                system(paste('Rscript collections/scripts/Import_Script.R','&'))
                #show modal when process is started
                shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
              }
            }
          }
        }
      }
    }
  }
})


# if confirm to continue with empty body is clicked run import script anyway
observeEvent(ignoreNULL = T,input$confirm_empty_body_mtf_no_db,{
  if(input$confirm_empty_body_mtf_no_db){
    data<-values$Import_mtf_meta_complete #create meta metadata vector
    meta_metadata<-data.frame(t(c(input$Import_mtf_dataset,input$UI_Import_name_mde1_mtf,input$UI_Import_name_mde2_mtf,input$UI_Import_name_mde3_mtf,input$UI_Import_name_mde4_mtf,input$UI_Import_name_mde5_mtf,
                                  input$UI_Import_name_mde6_mtf,input$UI_Import_name_mde7_mtf,input$UI_Import_name_mde8_mtf,input$UI_Import_name_mde9_mtf)))
    colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
    if(input$Import_mtf_mde1=="not required"){
      meta_metadata[,"mde1"]<-NULL
    }
    if(input$Import_mtf_mde2=="not required"){
      meta_metadata[,"mde2"]<-NULL
    }
    if(input$Import_mtf_mde3=="not required"){
      meta_metadata[,"mde3"]<-NULL
    }
    if(input$Import_mtf_mde4=="not required"){
      meta_metadata[,"mde4"]<-NULL
    }
    if(input$Import_mtf_mde5=="not required"){
      meta_metadata[,"mde5"]<-NULL
    }
    if(input$Import_mtf_mde6=="not required"){
      meta_metadata[,"mde6"]<-NULL
    }
    if(input$Import_mtf_mde7=="not required"){
      meta_metadata[,"mde7"]<-NULL
    }
    if(input$Import_mtf_mde8=="not required"){
      meta_metadata[,"mde8"]<-NULL
    }
    if(input$Import_mtf_mde9=="not required"){
      meta_metadata[,"mde9"]<-NULL
    }
    #save needed parameters
    parameters<-list(data,db=FALSE,lang=data[1,"language"],input$Import_mtf_date_format,meta_metadata)
    #create process ID
    ID<-get_task_id_counter()+1
    set_task_id_counter(ID)
    #save metadata for process
    process_info<-list(ID,paste("New Data - ",input$Import_mtf_dataset,sep=""),"Create import csv files",as.character(Sys.time()))
    #save logfile path
    logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
    #create logfile
    write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
    #save data needed in script execution 
    save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
    #start script
    system(paste('Rscript collections/scripts/Import_Script.R','&'))
    #show modal when process is started
    shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success")
  }
})


observeEvent(input$Import_mtf_start_preprocess_and_write,{
  #test if metadata is valid 
  data<-values$Import_mtf_meta_complete
  if(length(unique(data[,"id_doc"]))!=dim(data)[1]){
    shinyWidgets::sendSweetAlert(session=session,title = "Document id not unique",text = "Please specify id_doc to be unique for every document!",type = "error")
  }
  else{
    if(!(is.numeric(as.numeric(data[,"id_doc"])))){
      shinyWidgets::sendSweetAlert(session=session,title = "Document id not a number",text = "Please specify id_doc to be an integer",type = "error")
    }
    else{
      if(nchar(as.character(data[1,"dataset"]))==0){
        shinyWidgets::sendSweetAlert(session=session,title = "dataset abbreviation not specified",text = "Please specify a abbreviation for the data",type = "error")
      }
      else{
        if(stringr::str_detect(as.character(data[1,"dataset"]),pattern = "_")){
          shinyWidgets::sendSweetAlert(session=session,title = "'_' not allowed",text = "Please specify a abbreviation without using '_'",type = "error")
        }
        else{
          if(nchar(data[1,"dataset"])>50){
            shinyWidgets::sendSweetAlert(session=session,title = "abbreviation too long",text = "Please specify a abbreviation with maximum 50 chars.",type = "error")
          }
          else{
            if(any(inherits(try({as.Date(data[,"date"],input$Import_mtf_date_format)}),"Date")==F)){
              shinyWidgets::sendSweetAlert(session=session,title = "At least one given date can't be imported",text = "Please specify the date and the date format or if you are not intrested in using dates, just use the 'autoamtic'-option",type = "error")
            }
            else{
              if(any(nchar(data[,"body"])<1)){
                #shinyWidgets::sendSweetAlert(session=session,title = "Body is empty for at least one document",type = "warning")
                confirmSweetAlert(
                  session = session,
                  inputId = "confirm_empty_body_mtf_db",
                  title = NULL,
                  type="warning",
                  text = tags$b(
                    "There is at least one document with empty body"
                  ),
                  btn_labels = c("Cancel and change settings", "Continue anyway"),
                  html = TRUE
                )
              }
              else{
                #create meta metadata vector
                meta_metadata<-data.frame(t(c(input$Import_mtf_dataset,input$UI_Import_name_mde1_mtf,input$UI_Import_name_mde2_mtf,input$UI_Import_name_mde3_mtf,input$UI_Import_name_mde4_mtf,input$UI_Import_name_mde5_mtf,
                                              input$UI_Import_name_mde6_mtf,input$UI_Import_name_mde7_mtf,input$UI_Import_name_mde8_mtf,input$UI_Import_name_mde9_mtf)))
                colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
                if(input$Import_mtf_mde1=="not required"){
                  meta_metadata[,"mde1"]<-NULL
                }
                if(input$Import_mtf_mde2=="not required"){
                  meta_metadata[,"mde2"]<-NULL
                }
                if(input$Import_mtf_mde3=="not required"){
                  meta_metadata[,"mde3"]<-NULL
                }
                if(input$Import_mtf_mde4=="not required"){
                  meta_metadata[,"mde4"]<-NULL
                }
                if(input$Import_mtf_mde5=="not required"){
                  meta_metadata[,"mde5"]<-NULL
                }
                if(input$Import_mtf_mde6=="not required"){
                  meta_metadata[,"mde6"]<-NULL
                }
                if(input$Import_mtf_mde7=="not required"){
                  meta_metadata[,"mde7"]<-NULL
                }
                if(input$Import_mtf_mde8=="not required"){
                  meta_metadata[,"mde8"]<-NULL
                }
                if(input$Import_mtf_mde9=="not required"){
                  meta_metadata[,"mde9"]<-NULL
                }
                #save needed parameters
                parameters<-list(data,db=TRUE,lang=data[1,"language"],input$Import_mtf_date_format,meta_metadata)
                #create process ID
                ID<-get_task_id_counter()+1
                set_task_id_counter(ID)
                #save metadata for process
                process_info<-list(ID,paste("New Data - ",input$Import_mtf_dataset,sep=""),"Create import csv files and write to DB and solr",as.character(Sys.time()))
                #save logfile path
                logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
                #create logfile
                write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
                #save data needed in script execution 
                save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
                #start script
                system(paste('Rscript collections/scripts/Import_Script.R','&'))
                #show modal when process is started
                shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success",closeOnEsc = T)
              }
            }
          }
        }
      }
    }
  }
})

# if confirm to continue with empty body is clicked run import script anyway
observeEvent(ignoreNULL = T,input$confirm_empty_body_mtf_db,{
  if(input$confirm_empty_body_mtf_db){
    data<-values$Import_mtf_meta_complete
    #create meta metadata vector
    meta_metadata<-data.frame(t(c(input$Import_mtf_dataset,input$UI_Import_name_mde1_mtf,input$UI_Import_name_mde2_mtf,input$UI_Import_name_mde3_mtf,input$UI_Import_name_mde4_mtf,input$UI_Import_name_mde5_mtf,
                                  input$UI_Import_name_mde6_mtf,input$UI_Import_name_mde7_mtf,input$UI_Import_name_mde8_mtf,input$UI_Import_name_mde9_mtf)))
    colnames(meta_metadata)<-c("dataset","mde1","mde2","mde3","mde4","mde5","mde6","mde7","mde8","mde9")
    if(input$Import_mtf_mde1=="not required"){
      meta_metadata[,"mde1"]<-NULL
    }
    if(input$Import_mtf_mde2=="not required"){
      meta_metadata[,"mde2"]<-NULL
    }
    if(input$Import_mtf_mde3=="not required"){
      meta_metadata[,"mde3"]<-NULL
    }
    if(input$Import_mtf_mde4=="not required"){
      meta_metadata[,"mde4"]<-NULL
    }
    if(input$Import_mtf_mde5=="not required"){
      meta_metadata[,"mde5"]<-NULL
    }
    if(input$Import_mtf_mde6=="not required"){
      meta_metadata[,"mde6"]<-NULL
    }
    if(input$Import_mtf_mde7=="not required"){
      meta_metadata[,"mde7"]<-NULL
    }
    if(input$Import_mtf_mde8=="not required"){
      meta_metadata[,"mde8"]<-NULL
    }
    if(input$Import_mtf_mde9=="not required"){
      meta_metadata[,"mde9"]<-NULL
    }
    #save needed parameters
    parameters<-list(data,db=TRUE,lang=data[1,"language"],input$Import_mtf_date_format,meta_metadata)
    #create process ID
    ID<-get_task_id_counter()+1
    set_task_id_counter(ID)
    #save metadata for process
    process_info<-list(ID,paste("New Data - ",input$Import_mtf_dataset,sep=""),"Create import csv files and write to DB and solr",as.character(Sys.time()))
    #save logfile path
    logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
    #create logfile
    write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
    #save data needed in script execution 
    save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
    #start script
    system(paste('Rscript collections/scripts/Import_Script.R','&'))
    #show modal when process is started
    shinyWidgets::sendSweetAlert(session=session,title = "Started Import Script",type = "success",closeOnEsc = T)
  }
})











autoInvalidate_normal <- reactiveTimer(500)
observe({
  autoInvalidate_normal()
  values$import_files_changed<-length(list.files("data_import/processed_data/"))
})


########################################
#            DB & SOLR                 #
########################################

output$Import_Files_UI<-renderUI({
  values$import_files_changed
  return(
    shinyWidgets::prettyRadioButtons(
      inputId = "Import_Files",
      label = "available data for upload",
      choices = unique(
        stringr::str_replace(
          string = stringr::str_replace_all(
            string = list.files("data_import/processed_data/"),
            pattern = ".csv",
            replacement = ""
          ),
          pattern = "[a-zA-Z]+\\_",
          replacement = ""
        )
      ),
      fill = F,
      animation = "tada",
      selected = character(0),
      shape="curve",
      inline=T,outline = T,plain = T,bigger = T
    )
  )
})




observeEvent(input$Upload_Data,{
  #check if already imported
  withBusyIndicatorServer("Upload_Data", {
    if(is.null(input$Import_Files)){
      shinyWidgets::sendSweetAlert(session=session,title = "no import file specified",text = "please specify a file you want to import!",type = "warning")
    }
    else{
      meta_metadata<-readr::read_csv(file=paste0("data_import/processed_data/metameta_",input$Import_Files,".csv"))
      mydb <- RMariaDB::dbConnect(RMariaDB::MariaDB(), user='root', password='ilcm', dbname='ilcm', host=values$host,port=isolate(values$db_port))
      a<-readr::read_csv(file = paste0("data_import/processed_data/meta_",input$Import_Files,".csv"),col_names = FALSE)[1,c(1,2)]
      b<-RMariaDB::dbGetQuery(mydb,paste0("Select title from documents where id_doc=",a[1,2]," and dataset='",a[1,1],"' limit 1;"))
      if(dim(b)[1]!=0){
        shinyWidgets::sendSweetAlert(type = "warning",session = session,title =  "Data seems to be uploaded already")
      }
      else{
        if(dim(meta_metadata)[2]==1){
          query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/meta_",input$Import_Files,".csv","' INTO TABLE ilcm.documents  CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' 
                    (dataset,id_doc,title,body,date,token,language",",entities) ;")
          rs<- dbSendQuery(mydb, query)
        }
        else{
          query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/meta_",input$Import_Files,".csv","' INTO TABLE ilcm.documents CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' 
                    (dataset,id_doc,title,body,date,token,language,",paste(colnames(meta_metadata)[2:dim(meta_metadata)[2]],collapse=","),",entities) ;")
          rs<- dbSendQuery(mydb, query)
        }
        query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/token_",input$Import_Files,".csv","' INTO TABLE ilcm.token CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","';")
        rs<- RMariaDB::dbSendStatement(mydb, query)
        try({
          if(dim(meta_metadata)[2]==1){
            query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/metameta_",input$Import_Files,".csv","' INTO TABLE ilcm.metadata_names CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' IGNORE 1 LINES (dataset",");")
            rs<- dbSendQuery(mydb, query)
          }
          else{
            query<-paste0("LOAD DATA LOCAL INFILE '","data_import/processed_data/metameta_",input$Import_Files,".csv","' INTO TABLE ilcm.metadata_names CHARACTER SET utf8mb4 FIELDS TERMINATED BY ',' ENCLOSED BY '\"' LINES TERMINATED BY '","\n","' IGNORE 1 LINES (dataset,",paste(colnames(meta_metadata)[2:dim(meta_metadata)[2]],collapse=","),");")
            rs<- dbSendQuery(mydb, query)
          }
        })
        
        
        #update meta tables in database
        data<-data.frame(readtext::readtext(file =paste0("data_import/processed_data/meta_",input$Import_Files,".csv") ),stringsAsFactors = F)
        #remove entities table from data
        data<-data[,1:(ncol(data)-1)]
        #date
        dates<-unique(data[,6])
        dates<-cbind(rep(data[1,2],length(dates)),dates)
        rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_date (dataset, date) values ",paste(sprintf("('%s', '%s')", dates[,1], dates[,2]), collapse=', ') ,";"))
        #token
        token<-unique(data[,7])
        token<-cbind(rep(data[1,2],length(token)),token)
        rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_token (dataset, token) values ",paste(sprintf("('%s', %s)", token[,1], token[,2]), collapse=', ') ,";"))
        #mde1 
        
        try({
          mde1<-unique(data[,9])
          mde1<-cbind(rep(data[1,2],length(mde1)),mde1)
          #check if only NA
          if(any(!is.na(mde1[,2]))){
            mde1<-mde1[which(!is.na(mde1[,2])),]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde1 (dataset, mde1) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde1[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde1[,2])), collapse=', ') ,";"))
          }
        })
        #mde2
        try({
          mde2<-unique(data[,10])
          mde2<-cbind(rep(data[1,2],length(mde2)),mde2)
          #check if only NA
          if(any(!is.na(mde2[,2]))){
            mde2<-mde2[which(!is.na(mde2[,2])),]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde2 (dataset, mde2) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde2[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde2[,2])), collapse=', ') ,";"))
          }
        })
        #mde3
        try({
          mde3<-unique(data[,11])
          mde3<-cbind(rep(data[1,2],length(mde3)),mde3)
          #check if only NA
          if(any(!is.na(mde3[,2]))){
            mde3<-mde3[which(!is.na(mde3[,2])),]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde3 (dataset, mde3) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde3[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde3[,2])), collapse=', ') ,";"))
          }
        })
        #mde4
        try({
          mde4<-unique(data[,12])
          mde4<-cbind(rep(data[1,2],length(mde4)),mde4)
          #check if only NA
          if(any(!is.na(mde4[,2]))){
            mde4<-mde4[which(!is.na(mde4[,2])),]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde4 (dataset, mde4) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde4[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde4[,2])), collapse=', ') ,";"))
          }
        })
        #mde5
        try({
          mde5<-unique(data[,13])
          mde5<-cbind(rep(data[1,2],length(mde5)),mde5)
          #check if only NA
          if(any(!is.na(mde5[,2]))){
            mde5<-mde5[which(!is.na(mde5[,2])),]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde5 (dataset, mde5) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde5[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde5[,2])), collapse=', ') ,";"))
          }
        })
        #mde6
        try({
          mde6<-unique(data[,14])
          mde6<-cbind(rep(data[1,2],length(mde6)),mde6)
          #check if only NA
          if(any(!is.na(mde6[,2]))){
            mde6<-mde6[which(!is.na(mde6[,2])),]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde6 (dataset, mde6) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde6[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde6[,2])), collapse=', ') ,";"))
          }
        })
        #mde7
        try({
          mde7<-unique(data[,15])
          mde7<-cbind(rep(data[1,2],length(mde7)),mde7)
          #check if only NA
          if(any(!is.na(mde7[,2]))){
            mde7<-mde7[which(!is.na(mde7[,2])),]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde7 (dataset, mde7) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde7[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde7[,2])), collapse=', ') ,";"))
          }
        })
        #mde8
        try({
          mde8<-unique(data[,16])
          mde8<-cbind(rep(data[1,2],length(mde8)),mde8)
          #check if only NA
          if(any(!is.na(mde8[,2]))){
            mde8<-mde8[which(!is.na(mde8[,2])),]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde8 (dataset, mde8) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde8[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde8[,2])), collapse=', ') ,";"))
          }
        })
        #mde9
        try({
          mde9<-unique(data[,17])
          mde9<-cbind(rep(data[1,2],length(mde9)),mde9)
          #check if only NA
          if(any(!is.na(mde9[,2]))){
            mde9<-mde9[which(!is.na(mde9[,2])),]
            rs<-dbSendStatement(mydb, paste0("Insert Ignore into ilcm.meta_mde9 (dataset, mde9) values ",paste(sprintf("(%s, %s)", RMariaDB::dbQuoteString(conn = mydb,x = mde9[,1]),RMariaDB::dbQuoteString(conn = mydb,x= mde9[,2])), collapse=', ') ,";"))
          }
        })
        rs <- RMariaDB::dbSendStatement(mydb, 'set character set "utf8"')
        
        shinyWidgets::sendSweetAlert(type = "success",session = session,title =  "successfully imported data to database")
        values$update_datasets_avaiable<-runif(1,0,1)
      }
      RMariaDB::dbDisconnect(mydb)
    }
  })
})

observeEvent(input$Import_to_solr,{
  #import data to solr
  withBusyIndicatorServer("Import_to_solr", {
    url<-stringr::str_replace(string = values$solr_url,pattern = "select/",replacement = "")
    z<-RCurl::getURL(
      paste0(url,"dataimport?command=delta-import"),followlocation=TRUE
    )
    #initiate suggest
    z<-RCurl::getURL(
      paste0(url,"suggest?suggest.build=true"),followlocation=TRUE
    )
    shinyWidgets::sendSweetAlert(type = "success",session = session,title =  "successfully started solr delta import and solr suggest")
  })
})


observeEvent(input$Import_delete,{
  shinyWidgets::confirmSweetAlert(session = session,inputId = "confirm_delete_import",type = "warning",title = "Are you sure you want to delete the selected input files",danger_mode = T)
})

observeEvent(input$confirm_delete_import,{
  if(isTRUE(input$confirm_delete_import)){
    file.remove(list.files(path = "data_import/processed_data/",pattern = input$Import_Files, full.names = T))
  }
}
)
