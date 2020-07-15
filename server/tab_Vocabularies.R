output$UI_files_vocabulary<-renderUI({
  values$invalidate_vocabularies
  validate(
    need(length(list.files("collections/vocabularies/"))>0,message="no lists found")
  )
  shinyWidgets::prettyRadioButtons(inputId = "files_vocabulary",label = "available vocabularies",
                                   choices = stringr::str_replace_all(string = list.files("collections/vocabularies/"),pattern = ".RDS",replacement = ""),
                                   fill=T,animation = "tada",selected = NULL)
})

observeEvent(input$new_vocabulary_list,{
  values$vocabulary_text<-""
  updateTextAreaInput(session = session,inputId = "vocabulary_textarea",value = "")
})

observeEvent(input$change_vocabulary_list,{
  if(is.null(input$files_vocabulary)){
    shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "No list to update")
  }
  else{
    values$vocabulary_text<-paste0(readRDS(paste0("collections/vocabularies/",input$files_vocabulary,".RDS")),collapse=",")
    #values$vocabulary_text<-readChar(con=paste0("collections/vocabularies/",input$files_vocabulary,".txt"),nchars = file.info(paste0("collections/vocabularies/",input$files_vocabulary,".txt"))$size)
  }
})


# show modal if vocab should be imported from tasks
observeEvent(input$vocabulary_import_vocab_from_task,{
  
  showModal(
    modalDialog(title = "Import vocabulary from task",size = "l",
                radioGroupButtons(inputId = "vocabulary_import_type_of_analysis",label = "Type of analysis",choices = c("Topic Model"="topic-model","Cooccurrence Analysis"="cooccurrence-analysis",
                                                                                                                        "Frequency Analysis"="frequency-extraction","Volatility Analysis"="volatility-analysis",
                                                                                                                        "Classification"="classification"),
                                  status="primary",individual = T ),
                conditionalPanel(condition = 'input.vocabulary_import_type_of_analysis=="classification"',
                                 fluidRow(style="margin-left:0px;margin-right:0px",
                                   column(3,
                                          selectInput(inputId="vocabulary_import_classification_type",label="Type of classification",choices=c("Active Learning"="activeLearning","Active Learning whole Documents"="activeLearning_documents",
                                                                                                                                               "Classification on whole collection"="classifyCollection",
                                                                                                                                               "Evaluation of Training Set"="evaluateTraining"))
                                   ),
                                   column(3,
                                          conditionalPanel(condition = 'input.vocabulary_import_classification_type==activeLearning || input.vocabulary_import_classification_type==activeLearning_documents',
                                                           uiOutput(outputId = "vocabulary_import_classification_active_learning_scheme_UI")
                                          )
                                   )
                                 )
                ),
                tags$hr(),
                uiOutput(outputId = "vocabulary_import_found_tasks_UI"),
                tags$hr(),
                uiOutput("vocabulary_import_start_import_UI")
                
                
                
    )
  )  
})

# move vocabulary from analysis to preset vocabulary
 observeEvent(input$vocabulary_import_start_import,{

   file.copy(from = list.files(input$vocabulary_import_found_tasks,pattern = "vocab_[a-z0-9]+\\.RDS$",full.names = T)[1],to = "collections/vocabularies/",overwrite = T)
   
   # update input options in Task Scheduler
   updateSelectInput(session = session,inputId = "TM_fixed_vocab",choices = list.files("collections/vocabularies/"), selected=input$TM_fixed_vocab)
   updateSelectInput(session = session,inputId = "CL_fixed_vocab",choices = list.files("collections/vocabularies/"), selected=input$CL_fixed_vocab)
   updateSelectInput(session = session,inputId = "FE_fixed_vocab",choices = list.files("collections/vocabularies/"), selected=input$FE_fixed_vocab)
   updateSelectInput(session = session,inputId = "CA_fixed_vocab",choices = list.files("collections/vocabularies/"), selected=input$CA_fixed_vocab)
   updateSelectInput(session = session,inputId = "VA_fixed_vocab",choices = list.files("collections/vocabularies/"), selected=input$VA_fixed_vocab)
   values$invalidate_vocabularies<-runif(1,0,1)

   shinyWidgets::sendSweetAlert(session = session,title = "Success",type = "success",text = "The vocabulary has been imported to the vocabulary preset directory. It can now be chosen in the Task Scheduler.")
   
 })



# try to locate vocab file and show buttons to start import if vocabulary found
output$vocabulary_import_start_import_UI<-renderUI({
  validate(
    need(!is.null(input$vocabulary_import_found_tasks),message=F)
  )
  
  task<-input$vocabulary_import_found_tasks
  validate(
    need(length(list.files(task,pattern = "vocab_[a-z0-9]+\\.RDS$"))>0,message="No vocabulary found for this task")
  )
  
  vocab<-readRDS(list.files(task,pattern = "vocab_[a-z0-9]+\\.RDS$",full.names = T)[1])
  
  tagList(
    tags$div(HTML(paste0("This vocabulary contains <b>", length(vocab),"</b> entries."))),
    bsButton(inputId = "vocabulary_import_start_import",label = "Start Import",icon = icon("upload"),style = "success")
  )
  
})


# show active learning schema if active learning is chosen
output$vocabulary_import_classification_active_learning_scheme_UI<-renderUI({
  selectInput(inputId="vocabulary_import_classification_active_learning_scheme",label="annotation Scheme",choices=list.files(path = paste0("collections/results/classification/",input$vocabulary_import_classification_type,"/")))
  
})


# show available tasks depending on chosen analysis
output$vocabulary_import_found_tasks_UI<-renderUI({
  validate(
    need(!is.null(input$vocabulary_import_type_of_analysis),message=F)
  )
  if(input$vocabulary_import_type_of_analysis=="classification"){
    validate(
      need(length(list.files(path = paste0("collections/results/",input$vocabulary_import_type_of_analysis,"/",input$vocabulary_import_classification_type,"/"),full.names = F))>0,message="No tasks found")
    )
    if(input$vocabulary_import_classification_type%in%c("activeLearning","activeLearning_documents")){
      choices=set_names(x = list.files(path = paste0("collections/results/",input$vocabulary_import_type_of_analysis,"/",input$vocabulary_import_classification_type,"/",input$vocabulary_import_classification_active_learning_scheme,"/"),full.names = T),
                        nm = paste0("Task ID: ",stringr::str_split(list.files(path = paste0("collections/results/",input$vocabulary_import_type_of_analysis,"/",input$vocabulary_import_classification_type,"/",input$vocabulary_import_classification_active_learning_scheme,"/"),full.names = F),pattern = "_",simplify = T)[,1]))
      
    }
    else{
      choices=set_names(x = list.files(path = paste0("collections/results/",input$vocabulary_import_type_of_analysis,"/",input$vocabulary_import_classification_type,"/"),full.names = T),
                        nm = paste0("Task ID: ",stringr::str_split(list.files(path = paste0("collections/results/",input$vocabulary_import_type_of_analysis,"/",input$vocabulary_import_classification_type,"/"),full.names = F),pattern = "_",simplify = T)[,1]))
    }
  }
  else{
    validate(
      need(length(list.files(path = paste0("collections/results/",input$vocabulary_import_type_of_analysis,"/"),full.names = F))>0,message="No tasks found")
    )
    choices=set_names(x = list.files(path = paste0("collections/results/",input$vocabulary_import_type_of_analysis,"/"),full.names = T),
                      nm = paste0("Task ID: ",stringr::str_split(list.files(path = paste0("collections/results/",input$vocabulary_import_type_of_analysis,"/"),full.names = F),pattern = "_",simplify = T)[,1]))
    
    
  }
  tasks<-tagList(
    selectInput(inputId="vocabulary_import_found_tasks",label="Found Tasks",choices=choices,width="30%")
  )
  return(tasks)
})




output$UI_vocabulary_textarea<-renderUI({
  if(is.null(values$vocabulary_text)){
    values$vocabulary_text<-""
  }
  textAreaInput(inputId = "vocabulary_textarea",label = "specify terms comma-seperated",cols = 300,rows=10,value = values$vocabulary_text)
})


observeEvent(input$save_vocabulary_list,{ 
  showModal(
    modalDialog(
      title = "Save vocabularies",
      size="l",
      textInput(
        inputId = "Save_vocabularie_name",
        label = "Specify a name"
      ),
      shinyBS::bsButton(inputId = "Save_vocabulary_confirm",label = "Save",icon = icon("save"))
    )
  ) 
}
)

observeEvent(input$Save_vocabulary_confirm,{
  shiny::removeModal()
  path<-paste0("collections/vocabularies/",input$Save_vocabularie_name,".RDS")
  #writeLines(input$vocabulary_textarea,con=path)
  vocab<-stringr::str_split(string =input$vocabulary_textarea,pattern = ",",simplify = T )[1,]
  saveRDS(object = vocab,file = path)
  # update input options in Task Scheduler
  updateSelectInput(session = session,inputId = "TM_fixed_vocab",choices = list.files("collections/vocabularies/"), selected=input$TM_fixed_vocab)
  updateSelectInput(session = session,inputId = "CL_fixed_vocab",choices = list.files("collections/vocabularies/"), selected=input$CL_fixed_vocab)
  updateSelectInput(session = session,inputId = "FE_fixed_vocab",choices = list.files("collections/vocabularies/"), selected=input$FE_fixed_vocab)
  updateSelectInput(session = session,inputId = "CA_fixed_vocab",choices = list.files("collections/vocabularies/"), selected=input$CA_fixed_vocab)
  updateSelectInput(session = session,inputId = "VA_fixed_vocab",choices = list.files("collections/vocabularies/"), selected=input$VA_fixed_vocab)
  values$invalidate_vocabularies<-runif(1,0,1)
  shinyWidgets::sendSweetAlert(session = session,title = "Success",text = "successfully saved vocabulary",type = "sucess")

})
