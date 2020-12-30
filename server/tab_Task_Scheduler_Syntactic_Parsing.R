
#' render parameters for syntactic parsing
output$Analysis_Parameter_SP<-renderUI({
  tagList(
    #specific parameters
    tags$hr(),
    tags$h4("Syntactic parameters"),
    fluidRow(
      column(2,
             numericInput(inputId = "SP_cores",label = "number of cores to use for parsing",value = 1,min = 1,max=(parallel:::detectCores()-1))
             )
     ),
    bsButton(inputId = "SP_Submit_Script",label = "Submit Request",icon = icon("play-circle"),type = "primary")
    )
})




#' start Syntactic Parsing script, if submit button is clicked
#' depends on:
#'   input$SP_Submit_Script:  submit the syntactic parsing script
#'   input$collection_selected: selected collection
#'   input$SP_cores: number of cores to use for parsing
#'   input$analysis_selected: selected analysis
#'   input$use_custom_script: should a custome script be used
#'   input$custom_script_options: show possible custom script options
observeEvent(input$SP_Submit_Script,{
  #save needed parameters
  parameters<-list(
    collection=input$collection_selected,
    cores=input$SP_cores
  )
  #create process ID
  ID<-get_task_id_counter()+1
  set_task_id_counter(ID)
  
  #save metadata for process
  process_info<-list(ID,isolate(input$collection_selected),isolate(input$analysis_selected),as.character(Sys.time()))
  #save logfile path
  logfile<-paste("collections/logs/running/",process_info[[1]],".txt",sep="")
  #create logfile
  write(paste(paste0("Task ID: <b>",process_info[[1]],"</b>"), paste0("Collection: <b> ",process_info[[2]],"</b>"),paste0("Task: <b>",process_info[[3]],"</b>"),paste0("Started at: <b>",process_info[[4]],"</b>"),"","",sep = "\n"),file = logfile)
  #save data needed in script execution 
  save(process_info,logfile,parameters,file="collections/tmp/tmp.RData")
  #start script
  if(input$use_custom_script==TRUE && !is.null(input$custom_script_options)){
    shinyWidgets::sendSweetAlert(session=session,title = "Starting a custom script",text = "You are about to start a custom script. Caution with the calculation and results!",type = "info")
    system(paste0('Rscript collections/scripts/Syntactic_Parsing/',input$custom_script_options,' &'))
  }
  else{
    system(paste('Rscript collections/scripts/Syntactic_Parsing_Script.R','&'))
    #show modal when process is started
    showModal(modalDialog(
      title = "Process started",
      "The process was succesfully started. Check details in 'My Tasks'."
    ))
  }
})
