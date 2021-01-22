#' update logiles every 5 seconds
autoInvalidate_slow <- reactiveTimer(5000)
autoInvalidate_normal <- reactiveTimer(500)
autoInvalidate_medium <- reactiveTimer(1000)
autoInvalidate_fast<-reactiveTimer(50)

#' render logfiles for the chosen category (running, finished, failed)
#' depends on:
#'   input$reload_logs: reload log files
#'   values$reload_logs_auto: reload log files (auto generated)
#'   values$infobox: info box message
#'   output$log_table: log table
output$Running_Tasks<-renderUI({
  #invalidate when reload is pressed or a log file got deleted
  input$reload_logs
  values$reload_logs_auto
  data_tasks<-matrix(c(0),0,4)
  validate(
    need(!is.null(values$infobox),message="click on a box"),
    need(length(list.files(paste("collections/logs/",isolate(values$infobox),sep=""),full.names = T))>0,"no logs")
  )
  print("reload")  
  #get logfiles
  files<-list.files(paste("collections/logs/",isolate(values$infobox),sep=""),full.names = T)
  if(length(files)>=1){
    for(i in 1:length(files)){
      #read logfile metadata
      data_tasks<-rbind(data_tasks,str_split(string=readChar(files[i],file.info(files[i])$size),pattern="\\n",simplify = TRUE)[1,1:4])
    }
  }
  #order tasks decreasing by 'time started'
  files<-files[order(data_tasks[,4],decreasing = T)]
  data_tasks<-matrix(data_tasks[order(data_tasks[,4],decreasing = T),],ncol=4)
  values$log_files<-files
  
  #remove input bindigns for delete buttons
  remove_delete_buttons(1:max(length(files),25))
  #add delete button for logs
  Delete = shinyInput(
    shinyBS::bsButton,
    length(files),
    'logs_button_',
    label = "Delete",
    status="danger",
    onclick = 'Shiny.onInputChange(\"delete_button_logs\",  this.id)'
  )
  #create output datatable object
  colnames(data_tasks)<-c("id","collection","process","time started")
  data_tasks[,1]<-stringr::str_replace_all(string = data_tasks[,1],pattern = "Task ID: ",replacement = "")
  data_tasks[,2]<-stringr::str_replace_all(string = data_tasks[,2],pattern = "Collection: ",replacement = "")
  data_tasks[,3]<-stringr::str_replace_all(string = data_tasks[,3],pattern = "Task: ",replacement = "")
  data_tasks[,4]<-stringr::str_replace_all(string = data_tasks[,4],pattern = "Started at: ",replacement = "")
  
  data_tasks<-data.frame(data_tasks,Delete=Delete)
  data_tasks->values$my_tasks_data_tasks
  dataTableOutput("log_table")
})


output$log_table<-DT::renderDataTable({
  data_tasks<-values$my_tasks_data_tasks
  datatable(data = data_tasks, selection = "single",options = list(dom="tp",pageLength = 5),escape = F)
} ,server = F)



#' render info box for finished processes
output$finished_box<-renderInfoBox({
  #create info box
  box1<-infoBox(title = "finished processes:",
                #number of logfiles
                value = values$my_tasks_finished_file_length,
                icon = icon("tasks"),
                color="green",
                href="#"
  )
  #make box clickable
  box1$children[[1]]$attribs$class<-"action-button"
  box1$children[[1]]$attribs$id<-"button_box_finished"
  return(box1)
})

#' render info box for running processes
output$running_box<-renderInfoBox({
  #create info box
  box2<-infoBox(title = "running processes:",
                #number of logfiles
                value = values$my_tasks_running_file_length,
                icon = icon("spinner"),
                color="orange",
                href="#"
  )
  #make box clickable
  box2$children[[1]]$attribs$class<-"action-button"
  box2$children[[1]]$attribs$id<-"button_box_running"
  return(box2)
})

#' render info box for failed processes
output$failed_box<-renderInfoBox({
  #create info box
  box3<-infoBox(title = "failed processes:",
                #number of logfiles
                value = values$my_tasks_failed_file_length,
                icon = icon("exclamation-triangle"),
                color="red",
                href="#"
  )
  #make box clickable
  box3$children[[1]]$attribs$class<-"action-button"
  box3$children[[1]]$attribs$id<-"button_box_failed"
  return(box3)
})

#'set values$infobox if running_button is clicked
#'depends on:
#'  values$infobox: infobox-text
observeEvent(input$button_box_running,{
  print("running")
  values$infobox<-"running"
})

#'set values$infobox if fnished_button is clicked
#'depends on:
#'  values$infobox: infobox-text
observeEvent(input$button_box_finished,{
  print("finished")
  values$infobox<-"finished"
})

#' set values$infobox if failed_button is clicked
#'depends on:
#'  values$infobox: infobox-text
observeEvent(input$button_box_failed,{
  print("failed")
  values$infobox<-"failed"
})


#' render information in log-file
#' depends on:
#'   input$log_table_rows_selected: selected rows from log table
#'   values$log_files: log files
output$log_text = renderUI({
  #get selected row of log_table
  autoInvalidate_medium()
  s = input$log_table_rows_selected
  if (length(s)){
    #get the corresponding file
    files<-values$log_files
    validate(
      need(!is.na(files),"no log file left"),
      need(file.exists(files[s]),message=FALSE)
    )
    #output the data
    log_text<-(readChar(files[s],file.info(files[s])$size))
    log_text<-stringr::str_split(string = log_text,pattern = "\n",simplify = T)
    log_text<-stringr::str_replace_all(string = log_text,pattern = "\\[1\\]",replacement="")
    log_text<-gsub(' \"', "", log_text, fixed = TRUE)
    return(HTML(paste(log_text, collapse = '<br/>')))
  }
})

#' check wheather delete button for a certain log was pressed, if yes, delete corresponding log file
#' depends on:
#'   input$delete_button_logs: delete log button
#'   values$log_files: log files
observeEvent(input$delete_button_logs, {
  selectedRow <-as.numeric(strsplit(input$delete_button_logs, "_")[[1]][3])
  if(selectedRow>0){
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"delete_button_logs\",  "logs_button_0")'))
    file.remove(values$log_files[selectedRow])
    values$reload_logs_auto<-runif(1,0,1)
  }
})



observe({
  autoInvalidate_slow()
  values$my_tasks_failed_file_length<-length(list.files("collections/logs/failed",full.names = T))
  values$my_tasks_finished_file_length<-length(list.files("collections/logs/finished",full.names = T))
  values$my_tasks_running_file_length<-length(list.files("collections/logs/running",full.names = T))
})

