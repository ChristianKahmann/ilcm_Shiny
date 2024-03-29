


#' render table with finished results for classification task
#' depends on:
#'   values$reload_classification_result: reload classification result list after element deleted 
#'   values$collection_selected: selected collection for classification task
#'   values$reload_classification_result: reload results from classification task list after on element was deleted
#'   values$collection_selected: selected collection for classification task
#'   values$Classification_Results_Files: result files for classification
#'   values$tasks_class: classification task
output$Classification_Results <- renderDataTable({
  #reload table if a result was deleted
  values$reload_classification_result
  isolate(values$reload_classification_result<-FALSE)
  files <- list.files("collections/results/classification/classifyCollection")
  files <- c(files,list.files("collections/results/classification/evaluateTraining"))
  dirs_activeLearning<-list.files("collections/results/classification/activeLearning")
  for(dir in dirs_activeLearning){
    files <- c(files,list.files(paste0("collections/results/classification/activeLearning/",dir)))
  }
  dirs_activeLearning_whole_Documents<-list.files("collections/results/classification/activeLearning_documents")
  for(dir in dirs_activeLearning_whole_Documents){
    files <- c(files,list.files(paste0("collections/results/classification/activeLearning_documents//",dir)))
  }
  
  validate(
    need(length(files)>0,"no results found")
  )
  files_for_date <-
    list.files("collections/results/classification/classifyCollection", full.names = T)
  files_for_date <- c(files_for_date,list.files("collections/results/classification/evaluateTraining",full.names = T))
  dirs_activeLearning<-list.files("collections/results/classification/activeLearning")
  for(dir in dirs_activeLearning){
    files_for_date <- c(files_for_date,list.files(paste0("collections/results/classification/activeLearning/",dir),full.names = T))
  }
  dirs_activeLearning_whole_Documents<-list.files("collections/results/classification/activeLearning_documents")
  for(dir in dirs_activeLearning_whole_Documents){
    files_for_date <- c(files_for_date,list.files(paste0("collections/results/classification/activeLearning_documents//",dir),full.names = T))
  }
  
  #get parameters
  parameters_all_tasks<-list()
  for(i in 1:length(files_for_date)){
    parameters<-list()
    try({
      load(paste0(files_for_date[i],"/parameters.RData"))
    },silent = T)
    parameters_all_tasks[[i]]<-parameters
  }
  
  parameters_all_tasks<-lapply(parameters_all_tasks,FUN = function(x){
    lapply(x,FUN = function(y){
      paste(as.character(y),collapse=" ")
    })
  })
  
  parameters_all_tasks<-data.table::rbindlist(lapply(parameters_all_tasks,FUN = function(x){
    parameter_values<-unlist(x)
    if(length(parameter_values)>0){
      parameters<-data.frame(t(parameter_values))
    }
    else{
      parameters<-data.frame(collection="")
    }
    return(parameters)
  }),
  fill=T)
  
  data_finished <- matrix(c(0), 0, 3)
  if (length(files) > 0) {
    for (i in 1:length(files)) {
      data_finished <-
        rbind(data_finished, c(
          stringr::str_split(
            string = files[i],
            pattern = "_",
            simplify = TRUE
          )[1, 1:2],
          as.character(file.info(files_for_date[i])$mtime)
        ))
    }
  }
  
  data_finished<-cbind(data_finished,parameters_all_tasks[,-1])
  
  files<-files_for_date
  #just show the results for current selection if a collection is selected
  if(!is.null(values$collection_selected)){
    files<-files_for_date[which(data_finished[,2]==values$collection_selected)]
    data_finished<-data_finished[which(data_finished[,2]==values$collection_selected),,drop=F]
  }
  validate(
    need(length(files)>0,"no results for this collection")
  )
  files<-files[order(data_finished[,3],decreasing = T)]
  files_for_date<-files_for_date[order(data_finished[,3],decreasing = T)]
  data_finished<-data_finished[order(data_finished[,3],decreasing=T),,drop=F]
  values$Classification_Results_Files<-files
  
  #select parameters to show
  colnames(data_finished)[1:3]<-c("task id","collection","creation time")
  data_finished<-data.frame(data_finished)
  values$tasks_class<-data_finished
  
  available_parameters<-intersect(c("task.id","collection","creation.time","Project","cl_Mode","cl_positive_Threshold","cl_c"),colnames(data_finished))
  data_finished<-data_finished[,available_parameters]
  
  #delete buttons
  data_finished<-cbind(data_finished,Delete = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'delete_button_classification_results_',
    label = "",
    size="small",
    style="danger",
    icon=icon("trash"),
    onclick = 'Shiny.onInputChange(\"delete_classification_results\",  this.id)'
  ))
  #more details buttons
  data_finished<-cbind(data_finished,More_Details = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'more_details_button_classification_results_',
    label = "",
    size="small",
    style="primary",
    icon=icon("info"),
    onclick = 'Shiny.onInputChange(\"more_details_classification_results\",  this.id)'
  ))
  #open details window buttons 
  Open = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'open_details_button_classification_results_',
    label = "",
    size="extra-small",
    style="info",
    icon=icon("search"),
    onclick = 'Shiny.onInputChange(\"open_details_classification_results\",  this.id)'
  )
  try({
    colnames(data_finished)<-c("Task id","Collection","Creation time","Project","Mode",
                               "positive Threshold","SVM C","Delete","More details")
  })
  colnames(data_finished) = str_wrap(colnames(data_finished),width = 8)
  data_finished<-replace_TRUE_FALSE(data_finished)
  values$results_classification<-data_finished
  
  data_finished<-cbind(Open,data_finished)
  DT = datatable(data_finished,
                 selection = "none",
                 options = list(dom = 'tp',ordering=F),
                 rownames = F,class = "row-border compact",escape = F
  )
})


#' check wheather a certain result was clicked and then switch with needed information to classification tab
#' depends on:
#'   input$Classification_Results_rows_selected: selected rows from classification result list
#'   values$Details_Analysis: details of classification task
#'   values$Details_Data_CL: details of classification data
#'   values$Classification_Results_Files: classification result files
#'   values$Details_CL_mode: details on selected classification mode
#'   values$classification_project: selected classification task project
#'   values$tasks_class: classification tasks
#'   values$Det_CL_results_complete: complete results of detailed classification
#'   values$Class_timeseries_data: data from timesiries of classification
observeEvent(input$open_details_classification_results, {
  s <- as.numeric(strsplit(input$open_details_classification_results, "_")[[1]][6])
  if (length(s)) {
    if(s>0){
      values$Details_Analysis <- "CL"
      isolate(values$parameters_finished <- FALSE)
      isolate(values$Details_Data_CL <-
                values$Classification_Results_Files[s])
      isolate(shinyjs::runjs('Shiny.onInputChange(\"open_details_classification_results\",  "open_details_button_classification_results_0")'))
      if(grepl(x = values$Classification_Results_Files[s],pattern = "activeLearning")||grepl(x = values$Classification_Results_Files[s],pattern = "activeLearning_documents")){
        values$Details_CL_mode<-"activeLearning"
        # change selcted sidebar tab to categories
        updateTabItems(session=session,
                       inputId="tabs",
                       selected="Categories")
        # change selected navbar tab to Classifications
        updateTabsetPanel(session = session,
                          inputId = "category",
                          selected = "Classifications")
        # update chosen project to used annotation schema in selected task
        values$classification_project<-values$tasks_class[s,"Project"]
        return(NULL)
      }
      else{
        if(grepl(x = values$Classification_Results_Files[s],pattern = "evaluateTraining")){
          values$Det_CL_results_complete<-NULL
          values$Details_CL_mode<-"evaluate"
        }
        else{
          values$Details_CL_mode<-"whole_collection"
          values$Class_timeseries_data<-NULL
        }
        isolate(values$current_task_id<- values$results_classification[s,1])
        updateTabsetPanel(session = session,
                          inputId = "coll",
                          selected = "Details")
        return(NULL)
      }
    }
  }
})








#' if delete classification result is clicked delete files and data base entry
#' depends on:
#'   input$delete_classification_results: delete classification results
#'   values$class_selected_row: selected rows from classification result list
#'   values$Classification_Results_Files: result files from classification task
#'   values$reload_classification_result: reload result list after one element was deleted
observeEvent(input$delete_classification_results, {
  selectedRow <-
    as.numeric(strsplit(input$delete_classification_results, "_")[[1]][5])
  if(selectedRow>0){
    values$class_selected_row<-selectedRow
    unlink(values$Classification_Results_Files[selectedRow],recursive = T)
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"delete_classification_results\",  "delete_button_classification_results_0")'))
    values$reload_classification_result<-TRUE
  }
})

#' if more details button is clicked open modal showing all parameters
#' depends on:
#'   input$more_details_classification_results: details on classification results
#'   values$class_selected_row: selected rows of classification list
observeEvent(input$more_details_classification_results,{
  selectedRow <-
    as.numeric(strsplit(input$more_details_classification_results, "_")[[1]][6])
  if(selectedRow>0){
    values$class_selected_row<-selectedRow
    isolate(shinyjs::runjs('Shiny.onInputChange(\"more_details_classification_results\",  "more_details_button_classification_results_0")'))
    showModal(
      modalDialog(easyClose = T,fade = T,
                  title=paste("Parameter information for task:",as.character(values$tasks_class[selectedRow,"task id"])),
                  DT::dataTableOutput(outputId = "more_details_classification_table")
      )
    )
  }
})

#' show chosen parameters for classification results 
#' depends on:
#'   values$class_selected_row: selected row from classification result list
#'   values$tasks_class: classification tasks
output$more_details_classification_table<-DT::renderDataTable({
  validate(
    need(values$class_selected_row>0,message=F)
  )
  data<-isolate(values$tasks_class[values$class_selected_row,,drop=F])
  data<-t(data)
  nas<-which(is.na(data[,1]))
  if(length(nas)>0){
    data<-data[-which(is.na(data[,1])),1,drop=F]
  }
  colnames(data)<-paste("Task:",data[1,1])
  datatable(data = data,selection = "none",
            options = list(dom = 'tp',ordering=F,pageLength=100)
  )
})
