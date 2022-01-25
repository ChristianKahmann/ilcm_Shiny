#' render table with finished results for vectorility analysis
#' depends:
#'   values$reload_deduplication_result: reload deduplication results after changes
#'   values$collection_selected: selected collection to work with
#'   values$Deduplication_Results_Files: all deduplication result files
#'   values$tasks_deduplication: current deduplication task
#'   values$results_deduplication: results from deduplication tasks
output$Deduplication_Results <- renderDataTable({
  #reload table if a result was deleted
  values$reload_deduplication_result
  isolate(values$reload_deduplication_result<-FALSE)
  files <- list.files("collections/results/document-deduplication//")
  validate(
    need(length(files)>0,"no results found")
  )
  files_for_date <-
    list.files("collections/results/document-deduplication//", full.names = T)
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
  data_finished<-data_finished[order(data_finished[,3],decreasing=T),,drop=F]
  values$Deduplication_Results_Files<-files
  
  #get parameter settings for the tasks from database
  colnames(data_finished)[1:3]<-c("task id","collection","creation time")
  data_finished<-data.frame(data_finished)
  values$tasks_deduplication<-data_finished
  
  
  available_parameters<-intersect(c("task.id","collection","creation.time","DD_similarity_measure"),colnames(data_finished))
  data_finished<-data_finished[,available_parameters]
  
  
  data_finished<-cbind(data_finished,Delete = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'delete_button_deduplication_results_',
    label = "",
    size="small",
    style="danger",
    icon=icon("trash"),
    onclick = 'Shiny.onInputChange(\"delete_deduplication_results\",  this.id)'
  ))
  data_finished<-cbind(data_finished,More_Details = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'more_details_button_deduplication_results_',
    label = "",
    size="small",
    style="primary",
    icon=icon("info"),
    onclick = 'Shiny.onInputChange(\"more_details_deduplication_results\",  this.id)'
  ))
  #open details window buttons 
  Open = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'open_details_button_deduplication_results_',
    label = "",
    size="extra-small",
    style="info",
    icon=icon("search"),
    onclick = 'Shiny.onInputChange(\"open_details_deduplication_results\",  this.id)'
  )
  try({
    colnames(data_finished)<-c("Task id","Collection","Creation time","Similarity measure","Delete","More details")
  })
  colnames(data_finished) = str_wrap(colnames(data_finished),width = 8)
  data_finished<-replace_TRUE_FALSE(data_finished)
  values$results_deduplication<-data_finished
  data_finished<-cbind(Open,data_finished)
  
  DT = datatable(data_finished,
                 selection = "none",
                 options = list(dom = 'tp',ordering=F),
                 rownames = F,class = "row-border compact",escape = F
  )
})


#' check wheather a certain result was clicked and then switch with needed information to details tab
#' depends on:
#'   input$Deduplication_Results_rows_selected: selected rows from deduplication results
#'   values$Details_Analysis: details on analysis
#'   values$Details_Data_DD: details on data deduplication
#'   values$Deduplication_Results_Files: deduplication result files
observeEvent(input$open_details_deduplication_results, {
  s <- as.numeric(strsplit(input$open_details_deduplication_results, "_")[[1]][6])
  if (length(s)) {
    if(s>0){
    values$Details_Analysis <- "DD"
    isolate(values$parameters_finished <- FALSE)
    isolate(values$Details_Data_DD <-
              values$Deduplication_Results_Files[s])
    isolate(shinyjs::runjs('Shiny.onInputChange(\"open_details_deduplication_results\",  "open_details_button_deduplication_results_0")'))
    updateTabsetPanel(session = session,
                      inputId = "coll",
                      selected = "Details")
    return(NULL)
    }
  }
})





#' if delete vectorility analysis result is clicked delete files and db entry
#' depends on:
#'   input$delete_deduplication_results: delete a chosen deduplication result
#'   values$Deduplication_Results_Files: all deduplication result files
#'   values$reload_deduplication_result: relpad deduplication results afte elements were deleted
observeEvent(input$delete_deduplication_results, {
  selectedRow <-
    as.numeric(strsplit(input$delete_deduplication_results, "_")[[1]][5])
  if(selectedRow>0){
    unlink(values$Deduplication_Results_Files[selectedRow],recursive = T)
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"delete_deduplication_results\",  "delete_button_deduplication_results_0")'))
    values$reload_deduplication_result<-TRUE
  }
})


#' if more details button is clicked open modal showing all parameters
#' depends on:
#'   input$more_details_deduplication_results: show more details of selected deduplication results
#'   values$deduplication_selected_row: selected row in deduplication results
observeEvent(input$more_details_deduplication_results,{
  selectedRow <-
    as.numeric(strsplit(input$more_details_deduplication_results, "_")[[1]][6])
  if(selectedRow>0){
    values$deduplication_selected_row<-selectedRow
    isolate(shinyjs::runjs('Shiny.onInputChange(\"more_details_deduplication_results\",  "more_details_button_deduplication_results_0")'))
    showModal(
      modalDialog(easyClose = T,fade = T,
                  title=paste("Parameter information for task:",as.character(values$tasks_deduplication[selectedRow,"task id"])),
                  DT::dataTableOutput(outputId = "more_details_deduplication_table")
      )
    )
  }
})

#' show more details in deduplication table 
#' depends on:
#'   values$deduplication_selected_row: selected row from deduplication results
output$more_details_deduplication_table<-DT::renderDataTable({
  validate(
    need(values$deduplication_selected_row>0,message=F)
  )
  data<-isolate(values$tasks_deduplication[values$deduplication_selected_row,,drop=F])
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