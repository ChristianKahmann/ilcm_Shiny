#' render table with finished results for co-occurrence analysis
#' depends on:
#'   values$reload_coocs_result: reload result list after element was deleted
#'   values$collection_selected: selected collection for cooccurrence analysis
#'   values$Coocs_Results_Files: result files from cooccurrence analysis
#'   values$results_coocs: results from cooccurrence analysis
output$Coocs_Results <- DT::renderDataTable({
  #reload table if a result was deleted
  values$reload_coocs_result
  isolate(values$reload_coocs_result<-FALSE)
  files <- list.files("collections/results/cooccurrence-analysis/")
  validate(
    need(length(files)>0,"no results found")
  )
  files_for_date <-
    list.files("collections/results/cooccurrence-analysis/", full.names = T)
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
  values$Coocs_Results_Files<-files
  #get parameter settings for the tasks from database
  colnames(data_finished)[1:3]<-c("task id","collection","creation time")
  data_finished<-data.frame(data_finished)
  values$tasks_coocs<-data_finished

  available_parameters<-intersect(c("task.id","collection","creation.time","baseform_reduction","remove_stopwords","min_cooc_freq","cooc_window","termfreq_type","min_term","max_term","blacklist","whitelist"),colnames(data_finished))
  data_finished<-data_finished[,available_parameters]
  
  # try({parameters<-get_parameters_from_database(data_finished)})
  # if(dim(parameters)[1]>0){
  #   colnames(parameters)[1]<-"task id"
  #   data_finished<-plyr::join(x = data.frame(data_finished),y = data.frame(parameters),type="left")
  #   colnames(data_finished)[1]<-"task id"
  #   values$tasks_coocs<-data_finished
  #   data_finished<-data_finished[,c("task id","collection","creation.time","baseform","stopwords","min_cooc_freq","coocs_level","Pruning.Term","min.term","max.term","blacklist","whitelist")]
  # }
  # else{
  #   values$tasks_coocs<-data_finished
  # }
  data_finished<-cbind(data_finished,Delete = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'delete_button_coocs_results_',
    label = "",
    size="small",
    style="danger",
    icon=icon("trash"),
    onclick = 'Shiny.onInputChange(\"delete_coocs_results\",  this.id)'
  ))
  data_finished<-cbind(data_finished,More_Details = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'more_details_button_coocs_results_',
    label = "",
    size="small",
    style="primary",
    icon=icon("info"),
    onclick = 'Shiny.onInputChange(\"more_details_coocs_results\",  this.id)'
  ))
  
  try({
    colnames(data_finished)<-c("Task id","Collection","Creation time","Baseform reduction",
                               "Remove Stopwords","Minimal co-occurrence frequency","co-occurrence window","Pruning term","min term","max term","Blacklist","Whitelist","Delete","More details")
  })
  colnames(data_finished) = str_wrap(colnames(data_finished),width = 8)
  data_finished<-replace_TRUE_FALSE(data_finished)
  values$results_coocs<-data_finished
  DT = DT::datatable(data_finished,
                     selection = "single",
                     options = list(dom = 'tp',ordering=F,
                                    columnDefs=list(list(className="no_select",targets=((dim(data_finished)[2]-1):(dim(data_finished)[2]-2)))))
                     ,rownames = F,class = "row-border compact",escape = F,
                     callback = JS('table.on("click", "td.no_select", function(e) {
                                console.log("in der funktion drin");
                                e.stopPropagation()
                                });')
  )
})

#' check wheather a certain rresult was clicked and then switch with needed information to details tab
#' depends on:
#'   input$Coocs_Results_rows_selected: selected rows from result list
#'   values$Details_Analysis: details of cooccurrence analysis 
#'   values$Details_Data_CO: details of cooccurrence analysis data
#'   values$Coocs_Results_Files: result files for cooccurrence analysis
observe({
  s = input$Coocs_Results_rows_selected
  if (length(s)) {
    values$Details_Analysis <- "CO"
    isolate(values$parameters_finished <- FALSE)
    isolate(values$Details_Data_CO <-
              values$Coocs_Results_Files[s])
    updateTabsetPanel(session = session,
                      inputId = "coll",
                      selected = "Details")
    return(NULL)
  }
})


#' if delete co-occurrence analysis result is clicked, delete files and db entry
#' depends on:  
#'   input$delete_coocs_results: deleted results from result list
#'   values$Coocs_Results_Files: cooccurrence analysis result files
#'   values$reload_coocs_result: reload cooccurrence analysis result list after element was deleted
observeEvent(input$delete_coocs_results, {
  selectedRow <-
    as.numeric(strsplit(input$delete_coocs_results, "_")[[1]][5])
  if(selectedRow>0){
    unlink(values$Coocs_Results_Files[selectedRow],recursive = T)
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"delete_coocs_results\",  "delete_button_coocs_results_0")'))
    values$reload_coocs_result<-TRUE
  }
})


#' if more details button is clicked open modal showing all parameters
#' depends on:
#'   input$more_details_coocs_results: details on cooccurrence analysis results
#'   values$coocs_selected_row: selected rows from cooccurrence result list
observeEvent(input$more_details_coocs_results,{
  selectedRow <-
    as.numeric(strsplit(input$more_details_coocs_results, "_")[[1]][6])
  if(selectedRow>0){
    values$coocs_selected_row<-selectedRow
    isolate(shinyjs::runjs('Shiny.onInputChange(\"more_details_coocs_results\",  "more_details_button_coocs_results_0")'))
    showModal(
      modalDialog(easyClose = T,fade = T,
                  title=paste("Parameter information for task:",as.character(values$tasks_coocs[selectedRow,"task id"])),
                  DT::dataTableOutput(outputId = "more_details_coocs_table")
      )
    )
  }
})

#' if more details button is clicked open modal showing all parameters
#' depends on:  
#'   values$coocs_selected_row: selected rows from cooccurrence result list
#'   values$tasks_coocs: cooccurrence tasks
output$more_details_coocs_table<-DT::renderDataTable({
  validate(
    need(values$coocs_selected_row>0,message=F)
  )
  data<-isolate(values$tasks_coocs[values$coocs_selected_row,,drop=F])
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