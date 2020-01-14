source("global/functions_used_in_scripts.R")


#render table with finished results for topic models
output$Topic_Results <- renderDataTable({
  #reload table if a result was deleted
  values$reload_topic_result
  isolate(values$reload_topic_result<-FALSE)
 
  files <- list.files("collections/results/topic-model/")
  validate(
    need(length(files)>0,"no results found")
  )
  files_for_date <-
    list.files("collections/results/topic-model/", full.names = T)
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
          )
          # removed: following part uses mtime instead time stamp in file name which is different
          #[1, 1:2],
          #as.character(file.info(files_for_date[i])$mtime)
        ))
    }
  }
  data_finished[,2]<-stringr::str_replace(string = data_finished[,2],pattern = ".RData",replacement = "")
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
  
  values$Topic_Results_Files<-files
  
  #select parameters to show
  colnames(data_finished)[1:3]<-c("task id","collection","creation time")
  data_finished<-data.frame(data_finished)
  values$tasks_tm<-data_finished

  available_parameters<-intersect(c("task.id","collection","creation.time","tm_method","tm_number_of_topics","baseform_reduction","remove_stopwords","termfreq_type","min_term","max_term","blacklist","whitelist"),colnames(data_finished))
  data_finished<-data_finished[,available_parameters]

  #delete buttons
  data_finished<-cbind(data_finished,Delete = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'delete_button_topic_results_',
    label = "",
    size="small",
    style="danger",
    icon=icon("trash"),
    onclick = 'Shiny.onInputChange(\"delete_topic_results\",  this.id)'
  ))
  #more details buttons
  data_finished<-cbind(data_finished,More_Details = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'more_details_button_topic_results_',
    label = "",
    size="small",
    style="primary",
    icon=icon("info"),
    onclick = 'Shiny.onInputChange(\"more_details_topic_results\",  this.id)'
  ))
  
  try({
    colnames(data_finished)<-c("Task id","Collection","Creation time","Method","Number of topics","Baseform reduction",
                               "Remove Stopwords","Pruning term","min term","max term","Blacklist","Whitelist","Delete","More details")
  })
  colnames(data_finished) = str_wrap(colnames(data_finished),width = 8)
  data_finished<-replace_TRUE_FALSE(data_finished)
  values$results_topic<-data_finished
  values$Details_Analysis <- NULL
  DT = datatable(data_finished,
                 selection = "single",
                 options = list(dom = 'tp',ordering=F,
                                columnDefs=list(list(className="no_select",targets=((dim(data_finished)[2]-1):(dim(data_finished)[2]-2)))))
                 ,rownames = F,class = "row-border compact",escape = F,
                 callback = JS('table.on("click", "td.no_select", function(e) {
                                e.stopPropagation()
                                });')
  )
})


#check wheather a certain result was clicked and then switch with needed information to details tab
observe({
  s = input$Topic_Results_rows_selected
  if (length(s)) {
    values$Details_Analysis <- "TM"
    isolate(values$Details_Data_TM <-
              values$Topic_Results_Files[s])
    
    isolate(values$current_task_id<- values$results_topic[s,1])
    updateTabsetPanel(session = session,
                      inputId = "coll",
                      selected = "Details")
    return(NULL)
  }
})

#if delete topic model result is clicked delete files and db entry
observeEvent(input$delete_topic_results, {
  selectedRow <-
    as.numeric(strsplit(input$delete_topic_results, "_")[[1]][5])
  if(selectedRow>0){
    unlink(values$Topic_Results_Files[selectedRow],recursive = T)
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"delete_topic_results\",  "delete_button_topic_results_0")'))
    delete_result_from_datbase(isolate(values$results_topic[selectedRow,]))
    values$reload_topic_result<-TRUE
  }
})

#if more details button is clicked open modal showing all parameters
observeEvent(input$more_details_topic_results,{
  selectedRow <-
    as.numeric(strsplit(input$more_details_topic_results, "_")[[1]][6])
  if(selectedRow>0){
    values$tm_selected_row<-selectedRow
    isolate(shinyjs::runjs('Shiny.onInputChange(\"more_details_topic_results\",  "more_details_button_topic_results_0")'))
    showModal(
      modalDialog(easyClose = T,fade = T,
      title=paste("Parameter information for task:",as.character(values$tasks_tm[selectedRow,"task id"])),
       DT::dataTableOutput(outputId = "more_details_topic_table")
      )
    )
  }
})

output$more_details_topic_table<-DT::renderDataTable({
  validate(
    need(values$tm_selected_row>0,message=F)
  )

  data<-isolate(values$tasks_tm[values$tm_selected_row,,drop=F])
  data<-t(data)
  nas<-which(is.na(data[,1]))
  if(length(nas)>0){
    data<-data[-which(is.na(data[,1])),1,drop=F]
  }
  colnames(data)<-paste("Task:",data[1,1])
  dataToUse <- data

  # # get parameters from RData object using the first 3 columns being task id, collname and timestamp to get filepath and stored parameters.RData, this only works if in output$Topic_Results mtime is not used (removed there)
  # specificResultFolderName <- getSpecificResultFolderNameFromSelectedTopic(data)
  # parametersForSelectedTM <- getParametersFromRData(pathToResultsFolder = "collections/results/topic-model/", specificFolderName=specificResultFolderName)
  # # add this parameter list to data,  matrix need all same type, so tranform values of to string first
  # indexOfData <- length(dataToUse)+1
  # for(parameterName in names(parametersForSelectedTM)){
  #   parameterValue <- parametersForSelectedTM[[parameterName]]
  #   parameterValueAsString <- as.String(parameterValue)
  #   rowNamesBefore <- rownames(dataToUse)
  #   dataToUse <- rbind(dataToUse, c(parameterValueAsString))
  #   rownames(dataToUse) <- c(rowNamesBefore,parameterName)
  #   indexOfData <- indexOfData +1
  # }
  
  datatable(data = dataToUse,selection = "none",
            options = list(dom = 'tp',ordering=F,pageLength=100)
  )
})

