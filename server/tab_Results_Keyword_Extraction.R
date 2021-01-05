

#' render table with finished results for vectorility analysis
#' depends on:
#'   values$reload_keyword_result: reload keyword extraction results
#'   values$collection_selected: selected collection
#'   values$tasks_keyword: keyword extraction tasks
#'   values$Keyword_Results_Files: keyword extraction result files
#'   values$results_keyword: keyword extraction results
output$Keyword_Extraction_Results <- renderDataTable({
  #reload table if a result was deleted
  values$reload_keyword_result
  isolate(values$reload_keyword_result<-FALSE)
  files <- list.files("collections/results/keyword-extraction//")
  validate(
    need(length(files)>0,"no results found")
  )
  files_for_date <-
    list.files("collections/results/keyword-extraction//", full.names = T)
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
    ddata_finished<-data_finished[which(data_finished[,2]==values$collection_selected),,drop=F]
  }
  validate(
    need(length(files)>0,"no results for this collection")
  )
  files<-files[order(data_finished[,3],decreasing = T)]
  data_finished<-data_finished[order(data_finished[,3],decreasing=T),,drop=F]
  values$Keyword_Results_Files<-files
  
  #select parameters to show
  colnames(data_finished)[1:3]<-c("task id","collection","creation time")
  data_finished<-data.frame(data_finished)
  values$tasks_keyword<-data_finished

  available_parameters<-intersect(c("task.id","collection","creation.time","KE_Mode","KE_no_ref_method","KE_no_ref_n_min","KE_no_ref_ngram_max","KE_filter","KE_phrase"),colnames(data_finished))
  data_finished<-data_finished[,available_parameters]

  data_finished<-cbind(data_finished,Delete = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'delete_button_keyword_results_',
    label = "",
    size="small",
    style="danger",
    icon=icon("trash"),
    onclick = 'Shiny.onInputChange(\"delete_keyword_results\",  this.id)'
  ))
  data_finished<-cbind(data_finished,More_Details = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'more_details_button_keyword_results_',
    label = "",
    size="small",
    style="primary",
    icon=icon("info"),
    onclick = 'Shiny.onInputChange(\"more_details_keyword_results\",  this.id)'
  ))
  
  try({
    colnames(data_finished)<-c("Task id","Collection","Creation time","Mode","Method","n min","ngram max","Filter","Phrase","Delete","More details")
  })
  colnames(data_finished) = str_wrap(colnames(data_finished),width = 8)
  data_finished<-replace_TRUE_FALSE(data_finished)
  values$results_keyword<-data_finished
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


#' check wheather a certain result was clicked and then switch with needed information to details tab
#' depends on:
#'   input$Keyword_Extraction_Results_rows_selected: selected rows from result list of keyword extraction
#'   values$Details_Analysis: keyword extraction analysis details
#'   values$Details_Data_KE: data details from keyword extraction
#'   values$Keyword_Results_Files: keyword extraction result files
observeEvent(ignoreInit = T,input$Keyword_Extraction_Results_rows_selected,{
  s = input$Keyword_Extraction_Results_rows_selected
  if (length(s)) {
    values$Details_Analysis <- "KE"
    isolate(values$Details_Data_KE <-
              values$Keyword_Results_Files[s])
    updateTabsetPanel(session = session,
                      inputId = "coll",
                      selected = "Details")
    return(NULL)
  }
})





#' if delete vectorility analysis result is clicked delete files and db entry
#' depends on:
#'   input$delete_keyword_results: reload result list after keyword extraction file was deleted 
#'   values$Keyword_Results_Files: keyword extraction result files
#'   
observeEvent(input$delete_keyword_results, {
  selectedRow <-
    as.numeric(strsplit(input$delete_keyword_results, "_")[[1]][5])
  if(selectedRow>0){
    unlink(values$Keyword_Results_Files[selectedRow],recursive = T)
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"delete_keyword_results\",  "delete_button_keyword_results_0")'))
    values$reload_keyword_result<-TRUE
  }
})


#' if more details button is clicked open modal showing all parameters
#' depends on:
#'   input$more_details_keyword_results: more details on keyword extraction results
#'   values$keyword_selected_row: selected row from result list
#'   values$Det_KE_data: detailes keyword extraction data 
#'   values$tasks_keyword: keyword extraction tasks
#'   
observeEvent(input$more_details_keyword_results,{
  selectedRow <-
    as.numeric(strsplit(input$more_details_keyword_results, "_")[[1]][6])
  if(selectedRow>0){
    values$keyword_selected_row<-selectedRow
    isolate(shinyjs::runjs('Shiny.onInputChange(\"more_details_keyword_results\",  "more_details_button_keyword_results_0")'))
    values$Det_KE_data<-NULL
    showModal(
      modalDialog(easyClose = T,fade = T,
                  title=paste("Parameter information for task:",as.character(values$tasks_keyword[selectedRow,"task id"])),
                  DT::dataTableOutput(outputId = "more_details_keyword_table")
      )
    )
  }
})

#'  if more details button is clicked open modal showing all parameters
#'  depends on:
#'    values$keyword_selected_row: selected rows from keyword extraction result list
#'    values$tasks_keyword: keyword extraction tasks
output$more_details_keyword_table<-DT::renderDataTable({
  validate(
    need(values$keyword_selected_row>0,message=F)
  )
  data<-isolate(values$tasks_keyword[values$keyword_selected_row,,drop=F])
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