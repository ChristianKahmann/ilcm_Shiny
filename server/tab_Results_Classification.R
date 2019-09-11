


#render table with finished results for classification task
output$Classification_Results <- renderDataTable({
  #reload table if a result was deleted
  values$reload_classification_result
  isolate(values$reload_classification_result<-FALSE)
  files <- list.files("collections/results/classification/classifyCollection")
  files <- c(files,list.files("collections/results/classification/evaluateTraining"))
  validate(
    need(length(files)>0,"no results found")
  )
  files_for_date <-
    list.files("collections/results/classification/classifyCollection", full.names = T)
  files_for_date <- c(files_for_date,list.files("collections/results/classification/evaluateTraining",full.names = T))
  
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
  data_finished[,2]<-stringr::str_replace(string = data_finished[,2],pattern = ".RData",replacement = "")
  files<-files_for_date
  #just show the results for current selection if a collection is selected
  if(!is.null(values$collection_selected)){
    files<-files_for_date[which(data_finished[,2]==values$collection_selected)]
    data_finished<-matrix(data_finished[which(data_finished[,2]==values$collection_selected),],ncol=dim(data_finished)[2])
  }
  validate(
    need(length(files)>0,"no results for this collection")
  )
  files<-files[order(data_finished[,3],decreasing = T)]
  data_finished<-matrix(data_finished[order(data_finished[,3],decreasing=T),],ncol=3)
  values$Classification_Results_Files<-files
  #get parameter settings for the tasks from database
  colnames(data_finished)<-c("task id","collection","creation time")
  try({parameters<-get_parameters_from_database(data_finished)})
  if(dim(parameters)[1]>0){
    colnames(parameters)[1]<-"task id"
    data_finished<-plyr::join(x = data.frame(data_finished),y = data.frame(parameters),type="left")
    colnames(data_finished)[1]<-"task id"
    values$tasks_class<-data_finished
    data_finished<-data_finished[,c("task id","collection","creation.time","Project","Mode","positive.Threshold","SVM_C")]
  }
  else{
    values$tasks_class<-data_finished
  }
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
  try({
    colnames(data_finished)<-c("Task id","Collection","Creation time","Project","Mode",
                               "positive Threshold","SVM C","Delete","More details")
  })
  colnames(data_finished) = str_wrap(colnames(data_finished),width = 8)
  data_finished<-replace_TRUE_FALSE(data_finished)
  values$results_classification<-data_finished
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
observeEvent(input$Classification_Results_rows_selected,ignoreInit = T,{
  s = input$Classification_Results_rows_selected
  if (length(s)) {
    values$Details_Analysis <- "CL"
    isolate(values$Details_Data_CL <-
              values$Classification_Results_Files[s])
    if(grepl(x = values$Classification_Results_Files[s],pattern = "evaluateTraining")){
      values$Details_CL_mode<-"evaluate"
    }
    else{
      values$Details_CL_mode<-"whole_collection"
    }
    isolate(values$current_task_id<- values$results_classification[s,1])
    updateTabsetPanel(session = session,
                      inputId = "coll",
                      selected = "Details")
    return(NULL)
  }
})








#if delete classification result is clicked delete files and db entry
observeEvent(input$delete_classification_results, {
  selectedRow <-
    as.numeric(strsplit(input$delete_classification_results, "_")[[1]][5])
  if(selectedRow>0){
    values$class_selected_row<-selectedRow
    unlink(values$Classification_Results_Files[selectedRow],recursive = T)
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"delete_classification_results\",  "delete_button_classification_results_0")'))
    delete_result_from_datbase(isolate(values$results_classification[selectedRow,]))
    values$reload_classification_result<-TRUE
  }
})

#if more details button is clicked open modal showing all parameters
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

output$more_details_classification_table<-DT::renderDataTable({
  validate(
    need(values$class_selected_row>0,message=F)
  )
  data<-isolate(values$tasks_class[values$class_selected_row,,drop=F])
  data<-t(data)
  data<-data[-which(is.na(data[,1])),1,drop=F]
  colnames(data)<-paste("Task:",data[1,1])
  datatable(data = data,selection = "none",
            options = list(dom = 'tp',ordering=F,pageLength=100)
  )
})
