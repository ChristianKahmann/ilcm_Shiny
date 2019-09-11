

#render table with finished results for vectorility analysis
output$Syntactic_Parsing_Results <- renderDataTable({
  #reload table if a result was deleted
  values$reload_parsing_result
  isolate(values$reload_parsing_result<-FALSE)
  files <- list.files("collections/results/syntactic-parsing//")
  validate(
    need(length(files)>0,"no results found")
  )
  files_for_date <-
    list.files("collections/results/syntactic-parsing//", full.names = T)
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
  values$Parsing_Results_Files<-files
  
  #get parameter settings for the tasks from database
  colnames(data_finished)<-c("task id","collection","creation time")
  try({parameters<-get_parameters_from_database(data_finished)})
  if(dim(parameters)[1]>0){
    colnames(parameters)[1]<-"task id"
    data_finished<-plyr::join(x = data.frame(data_finished),y = data.frame(parameters),type="left")
    colnames(data_finished)[1]<-"task id"
    values$tasks_parsing<-data_finished
    data_finished<-data_finished[,c("task id","collection","creation.time")]
  }
  else{
    values$tasks_parsing<-data_finished
  }
  data_finished<-cbind(data_finished,Delete = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'delete_button_parsing_results_',
    label = "",
    size="small",
    style="danger",
    icon=icon("trash"),
    onclick = 'Shiny.onInputChange(\"delete_parsing_results\",  this.id)'
  ))
  data_finished<-cbind(data_finished,More_Details = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'more_details_button_parsing_results_',
    label = "",
    size="small",
    style="primary",
    icon=icon("info"),
    onclick = 'Shiny.onInputChange(\"more_details_parsing_results\",  this.id)'
  ))
  
  try({
    colnames(data_finished)<-c("Task id","Collection","Creation time","Delete","More details")
  })
  colnames(data_finished) = str_wrap(colnames(data_finished),width = 8)
  data_finished<-replace_TRUE_FALSE(data_finished)
  values$results_parsing<-data_finished
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
observeEvent(ignoreInit = T,input$Syntactic_Parsing_Results_rows_selected,{
  s = input$Syntactic_Parsing_Results_rows_selected
  if (length(s)) {
    values$Details_Analysis <- "SP"
    isolate(values$Details_Data_SP <-
              values$Parsing_Results_Files[s])
    updateTabsetPanel(session = session,
                      inputId = "coll",
                      selected = "Details")
    return(NULL)
  }
})





#if delete vectorility analysis result is clicked delete files and db entry
observeEvent(input$delete_parsing_results, {
  selectedRow <-
    as.numeric(strsplit(input$delete_parsing_results, "_")[[1]][5])
  if(selectedRow>0){
    unlink(values$Parsing_Results_Files[selectedRow],recursive = T)
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"delete_parsing_results\",  "delete_button_parsing_results_0")'))
    delete_result_from_datbase(isolate(values$results_parsing[selectedRow,]))
    values$reload_parsing_result<-TRUE
  }
})


#if more details button is clicked open modal showing all parameters
observeEvent(input$more_details_parsing_results,{
  selectedRow <-
    as.numeric(strsplit(input$more_details_parsing_results, "_")[[1]][6])
  if(selectedRow>0){
    values$parsing_selected_row<-selectedRow
    isolate(shinyjs::runjs('Shiny.onInputChange(\"more_details_parsing_results\",  "more_details_button_parsing_results_0")'))
    values$Det_KE_data<-NULL
    showModal(
      modalDialog(easyClose = T,fade = T,
                  title=paste("Parameter information for task:",as.character(values$tasks_parsing[selectedRow,"task id"])),
                  DT::dataTableOutput(outputId = "more_details_parsing_table")
      )
    )
  }
})

output$more_details_parsing_table<-DT::renderDataTable({
  validate(
    need(values$parsing_selected_row>0,message=F)
  )
  data<-isolate(values$tasks_parsing[values$parsing_selected_row,,drop=F])
  data<-t(data)
  colnames(data)<-paste("Task:",data[1,1])
  datatable(data = data,selection = "none",
            options = list(dom = 'tp',ordering=F,pageLength=100)
  )
})