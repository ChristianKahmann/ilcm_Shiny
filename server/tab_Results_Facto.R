

#render table with finished results for factoility analysis
output$Facto_Results <- renderDataTable({
  #reload table if a result was deleted
  values$reload_facto_result
  isolate(values$reload_facto_result<-FALSE)
  files <- list.files("collections/results/factorial-analysis/")
  validate(
    need(length(files)>0,"no results found")
  )
  files_for_date <-
    list.files("collections/results/factorial-analysis/", full.names = T)
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
  values$Facto_Results_Files<-files
  
  #get parameter settings for the tasks from database
  colnames(data_finished)<-c("task id","collection","creation time")
  parameters<-get_parameters_from_database(data_finished)
  try({
    if(dim(data_finished)[1]==dim(parameters)[1]){
      data_finished<-cbind(data_finished,parameters[,c("stopwords","minchar","baseform","Pruning Term","min term","max term","Pruning Doc","min doc","max doc",
                                                       "remove_numbers","remove_punctuation","remove_hyphenation","lowercase","ngrams")])
    }
  })
  data_finished<-cbind(data_finished,Delete = shinyInput(
    shinyBS::bsButton,
    dim(data_finished)[1],
    'delete_button_facto_results_',
    label = "",
    size="small",
    style="danger",
    icon=icon("trash"),
    onclick = 'Shiny.onInputChange(\"delete_facto_results\",  this.id)'
  ))
  try({
    colnames(data_finished)<-c("task id", "collection", "creation time","remove stopwords","min char","baseform reduction","Pruning Term","min term","max term","Pruning Doc","min doc",
                               "max doc","remove numbers","remove punctuation","remove hyphenation","lowercase","ngrams","Delete")
  })
  colnames(data_finished) = str_wrap(colnames(data_finished),width = 8)
  data_finished<-replace_TRUE_FALSE(data_finished)
  values$results_facto<-data_finished
  DT = datatable(data_finished,
                 selection = "single",
                 options = list(dom = 'tp',ordering=F,
                                columnDefs=list(list(className="no_select",targets=(dim(data_finished)[2]-1))))
                 ,rownames = F,class = "row-border compact",escape = F,
                 callback = JS('table.on("click", "td.no_select", function(e) {
                               e.stopPropagation()
});')
  )
  })


#check wheather a certain result was clicked and then switch with needed information to details tab
observe({
  s = input$Facto_Results_rows_selected
  if (length(s)) {
    values$Details_Analysis <- "FA"
    isolate(values$Details_Data <-
              values$Facto_Results_Files[s])
    isolate(values$Details_Data_FA <-
              values$Facto_Results_Files[s])
    values$Facto_ready<-FALSE
    values$Facto_ready_FAMD<-FALSE
    values$Facto_ready_HCPC<-FALSE
    values$FActo_ready_PCA<-FALSE
    updateTabsetPanel(session = session,
                      inputId = "coll",
                      selected = "Details")
    return(NULL)
  }
})





#if delete factoility analysis result is clicked delete files and db entry
observeEvent(input$delete_facto_results, {
  selectedRow <-
    as.numeric(strsplit(input$delete_facto_results, "_")[[1]][5])
  if(selectedRow>0){
    unlink(values$Facto_Results_Files[selectedRow],recursive = T)
    shinyjs::useShinyjs()
    isolate(shinyjs::runjs('Shiny.onInputChange(\"delete_facto_results\",  "delete_button_facto_results_0")'))
    delete_result_from_datbase(isolate(values$results_facto[selectedRow,]))
    values$reload_facto_result<-TRUE
  }
})
