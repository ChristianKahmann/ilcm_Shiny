output$projects<-renderUI({
 values$update_projects
  validate(
    need(length(list.files("collections/annotation_schemes/"))>0,message="no projects found")
           )
  shinyWidgets::prettyRadioButtons(inputId = "project_selected",label = "Projects",
                                   choices = stringr::str_replace_all(string = list.files("collections/annotation_schemes/"),pattern = ".RData",replacement = ""),
                                   fill=T,animation = "tada",selected = NULL)
})

output$annotation_filterUI<-renderUI({
  validate(
    need(!is.null(input$project_selected),"choose a project to apply category filters")
  )
  load(paste0("collections/annotation_schemes/",input$project_selected,".RData"))
  categories<-as.character(unlist(anno)[stringr::str_detect(string = names(unlist(anno)),"\\.name$")])
  selectInput(inputId = "annotation_filter",label = "filter by category",choices = categories,multiple = T)
})