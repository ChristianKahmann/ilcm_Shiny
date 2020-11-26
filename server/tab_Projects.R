#' show available projects/annotations schemata
#' depends on:
#'   values$newschema: reactive variable changing if annotation schemata changes
#'   values$classification_project: classification project
output$projects<-renderUI({
  # @values$newschema reactive variable changing if annotation schemata change
  values$newscheme
  validate(
    need(length(list.files("collections/annotation_schemes/"))>0,message="no projects found")
           )
  shinyWidgets::prettyRadioButtons(inputId = "project_selected",label = "Projects",
                                   choices = stringr::str_replace_all(string = list.files("collections/annotation_schemes/"),pattern = ".RData",replacement = ""),
                                   fill=T,animation = "tada",selected =  values$classification_project)
})


#' select Input to filter available categories when investigating present annotations
#' depends on:
#'   input$project_selected: selected project
#'   
output$annotation_filterUI<-renderUI({
  validate(
    need(!is.null(input$project_selected),"choose a project to apply category filters")
  )
  load(paste0("collections/annotation_schemes/",input$project_selected,".RData"))
  categories<-as.character(unlist(anno)[stringr::str_detect(string = names(unlist(anno)),"\\.name$")])
  selectInput(inputId = "annotation_filter",label = "filter by category",choices = categories,multiple = T)
})