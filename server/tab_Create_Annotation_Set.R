observeEvent(input$add_category_top_level,{
  values$anno <- operateOnAnnotationSystem(annotation_system = anno,operation = "add")
  values$edit_id<-NULL
},once = F)

observeEvent(input$add_category,{
  values$anno <- operateOnAnnotationSystem(parent_id = input$add_category$parent_id, annotation_system = anno, operation = "add")
  values$edit_id<-NULL
},once = F)

observeEvent(input$delete_category,{
  
  shinyWidgets::confirmSweetAlert(session = session,inputId = "confirm_delete",type = "warning",title = "Are you sure you want to delete this Category",danger_mode = T)
  
},once = F)

observeEvent(input$confirm_delete,{
  if(isTRUE(input$confirm_delete)){
    values$anno <- operateOnAnnotationSystem(parent_id = input$delete_category$parent_id, annotation_system = anno, operation = "delete")
    values$edit_id<-NULL
  }
})

observeEvent(input$sort_category_up,{
  values$anno <- operateOnAnnotationSystem(parent_id = input$sort_category_up$parent_id, annotation_system = anno, operation = "sort_up")
  values$edit_id<-NULL
},once = F)

observeEvent(input$sort_category_down,{
  values$anno <- operateOnAnnotationSystem(parent_id = input$sort_category_down$parent_id, annotation_system = anno, operation = "sort_down")
  values$edit_id<-NULL
},once = F)

observeEvent(input$save_category,{
  
  myElement <- list()
  myElement[["name"]] <- input$save_category$name
  myElement[["color"]] <- input$save_category$color
  myElement[["description"]] <- input$save_category$description
  myElement[["isDocumentAnnotation"]] <- as.logical(input$save_category$isDocumentAnnotation)
  values$edit_id<-NULL
  values$anno <- operateOnAnnotationSystem(parent_id = input$save_category$parent_id, annotation_system = anno, operation = "save_edit", updateElement = myElement)
},once = F)

observeEvent(input$cancel_category,{
  #Re-Render with
  values$edit_id<-NULL
  values$update_category_manager<-runif(1,0,1)
},once = F)

observeEvent(input$edit_category,{
  #anno <<- operateOnAnnotationSystem(parent_id = input$edit_category$parent_id, annotation_system = anno, operation = "sort_down")
  values$edit_id<-input$edit_category$parent_id
},once = F)




output$categoryManager <- renderUI({
  input$add_annotation_tagset
  values$update_category_manager
  anno<<-values$anno
  validate(
    need(!is.null(values$anno),"specify a tag set or create a new one")
  )
  if(is.null(values$edit_id)){
    annotateTextComponent_div_for_creation()
  }
  else{
    annotateTextComponent_div_for_creation(edit_id = values$edit_id)
  }
})



observeEvent(input$add_annotation_tagset,{
  anno_example <- list()
  anno_example[[stringi::stri_rand_strings(1, 5, '[A-Z0-9]')]] <- list(
    name = "Example",
    color = randomcoloR::randomColor(),
    description = "This is a standard description!",
    isDocumentAnnotation = T,
    sublist = list()
  )
  values$anno<-anno_example
  values$anno_creation<-"new"
})


observeEvent(input$save_annotation_tagset,{
  if(values$anno_creation=="change"){
    isolate(values$anno<-NULL)
    path<-paste0("collections/annotation_schemes/",input$project_selected,".RData")
    save(anno,file=path)
    values$update_projects<-runif(1,0,1)
  }
  else{
    showModal(
      modalDialog(
        size = "l",
        title = "Save Annotation Set",
        textInput(
          inputId = "Save_Tagset_name",
          label = "Specify a name"
        ),
        actionButton(inputId = "Save_Tagset_confirm",label = "Save")
      )
    )
  }
})

observeEvent(input$Save_Tagset_confirm,{
  shiny::removeModal()
  isolate(values$anno<-NULL)
  path<-paste0("collections/annotation_schemes/",input$Save_Tagset_name,".RData")
  save(anno,file=path)
  values$update_projects<-runif(1,0,1)
  values$newscheme<-runif(1,0,1)
})


observeEvent(input$change_annotation_tagset,{
  if(length(list.files("collections/annotation_schemes/"))>0){
    values$tagset_selected_for_change<-input$project_selected
  }
  else{
    shinyWidgets::sendSweetAlert(session = session,title = "no annotation sets found. Create a new one.")
  }
})


observe({
  validate(
    need(!is.null(values$tagset_selected_for_change),message=FALSE)
  )
  values$anno_creation<-"change"
  load(paste0("collections/annotation_schemes/",values$tagset_selected_for_change,".RData"))
  values$anno<-anno
})
