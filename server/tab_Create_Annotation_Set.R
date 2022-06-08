#' create and save an annotation set

#' add a new category to the top level
#' depends on:
#'   input$add_category_top_level: confirm adding a category on top level
#'   values$anno: annotation value
#'   values$edit_id: edit id for new anotation
observeEvent(input$add_category_top_level,{
  values$anno <- operateOnAnnotationSystem(annotation_system = anno,operation = "add")
  values$edit_id<-NULL
},once = F)

#' add a new category
#' depends on:
#'   input$add_category: confirm adding a new category
#'   values$anno: annotation values
#'   values$edit_id: edit id for new annotation
observeEvent(input$add_category,{
  values$anno <- operateOnAnnotationSystem(parent_id = input$add_category$parent_id, annotation_system = anno, operation = "add")
  values$edit_id<-NULL
},once = F)

#' delete a category
#' depends on:
#'   input$delete_category: confirm to delete a category
observeEvent(input$delete_category,{
  
  shinyWidgets::confirmSweetAlert(session = session,inputId = "confirm_delete",type = "warning",title = "Are you sure you want to delete this Category",danger_mode = T)
  
},once = F)

#' confirm that an element should be deleted
#' depends on:
#'   input$confirm_delete: button to confrim to delete an element is selected
#'   input$delete_category: delete a category
#'   values$anno: selected annotation
#'   values$edit_id: edited id of selected element
observeEvent(input$confirm_delete,{
  if(isTRUE(input$confirm_delete)){
    values$anno <- operateOnAnnotationSystem(parent_id = input$delete_category$parent_id, annotation_system = anno, operation = "delete")
    values$edit_id<-NULL
  }
})

#' sort all categories in ascending sequence
#' depends on:
#'   values$anno: vales of annotation
#'   input$sort_category_up: initiate sorting the elements in ascending sequence
#'   values$edit_id: edit id of selected annotation
observeEvent(input$sort_category_up,{
  values$anno <- operateOnAnnotationSystem(parent_id = input$sort_category_up$parent_id, annotation_system = anno, operation = "sort_up")
  values$edit_id<-NULL
},once = F)

#' sort all categories downward
#' depends on:
#'   values$anno: vales of annotation
#'   input$sort_category_up: initiate sorting the elements downward
#'   values$edit_id: edit id of selected annotation
observeEvent(input$sort_category_down,{
  values$anno <- operateOnAnnotationSystem(parent_id = input$sort_category_down$parent_id, annotation_system = anno, operation = "sort_down")
  values$edit_id<-NULL
},once = F)

#' save a category
#' depends on:
#'   input$save_category: save all information from category
#'   values$edit_id: edit id of chosen element
#'   values$anno: define annotation of category
observeEvent(input$save_category,{
  
  myElement <- list()
  myElement[["name"]] <- input$save_category$name
  myElement[["color"]] <- input$save_category$color
  myElement[["description"]] <- input$save_category$description
  myElement[["isDocumentAnnotation"]] <- as.logical(input$save_category$isDocumentAnnotation)
  values$edit_id<-NULL
  values$anno <- operateOnAnnotationSystem(parent_id = input$save_category$parent_id, annotation_system = anno, operation = "save_edit", updateElement = myElement)
},once = F)

#' 
observeEvent(input$cancel_category,{
  #Re-Render with
  values$edit_id<-NULL
  values$update_category_manager<-runif(1,0,1)
},once = F)

observeEvent(input$edit_category,{
  #anno <<- operateOnAnnotationSystem(parent_id = input$edit_category$parent_id, annotation_system = anno, operation = "sort_down")
  values$edit_id<-input$edit_category$parent_id
},once = F)



#' category Manager for annotation set
#' depends on:
#'   input$add_annotation_tagset: add annotation tagset?
#'   values$update_category_manager: update category manager after changes
#'   values$anno: annotations
#'   values$edit_id: edit id 
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


#' add an annotation tagset
#' depends on:
#'   input$add_annotation_tagset: select to add an annotation tagset
#'   values$anno: annotations
#'   values$anno_creation: new annotations
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

#' save annotation tagset
#' depends on:
#'   input$save_annotation_tagset: initiate to save annotation tagset
#'   values$anno_creation: change annotation
#'   values$newscheme: new selected scheme
#'   values$anno: annotation
#'   input$project_selected: selected project
observeEvent(input$save_annotation_tagset,{
  validate(
    need(!is.null(values$anno_creation),message = "Nothing to save here. Create a new annotation set or change a existing one.")
  )
  if(values$anno_creation=="change"){
    isolate(values$anno<-NULL)
    path<-paste0("collections/annotation_schemes/",input$project_selected,".RData")
    save(anno,file=path)
    values$newscheme<-runif(1,0,1)
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
        actionButton(inputId = "Save_Tagset_confirm",label = "Save",styleclass = "success")
      )
    )
  }
})

#' confirm to save a tagset
#' depends on:
#'   input$Save_Tagset_confirm: check if confirmed to save a tagset
#'   values$anno: annotation
#'   input$Save_Tagset_name: save name of tagset
#'   values$newscheme: new scheme
observeEvent(input$Save_Tagset_confirm,{
  if(grepl(pattern = "_",x = input$Save_Tagset_name)==F){
    shiny::removeModal()
    isolate(values$anno<-NULL)
    path<-paste0("collections/annotation_schemes/",input$Save_Tagset_name,".RData")
    save(anno,file=path)
    values$newscheme<-runif(1,0,1)
  }
  else{
    shinyWidgets::sendSweetAlert(session = session,title = "Please use a name without '_' for the annotation scheme name.",type = "warning") 
  }
})

#' change annotation tagset
#' depends on:
#'   input$change_annotation_tagset: user selected annotation tagset
#'   values$tagset_selected_for_change: changes for selected tagsets
#'   input$project_selected: selected project
observeEvent(input$change_annotation_tagset,{
  if(length(list.files("collections/annotation_schemes/"))>0){
    values$tagset_selected_for_change<-input$project_selected
  }
  else{
    shinyWidgets::sendSweetAlert(session = session,title = "no annotation sets found. Create a new one.")
  }
})

#' validate selected changes on tagset
#' depends on:
#'   values$tagset_selected_for_change: selected tagset for changes
#'   values$anno_creation: changes from annotations
#'   values$anno: annotations
observe({
  validate(
    need(!is.null(values$tagset_selected_for_change),message=FALSE)
  )
  values$anno_creation<-"change"
  load(paste0("collections/annotation_schemes/",values$tagset_selected_for_change,".RData"))
  values$anno<-anno
})
