

operateOnAnnotationSystem <- function(parent_id = NULL, annotation_system = NULL, operation = "add", updateElement = NULL)
{
  #If there is no parent id we assume to add a category to the upper level
  if(is.null(parent_id))
  {
    annotation_system <- addCategory(annotation_system)
    return(annotation_system)
  }
  
  for(id in names(annotation_system))
  {
    #cat(paste(id,"\n"))
    
    if(id == parent_id)
    {
      tmp <- switch(operation,
                    delete = {
                      annotation_system <- annotation_system[-grep(id,names(annotation_system))]
                    },
                    save_edit = {
                      annotation_system[[id]] <- persistEdit(annotation_system[[id]], updateElement)
                    },
                    add = {
                      annotation_system[[id]]$sublist <- addCategory(annotation_system[[id]]$sublist)
                    },
                    sort_up = {
                      #Get list parent
                      #Get parent list names
                      element_names <- names(annotation_system)
                      
                      #Find name of selected id
                      idx_from <- grep(id,element_names)
                      idx_to <- idx_from
                      
                      if(idx_from>1)
                      {
                        idx_to <- idx_from - 1
                      }
                      
                      #Switch name list
                      element_names[c(idx_to, idx_from)] = element_names[c(idx_from, idx_to)]
                      #Subselect with switched name list
                      annotation_system <- annotation_system[element_names]
                    },
                    sort_down = {
                      #Get list parent
                      #Get parent list names
                      element_names <- names(annotation_system)
                      
                      #Find name of selected id
                      idx_from <- grep(id,element_names)
                      idx_to <- idx_from
                      
                      if(idx_from<length(element_names))
                      {
                        idx_to <- idx_from + 1
                      }
                      
                      #Switch name list
                      element_names[c(idx_to, idx_from)] = element_names[c(idx_from, idx_to)]
                      #Subselect with switched name list
                      annotation_system <- annotation_system[element_names]
                    })
    }
    else
    {
      annotation_system[[id]]$sublist <- operateOnAnnotationSystem(parent_id, annotation_system[[id]]$sublist, operation =  operation, updateElement = updateElement)  
    }
    
  }
  annotation_system
}

addCategory <- function(edit_list){
  tmp <- list()
  tmp[[stringi::stri_rand_strings(1, 5, '[A-Z0-9]')]] <- list(
    name = "NEW",
    color = paste0("#",stringi::stri_rand_strings(1, 6, '[A-F0-9]')),
    isDocumentAnnotation = T,
    sublist = list()
  )
  edit_list <- append(tmp,edit_list)
  return(edit_list)
}

persistEdit <- function(listEntry, updateEntry){
  
  listEntry$name <- updateEntry$name
  listEntry$color <- updateEntry$color
  listEntry$isDocumentAnnotation <- updateEntry$isDocumentAnnotation
  listEntry$description <- updateEntry$description
  
  return(listEntry)
}


getElementsForEntry_creation <- function(annotations, level, edit_id = NULL) {
  
  gray_out_class <- ""
  
  if(!is.null(edit_id))
  {
    
    gray_out_class <- "gray_out_icon"
  }
  
  lapply(names(annotations), function(x) {
    el <- annotations[[x]]
    
    class <- paste0("Order", level, " ", "clickableAnnotation")
    content <- el[["name"]]
    
    if(el[["isDocumentAnnotation"]])
    {
      class <- paste0(class," documentAnnotation")
      content <- tagList(el$name,icon("certificate"))
    } else
    {
      class <- paste0(class," regularAnnotation")
    }
    
    if(identical(edit_id,x))
    {
      content <- editElements(el[["name"]], x, el[["color"]], as.logical(el[["isDocumentAnnotation"]]), el[["description"]])
      content <- tagList(content, 
                         tags$span(id = paste0(x,"_","delete"), icon("trash",class=paste("category-delete",gray_out_class)),title="Delete Category"),
                         tags$span(id = paste0(x,"_","add"), icon("plus-circle ",class=paste("add-category",gray_out_class)),title="Add Category"),
                         tags$span(id = paste0(x,"_","up"), icon("angle-up",class=paste("category-up",gray_out_class)),title="Move up"),
                         tags$span(id = paste0(x,"_","down"), icon("angle-down",class=paste("category-down",gray_out_class)),title="Move down")
                         
      )
    }
    else{
      content <- tagList(content,
                         tags$span(id = paste0(x,"_","edit"), icon("edit",class=paste("edit-category",gray_out_class)),title="Edit Category"),
                         tags$span(id = paste0(x,"_","delete"), icon("trash",class=paste("category-delete",gray_out_class)),title="Delete Category"),
                         tags$span(id = paste0(x,"_","add"), icon("plus-circle ",class=paste("add-category",gray_out_class)),title="Add Category"),
                         tags$span(id = paste0(x,"_","up"), icon("angle-up",class=paste("category-up",gray_out_class)),title="Move up"),
                         tags$span(id = paste0(x,"_","down"), icon("angle-down",class=paste("category-down",gray_out_class)),title="Move down"),
                         tags$span(id = paste0(x,"_","info"), icon("info",class=paste("category-info",gray_out_class)),title=el[["description"]])
      )
    }
    
    
    if (length(el[["sublist"]]) < 1)
    {
      new_div <-
        div(
          id = x,
          class = class,
          content,
          style = paste0("background-color:", el[["color"]])
        )
      
      return(new_div)
    }
    else
    {
      new_div <-
        div(
          class = class,
          id = x,
          tagList(content, getElementsForEntry_creation(el[["sublist"]], level + 1, edit_id = edit_id)),
          style = paste0("background-color:", el[["color"]])
        )
      
      return(new_div)
    }
  })
}

editElements <- function(category_name, id, color, isDocument, description)
{
  elements <- tagList(
    tags$input(id="CategoryNameEdit", value=category_name, style="color:black"),
    #colorpicker
    colourpicker::colourInput(inputId = "CategoryColorEdit", "Select colour", value = color),
    textAreaInput(inputId = "CategoryDescriptionEdit", "Edit description",value = description),
    checkboxInput(inputId = "CategoryDocumentEdit", label = "Is Document Annotation?", value = isDocument),
    tags$span(id = paste0(id,"_","save"), icon("check-square",class="save-category"),title="Save Edit"),
    tags$span(id = paste0(id,"_","cancel"), icon("ban", class="cancel-category"),title="Cancel Edit")
  )
}

annotateTextComponent_div_for_creation <- function(edit_id = NULL) {
  
  elements <- getElementsForEntry_creation(anno, 1,edit_id = edit_id)
  
  
  
  myStyle <- tags$style(
    "
    #annotationContainer_for_creation {
    color:white;
    font-size:1em;
    font-weight:bold;
    border: 1px solid #DDD;
    box-shadow: 3px 3px 0px rgba(0,0,0, .2);
}

.popover{
color:black;
}

.add_category_top_level{
color:brown;
}

#annotationContainer_for_creation div.annotationLegend{
color:black;
padding:0.2em;
}

#annotationContainer_for_creation div i{
cursor:pointer;
}




#annotationContainer_for_creation div.Order1{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
}

#annotationContainer_for_creation div.unsetAnnotation{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
text-transform: uppercase;
}

#annotationContainer_for_creation div.Order2{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
}

i.gray_out_icon{
pointer-events: none;
opacity: 0.5;
}

#annotationContainer_for_creation div.Order3{
transition:all 0.3s ease;
margin:0.4em;
padding:0.2em;
border: 1px solid #FFF;
border-radius: 5px;
-moz-border-radius: 5px;
-webkit-border-radius: 5px;
box-shadow: 1px 2px 4px rgba(0,0,0,.4);
}

.clickableAnnotation:hover{
background: rgb(106, 156, 237) !important; /* make this whatever you want */
/*content:\"Annotate\" !important;*/
}
"
  )
  
  myJavaScript <- 
    "
  $(document).ready(function() {
  
  $(document).on('click','i.edit-category',function(e) {
  Shiny.onInputChange('edit_category', { parent_id:$(e.target).parent().parent().attr('id'), trigger:Math.random()});
  }); 
  
  $(document).on('click','i.save-category',function(e) {
  Shiny.onInputChange('save_category', { 
  parent_id:$(e.target).parent().parent().attr('id'), 
  trigger:Math.random(),
  name:$('#CategoryNameEdit').val(),
  color:$('#CategoryColorEdit').val(),
  description:$('#CategoryDescriptionEdit').val(),
  isDocumentAnnotation:$('#CategoryDocumentEdit').is(':checked')
  });
  }); 
  
  $(document).on('click','i.cancel-category',function(e) {
  Shiny.onInputChange('cancel_category', { parent_id:$(e.target).parent().parent().attr('id'), trigger:Math.random()});
  }); 
  
  $(document).on('click','i.add_category_top_level',function(e) {
  //alert('click bound to document listening for #test-element');
  //Simulate Change value with random number
  Shiny.onInputChange('add_category_top_level', Math.random());
  }); 
  
  $(document).on('click','i.add-category',function(e) {
  Shiny.onInputChange('add_category', { parent_id:$(e.target).parent().parent().attr('id'), trigger:Math.random()});
  }); 
  
  $(document).on('click','i.category-up',function(e) {
  Shiny.onInputChange('sort_category_up', { parent_id:$(e.target).parent().parent().attr('id'), trigger:Math.random()});
  }); 
  
  $(document).on('click','i.category-down',function(e) {
  Shiny.onInputChange('sort_category_down', { parent_id:$(e.target).parent().parent().attr('id'), trigger:Math.random()});
  
  });
  
  $(document).on('click','i.category-delete',function(e) {
  Shiny.onInputChange('delete_category', { parent_id:$(e.target).parent().parent().attr('id'), trigger:Math.random()});
  });
  
  });
  "
  myTags <- tagList(tags$div(icon("plus",class="add_category_top_level")),elements)
  annotation <- div(id = "annotationContainer_for_creation",
                    tagList(tags$script(HTML(myJavaScript)), myStyle, myTags, div(tagList(icon("certificate"),"=Document-wide annotation"),class="annotationLegend")))
  
  return(annotation)
}