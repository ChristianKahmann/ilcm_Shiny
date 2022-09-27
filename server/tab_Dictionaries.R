#' create a dictionary
#' depends on:
#'   input$Dict_create: initiate creating a dictionary
#'   values$dict_name: name of the dictionary
#'   values$dict_data: dictionary data
observeEvent(input$Dict_create,{
  values$dict_name<-""
  values$dict_data<-data.frame('A'=c("lorem","ipsum","",""))
})

#' change a dictionary
#' depends on:
#'    input$Dict_change: initiate to change a dictionary
observeEvent(input$Dict_change,{
  number_of_dicts <- length(list.files(path = "collections/dictionaries/"))
  if(number_of_dicts>0){
    showModal(modalDialog(easyClose = F,
                          title = "Which Dictionary would like to change?",
                          shinyWidgets::prettyRadioButtons(inputId = "dict_change_dict",label = "Dictionaries",
                                                           choices = stringr::str_replace_all(string = list.files("collections/dictionaries/"),pattern = ".RData",replacement = ""),
                                                           fill=T,animation = "tada",selected = NULL),
                          shinyBS::bsButton(inputId = "dict_change_accept",label = "confirm")
    ))
  }
  else{
    shinyWidgets::sendSweetAlert(session = session,title = "No Dictionaries found",text = "Use the 'Create-Button' to start a new dictionary",type = "warning",closeOnClickOutside = T)
  }
})

#' accept changes on dictionary
#' depends on:
#'  input$dict_change_accept: check if dictionary changes are acceptable
#'  values$dict_name: dictionary name
#'  input$dict_change_dict: changed dictionary contents
#'  values$dict_data: dictionary data
observeEvent(input$dict_change_accept,{
  shiny::removeModal()
  if(is.null(input$dict_change_dict)){
    shinyWidgets::sendSweetAlert(session=session,title = "No dictionary selected",text = "Please select a dictionary.",type = "warning")
  }
  else{
    values$dict_name<-input$dict_change_dict
    load(paste0("collections/dictionaries/",input$dict_change_dict,".RData"))
    values$dict_data<-Dict_to_DF(dict)
  }
})

#' delete dictionary
#' depends on:
#'   input$Dict_delete: initiate deleting a dictionary
observeEvent(input$Dict_delete,{
  number_of_dicts <- length(list.files(path = "collections/dictionaries/"))
  if(number_of_dicts>0){
    showModal(modalDialog(easyClose = F,
                          title = "Which Dictionary would like to delete?",
                          shinyWidgets::prettyRadioButtons(inputId = "dict_delete_dict",label = "Dictionaries",
                                                           choices = stringr::str_replace_all(string = list.files("collections/dictionaries/"),pattern = ".RData",replacement = ""),
                                                           fill=T,animation = "tada",selected = NULL),
                          shinyBS::bsButton(inputId = "dict_delete","Delete selected dictionary")
    ))
  }
  else{
    shinyWidgets::sendSweetAlert(session = session,title = "No Dictionaries found",text = "Nothing to delete!",type = "warning",closeOnClickOutside = T)
  }
})

#' observe if selected dictionary should be deleted
#' depends on:
#'   input$dict_delete: delete a dictionary
observeEvent(input$dict_delete,{
  shinyWidgets::confirmSweetAlert(session = session,inputId = "dict_confirm_delete",type = "warning",title = "Are you sure you want to delete this dictionary",danger_mode = T)
})

#' confirm to delete a dictionary
#' depends on:
#'   input$dict_confirm_delete: confirm to delete a dictionary
#'   input$dict_delete_dict: dictionary to delete
observeEvent(input$dict_confirm_delete,{
  if(isTRUE(input$dict_confirm_delete)){
    if(is.null(input$dict_delete_dict)){
      shinyWidgets::sendSweetAlert(session=session,title = "No dictionary selected",text = "Please select a dictionary.",type = "warning")
    }
    else{
      shiny::removeModal()
      file.remove(paste0("collections/dictionaries/",input$dict_delete_dict,".RData"))
    }
  }
})

#' render table of dictionaries
#' depends on:
#'   values$dict_data: dictionary data
output$Dict_table_ui<-renderRHandsontable({
  validate(
    need(!is.null(values$dict_data),message=FALSE
    )
  )
  data<-values$dict_data
  if(colnames(data)=="A"){
    return(
      rhandsontable(data = data,useTypes = F,colHeaders = c("A","B","C","D","E","F","G","H","I","J"))
    )
  }
  else{
    return(
      rhandsontable(data = data,useTypes = F,colHeaders = colnames(data))
    )
  }
})

#' save dictionary
#' depends on:
#'   values$dict_data: dictionary data
output$dict_save_ui<-renderUI({
  validate(
    need(!is.null(values$dict_data),message=FALSE
    )
  )
  shinyBS::bsButton(inputId = "dict_save","save dictionary",icon=icon("save"),style = "success")
})


#' save dictionary
#' depends on:
#'   input$dict_save: initiate saving the dictionary
#'   input$Dict_table_ui: user interface input for dictionary
observeEvent(input$dict_save,{
  header_inputs<-tagList(
    lapply(1:length(input$Dict_table_ui$params$data[[1]]),FUN = function(x){
      er<-try({header<-input$Dict_table_ui$params$colHeaders[[x]]})
      if(class(er)=="try-error"){
        header<-input$Dict_table_ui$params$colHeaders[[1]]
      }
      if(is.null(header)){
        header<-""
      }
      return(textInput(inputId =paste("dict_header_",x),label = paste0("Name of Category ",x),value = header))
    }))
  showModal(modalDialog(easyClose = F,
                        title = "Save dictionary",
                        textInput(inputId = "dict_name",label = "Dictionary name",value = values$dict_name),
                        tags$div(icon("info"))%>%
                          bs_embed_popover(
                            title ="If you would like to use the Dictionary for a Classification Initialization, please make sure that the category names of the Dictionary match those of the classification scheme.", placement = "right"
                          ),
                        tags$hr(),
                        header_inputs,
                        shinyBS::bsButton(inputId = "dict_save_really","Save")
  ))
})

#' show dictionary datatable
#' depends on:
#'   input$Dict_table_ui: user interface for dictionary table
#'   values$dict_headers: dictionary headers
observe({
  headers<-lapply(1:length(input$Dict_table_ui$params$data[[1]]),FUN = function(i){
    return(input[[paste("dict_header_",i)]])
  }
  )
  values$dict_headers<-do.call(what = base::c,headers)
})

#' save dictionary
#' depends on:
#'   input$dict_save_really: initiate saving
#'   values$dict_headers: dictionary headers
#'   input$dict_name: dictionary name
#'   input$Dict_table_ui: dictionary user interface 
#'   input$CL_dict: dictionary from classification
#'   input$DE_dict: dictionary from dictionary extraction
observeEvent(input$dict_save_really,{
  if(any(nchar(values$dict_headers)==0)){
    shinyWidgets::sendSweetAlert(session=session,title = "Not all categories have names!",text = "Please specify a name for every category!",type = "warning")
  }
  else{
    if(length(unique(values$dict_headers))!=length(values$dict_headers)){
      shinyWidgets::sendSweetAlert(session=session,title = "Not all categories have unique names!",text = "Please ensure every category has a unique name!",type = "warning")
    }
    else{
      if(input$dict_name==""){
        shinyWidgets::sendSweetAlert(session=session,title = "No dictionary name given!",text = "Please specify a name for the dictionary!",type = "warning")
      }
      else{
        if(input$dict_name %in% stringr::str_remove_all(string = list.files("collections/dictionaries/"),pattern = ".RData")){
          shinyWidgets::sendSweetAlert(session=session,title = "Dictionary Name already in Use",text = "Please specify a other name!",type = "warning")
        }
        else{
          shiny::removeModal()
          data<-input$Dict_table_ui$params$data
          df<-matrix(c(0),length(data),length(data[[1]]))
          for(i in 1:length(data)){
            for(j in 1:length(data[[1]])){
              x<-data[[i]][[j]]
              if(is.null(x))x<-""
              if(is.na(x))x<-""
              df[i,j]<-x
            }
          }
          colnames(df)<-values$dict_headers
          dict<-DF_to_Dict(df)
          save(dict,file=paste0("collections/dictionaries/",input$dict_name,".RData"))
          shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "CL_dict",choices = stringr::str_replace_all(string = list.files("collections/dictionaries/"),pattern = ".RData",replacement = ""),
                                                 selected=input$CL_dict,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
          shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "DE_dict",choices = stringr::str_replace_all(string = list.files("collections/dictionaries/"),pattern = ".RData",replacement = ""),
                                                 selected=input$DE_dict,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
          
          
          shinyWidgets::sendSweetAlert(session=session,title = "Dictionary saved",type = "success")
        }
      }
    }
  }
})
