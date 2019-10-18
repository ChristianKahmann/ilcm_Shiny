observeEvent(input$Dict_create,{
  values$dict_name<-""
  values$dict_data<-data.frame(example=c("lorem","ipsum"))
})

observeEvent(input$Dict_change,{
  showModal(modalDialog(easyClose = F,
                        title = "Which Dictionary would like to change?",
                        shinyWidgets::prettyRadioButtons(inputId = "dict_change_dict",label = "Dictionaries",
                                                         choices = stringr::str_replace_all(string = list.files("collections/dictionaries/"),pattern = ".RData",replacement = ""),
                                                         fill=T,animation = "tada",selected = NULL),
                        shinyBS::bsButton(inputId = "dict_change_accept",label = "confirm")
  ))
})

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

observeEvent(input$Dict_delete,{
  showModal(modalDialog(easyClose = F,
                        title = "Which Dictionary would like to delete?",
                        shinyWidgets::prettyRadioButtons(inputId = "dict_delete_dict",label = "Dictionaries",
                                                         choices = stringr::str_replace_all(string = list.files("collections/dictionaries/"),pattern = ".RData",replacement = ""),
                                                         fill=T,animation = "tada",selected = NULL),
                        shinyBS::bsButton(inputId = "dict_delete","Delete selected dictionary")
  ))
})

observeEvent(input$dict_delete,{
  shinyWidgets::confirmSweetAlert(session = session,inputId = "dict_confirm_delete",type = "warning",title = "Are you sure you want to delete this dictionary",danger_mode = T)
})


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


output$Dict_table_ui<-renderRHandsontable({
  validate(
    need(!is.null(values$dict_data),message=FALSE
    )
  )
  data<-values$dict_data
  rhandsontable(data = data,useTypes = F)
})


output$dict_save_ui<-renderUI({
  validate(
    need(!is.null(values$dict_data),message=FALSE
    )
  )
  shinyBS::bsButton(inputId = "dict_save","save dictionary",icon=icon("save"),style = "success")
})


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
                        tags$hr(),
                        header_inputs,
                        shinyBS::bsButton(inputId = "dict_save_really","Save")
  ))
})


observe({
  headers<-lapply(1:length(input$Dict_table_ui$params$data[[1]]),FUN = function(i){
    return(input[[paste("dict_header_",i)]])
  }
  )
  values$dict_headers<-do.call(what = base::c,headers)
})

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
          values$update_dicts<-runif(1,0,1)
          shinyWidgets::sendSweetAlert(session=session,title = "Dictionary saved",type = "success")
        }
      }
    }
  }
})
