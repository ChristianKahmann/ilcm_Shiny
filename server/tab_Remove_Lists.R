output$UI_files_remove<-renderUI({
  values$invalidate_blacklists
  validate(
    need(length(list.files("collections/blacklists/"))>0,message="no lists found")
  )
  shinyWidgets::prettyRadioButtons(inputId = "files_remove",label = "Blacklists",
                                   choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                   fill=T,animation = "tada",selected = NULL)
})

observeEvent(input$new_remove_list,{
  values$remove_text<-""
  updateTextAreaInput(session = session,inputId = "remove_textarea",value = "")
})

observeEvent(input$change_remove_list,{
  if(is.null(input$files_remove)){
    shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "No list to update")
  }
  else{
    values$remove_text<-readChar(con=paste0("collections/blacklists/",input$files_remove,".txt"),nchars = file.info(paste0("collections/blacklists/",input$files_remove,".txt"))$size)
  }
})


output$UI_remove_textarea<-renderUI({
  if(is.null(values$remove_text)){
    values$remove_text<-""
  }
  textAreaInput(inputId = "remove_textarea",label = "specify terms comma-seperated",cols = 300,rows=10,value = values$remove_text)
})


observeEvent(input$save_remove_list,{
  showModal(
    modalDialog(
      title = "Save Blacklist",
      size="l",
      textInput(
        inputId = "Save_Blacklist_name",
        label = "Specify a name"
      ),
      shinyBS::bsButton(inputId = "Save_Blacklist_confirm",label = "Save",icon = icon("save"))
    )
  ) 
}
)

observeEvent(input$Save_Blacklist_confirm,{
  shiny::removeModal()
  path<-paste0("collections/blacklists/",input$Save_Blacklist_name,".txt")
  writeLines(input$remove_textarea,con=path)
  values$invalidate_blacklists<-runif(1,0,1)
})
