output$UI_files_keep<-renderUI({
  values$invalidate_whitelists
  validate(
    need(length(list.files("collections/whitelists/"))>0,message="no lists found")
  )
  shinyWidgets::prettyRadioButtons(inputId = "files_keep",label = "Whitelists",
                                   choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                                   fill=T,animation = "tada",selected = NULL)
})

observeEvent(input$new_keep_list,{
  values$keep_text<-""
  updateTextAreaInput(session = session,inputId = "keep_textarea",value = "")
})

observeEvent(input$change_keep_list,{
  if(is.null(input$files_keep)){
    shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "No list to update")
  }
  else{
    values$keep_text<-readChar(con=paste0("collections/whitelists/",input$files_keep,".txt"),nchars = file.info(paste0("collections/whitelists/",input$files_keep,".txt"))$size)
  }
})


output$UI_keep_textarea<-renderUI({
  if(is.null(values$keep_text)){
    values$keep_text<-""
  }
  textAreaInput(inputId = "keep_textarea",label = "specify terms comma-seperated",cols = 300,rows=10,value = values$keep_text)
})


observeEvent(input$save_keep_list,{
  showModal(
    modalDialog(
      title = "Save Whitelists",
      size="l",
      textInput(
        inputId = "Save_Whitelist_name",
        label = "Specify a name"
      ),
      shinyBS::bsButton(inputId = "Save_Whitelist_confirm",label = "Save",icon = icon("save"))
    )
  ) 
}
)

observeEvent(input$Save_Whitelist_confirm,{
  shiny::removeModal()
  path<-paste0("collections/whitelists/",input$Save_Whitelist_name,".txt")
  writeLines(input$keep_textarea,con=path)
  values$invalidate_whitelists<-runif(1,0,1)
})
