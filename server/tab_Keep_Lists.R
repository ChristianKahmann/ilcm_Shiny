#' user interface to keep files
#' depends on:
#'    alues$invalidate_whitelists: invalidaze whitelists
#'    
output$UI_files_keep<-renderUI({
  values$invalidate_whitelists
  validate(
    need(length(list.files("collections/whitelists/"))>0,message="no lists found")
  )
  shinyWidgets::prettyRadioButtons(inputId = "files_keep",label = "Whitelists",
                                   choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                                   fill=T,animation = "tada",selected = NULL)
})

#' select lists to keep
#' depends on:
#'   input$new_keep_list:new list to keep
#'   input$new_keep_list: text to keep
observeEvent(input$new_keep_list,{
  values$keep_text<-""
  updateTextAreaInput(session = session,inputId = "keep_textarea",value = "")
})

#' change lists to keep
#' depends on:
#'   input$change_keep_list: make changes in lists to keep 
#'   input$files_keep: files to keep
#'   values$keep_text: text to keep
observeEvent(input$change_keep_list,{
  if(is.null(input$files_keep)){
    shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "No list to update")
  }
  else{
    values$keep_text<-readChar(con=paste0("collections/whitelists/",input$files_keep,".txt"),nchars = file.info(paste0("collections/whitelists/",input$files_keep,".txt"))$size)
  }
})

#' textarea to keep
#' depends on:
#'   values$keep_text: text to keep, seperated by comma
output$UI_keep_textarea<-renderUI({
  if(is.null(values$keep_text)){
    values$keep_text<-""
  }
  textAreaInput(inputId = "keep_textarea",label = "specify terms comma-seperated",cols = 300,rows=10,value = values$keep_text)
})

#' lists to keep
#' depends on:
#'   input$save_keep_list: save lists to keep
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

#' confirm selected whitelist
#' depends on:
#'   Save_Whitelist_confirm: save confirmed whitelist
#'   input$CL_whitelist: whitelist from classification task
#'   input$CA_whitelist: whitelist from cooccurrence analysis
#'   input$DE_whitelist: whitelist from dictionary extraction
#'   input$FE_whitelist: whitelist from frequency extraction 
#'   input$TM_whitelist: whitelist from topic model
#'   input$VA_whitelist: whitelist from volatility analysis
observeEvent(input$Save_Whitelist_confirm,{
  shiny::removeModal()
  path<-paste0("collections/whitelists/",input$Save_Whitelist_name,".txt")
  writeLines(input$keep_textarea,con=path)
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "CL_whitelist",choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                                         selected=input$CL_whitelist,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "CA_whitelist",choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                                         selected=input$CA_whitelist,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "DE_whitelist",choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                                         selected=input$DE_whitelist,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "FE_whitelist",choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                                         selected=input$FE_whitelist,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "TM_whitelist",choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                                         selected=input$TM_whitelist,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "VA_whitelist",choices = stringr::str_replace_all(string = list.files("collections/whitelists/"),pattern = ".txt",replacement = ""),
                                         selected=input$VA_whitelist,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
})
