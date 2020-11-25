#' remove files from list
#' depends on:
#'   values$invalidate_blacklists: invalidated blacklist
#'   
output$UI_files_remove<-renderUI({
  values$invalidate_blacklists
  validate(
    need(length(list.files("collections/blacklists/"))>0,message="no lists found")
  )
  shinyWidgets::prettyRadioButtons(inputId = "files_remove",label = "Blacklists",
                                   choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                   fill=T,animation = "tada",selected = NULL)
})

#' remove text area
#' depends on:
#'   values$remove_text: remove text
observeEvent(input$new_remove_list,{
  values$remove_text<-""
  updateTextAreaInput(session = session,inputId = "remove_textarea",value = "")
})

#' save changes
#' depends on:
#'   input$change_remove_list: change removed elements from list
#'   input$files_remove: remove files
#'   values$remove_text: remove text
#'   
observeEvent(input$change_remove_list,{
  if(is.null(input$files_remove)){
    shinyWidgets::sendSweetAlert(type = "warning",session = session,title = "No list to update")
  }
  else{
    values$remove_text<-readChar(con=paste0("collections/blacklists/",input$files_remove,".txt"),nchars = file.info(paste0("collections/blacklists/",input$files_remove,".txt"))$size)
  }
})

#' tremove textarea
#' depends on:
#'   values$remove_text: remove text
#'   
output$UI_remove_textarea<-renderUI({
  if(is.null(values$remove_text)){
    values$remove_text<-""
  }
  textAreaInput(inputId = "remove_textarea",label = "specify terms comma-seperated",cols = 300,rows=10,value = values$remove_text)
})

#' save list after elements removed
#' depends on:
#'   input$save_remove_list: save list after elements removed
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

#' save changes on blacklist
#' depends on:
#'   input$Save_Blacklist_confirm: confirm blacklist bevor saving them
#'   input$Save_Blacklist_name: save name of the blacklist
#'   input$remove_textarea: remove textarea
#'   input$CL_blacklist: blacklist from classification
#'   input$DE_blacklist: blacklist from dictionary extraction
#'   input$FE_blacklist: blacklist from frequency extraction
#'   input$TM_blacklist: blacklist from topic modelling
#'   input$VA_blacklist: blacklist from volatility analysis
observeEvent(input$Save_Blacklist_confirm,{
  shiny::removeModal()
  path<-paste0("collections/blacklists/",input$Save_Blacklist_name,".txt")
  writeLines(input$remove_textarea,con=path)
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "CL_blacklist",choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                         selected=input$CL_blacklist,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "CA_blacklist",choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                         selected=input$CL_blacklist,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "DE_blacklist",choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                         selected=input$DE_blacklist,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "FE_blacklist",choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                         selected=input$FE_blacklist,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "TM_blacklist",choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                         selected=input$TM_blacklist,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)
  shinyWidgets::updatePrettyRadioButtons(session = session,inputId = "VA_blacklist",choices = stringr::str_replace_all(string = list.files("collections/blacklists/"),pattern = ".txt",replacement = ""),
                                         selected=input$VA_blacklist,prettyOptions = list(fill=T,animation = "tada",shape="round",plain=T),inline = T)

})
