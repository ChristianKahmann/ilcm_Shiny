output$sidebarpanel_UI<-renderUI({
  sidebarmenu<-sidebarMenu(id = "tabs",
                           menuItem(text = "Explorer",tabName = "Explorer",icon = icon("table"),selected = T),
                           conditionalPanel(condition = "input.tabs == 'Explorer'",
                                            uiOutput("datasets_avaiable")
                           ),
                           menuItem(text = "Collection Worker",tabName = "Collection",icon = icon("database")),
                           menuItem(text = "Categories",tabName = "Categories",icon = icon("cloud")),
                           menuItem(text = "Scripts",tabName = "Scripts",icon = icon("terminal")),
                           menuItem(text=  "Import/Export",tabName= "Importer",icon=icon("upload")),
                           tags$a("Instructions iLCM",id="manual_link",target="_blank", href="http://ilcm.informatik.uni-leipzig.de/download/starting_guide.pdf"),
                           tags$a("Universität Leipzig",id="ul_link",target="_blank", href="http://asv.informatik.uni-leipzig.de/")
                           #menuItem(text= "ORC",tabName="ORC",icon=icon("subscript"))
                           #themeSelector()
  )
  if(USER$login==FALSE){
    return(NULL)
  }
  else{
    return(sidebarmenu)
  }
})