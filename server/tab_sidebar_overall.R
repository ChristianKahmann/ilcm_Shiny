#' UI for sidebarpanle with different menu-items
output$sidebarpanel_UI<-renderUI({
  sidebarmenu<-sidebarMenu(id = "tabs",
                           # menu for explorer 
                           menuItem(text = "Explorer",tabName = "Explorer",icon = icon("table")),
                           conditionalPanel(condition = "input.tabs == 'Explorer'",
                                            # if Explorer Menu is chosen, show select input with available corpora
                                            uiOutput("datasets_avaiable")
                           ),
                           # menu for collection worker
                           menuItem(text = "Collection Worker",tabName = "Collection",icon = icon("database")),
                           # menu for categories
                           menuItem(text = "Categories",tabName = "Categories",icon = icon("cloud")),
                           # menu for scripts
                           menuItem(text = "Scripts",tabName = "Scripts",icon = icon("terminal")),
                           # menu for import/export
                           menuItem(text=  "Import/Export",tabName= "Importer",icon=icon("upload")),
                           tags$br(),
                           tags$br(),
                           # button to take screenshots of the iLCM
                           shinyscreenshot::screenshotButton(),
                           # link to manual
                           tags$a("Instructions iLCM",id="manual_link",target="_blank", href="http://ilcm.informatik.uni-leipzig.de/download/starting_guide.pdf"),
                           # link to nlp group Leipzig website
                           tags$a("Universität Leipzig",id="ul_link",target="_blank", href="http://asv.informatik.uni-leipzig.de/")
  )
  # if no user is logged in show empty sidebar, else show sidebar with specified menu's
  if(USER$login==FALSE){
    return(NULL)
  }
  else{
    return(sidebarmenu) 
  }
})