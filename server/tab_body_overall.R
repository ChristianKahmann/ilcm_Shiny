# specify the body; body is split into several tabs corresponding with the sidebar menus
output$body_UI<-renderUI({
  body<-tabItems(
    # first tab: Explorer
    tabItem(tabName = "Explorer",
            column(width=10,style = 'height: 92vh; overflow-y: hidden; overflow-x:hidden;padding-right:10px;',
                   navbarPage(title = "", theme = shinytheme(navbarstyle),id = "expl",
                              # show results of search
                              source(file.path("ui","tab_Search_Results.R"),local = T)$value,
                              # show diachronic view of found documents
                              source(file.path("ui","tab_Time_Series.R"),local = T)$value,
                              # show and annotate documents
                              source(file.path("ui","tab_Document_View.R"),local = T)$value,
                              # show facetted view of the found documents metadata
                              source(file.path("ui","tab_Facets.R"),local = T)$value
                   )
            ),
            # column on the right shows search, annotation and collection saving UI depending on the active window in column on the left side
            column(width=2,style = 'height: 92vh; overflow-y: auto; overflow-x:hidden;padding-right:0px;',
                   fluidRow(style="margin-left:0px;margin-right:0px;",
                            # tab Boxwith the 3 search interfaces
                            tabBox(width = 12,id = "tabBoxsearch",
                                   # simple search; combine keaywords
                                   source(file.path("ui","tab_Simple.R"),local = T)$value,
                                   # detailed search; combine keywords and add metadata to search
                                   source(file.path("ui","tab_Detailed.R"),local = T)$value,
                                   # custom search for experienced users
                                   source(file.path("ui","tab_Custom.R"),local = T)$value
                                   
                            )),
                   fluidRow(style="margin-left:0px;margin-right:0px;padding-right:0px;",
                            # show annotation UI
                            source(file.path("ui","tab_DV_Annotations.R"),local = T)$value, 
                            # UI for saving collections
                            source(file.path("ui","tab_save_Collection.R"),local = T)$value  
                   )
            )
    ),
    # second tab: Collection Worker
    tabItem(tabName="Collection",
            column(width=12,style = 'height: 92vh; overflow-y: hidden; overflow-x:hidden; padding-right:0px;',
                   navbarPage(title="",theme = shinytheme(navbarstyle),id="coll",
                              # table of results of finished tasks
                              source(file.path("ui","tab_Results.R"),local = T)$value,
                              # depending on chosen result, results visualisation
                              source(file.path("ui","tab_Details.R"),local = T)$value,
                              # show documents of a selected collection
                              source(file.path("ui","tab_Documents.R"),local = T)$value,
                              # show and annotate documents
                              source(file.path("ui","tab_Document_View2.R"),local = T)$value,
                              # main window to start new tasks
                              source(file.path("ui","tab_Task_Scheduler.R"),local = T)$value,
                              # check the logs of started tasks
                              source(file.path("ui","tab_My_Tasks.R"),local = T)$value,
                              # if Results oder Document Tab are active show table of created collections
                              conditionalPanel(condition = "input.coll=='Results' | input.coll=='Documents' ",
                                               source(file.path("ui","tab_Collections.R"),local = T)$value         
                              )
                   )
            )
    ),
    # third tab: Categories
    tabItem(tabName="Categories",
            column(width=10,style = 'height: 92vh; overflow-y: hidden; overflow-x:hidden;padding-right:10px;',
                   navbarPage(title="",theme = shinytheme(navbarstyle),id="category",
                              # create new annotation schemata
                              source(file.path("ui","tab_Create_Annotation_Set.R"),local = T)$value,
                              # show made annotations
                              source(file.path("ui","tab_Annotations.R"),local = T)$value,
                              # show or annotate documents
                              source(file.path("ui","tab_Document_View3.R"),local = T)$value,
                              # results page for made classifications
                              source(file.path("ui","tab_Classifications.R"),local = T)$value
                   )
            ),
            column(width=2,style = 'height: 92vh; overflow-y: auto; overflow-x:hidden;padding-right:10px;',
                   conditionalPanel("input.category=='Document View3'",
                                    # if document view is present show annotations UI
                                    source(file.path("ui","tab_Anno_Annotations.R"),local = T)$value        
                   ),
                   conditionalPanel("input.category!='Document View3'",
                                    navbarPage(title="",theme=shinytheme(navbarstyle),
                                               # if document view is not present show radio buttons with available projects
                                               source(file.path("ui","tab_Projects.R"),local = T)$value
                                    )
                   )
            )         
    ),
    # fourth tab Scripts
    tabItem(tabName="Scripts",
            navbarPage(title = "Scripting",id = "scrpt",
                       # create new or change existing analysis scripts
                       source(file.path("ui","tab_Scripting.R"),local = T)$value,
                       # create or change blacklists
                       source(file.path("ui","tab_Remove_Lists.R"),local = T)$value,
                       # create or change whitelists
                       source(file.path("ui","tab_Keep_Lists.R"),local = T)$value,
                       # create or change dictionaries
                       source(file.path("ui","tab_Dictionaries.R"),local = T)$value,
                       # create or change vocabularies
                       source(file.path("ui","tab_Vocabularies.R"),local = T)$value
            )
    ),
    # fifth tab: Importer/Exporter
    tabItem(tabName="Importer",
            column(12,style = 'height: 92vh; overflow-y: hidden; overflow-x:hidden; padding-right:0px;',
                   navbarPage(title = "",id = "import",theme = shinytheme(navbarstyle),
                              # UI to import new data
                              source(file.path("ui","tab_Importer.R"),local = T)$value,
                              # UI to export existing data
                              source(file.path("ui","tab_Exporter.R"),local = T)$value
                   )
            )
    )
  )
  #if no user is logged in return loginpage, else show body with specified tabs
  if(USER$login==FALSE){
    return(loginpage)
  }
  else{
    return(body)
  }
})