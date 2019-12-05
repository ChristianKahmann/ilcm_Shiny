output$body_UI<-renderUI({
  body<-tabItems(
    tabItem(tabName = "Explorer",
            column(width=10,style = 'height: 92vh; overflow-y: hidden; overflow-x:hidden;padding-right:10px;',
                   navbarPage(title = "", theme = shinytheme(navbarstyle),id = "expl",
                              source(file.path("ui","tab_Search_Results.R"),local = T)$value,
                              source(file.path("ui","tab_Time_Series.R"),local = T)$value,
                              source(file.path("ui","tab_Document_View.R"),local = T)$value,
                              source(file.path("ui","tab_Facets.R"),local = T)$value
                   )
            ),
            column(width=2,style = 'height: 92vh; overflow-y: auto; overflow-x:hidden;padding-right:0px;',
                   fluidRow(style="margin-left:0px;margin-right:0px;",
                            tabBox(width = 12,id = "tabBoxsearch",
                                   source(file.path("ui","tab_Simple.R"),local = T)$value,
                                   source(file.path("ui","tab_Detailed.R"),local = T)$value,
                                   source(file.path("ui","tab_Custom.R"),local = T)$value
                                   
                            )),
                   fluidRow(style="margin-left:0px;margin-right:0px;padding-right:0px;",
                            source(file.path("ui","tab_DV_Annotations.R"),local = T)$value,  
                            source(file.path("ui","tab_save_Collection.R"),local = T)$value  
                   )
            )
    ),
    tabItem(tabName="Collection",
            column(width=12,style = 'height: 92vh; overflow-y: hidden; overflow-x:hidden; padding-right:0px;',
                   navbarPage(title="",theme = shinytheme(navbarstyle),id="coll",
                              source(file.path("ui","tab_Results.R"),local = T)$value,
                              source(file.path("ui","tab_Details.R"),local = T)$value,
                              source(file.path("ui","tab_Documents.R"),local = T)$value,
                              source(file.path("ui","tab_Document_View2.R"),local = T)$value,
                              source(file.path("ui","tab_Task_Scheduler.R"),local = T)$value,
                              source(file.path("ui","tab_My_Tasks.R"),local = T)$value,
                              conditionalPanel(condition = "input.coll=='Results' | input.coll=='Documents' ",
                                               source(file.path("ui","tab_Collections.R"),local = T)$value         
                              )
                              
                   )
            )
    ),
    tabItem(tabName="Categories",
            column(width=10,style = 'height: 92vh; overflow-y: hidden; overflow-x:hidden;padding-right:10px;',
                   navbarPage(title="",theme = shinytheme(navbarstyle),id="category",
                              source(file.path("ui","tab_Create_Annotation_Set.R"),local = T)$value,
                              source(file.path("ui","tab_Annotations.R"),local = T)$value,
                              source(file.path("ui","tab_Document_View3.R"),local = T)$value,
                              source(file.path("ui","tab_Classifications.R"),local = T)$value
                   )
            ),
            column(width=2,style = 'height: 92vh; overflow-y: auto; overflow-x:hidden;padding-right:10px;',
                   conditionalPanel("input.category=='Document View3'",
                                    source(file.path("ui","tab_Anno_Annotations.R"),local = T)$value        
                   ),
                   conditionalPanel("input.category!='Document View3'",
                                    navbarPage(title="",theme=shinytheme(navbarstyle),
                                               source(file.path("ui","tab_Projects.R"),local = T)$value
                                    )
                   )
            )         
    ),
    tabItem(tabName="Scripts",
            navbarPage(title = "Scripting",id = "scrpt",
                       source(file.path("ui","tab_Scripting.R"),local = T)$value,
                       #source(file.path("ui","tab_Script_inside_App.R"),local = T)$value,
                       source(file.path("ui","tab_Remove_Lists.R"),local = T)$value,
                       source(file.path("ui","tab_Keep_Lists.R"),local = T)$value,
                       source(file.path("ui","tab_Dictionaries.R"),local = T)$value
            )
    ),
    tabItem(tabName="Importer",
            column(12,style = 'height: 92vh; overflow-y: hidden; overflow-x:hidden; padding-right:0px;',
                   navbarPage(title = "",id = "import",theme = shinytheme(navbarstyle),
                              source(file.path("ui","tab_Importer.R"),local = T)$value,
                              source(file.path("ui","tab_Exporter.R"),local = T)$value
                   )
            )
    )#,
    #tabItem(tabName="ORC",
    #         navbarPage(title = "ORC",id = "orc",
    #                    source(file.path("ui","tab_ORC.R"),local = T)$value
    #          )
    #     )
  )
  if(USER$login==FALSE){
    return(loginpage)
  }
  else{
    return(body)
  }
})