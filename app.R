library(shiny)
library(shinyFiles)
library(data.table)
library(stringr)
library(shinydashboard)
#library(semantic.dashboard)
library(dashboardthemes)
library(shinythemes)
library(shinyTree)
library(shinyTable)
library(rhandsontable)
library(readtext)
library(tools)
library(colourpicker)
library(formattable)
library(visNetwork)
library(Matrix)
library(shinyAce)
library(plotly)
library(wordVectors)
library(magrittr)
library(solr)
library(tmca.unsupervised)                                                                                                                                                                                                                                                                                                               
library(RMariaDB)
library(wordcloud2)
library(networkD3)
library(cleanNLP)
library(tmca.util)
library(DT)
library(bsplus)
library(LDAvis)
library(shinyjqui)
library(shinysky)
library(shinyWidgets)
library(dplyr)
library(shinyBS)
library(d3heatmap)
library(shinycssloaders)
library(sparkline)
library(shinyjs)
library(promises)
library(future)
library(shinyjqui)
#library(Factoshiny)
#library(FactoMineR)
plan(multiprocess)
#killDbConnections <- function () {
#  
#  all_cons <- dbListConnections(MySQL())
#  
#  print(all_cons)
#  
#  for(con in all_cons)
#    +  dbDisconnect(con)
#  
#  print(paste(length(all_cons), " connections killed."))
#  
#}
#killDbConnections()
options(stringsAsFactors = FALSE)

source("global/createWordcloud.R")
source("global/add color tag.R")
source("global/number_of_dates.R")
source("global/solr_highlight_advanced.R")
source("global/calenderHeatmap.R")
source("global/cooc_chart.R")
source("global/kwic2.R")
source("global/prepare_for_hist.R")
source("global/shinyInputValue.R")
source("global/removCheckboxes.R")
source("global/log_to_file.R")
source("global/make_annotations.R")
source("global/get_categories.R")
source("global/save_collection_to_db.R")
source("global/make_annotations_for_creations.R")
source("global/transform_to_NULL.R")
source("global/shinyInput.R")
source("global/write_annotations_toMariaDB.R")
source("global/DF_to_Dict.R")
source("global/create_body_for_solr_atomic_update.R")
source("global/busy_Indicator.R")
source("global/topic_relevance.R")
source("global/check_validity_of_input_parameters.R")
source("global/get_token_from_db.R")
source("global/get_metadata_from_db.R")
source("www/ilcm_dashboard_theme.R")
source("config_file.R")




if(look=="fancy"){
  navbarstyle="spacelab"
  #navbarstyle="paper"
  dashboardstyle=ilcm_dashboard_theme_098
  #dashboardstyle=NULL
}
if(look=="scientific"){
  navbarstyle="cerulean"
  dashboardstyle=NULL
}


options(scipen=999)

ui <- dashboardPage(
  dashboardHeader(title = "🅸🅻🅲🅼",titleWidth="200px",
                  tags$li(actionLink("openOptionsModal", label = "", icon = icon("cog")),
                          dropdownMenuOutput(outputId = "dropdown_info"),
                          class = "dropdown")
                  
  ),
  dashboardSidebar(width="200px",
                   sidebarMenu(id = "tabs",
                               menuItem(text = "Explorer",tabName = "Explorer",icon = icon("table")),
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
  ),
  dashboardBody(
    dashboardstyle,
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-A', 'null'); }"),
    tags$head(
      tags$link(rel="stylesheet",type="text/css",href="custom.css")
    ),
    tags$style(HTML('.popover-title {color:black;}
                               .popover-content {color:black;}
                               .content-wrapper {z-index:auto;}')),
    tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
    tabItems(
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
                         source(file.path("ui","tab_Script_inside_App.R"),local = T)$value,
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
  )
)



server <- function(input, output, session) {
  values<-reactiveValues()
  status=reactiveValues(res1IsDone=FALSE,res2HasRendered=FALSE)
  values$solr_url<-url
  values$update_solr_url<-update_solr_url
  values$update_solr_port<-update_solr_port
  values$host<-host
  values$db_port<-db_port
  values$user<-Sys.info()["login"]
  options(shiny.maxRequestSize=max_upload_file_size*1024^2) 
  
  
  source(file.path("server","System_Load_Menu.R"),local = T)$value
  source(file.path("server","tab_Simple.R"),local = T)$value
  source(file.path("server","tab_Detailed.R"),local = T)$value
  source(file.path("server","tab_Search_Results.R"),local = T)$value
  source(file.path("server","tab_Document_View.R"),local = T)$value
  source(file.path("server","tab_DV_Annotations.R"),local = T)$value
  source(file.path("server","tab_Annotations.R"),local = T)$value
  source(file.path("server","tab_Facets.R"),local = T)$value
  source(file.path("server","tab_Time_Series.R"),local = T)$value
  source(file.path("server","tab_ShinyAce.R"),local = T)$value
  source(file.path("server","tab_Projects.R"),local = T)$value
  source(file.path("server","tab_Task_Scheduler.R"),local = T)$value
  source(file.path("server","tab_save_Collection.R"),local = T)$value
  source(file.path("server","tab_Scripting.R"),local = T)$value
  source(file.path("server","tab_My_Tasks.R"),local = T)$value
  source(file.path("server","tab_Results.R"),local = T)$value
  source(file.path("server","tab_Details.R"),local = T)$value
  source(file.path("server","tab_Importer.R"),local = T)$value
  source(file.path("server","tab_Documents.R"),local=T)$value
  source(file.path("server","tab_Document_View2.R"),local=T)$value
  source(file.path("server","tab_Document_View3.R"),local=T)$value
  source(file.path("server","tab_Create_Annotation_Set.R"),local=T)$value
  source(file.path("server","tab_Remove_Lists.R"),local=T)$value
  source(file.path("server","tab_Keep_Lists.R"),local=T)$value
  source(file.path("server","tab_Custom.R"),local=T)$value
  source(file.path("server","tab_Script_inside_App.R"),local=T)$value
  source(file.path("server","tab_Collections.R"),local=T)$value
  source(file.path("server","tab_Classifications.R"),local=T)$value
  source(file.path("server","tab_Dictionaries.R"),local=T)$value
  source(file.path("server","tab_Simple_Sub.R"),local=T)$value
  source(file.path("server","tab_Detailed_Sub.R"),local=T)$value
  source(file.path("server","tab_Custom_Sub.R"),local=T)$value
  source(file.path("server","tab_Exporter.R"),local=T)$value
  #source(file.path("server","tab_Details_Facto_CA.R"),local=T)$value
  #source(file.path("server","tab_Details_Facto_FAMD.R"),local=T)$value
  #source(file.path("server","tab_Details_Facto_HCPC.R"),local=T)$value
  #source(file.path("server","tab_Details_Facto_PCA.R"),local=T)$value
  #source(file.path("server","tab_ORC.R"),local=T)$value
  session$allowReconnect(TRUE)
  
}

shinyApp(ui, server)

