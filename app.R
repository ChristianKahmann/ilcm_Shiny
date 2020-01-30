# load required libraries
library(shiny)
library(shinyFiles)
library(data.table)
library(stringr)
library(shinydashboard)
library(dashboardthemes)
library(shinythemes)
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
library(waiter)
# tell library future how to handle requests; used when solr updates are started from inside the app in order to be able to continue using the app and not having to wait until the solr update is finished
plan(multiprocess)

options(stringsAsFactors = FALSE)
# load additional functions
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
source("global/deduplication_decision.R")
source("global/task_id_functions.R")
source("global/update-input.R")
# load the available themes
source("www/ilcm_dashboard_theme.R")
# load the current settings
source("config_file.R")
# load the UI for the login page
source("ui/tab_loginPage.R")
#set task id counter if its not already set
if(!file.exists("global/task_id_counter.RData")){
  set_task_id_counter(1)
}


# specify the appearance of the app depending on setting in config file
if(look=="fancy"){
  navbarstyle="spacelab"
  # navbarstyle="paper"
  dashboardstyle=ilcm_dashboard_theme_099
  # dashboardstyle=NULL
}
if(look=="scientific"){
  navbarstyle="cerulean"
  dashboardstyle=NULL
}


# don't use scientific notation like 1e+11 instead of 100000000000
options(scipen=999)

# definition of App UI; use of a dashboard layout with header, sidebar and body
ui <- dashboardPage(
  # app header
  dashboardHeader(title = "🅸🅻🅲🅼",titleWidth="200px",
                  # button which opens options modal
                  tags$li(actionLink("openOptionsModal", label = "", icon = icon("cog")),class = "dropdown"),
                  # button which open informations showing current app version and user
                  tags$li(dropdownMenuOutput(outputId = "dropdown_info"),class = "dropdown"),
                  # button which lets user log out and send him back to login page
                  tags$li(shinyjs::hidden(actionLink(inputId = "Logout",label = "",icon = icon("sign-out"))),class = "dropdown")
  ),
  # app sidebar
  dashboardSidebar(width="200px",
                   # UI for sidebar panel
                   uiOutput("sidebarpanel_UI")
  ),
  # app main body
  dashboardBody(
    #show loading screen on startup
    waiter::use_waiter(),
    #use specified dashboardstyle
    dashboardstyle,
    # enable shinyjs
    shinyjs::useShinyjs(),
    # add extra function to shinyjs to reset the count value of a button
    shinyjs::extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-A', 'null'); }"),
    # add extra function to shinyjs to get type of input to update this input when using a parameter preset in task scheduler
    shinyjs::extendShinyjs("www/app-shinyjs.js", functions = c("getInputType")),
    # use custom css to style certain elements
    tags$head(
      tags$link(rel="stylesheet",type="text/css",href="custom.css")
    ),
    # UI for body
    uiOutput("body_UI")
  ),
  # Hide the loading message when datawaiter_wait_object is rendered
  # @ waiter_wait_object: specified in config_file depending on @ hide_login 
  waiter::hide_waiter_on_drawn(waiter_wait_object)
)
server <- function(input, output, session) {
  # specify loading page on app startup
  waiter::show_waiter(
    color = "#272B30",
    div(
      style = "color:#FFFFFF;",
      waiter::spin_cube_grid(),
      "Starting your iLCM instance"
    )
  )
  # create main reactive value called values
  values<-reactiveValues()
  # specify inital reactive values from entries in config file
  values$solr_url<-url
  values$update_solr_url<-update_solr_url
  values$update_solr_port<-update_solr_port
  values$host<-host
  values$db_port<-db_port
  values$user<-"unknown"
  values$logged_in<-FALSE
  options(shiny.maxRequestSize=max_upload_file_size*1024^2) 

  USER<- reactiveValues(login = hide_login)
  # when the app load start with the Explorer Tab selected
  shiny::updateTabsetPanel(session = session,inputId = "tabs",selected = "Explorer")
  
  # if login Button is clicked, start the login mechanism
  observeEvent(input$login,{ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
              isolate(values$user<-Username)
              shinyjs::show(id = "Logout")
              updateTabItems(session = session,inputId = "tabs",selected = "Explorer")
            }
            else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  # button to log out
  output$logoutbtn <- renderUI({
    req(USER$login)
    actionLink(inputId = "Logout",label = "",icon = icon("sign-out"))
  })
  
  # if logout button is clicked set login to FALSE
  observeEvent(input$Logout,{
    shinyjs::hide(id = "Logout")
    USER$login<-FALSE
    values$user<-"unknown"
  })
  
  #source the server parts of the app
  source(file.path("server","tab_body_overall.R"),local = T)$value
  source(file.path("server","tab_sidebar_overall.R"),local = T)$value
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

  
  # allow reconnect to app if connection got disturbed
  session$allowReconnect(TRUE)
}

# start the app
shiny::shinyApp(ui,server)

