tabPanel("My Tasks",
         div(style='height: 83vh; overflow-y: auto;',
               infoBoxOutput(outputId = "running_box"),
               infoBoxOutput(outputId = "finished_box"),
               infoBoxOutput(outputId = "failed_box"),
             shinyBS::bsButton(inputId = "reload_logs",label=NULL,icon = icon("refresh"),style = "primary",size = "small"),
             uiOutput("Running_Tasks"),
             box(width = 12,title = tags$h4("Logs",style="color:white;"),solidHeader = T,background = "black",
                 htmlOutput(outputId = "log_text"))
         )
)