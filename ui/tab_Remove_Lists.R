tabPanel(icon=icon("ban"),
         "Blacklists",
         uiOutput(outputId = "UI_files_remove"),
         tags$br(),
         fluidRow(
           shinyBS::bsButton(inputId = "new_remove_list",label = "create new blacklist",icon = icon("plus")),
           shinyBS::bsButton(inputId = "change_remove_list",label = "change selected blacklist",icon = icon("wrench"))
         ),
         tags$br(),
         uiOutput(outputId = "UI_remove_textarea"),
         tags$br(),
         shinyBS::bsButton(inputId = "save_remove_list",label = "save blacklist",icon = icon("save"))
)