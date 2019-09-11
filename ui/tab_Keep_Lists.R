tabPanel(icon=icon("check"),
         "Whitelists",
         uiOutput(outputId = "UI_files_keep"),
         tags$br(),
         fluidRow(
           shinyBS::bsButton(inputId = "new_keep_list",label = "create new whitelist",icon = icon("plus")),
           shinyBS::bsButton(inputId = "change_keep_list",label = "change selected whitelist",icon = icon("wrench"))
         ),
         tags$br(),
         uiOutput(outputId = "UI_keep_textarea"),
         tags$br(),
         shinyBS::bsButton(inputId = "save_keep_list",label = "save whitelist",icon = icon("save"))
)