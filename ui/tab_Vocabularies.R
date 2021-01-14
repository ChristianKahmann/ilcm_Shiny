#' panel for vocabularies to import and edit vocalulary lists 
tabPanel(icon=icon("keyboard"),
         "Vocabularies",
         fluidRow(style="margin-left:0px;margin-right:0px",
           column(3,
                  uiOutput(outputId = "UI_files_vocabulary")
           ),
           column(4,
                  bsButton(inputId = "vocabulary_import_vocab_from_task",label = "Import vocabulary from finished analysis",style = "success",icon = icon("upload"))
           )
         ),
         tags$br(),
         fluidRow(
           shinyBS::bsButton(inputId = "new_vocabulary_list",label = "create new vocabulary",icon = icon("plus")),
           shinyBS::bsButton(inputId = "change_vocabulary_list",label = "change selected vocabulary",icon = icon("wrench"))
         ),
         tags$br(),
         uiOutput(outputId = "UI_vocabulary_textarea"),
         tags$br(),
         shinyBS::bsButton(inputId = "save_vocabulary_list",label = "save vocabulary",icon = icon("save"))
)