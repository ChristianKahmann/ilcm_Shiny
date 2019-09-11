tabPanel("Create Annotation Set",
         fluidRow(
           column(2,
                  shinyBS::bsButton(inputId = "add_annotation_tagset",label = "create new annotation set",style = "success",icon=icon("plus-circle"))
                  ),
           column(2,
                  shinyBS::bsButton(inputId = "change_annotation_tagset",label = "change existing annotation set for selected project",style = "primary",icon=icon("exchange-alt"))
                 )
         ),
         tags$hr(),
         column(4,
          uiOutput(outputId = "categoryManager")
         ),
         shinyBS::bsButton(inputId = "save_annotation_tagset",label = "save current annotation set",style = "info",icon=icon("save"))
)