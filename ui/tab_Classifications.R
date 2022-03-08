#' panel for classification tasks
tabPanel("Classifications",
         tags$div(
           style = 'height:82vh; overflow-y:auto;',
           shinyBS::bsButton(inputId = "Classifications_Reset",label = "reload",style = "success",size = "extra-small",icon=icon("sync")),
           uiOutput(outputId = "classification_UI")
         )
)