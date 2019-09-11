tabPanel("Classifications",
         tags$div(
           style = 'height:82vh; overflow-y:auto;',
           busyIndicator(text = "Loading...",wait = 0),
           uiOutput(outputId = "classification_UI")
               )
)