tabPanel("Projects",
         uiOutput(outputId = "projects"),
         tags$hr(),
         tags$br(),
         tags$br(),
         conditionalPanel("input.category=='Annotations'",
                          uiOutput(outputId = "annotation_filterUI")
         )
)