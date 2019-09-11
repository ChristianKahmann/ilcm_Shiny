conditionalPanel("input.expl=='Search Results'",
                 box(width=NULL,title =tags$h4("Save Collection",style="color:white"),solidHeader = TRUE,status = "primary",
                     textInput(inputId = "Collection_Name",label = "Collection Name:",value = "test"),
                     checkboxInput(inputId = "Collection_limit_binary",label = "Limit Collection Size?",value = F),
                     conditionalPanel(condition = 'input.Collection_limit_binary==true',
                                      radioButtons(inputId = "Collection_limit_method",label = "limitation method",choices = c("random sample","most relevant documents")),
                                      uiOutput(outputId = "Collection_limit_UI")
                     ),
                     withBusyIndicatorUI(
                     shinyBS::bsButton(inputId = "save_Collection",label = "Save Collection",icon = icon("save"),style = "default")
                     )
                    )
)
