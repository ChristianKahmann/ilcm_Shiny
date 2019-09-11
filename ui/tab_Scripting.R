tabPanel("Scripting Analysis",icon=icon("subscript"),
         fluidRow(
           column(2,selectizeInput(inputId = "analysis_selected_script",label = "Analysis:",choices = c("Cooccurrence_Analysis","Frequency_Extraction",
                                                                                                        "Volatility_Analysis","Topic_Model","Dictionary_Extraction","Classification",
                                                                                                        "Sentiment_Analysis","Factorial_Analysis","Document_Deduplication","Keyword_Extraction"),
                                   options = list(maxItems=1))
           ),
           column(1,
                  checkboxInput(inputId = "script_use_custom_script",label = "custom script?",value = FALSE)
           ),
           column(2,
                  conditionalPanel(condition = 'input.script_use_custom_script==true',
                                   uiOutput(outputId = "script_custom_script_options_UI")
                  )
           ),
           column(2,
                  selectizeInput(inputId = "script_theme",label="Theme",choices=shinyAce::getAceThemes())
           )
         ),
         uiOutput(outputId = "script_UI")
         
)