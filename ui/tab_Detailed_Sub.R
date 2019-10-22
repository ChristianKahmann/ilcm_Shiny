tabPanel("Detailed",
         textOutput(outputId = "Det_text_Sub"),
         shinyTypeahead::typeaheadInput(inputId = "Det_inputtext_Sub",label = "Keyword",value = '',choices = c(),items = 10,minLength = 1),
         checkboxInput(inputId = "Det_check_Sub",label = "Use raw text"),
         uiOutput(outputId = "Det_von_Sub"),
         uiOutput(outputId = "Det_zu_Sub"),
         selectizeInput(inputId =  "Det_mde1_Sub",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde2_Sub",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde3_Sub",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde4_Sub",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde5_Sub",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde6_Sub",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde7_Sub",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde8_Sub",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde9_Sub",label="",choices=NULL,multiple=T),
         uiOutput(outputId = "Det_token_Sub"),
         withBusyIndicatorUI(
           shinyBS::bsButton(inputId = "Det_action_Sub",label = "Search",icon = icon("search"),style = "default")
         )
)