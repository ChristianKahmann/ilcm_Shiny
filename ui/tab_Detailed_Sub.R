tabPanel("Detailed",
         textOutput(outputId = "Det_text_Sub"),
         shinyTypeahead::typeaheadInput(inputId = "Det_inputtext_Sub",label = "Keyword",value = '',choices = c(),items = 10,minLength = 1),
         checkboxInput(inputId = "Det_check_Sub",label = "Use raw text"),
         uiOutput(outputId = "Det_von_Sub"),
         uiOutput(outputId = "Det_zu_Sub"),
         uiOutput(outputId = "Det_publication_Sub"),
         uiOutput(outputId = "Det_type_Sub"),
         uiOutput(outputId = "Det_section_Sub"),
         #uiOutput(outputId = "Det_author"),
         uiOutput(outputId = "Det_token_Sub"),
         withBusyIndicatorUI(
           shinyBS::bsButton(inputId = "Det_action_Sub",label = "Search",icon = icon("search"),style = "default")
         )
)