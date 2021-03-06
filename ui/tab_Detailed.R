#' create panel for explorer to select specific documents with the help of specific filters
#' input for keyword extraction
#' checkbox for the usage of raw data
#' input for time limitation (add specific start and end time)
#' input for other potential information that can be filtered from the used data (up to nine additional inputs)
#' input for number of tokens 
tabPanel("Detailed",title = "Detailed",
         textOutput(outputId = "Det_text"),
         shinyTypeahead::typeaheadInput(inputId = "Det_inputtext",label = "Keyword",value = '',choices = c(),items = 10,minLength = 1),
         checkboxInput(inputId = "Det_check",label = "Use raw text"),
         tags$div(tags$style(HTML( ".dropdown-menu{z-index:10000 !important;}"))),
          uiOutput(outputId = "Det_von"),
         uiOutput(outputId = "Det_zu"),
         selectizeInput(inputId =  "Det_mde1",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde2",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde3",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde4",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde5",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde6",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde7",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde8",label="",choices=NULL,multiple=T),
         selectizeInput(inputId =  "Det_mde9",label="",choices=NULL,multiple=T),
         uiOutput(outputId = "Det_token"),
         withBusyIndicatorUI(
           shinyBS::bsButton(inputId = "Det_action",label = "Search",icon = icon("search"),style = "default")
         )
)