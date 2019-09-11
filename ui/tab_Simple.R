tabPanel("Simple",
         tags$div("Simple search. Terms may be combined with + # -",style="font-size:75%"),
         tags$div("+ = AND",style="font-size:75%"),
         tags$div("# = OR",style="font-size:75%"),
         tags$div("- = NOT",style="font-size:75%"),
         shinyTypeahead::typeaheadInput(inputId = "simple_inputtext",label = "Keyword",value = '',choices = c(),items = 10,minLength = 1),
         #checkboxInput(inputId = "simple_check",label = "Use raw text"),
         withBusyIndicatorUI(
           shinyBS::bsButton(inputId = "simple_action",label = "Search",icon = icon("search"),style = "default")
         )
)
