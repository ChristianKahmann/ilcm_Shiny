#' panel to select an corpus from data sets
tabPanel("Corpus Selection", 
         selectInput(inputId = "dataset",label = "which corpus?",choices = datasets)
)

