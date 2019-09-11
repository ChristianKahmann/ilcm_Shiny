tabPanel("Custom",
         textAreaInput(inputId = "custom_inputtext",label = "custom input",value = "http://0.0.0.0:3842/solr/iLCM/select?q=*:*",rows=3),
         shinyBS::bsButton(inputId = "custom_action",label = "Search",icon = icon("search"),style = "default")
)
