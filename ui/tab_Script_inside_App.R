tabPanel("Script inside the App",icon=icon("terminal"),
         aceEditor(outputId = "R_Script_iA",showLineNumbers = T,mode = "r",theme = "monokai",autoComplete = "enabled",value = "",fontSize = 14,highlightActiveLine = T),        
         bsButton("eval_script_iA", "Run",style = "primary",icon = icon("play")),
         verbatimTextOutput(outputId = "ace_script_iA")
)