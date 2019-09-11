tabPanel(icon=icon("edit"),
         "Dictionaries",
         shinyBS::bsButton(inputId = "Dict_create",label = "Create dictionary",icon = icon("add"),style = "info"),
         shinyBS::bsButton(inputId = "Dict_change",label = "Change dictionary",icon = icon("edit"),style = "primary"),
         shinyBS::bsButton(inputId = "Dict_delete",label = "Delete dictionary",icon = icon("trash"),style = "warning"),
         uiOutput("dict_save_ui"),
         tags$br(),
         tags$div(icon("info"))%>%
           bs_embed_popover(
             title ="You can add rows and columns by rightclicking in the table. The names for the columns can be set after clicking 'save' button.", placement = "right"
           ),
         rHandsontableOutput(outputId = "Dict_table_ui")
)