tabPanel("Categories",
         textInput(inputId = "categories_number",label = "number of classes:",value = 5,width = 100),
         htable("tbl"),
         #update button
         actionButton("actionButtonID","apply table edits"),
         #to show saved edits
         tableOutput("tblNonEdit")
)