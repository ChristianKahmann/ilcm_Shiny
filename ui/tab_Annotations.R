tabPanel("Annotations",title = "Annotations",
         div(style = 'height: 81vh; overflow-y: auto; overflow-x:hidden;',        
             downloadButton(outputId = "download_token",label = "Download Annotations"),
             actionButton(inputId = "update_annotations",label = "Update"),
             DT::dataTableOutput(outputId = "annotations")
         )
)