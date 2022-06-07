#' annotation panel for downloading token or update annotations 
tabPanel(title = "Annotations",
         tabsetPanel(id = "annotations",type = "pills",
                     tabPanel(title = "Manual Annotations",
                              div(style = 'height: 81vh; overflow-y: auto; overflow-x:auto; padding-top:10px;',        
                                  downloadButton(outputId = "download_token",label = "Download Annotations"),
                                  shinyBS::bsButton(inputId = "update_annotations",label=NULL,icon = icon("sync"),style = "primary",size = "small"),
                                  DT::dataTableOutput(outputId = "annotations")
                              )
                     ),
                     tabPanel(title = "Active Learning Annotations",
                              div(style = 'height: 81vh; overflow-y: auto; overflow-x:auto; padding-top:10px;',    
                                  downloadButton(outputId = "download_annotations_active_learning",label = "Download Annotations"),
                                  shinyBS::bsButton(inputId = "update_annotations_active_learning",label=NULL,icon = icon("sync"),style = "primary",size = "small"),
                                  DT::dataTableOutput(outputId = "annotations_active_learning")
                              )
                     )
         )
)