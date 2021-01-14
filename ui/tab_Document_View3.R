#' panel for document view
#' show document wide annotations, etc.
#' (viewed on right side)
tabPanel(value = "Document View3",title = "Document View",
         box(width = 2,div(style = 'height: 80vh; overflow-y: auto;',
                           tabsetPanel(type = "pills",
                                       #tags$h3(tags$b("Metadata")),
                                       tabPanel("Metadata",
                                                tags$hr(),
                                                uiOutput(outputId = "Anno_DV_metadata_UI")
                                       ),
                                       tabPanel("Part of Speech Tags",
                                                tags$hr(),
                                                uiOutput("Anno_DV_POS")
                                       ),
                                       tabPanel("Named Entity Tags",
                                                tags$hr(),
                                                uiOutput("Anno_DV_Entity")
                                       )
                           ),
                           checkboxInput(inputId = "Anno_Doc_View_paragraph",label = "show paragraphs?",value = T)
         )
         ),
         box(width=10,
             div(style = 'height: 80vh; overflow-y: auto;',
                 fluidRow(style="margin-left:0px;margin-right:0px",
                          column(11,
                                 tags$h1(textOutput(outputId = "Anno_DV_title"))
                          ),
                          column(1,
                                 uiOutput("Anno_DV_documentwide_annotations") 
                          )
                 ),
                 tags$hr(),
                 annotateTextComponent3()
             )
         )
         
)


