#' panel for document view 
#' to set meta data visulization, entity-tags, POS-tags, show paragraph, etc.
#' (viewed on the left side in document view tab) 
tabPanel("Document View",type="tabs",
         div(style = 'height: 80vh; overflow-y: auto;',
             box(width=2,
                 tabsetPanel(type = "pills",
                             #tags$h3(tags$b("Metadata")),
                             tabPanel("Metadata",
                                      tags$hr(),
                                      uiOutput(outputId = "DV_metadata_UI")
                             ),
                             tabPanel("Part of Speech Tags",
                                      tags$hr(),
                                      uiOutput("DV_POS")
                             ),
                             tabPanel("Named Entity Tags",
                                      tags$hr(),
                                      uiOutput("DV_Entity")
                             )
                 ),
                 checkboxInput(inputId = "Doc_View_paragraph",label = "show paragraphs?",value = T)
             )
             ,
             box(width=10,
                 fluidRow(style="margin-left:0px;margin-right:0px",
                          column(11,
                                 tags$h1(textOutput(outputId = "DV_title"))
                          ),
                          column(1,
                                 uiOutput("DV_documentwide_annotations")  
                          )
                 ),
                 tags$hr(),
                 annotateTextComponent()
                 
             )
         )
)
