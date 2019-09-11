tabPanel(value = "Document View2",title = "Document View",
         box(width = 2,div(style = 'height: 80vh; overflow-y: auto;',
                           tabsetPanel(type = "pills",
                                       #tags$h3(tags$b("Metadata")),
                                       tabPanel("Metadata",
                                                tags$hr(),
                                                uiOutput(outputId = "Doc_DV_metadata_UI")
                                       ),
                                       tabPanel("Part of Speech Tags",
                                                tags$hr(),
                                                uiOutput("Doc_DV_POS")
                                       ),
                                       tabPanel("Named Entity Tags",
                                                tags$hr(),
                                                uiOutput("Doc_DV_Entity")
                                       )
                           ),
                           checkboxInput(inputId = "Doc_Doc_View_paragraph",label = "show paragraphs?",value = T)
         )
         ),
         box(width=8,
             div(style = 'height: 80vh; overflow-y: auto;',
                 column(11,
                 tags$h1(textOutput(outputId = "Doc_DV_title"))
                 ),
                 column(1,
                        uiOutput("Doc_DV_documentwide_annotations") 
                        ),
                 annotateTextComponent2()
             )
         ),
         column(2,
                div(style = 'height: 80vh; overflow-y: auto;',
                    conditionalPanel("input.coll=='Document View2'",
                                     box(width=NULL,title = tags$h4("Annotations",style="color:white"),solidHeader = TRUE,status = "danger",collapsible = T,
                                         
                                         uiOutput("Doc_DV_Annotation_Schemes"),
                                         uiOutput("annotationComponents2"),
                                         column(
                                           width = 5,
                                           actionButton(inputId = "Doc_save_annotations",label = "Save Annotations",styleclass = "info"),
                                           busyIndicator(text = "",wait = 0)
                                         )
                                     ),
                                     box(width=NULL,title=tags$h4("Made Annotations",style="color:black"),solidHeader = F,status = "danger",collapsible = T,
                                         uiOutput(outputId = "Doc_made_annotations"),
                                         shinyjqui::jqui_droppable(tags$div(id="Doc-trash","Remove",icon=icon("trash")),options = list(
                                           accept = '.anno-box', # jQuery selector to define which draggable element to monitor. Accept anything if not set.
                                           classes = list(
                                             `ui-droppable-active` = 'ui-state-focus', # change class when draggable element is dragging
                                             `ui-droppable-hover` = 'ui-state-highlight' # change class when draggable element is dragging over
                                           ),
                                           drop = JS('function(event, ui){$(this).addClass("ui-state-active");
                                 Shiny.onInputChange(\"Doc_delete_annotation_box\",  Math.random());
                                 }'
                                           ) # a javascrip callback to change class when draggable element is dropped in
                                         ),operation = "destroy")
                                     )
                    )
                )
         )
)
