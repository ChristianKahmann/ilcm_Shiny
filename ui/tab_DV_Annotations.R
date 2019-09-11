conditionalPanel("input.expl=='Document View'",
                 box(width=NULL,title = tags$h4("Annotations",style="color:white"),solidHeader = TRUE,status = "danger",collapsible = T,
                     
                     uiOutput("DV_Annotation_Schemes"),
                     uiOutput("annotationComponents"),
                     column(
                       width = 5,
                       shinysky::actionButton(inputId = "save_annotations",label = "Save Annotations",styleclass = "info"),
                       busyIndicator(text = "",wait = 0)
                     )
                 ),
                 box(width=NULL,title=tags$h4("Made Annotations",style="color:black"),solidHeader = F,status = "danger",collapsible = T,
                     uiOutput(outputId = "made_annotations"),
                     shinyjqui::jqui_droppable(tags$div(id="trash","Remove"),options = list(
                       accept = '.anno-box', # jQuery selector to define which draggable element to monitor. Accept anything if not set.
                       classes = list(
                         `ui-droppable-active` = 'ui-state-focus', # change class when draggable element is dragging
                         `ui-droppable-hover` = 'ui-state-highlight' # change class when draggable element is dragging over
                       ),
                       drop = JS('function(event, ui){$(this).addClass("ui-state-active");
                                 Shiny.onInputChange(\"delete_annotation_box\",  Math.random());
                                 }'
                       ) # a javascrip callback to change class when draggable element is dropped in
                     ),operation = "destroy")
                 )
               
                 
)
