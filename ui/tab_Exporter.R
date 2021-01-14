#' panel to export collections 
tabPanel("Exporter",
         tabBox(
           id = "tabBox_Export",
           width = 12,
           tabPanel("Collections",
                    tags$div(style="height:75vh; overflow-y:auto;",
                             selectInput(inputId = "export_collection",label = "Collections:",choices = stringr::str_remove(string = list.files("collections/collections/"),
                                                                                                                            pattern = ".RData"),multiple = F,width = "25%"),
                             radioGroupButtons(inputId = "export_coll_format",label = "Download as:",choices = c("DataFrame","iLCM Collection Format RData"),selected = "RData",checkIcon = list(
                               yes = tags$i(class = "fa fa-check-square", 
                                            style = "color: steelblue"),
                               no = tags$i(class = "fa fa-square-o", 
                                           style = "color: steelblue"))
                             ),
                             tags$br(),
                             tags$hr(),
                             conditionalPanel(condition = "input.export_coll_format=='iLCM Collection Format RData'",
                                              shiny::downloadButton(outputId = "download_button_coll_ilcm",label = "Download iLCM Collection Format")
                             ),
                             conditionalPanel(condition = "input.export_coll_format=='DataFrame' ",
                                              column(2,
                                                     numericInput(inputId = "export_download_batch_size",label = "Batch size (number of documents per csv-file)",value = 1000,min = 1,max = 100000)
                                              ),
                                              tags$br(),
                                              uiOutput(outputId = "Export_Analysis_Parameter_DL")   
                             )
                    )
           ),
           tabPanel("Results",
                    tags$div(style="height:75vh; overflow-y:auto;",
                             radioGroupButtons(inputId = "export_results_analysis",label = "Analysis",choices = c("classification"="classification",
                                                                                                                  "cooccurrence analysis"="cooccurrence-analysis",
                                                                                                                  "dictionary extraction"="dictionary-extraction",
                                                                                                                  "document deduplication"="document-deduplication",
                                                                                                                  "frequency extraction"="frequency-extraction",
                                                                                                                  "sentiment analysis"="sentiment_analysis",
                                                                                                                  "topic model"="topic-model",
                                                                                                                  "vector space representation"="vector-space-representation",
                                                                                                                  "volatility analysis"="volatility-analysis"
                             ),checkIcon = list(
                               yes = tags$i(class = "fa fa-check-square", 
                                            style = "color: steelblue"),
                               no = tags$i(class = "fa fa-square-o", 
                                           style = "color: steelblue"))),
                             uiOutput(outputId = "export_results_resultset_ui"),
                             tags$h4("Regular result files"),
                             DT::dataTableOutput(outputId = "export_results_files_ui"),
                             tags$h4("Special files to extract"),
                             uiOutput(outputId = "export_results_extra")
                    )                            
           ),
           tabPanel("Annotations",
                    tags$div(style="height:75vh; overflow-y:auto;",
                             shinyBS::bsButton(inputId = "export_update_annotations",label=NULL,icon = icon("refresh"),style = "primary",size = "small"),
                             DT::dataTableOutput(outputId = "export_annotations")
                    )
           ),
           tabPanel(
             "REFI-Export",
             shinyBS::bsButton(inputId = "refi_export_reset",label = "reload",style = "success",size = "extra-small",icon=icon("refresh")),
             tags$div(style="height:75vh; overflow-y:auto;",
                      box(
                        title = "Export REFI-QDA Project",
                        width = 8,
                        panel(
                          box(
                            width = 7,
                            tags$div(
                              selectInput(
                                inputId = "refi_export_select_collection",
                                label = "Collection",
                                choices = list("")
                              ),
                              DT::dataTableOutput(outputId = "refi_export_collection_table")
                            )
                          ),
                          box(
                            width = 5,
                            tags$div(
                              textOutput(outputId = "refi_export_detected_annotation_schemes_label"),
                              DT::dataTableOutput(outputId = "refi_export_detected_annotation_schemes_table")
                            )
                          )
                        ),
                        panel(
                          tags$div(
                            selectInput(
                              inputId = "refi_export_select_analysis",
                              label = "Analysis",
                              choices = c("Topic Model", "Classification")
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "input.refi_export_select_analysis == 'Topic Model'",
                          box(
                            width = 6,
                            DT::dataTableOutput(outputId = "refi_export_topic_model_table")
                          ),
                          box(
                            width = 6,
                            DT::dataTableOutput(outputId = "refi_export_topic_model_number_of_topics_table")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.refi_export_select_analysis == 'Classification'",
                          box(
                            width = 6,
                            tags$div(
                              DT::dataTableOutput(outputId = "refi_export_classification_table")
                            )
                          )
                        )
                      ),
                      box(
                        title = "Export REFI-QDA Codebook",
                        width = 4,
                        panel(
                          tags$div(
                            DT::dataTableOutput(outputId = "refi_export_table_annotation_scheme")
                          )
                        ),
                        panel(
                          tags$div(
                            conditionalPanel(
                              condition = "input.selectedAnnotationScheme != ''",
                              uiOutput("annotation_scheme_list")
                            )
                          )
                        )
                      )
             )
           ),
           tabPanel("All Files",
                    tags$div(style="height:75vh; overflow-y:auto;",
                             tags$br(),
                             shinyFilesButton('file', 'File select', 'Please select a file',multiple = T),
                             verbatimTextOutput('filepaths'),
                             tags$br(),
                             uiOutput("download_export_all_ui")
                             
                             
                    )
           )
         )
)


