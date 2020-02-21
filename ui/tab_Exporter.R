tabPanel("Exporter",
         tabBox(
           id = "tabBox_Export",
           width = 12,
           tabPanel("Collections",
                    tags$div(style="height:75vh; overflow-y:auto;",
                             selectInput(inputId = "export_collection",label = "Collections:",choices = stringr::str_remove(string = list.files("collections/collections/"),
                                                                                                                            pattern = ".RData"),multiple = F,width = "25%"),
                             radioGroupButtons(inputId = "export_coll_format",label = "Download as:",choices = c("RData","csv","refi"),selected = "RData",checkIcon = list(
                               yes = tags$i(class = "fa fa-check-square", 
                                            style = "color: steelblue"),
                               no = tags$i(class = "fa fa-square-o", 
                                           style = "color: steelblue"))
                             ),
                             tags$br(),
                             tags$hr(),
                             conditionalPanel(condition = "input.export_coll_format=='RData'",
                                              downloadButton("download_export_coll_RData","Download as RData")
                             ),
                             conditionalPanel(condition = "input.export_coll_format=='csv'",
                                              column(2,
                                                     numericInput(inputId = "download_batch_size",label = "Batch size (number of documents per csv-file)",value = 1000,min = 1,max = 100000)
                                              ),
                                              tags$br(),
                                              uiOutput(outputId = "Export_Analysis_Parameter_DL")   
                             ),
                             conditionalPanel(condition = "input.export_coll_format=='refi'",
                                              tags$div("tba")
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


