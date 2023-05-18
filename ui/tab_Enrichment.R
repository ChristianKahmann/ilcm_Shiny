tagList(column(width=12,style = 'height: 92vh; overflow-y: auto; overflow-x:auto; padding-right:0px;',
               column(6,
                      tags$h3("Data Selection:"),
                      switchInput(inputId="enrichment_upload_new_csv",label = "Upload new Transcript or use existing Interviews",value = T,onLabel = "Upload new Transcript",offLabel = "Existing Collection",size = "normal",offStatus = "success",onStatus = "primary"),
                      prettyCheckbox(inputId = "enrichment_include_ner_tags","Include NER Tags for Export?", value=T,status = "primary"),
                      conditionalPanel(condition='input.enrichment_upload_new_csv==true',
                                       fileInput(inputId = "enrichment_new_transcript",label = "Upload new Transcript",accept = "csv|zip", width="30%")
                      ),
                      conditionalPanel(condition='input.enrichment_upload_new_csv==false',
                                       column(3,
                                              selectInput(inputId="enrichment_collection",label = "Choose Collection",
                                                          choices=stringr::str_remove_all(string = list.files("collections/collections/"),pattern = ".RData"),
                                                          multiple=F)
                                       ),
                                       column(3,
                                              withBusyIndicatorUI(
                                                bsButton(inputId = "enrichment_load_collection",label = "Use chosen collection",icon = icon("load"),style = "primary")
                                              )
                                       )
                      )
               ),
               column(6,
                      tags$h3("Topic Model Selection:"),
                      column(6,
                             shinyBS::bsButton(inputId = "enrichment_reload_avail_topics",label = "reload",style = "success",size = "extra-small",icon=icon("sync")),
                             uiOutput("enrichment_topic_UI")
                      ),
                      column(3,
                             withBusyIndicatorUI(
                               bsButton(inputId = "enrichment_import_topic","Import Topic Model",icon = icon("upload"),style = "primary")
                             )
                      )
               ),
               box(title = "Data Basis:",solidHeader = T,collapsible = T,status = "primary",width = 12,collapsed = T,
                   DT::dataTableOutput(outputId = "enrichment_data_import_table")
               ),
               tags$hr(),
               box(title = "Topic Model:",status = "primary",solidHeader = T,collapsible = T,width = 12,collapsed = T,
                   column(12,
                          uiOutput("enrichment_topic_control_UI")
                   )
               ),
               box(title = "Apply Model to Data:",solidHeader = T,width = 12,status = "primary",collapsible = T,collapsed = F,
                   tags$h4("Configuration:"),
                   uiOutput("enrichment_configuration_UI"),
                   withBusyIndicatorUI(
                     bsButton(inputId = "enrichment_start_enrichment",label = "Start Enrichment",icon = icon("play"),style = "primary")
                   ),
                   uiOutput("enrichment_results_UI")
               ),
               box(title = "Result Export:",solidHeader = T,width = 12,collapsible = T,status = "primary",collapsed = F,
                   uiOutput("enrichment_download_UI")
               )
)
)




