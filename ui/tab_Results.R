tabPanel(icon=icon("list"),
         "Results",
         column(10,
                tags$div(
                  style = 'height:82vh; overflow-y:auto;',
                  shinyBS::bsButton(inputId = "Results_Reset",label = "reload",style = "success",size = "extra-small",icon=icon("refresh")),
                  tags$div(style='overflow-x:auto;',
                           box(
                             title = "Topic Models",
                             solidHeader = T,
                             collapsed = T,
                             collapsible = T,
                             width = 12,
                             div(
                               dataTableOutput(outputId = "Topic_Results"),
                               style="font-size:70%"
                             )
                           )
                  ),
                  box(
                    title = "Term Frequency Extraction",
                    solidHeader = T,
                    collapsed = T,
                    collapsible = T,
                    width = 12,
                    div(
                      dataTableOutput(outputId = "FE_Results"),
                      style="font-size:70%"
                    )
                  ),
                  box(
                    title = "Dictionary Frequency Extraction",
                    solidHeader = T,
                    collapsed = T,
                    collapsible = T,
                    width = 12,
                    div(
                      dataTableOutput(outputId = "DE_Results"),
                      style="font-size:70%"
                    )
                  ),
                  box(
                    title = "Classification",
                    solidHeader = T,
                    collapsed = T,
                    collapsible = T,
                    width = 12,
                    div(
                      dataTableOutput(outputId = "Classification_Results"),
                      style="font-size:70%"
                    )
                  ),
                  box(
                    title = "Cooccurrences",
                    solidHeader = T,
                    collapsed = T,
                    collapsible = T,
                    width = 12,
                    div(
                      DT::dataTableOutput(outputId = "Coocs_Results"),
                      style="font-size:80%"
                    )
                  ),
                  box(
                    title = "Context Volatility",
                    solidHeader = T,
                    collapsed = T,
                    collapsible = T,
                    width = 12,
                    div(
                      dataTableOutput(outputId = "Volat_Results"),
                      style="font-size:70%"
                    )
                  ),
                  box(
                    title = "Sentiment Analysis",
                    solidHeader = T,
                    collapsed = T,
                    collapsible = T,
                    width = 12,
                    div(
                      dataTableOutput(outputId = "Senti_Results"),
                      style="font-size:70%"
                    )
                  ),
                  # box(
                  #   title = "Factorial Analysis",
                  #   solidHeader = T,
                  #   collapsed = T,
                  #   collapsible = T,
                  #   width = 12,
                  #   div(
                  #     dataTableOutput(outputId = "Facto_Results"),
                  #     style="font-size:70%"
                  #   )
                  # ),
                  box(
                    title = "Vector Space Representation",
                    solidHeader = T,
                    collapsed = T,
                    collapsible = T,
                    width = 12,
                    div(
                      dataTableOutput(outputId = "Vector_Results"),
                      style="font-size:70%"
                    )
                  ),
                  box(
                    title = "Document Deduplication",
                    solidHeader = T,
                    collapsed = T,
                    collapsible = T,
                    width = 12,
                    div(
                      dataTableOutput(outputId = "Deduplication_Results"),
                      style="font-size:70%"
                    )
                  ),
                  box(
                    title = "Keyword Extraction",
                    solidHeader = T,
                    collapsed = T,
                    collapsible = T,
                    width = 12,
                    div(
                      dataTableOutput(outputId = "Keyword_Extraction_Results"),
                      style="font-size:70%"
                    )
                  ),
                  box(
                    title = "Syntactic Parsing",
                    solidHeader = T,
                    collapsed = T,
                    collapsible = T,
                    width = 12,
                    div(
                      dataTableOutput(outputId = "Syntactic_Parsing_Results"),
                      style="font-size:70%"
                    )
                  )
                )
         )
)