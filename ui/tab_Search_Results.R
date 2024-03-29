#' panel for explorer page, lists certain documents of loaded database
tabPanel("Search Results",
         shinyjs::useShinyjs(),
         div(style = 'height: 80vh; overflow-y: auto;',
             column(12,
                    column(6,
                           textOutput(outputId = "SR_Num_Found")
                    ),
                    column(6,
                           column(1,offset=11,
                                  shinyBS::bsButton(inputId = "Search_results_reset_delete",label=NULL,icon = icon("sync"),style = "primary",size = "small")
                           )
                    ),
                    DT::dataTableOutput(outputId = "search_results_datatable"),           
                    uiOutput(outputId = "SR_row")
             )
         )
)
