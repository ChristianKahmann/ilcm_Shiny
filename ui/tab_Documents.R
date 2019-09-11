tabPanel("Documents",
         column(10,
                div(style = 'height: 80vh; overflow-y: auto;',
                    fluidRow(style="margin-left:0px;margin-right:0px",
                             column(12,
                                    column(2,
                                           uiOutput(outputId = "Doc_Num_Found")),
                                    column(2,
                                           shinyBS::bsButton(inputId = "Doc_Search_Sub",label = "Search inside Collection",icon = icon("search"),style = "info")
                                    ),
                                    column(2,
                                           uiOutput(outputId = "Doc_Search_Sub_Save_Name_UI")
                                    ),
                                    column(2,
                                           uiOutput(outputId = "Doc_Search_Sub_Save_UI")
                                    ),
                                    column(4,
                                           column(1,offset=9,
                                                  shinyBS::bsButton(inputId = "Doc_Search_results_reset_delete",label=NULL,icon = icon("refresh"),style = "primary",size = "small")
                                           )
                                    ),
                                    tags$div(
                                      uiOutput("delete_documents_from_colelction_button_ui"),
                                      align="right",
                                      title="Delete documents from collection that are not marked with 'keep'."
                                    )
                             )
                    ),
                    fluidRow(style="margin-left:0px;margin-right:0px",
                                              DT::dataTableOutput("collection_documents")
                                                 ),
                    fluidRow(style="margin-left:0px;margin-right:0px",
                             column(12,
                                    uiOutput("Documents_row")
                             )
                    )
                )
         )
)