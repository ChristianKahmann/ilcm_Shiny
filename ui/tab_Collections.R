tags$div(id="collections_table_div",style="border-radius:0; ",
         column(2,style='padding-right:0px; padding-left:0px;',
                box(title = tags$h3("Collections",style="color:white;"),solidHeader = T,status = "primary",width = 12,
                    shinyBS::bsButton(inputId = "collections_Reset",label = "reload",style = "success",size = "extra-small",icon=icon("refresh")),
                    tags$br(),
                    tags$div(style='overflow-x:auto; overflow-y:auto;font-size:11px;',
                             DT::dataTableOutput(outputId = "collections")
                    )
                )
         )
)