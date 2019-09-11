tabPanel("Time Series",
         tags$div(style = 'height: 82vh; overflow:auto; position:relative; width:100%;',
                  tags$div(id="TS_config",sytle='width=100%',
                           column(8,
                                  column(2,
                                         selectInput(inputId = "TS_timeintervall",choices = c("day","month","year"),multiple = F,selected = "month",label=NULL)
                                  ),
                                  column(3,
                                         uiOutput(outputId = "TS_memory")
                                  ),
                                  column(1,
                                         actionButton(inputId = "TS_delete_memory",label = "delete")
                                  ),
                                  column(1,
                                         actionButton(inputId = "TS_reset",label = "reset")
                                  ),
                                  column(1,
                                         downloadButton(outputId = "TS_download_memory",label = "csv")
                                  )
                           ),
                           column(1,offset=3,
                                  radioButtons(inputId = "TS_rel_abs",choices = c("relative","absolute"),selected = "absolute",label=NULL)
                           )
                  ),
                  tags$div(style='width:100%;',
                           column(12,
                                  plotlyOutput(outputId = "TS_plot") %>% withSpinner(type = 7)
                           )
                           
                  ),
                  tags$div(style='width:100%;',
                           column(10,offset=1,
                                  tags$h2("Calender Heatmap"),
                                  uiOutput(outputId = "TS_calender") %>% withSpinner(type = 7)
                           )
                  )                
                  
         )
)
