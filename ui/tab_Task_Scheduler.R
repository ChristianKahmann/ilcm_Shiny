tabPanel("Task Scheduler",
         tags$div(style='height: 82vh; overflow-y: auto; padding-left:10px;',
                  box(solidHeader = T,width = 12,style="padding-left:30px", 
                      tags$h4("Standard parameters"),
                      fluidRow(style="margin-left:0px;margin-right:0px",
                               column(2,
                                      uiOutput(outputId = "Task_Scheduler_Collection")
                               ),
                               column(2,
                                      selectizeInput(inputId = "analysis_selected",label = tags$span("Analysis:",
                                                                                                     bsButton("analysis_help", label = "Help", icon = icon("info-circle"), style = "info", size = "extra-small")),
                                                     choices = c("Cooccurrence Analysis"="Cooccurrence_Analysis",
                                                                 "Frequency Extraction"="Frequency_Extraction",
                                                                 "Volatility Analysis"="Volatility_Analysis",
                                                                 "Topic Model"="Topic_Model",
                                                                 "Dictionary Extraction"="Dictionary_Extraction",
                                                                 "Classification"="Classification",
                                                                 "Sentiment Analysis"="Sentiment_Analysis",
                                                                 "Document Deduplication"="Document_Deduplication",
                                                                 "Keyword Extraction"="Keyword_Extraction", 
                                                                 "Syntactic Parsing"="Syntactic_Parsing",
                                                                 #"Factorial Analysis"="Factorial_Analysis",
                                                                 "Vector Space Representation"="Vector_Space_Representation",
                                                                 "Download Collection as CSV"= "Download Collection as CSV",
                                                                 "Save Collection as token object"="Save Collection as token object",
                                                                 "Save Collection as meta object"="Save Collection as meta object"),
                                                     selected = "Cooccurrence Analysis",multiple = F)
                                      
                               ),
                               column(1,
                                      checkboxInput(inputId = "use_custom_script",label = "use custom script?",value = FALSE)
                               ),
                               column(3,
                                      conditionalPanel(condition = 'input.use_custom_script==true',
                                                       uiOutput(outputId = "custom_script_options_UI")
                                      )
                               )
                      ),
                      conditionalPanel(condition="input.analysis_selected!='Download Collection as CSV' && input.analysis_selected!='Save Collection as token object' && 
                                       input.analysis_selected!='Save Collection as meta object'",
                                       conditionalPanel(condition = "input.analysis_selected=='Cooccurrence_Analysis'",
                                                        uiOutput(outputId = "Analysis_Parameter_CA")            
                                       ),
                                       conditionalPanel(condition = "input.analysis_selected=='Frequency_Extraction'",
                                                        uiOutput(outputId = "Analysis_Parameter_FE")            
                                       ),
                                       conditionalPanel(condition = "input.analysis_selected=='Volatility_Analysis'",
                                                        uiOutput(outputId = "Analysis_Parameter_VA")            
                                       ),
                                       conditionalPanel(condition = "input.analysis_selected=='Topic_Model'",
                                                        uiOutput(outputId = "Analysis_Parameter_TM")            
                                       ),
                                       conditionalPanel(condition = "input.analysis_selected=='Dictionary_Extraction'",
                                                        uiOutput(outputId = "Analysis_Parameter_DE")       
                                       ),
                                       conditionalPanel(condition = "input.analysis_selected=='Classification'",
                                                        uiOutput(outputId = "Analysis_Parameter_CL")       
                                       ),
                                       conditionalPanel(condition = "input.analysis_selected=='Sentiment_Analysis'",
                                                        uiOutput(outputId = "Analysis_Parameter_SA")       
                                       ),
                                       conditionalPanel(condition = "input.analysis_selected=='Factorial_Analysis'",
                                                        uiOutput(outputId = "Analysis_Parameter_FA") 
                                       ),
                                       conditionalPanel(condition = "input.analysis_selected=='Vector_Space_Representation'",
                                                        uiOutput(outputId = "Analysis_Parameter_VS") 
                                       ),
                                       conditionalPanel(condition = "input.analysis_selected=='Document_Deduplication'",
                                                        uiOutput(outputId = "Analysis_Parameter_DD") 
                                       ),
                                       conditionalPanel(condition = "input.analysis_selected=='Keyword_Extraction'",
                                                        uiOutput(outputId = "Analysis_Parameter_KE") 
                                       ),
                                       conditionalPanel(condition = "input.analysis_selected=='Syntactic_Parsing'",
                                                        uiOutput(outputId = "Analysis_Parameter_SP") 
                                       ),
                                       a("Show Script", onclick = "openTab('Scripts')"),
                                       tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")
                                       )#,
                                       # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       #                  p("Starting..."),
                                       #                  tags$img(src="busy.gif"))
                      ),
                      conditionalPanel(condition = "input.analysis_selected=='Download Collection as CSV'",
                                       column(2,
                                              numericInput(inputId = "download_batch_size",label = "Batch size (number of documents per csv-file)",value = 1000,min = 1,max = 100000)
                                       ),
                                       tags$br(),
                                       uiOutput(outputId = "Analysis_Parameter_DL")       
                      ),
                      conditionalPanel(condition = "input.analysis_selected=='Save Collection as token object'",
                                       shinyBS::bsButton(inputId = "start_token_saving",label = "Save token object to results folder"),
                                       busyIndicator(text = "preparing data...")
                      ),
                      conditionalPanel(condition = "input.analysis_selected=='Save Collection as meta object'",
                                       shinyBS::bsButton(inputId = "start_meta_saving",label = "Save meta object to results folder"),
                                       busyIndicator(text = "preparing data...")
                      )
                  )
         )
)