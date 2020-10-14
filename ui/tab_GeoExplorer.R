# ui code
tabPanel("GeoExplorer", fluid = TRUE,
         
         # div(style = 'height: 80vh; overflow-y: auto;',
         #     uiOutput(outputId = "GeoExplorer_UI_Output")
         # )
         tagList(
           tabsetPanel(
             tabPanel("Configuration", fluid = TRUE,
                      
                      sidebarLayout(
                        sidebarPanel(h5("", width=2),
                                     selectInput(inputId="geoExplorer_selectedCollection", label=h4("Select collection"), choices= availableCollections, multiple = F, selected = availableCollections[5]),
                                     selectInput(inputId="geocodingResult", label=h4("Select frequency extraction results"), choices= availableGeocodingResults, multiple = F),
                                     actionButton(inputId = "loadDataForCollection", "Load data")
                                     
                        ),
                        mainPanel(
                          fluidRow(
                            # leafletOutput(outputId="lmap"),
                             #textOutput(outputId = "textWithSelectedCollection"),
                             #textOutput(outputId = "metaData_dataLoaded_output")
                             #,
                            conditionalPanel(condition = "!output.dataLoaded",
                                             h3("Please first select the data to load!")),
                            
                            conditionalPanel(condition = "output.dataLoaded",
                                             wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                                               h3("Configuration of meta data from documents"),
                                               rHandsontableOutput('metaData_config'),
                                               h3("Configuration of geocodingResult"),
                                               rHandsontableOutput('geocodingResult_config')
                                              ),
                                             actionButton("config_apply","apply settings")
                                             
                            )
                            
                            
                             
                          )  
                          
                        )
                        
                      )
             ),
             tabPanel("RegEx", fluid = TRUE,
                      
                        fluidRow(
                          conditionalPanel(condition = "output.dataLoaded",
                                           wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                                                     h3("Configuration of regular expression to search in text"),
                                                     h5("For more information on regular expressions see https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html"),
                                                     h5("The default regEx shown here ('[0-9]{1,3}(,[0-9]{3})*(\\.[0-9]+)?') extracts numbers - with optional decimal separator using a dot ('.') and optional thousands separator comma (',')."),
                                                     textInput(inputId = "regexInput", label = "regular expression to apply", value = "[0-9]{1,3}(,[0-9]{3})*(\\.[0-9]+)?"),
                                                     
                                                     h4("params for regex matching"),
                                                     column(6,checkboxInput(inputId = "regex_includeTitleForRegExMatching", label = "include title for regex matching (additionally to text)", value = F)),
                                                     column(6, 
                                                            checkboxInput(inputId = "regex_tranformToNumeric", label = "transformToNumeric", value = TRUE),
                                                            textInput(inputId = "regex_separatorForThousandsUsedInTextData", label = "separator for thousands used in text data", value = ","),
                                                            textInput(inputId = "regex_separatorForDecimalUsedInTextData", label = "separator for decimal used in text data", value = ".")
                                                     ),
                                                     br(),
                                                     h4("params for output display here"),
                                                     column(4, numericInput(inputId = "regex_showMatches_topX", label = "Show matches of the first ", value = 20)),
                                                     column(4, radioButtons(inputId = "regex_showMatches_typeDocsOrDocsWithMatches", choices = c("documents","documents with matches"), selected = "documents", label = NULL)),
                                                     column(4, textInput(inputId = "regex_separatorForMatchesDisplay", label = "separate matches by", value = " - ")),
                                                     
                                                     h4("params for showing results under tab filtering & results"),
                                                     checkboxInput(inputId = "regex_filterAndResults_showDistributions", label = "show distributions", value = TRUE),
                                                     checkboxInput(inputId = "regex_filterAndResults_showNumericInfos", label = "show numeric infos (will be only performed if transformToNumeric is selected, else disregarded)", value = TRUE),
                                                     
                                                     
                                                     actionButton(inputId = "performRegexMatching","APPLY and perform regex matching"),
                                                     actionButton(inputId = "resetRegexMatching","RESET (do not apply any regex)"),
                                                     br(),
                                                     h4("Regex results"),
                                                     br(),
                                                     textOutput(outputId = "regExInfo"),
                                                     br(),
                                                    rHandsontableOutput('regexResults')
                                           )
                          )
                        )
                        
             ),
             tabPanel("Filtering & Results", fluid = TRUE,
                     
                      conditionalPanel(condition = "!output.dataLoaded",
                                       h3("Please first select the data to load under Configuration!")),
                      
                      conditionalPanel(condition = "output.dataLoaded",
                        sidebarPanel(h5("", width=2),
                                     
                                     tabsetPanel(
                                       tabPanel("Meta Data",
                                                wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 800px",
                                                  h3("filter based on the meta data of the documents"),
                                                  br(),
                                                  uiOutput("selectInputListForMetaData")
                                                )
                                        ),
                                       tabPanel("Geocoding Result",
                                                wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 800px",
                                                  h3("filter based on the found locations within documents"),
                                                  br(),
                                                  uiOutput("selectInputListForGeocodingResult")
                                                )
                                       )
                                     )
                                     
                        ),
                        mainPanel(
                          fluidRow(
                            
                            tabsetPanel(
                              wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 800px",
                                tabPanel("Stats", fluid = T,
                                         # how many results
                                         textOutput("metaData_numberOfResults1"),
                                         textOutput("geocodingResult_numberOfResults1"),
  
                                         # stats meta data
                                         h3("Stats meta data"),
                                         uiOutput("metaData_stats_distributions_plots"),
                                         tableOutput("metaData_stats_numeric_table"),
  
                                         # stats geocodingResult
                                         h3("Stats GeocodingResult"),
                                         uiOutput("geocodingResult_stats_distributions_plots"),
                                         tableOutput("geocodingResult_stats_numeric_table"),
  
                                         # stats regexData
                                         h3("Stats RegEx Data"),
                                         uiOutput("regexData_stats_distributions_plots"),
                                         tableOutput("regexData_stats_numeric_table")
                                         
  
                                )
                              ,
                                tabPanel("Map", fluid = T,
                                         # how many results
                                         textOutput("metaData_numberOfResults2"),
                                         textOutput("geocodingResult_numberOfResults2"),
                                         textOutput("geoDataToUse_numberOfResults"),

                                         # # map
                                         leafletOutput(outputId="lmap"),

                                         # clicked marker
                                         verbatimTextOutput("clickedMarker_infos"),
                                         uiOutput("clickedMarkerAllOutput")

                                )
                               )
                            )
                            
                            
                            
                          )
                        ) 
                      )
                      
                      
             )
             
           )
         )
)

