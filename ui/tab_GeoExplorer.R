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
                                               rHandsontableOutput('geocodingResult_config'),
                                               actionButton("config_apply","apply settings")
                                              )
                            )
                            
                            
                             
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
                            
                            # tabsetPanel(
                            #   tabPanel("Stats", fluid = T, 
                            #            # how many results
                            #            textOutput("metaData_numberOfResults1"),
                            #            textOutput("geocodingResult_numberOfResults1"),
                            #            
                            #            # stats meta data
                            #            h3("Stats meta data"),
                            #            plotlyOutput("metaData_stats_distributions_plots"),
                            #            tableOutput("metaData_stats_numeric_table"),
                            #            
                            #            # stats geocodingResult
                            #            h3("Stats GeocodingResult"),
                            #            plotlyOutput("geocodingResult_stats_distributions_plots"),
                            #            tableOutput("geocodingResult_stats_numeric_table")
                            #            
                            #            
                            #            
                            #   ),
                            #   tabPanel("Map", fluid = T, 
                            #            # how many results
                            #            textOutput("metaData_numberOfResults2"),
                            #            textOutput("geocodingResult_numberOfResults2"),
                            #            textOutput("geoDataToUse_numberOfResults"),
                            #            
                            #            # # map
                            #            leafletOutput(outputId="lmap"),
                            #            
                            #            # clicked marker
                            #            verbatimTextOutput("clickedMarker_infos"),
                            #            uiOutput("clickedMarkerAllOutput")
                            #            
                            #   )
                            # )
                            
                            
                            
                          )
                        ) 
                      )
                      
                      
             )
             
           )
         )
)

