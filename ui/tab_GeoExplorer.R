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
                             textOutput(outputId = "textWithSelectedCollection"),
                             textOutput(outputId = "metaData_dataLoaded_output")
                             
                             
                             
                          )
                        )
                        
                      )
             ),
             tabPanel("Filtering & Results", fluid = TRUE
                      #textOutput("TEST_TEXT")
             )
             
           )
         )
)

