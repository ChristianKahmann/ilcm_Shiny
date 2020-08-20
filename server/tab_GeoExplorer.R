# server code

library(shiny)
library(leaflet)
library(htmltools)
library(shinyWidgets)
library(dplyr)
library("xtable")
library(ggplot2)
library(ggplotify)
library(gridExtra)
library(cowplot)
library(plotly)
#######################
# general config
#######################
# get available collections
folderpathCollections <- "collections/collections/"
availableCollectionFiles = list.files(path = folderpathCollections)
availableCollections <- lapply(X = availableCollectionFiles, FUN = function(x) {result <- str_split(x, pattern = ".RData")[[1]][1]})

# load available geoResults
folderpathGeoCodingResults <- "collections/results/geocoding/"
availableGeocodingResults = list.files(path = folderpathGeoCodingResults)
#availableGeocodingResults <- lapply(X = availableGeoCodingFiles, FUN = function(x) {result <- str_split(x, pattern = ".RData")[[1]][1]})

# TODO: include option: load geolocation from meta data / from georesults

#######################
# UI
#######################

# output$GeoExplorer_UI_Output<-renderUI({
#   
#   tagList(
#     tabsetPanel(
#             tabPanel("Configuration", fluid = TRUE,
#                     
#                     sidebarLayout(
#                       sidebarPanel(h5("", width=2),
#                                    selectInput(inputId="geoExplorer_selectedCollection", label=h4("Select collection"), choices= availableCollections, multiple = F, selected = availableCollections[1]),
#                                    selectInput(inputId="geocodingResult", label=h4("Select frequency extraction results"), choices= availableGeocodingResults, multiple = F),
#                                    actionButton(inputId = "loadDataForCollection", "Load data")
#                                    
#                                    
#                                    
#                       ),
#                       mainPanel(
#                         fluidRow(
#                           # leafletOutput(outputId="lmap"),
#                            textOutput(outputId = "textWithNumberOfResults"),
#                            textOutput("TEST_TEXT")
#                           
#                         )
#                       )
#                         
#                     )
#             ),
#             tabPanel("Filtering & Results", fluid = TRUE
#                     # textOutput("TEST_TEXT")
#             )
# 
#         )
#   )
# })

############
# server
##############
dataLoaded <- F
metaData <- NULL
geocodingResult <- NULL

output$TEST_TEXT <- reactive({
  text <- paste("There are many results.")
})

output$textWithSelectedCollection <- reactive({
  selectedCollection <- input$geoExplorer_selectedCollection
  text <- paste("selected collection: ", selectedCollection, ".")
})

################
# select collection & geocoding result
#################
selectedCollection <- reactive({
  selectedCollection <- input$geoExplorer_selectedCollection
  if(is.null(selectedCollection)){ # needed because observe is executed at the very beginning where input$geoExplorer_selectedCollection value is not available yet (NULL)
    selectedCollection <- availableCollections[1]
  }
  return(selectedCollection)
})

observe({
  selectedCollection <- selectedCollection() # needed because observe is executed at the very beginning where input$geoExplorer_selectedCollection value is not available yet (NULL)

  # update ui selection based on collection
  availableGeocodingResultsForGivenCollection <-  availableGeocodingResults[grep(x = availableGeocodingResults, pattern = paste0("_",selectedCollection,"$"))]
  updateSelectInput(session, "geocodingResult",
                    label = "select geocoding results",
                    choices = availableGeocodingResultsForGivenCollection
  )
  dataLoaded <-F
})


############
# load initial data
#########################

geocodingResult_dataLoaded <- eventReactive(
  
  input$loadDataForCollection,
    {
      geocodingResult_dataLoaded <- list()
      selectedCollection <- input$geoExplorer_selectedCollection
      selectedGeoResult <- input$geocodingResult
      fileNameGeoResult <- availableGeoCodingFiles[grep(x = availableGeoCodingFiles, pattern = selectedGeoResult, fixed = T)]
      folderpathWithGeoResults <- paste0(folderpathGeoCodingResults,fileNameGeoResult)
      
      load(file = paste0(folderpathWithGeoResults,"/geocodingResult.RData"))
      geocodingResult_dataLoaded$geocodingResult <- geocodingResult
      
      load(file = paste0(folderpathWithGeoResults,"/geocodingResultForCountries.RData"))
      geocodingResult_dataLoaded$geocodingResultForCountries <- geocodingResultForCountries
      
      load(file = paste0(folderpathWithGeoResults,"/parameters.RData"))
      geocodingResult_dataLoaded$parameters <- parameters
      
      load(file = paste0(folderpathWithGeoResults,"/info.RData")) # info about the used collection
      geocodingResult_dataLoaded$collectionInfoForGeocodingResult <- info
      
      load(file = paste0(folderpathWithGeoResults,"/dtm.RData")) # only includes string entities of geolocations, so shouldn't be used for something else
      geocodingResult_dataLoaded$dtm <- dtm
      
      geocodingResult_dataLoaded$geocodingResult_columnNames <- names(geocodingResult_dataLoaded$geocodingResult)
      
      print(paste0("Georesult loaded: ", dim(geocodingResult_dataLoaded$geocodingResult)[1]))
     
      return(geocodingResult_dataLoaded)
   }
)

metaData_dataLoaded <- eventReactive(
  input$loadDataForCollection,
  {
    metaData_dataLoaded <- list()
    loadedGeocodingData <- geocodingResult_dataLoaded()
    collectionInfoForGeocodingResult <- loadedGeocodingData$collectionInfoForGeocodingResult
    metaData <- getMetaData(collectionIDs = collectionInfoForGeocodingResult[[1]], collectionDataSet = collectionInfoForGeocodingResult[[2]], host = host, port = db_port)
    metaData <- metaData$meta # nicer for code to work just with the name metaData
    metaData$areaId <- paste(metaData$dataset, metaData$id_doc, sep = "_") # Attention! this needs to be compliant with the way area IDs are assigned in GeocodingScript.R!
    
    metaData_dataLoaded$metaData <- metaData
    metaData_dataLoaded$metaData_columnNames <- names(metaData)
    
    return(metaData_dataLoaded)
  }
)

output$metaData_dataLoaded_output <- reactive({
  loadedMetaData <- metaData_dataLoaded()
  loadedGeocodingData <- geocodingResult_dataLoaded()
  numberOfDocs <- dim(loadedMetaData$metaData)[1]
  numberOfGeocodingData <- dim(loadedGeocodingData$geocodingResult)[1]
  
  text <- paste0("Number of docs loaded: ",numberOfDocs," with columns: ",  paste(loadedMetaData$metaData_columnNames,collapse = ",") )
  
})

############
# configuration of columns
#################

metaData_columnsConfig <- reactiveVal({
  metaData_dataLoaded <- metaData_dataLoaded()
  configEntriesDefault <- data.frame(
                              columnNameForMetaData=metaData_dataLoaded$metaData_columnNames,
                              useForFiltering = rep(as.logical(TRUE)), 
                              useForStats	= rep(as.logical(TRUE)),
                              type = rep(as.String("String")), # c(String,Numeric,Date)	
                              isMultiValue = rep(as.logical(FALSE)),
                              multiValueSeparator = rep(NA)
                               )
  
})


