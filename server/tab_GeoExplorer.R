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
library(rhandsontable)

# this is the server code, for UI see ui/tab_GeoExplorer.R

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


metaData <- NULL
geocodingResult <- NULL

myReactiveValues <- reactiveValues(dataLoaded = FALSE, 
                                   geocodingResult_dataLoaded = NA, 
                                   geocodingResult_columnsConfig = NA,
                                   metaData_dataLoaded = NA, 
                                   metaData_columnsConfig = NA)


# output$textWithSelectedCollection <- reactive({
#   selectedCollection <- input$geoExplorer_selectedCollection
#   text <- paste("selected collection: ", selectedCollection, ".")
# })

#########################
# select collection & geocoding result
#########################
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
})


#########################
# load initial data
#########################

output$dataLoaded <- reactive({
  result <- myReactiveValues$dataLoaded
  #print(paste0("+++Data loaded: ", result))
  return(result)
})
outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)

observeEvent(input$loadDataForCollection,{
  
  #---------------------
  # load geocoding data
  #---------------------
  geocodingResult_dataLoaded <- list()
  selectedCollection <- input$geoExplorer_selectedCollection
  selectedGeoResult <- input$geocodingResult
  fileNameGeoResult <- availableGeocodingResults[grep(x = availableGeocodingResults, pattern = selectedGeoResult, fixed = T)]
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
  
  myReactiveValues$geocodingResult_dataLoaded <- geocodingResult_dataLoaded
  
  #---------------------
  # load meta data 
  #---------------------
  metaData_dataLoaded <- list()
  collectionInfoForGeocodingResult <- geocodingResult_dataLoaded$collectionInfoForGeocodingResult
  metaData <- getMetaData(collectionIDs = collectionInfoForGeocodingResult[[1]], collectionDataSet = collectionInfoForGeocodingResult[[2]], host = host, port = db_port)
  metaData <- metaData$meta # nicer for code to work just with the name metaData
  metaData$areaId <- paste(metaData$dataset, metaData$id_doc, sep = "_") # Attention! this needs to be compliant with the way area IDs are assigned in GeocodingScript.R!
  
  metaData_dataLoaded$metaData <- metaData
  metaData_dataLoaded$columnNames <- names(metaData)
  
  myReactiveValues$metaData_dataLoaded <- metaData_dataLoaded
  
  #---------------------
  # initialze metaData_columnsConfig
  #---------------------
  
  myReactiveValues$metaData_columnsConfig <- data.frame(
    columnNameForMetaData=metaData_dataLoaded$columnNames,
    useForFiltering = rep(as.logical(TRUE)),
    useForStats	= rep(as.logical(TRUE)),
    type = rep(as.String("String")), # c(String,Numeric,Date)
    isMultiValue = rep(as.logical(FALSE)),
    multiValueSeparator = rep(as.String(NA)),
    stringsAsFactors = FALSE
  )
  
  #---------------------
  # initialze geocodingResult_columnsConfig
  #---------------------
  myReactiveValues$geocodingResult_columnsConfig <- data.frame(
    columnNameForMetaData=geocodingResult_dataLoaded$geocodingResult_columnNames,
    useForFiltering = rep(as.logical(TRUE)),
    useForStats	= rep(as.logical(TRUE)),
    type = rep(as.String("String")), # c(String,Numeric,Date)
    isMultiValue = rep(as.logical(FALSE)),
    multiValueSeparator = rep(as.String(NA)),
    stringsAsFactors = FALSE
  )
  
  myReactiveValues$dataLoaded <- TRUE
})


# output$metaData_dataLoaded_output <- reactive({
#   loadedMetaData <- myReactiveValues$metaData_dataLoaded
#   loadedGeocodingData <- myReactiveValues$geocodingResult_dataLoaded
#   numberOfDocs <- dim(loadedMetaData$metaData)[1]
#   numberOfGeocodingData <- dim(loadedGeocodingData$geocodingResult)[1]
#   
#   text <- paste0("Number of docs loaded: ",numberOfDocs," with columns: ",  paste(loadedMetaData$columnNames,collapse = ",") )
#   return(text)
# })

#############################
# configuration of columns
#############################

output$metaData_config <- renderRHandsontable({
    rhandsontable(myReactiveValues$metaData_columnsConfig)
})

output$geocodingResult_config <- renderRHandsontable({
  rhandsontable(myReactiveValues$geocodingResult_columnsConfig)
})

observeEvent(input$config_apply, {
  myReactiveValues$metaData_columnsConfig <-  hot_to_r(input$metaData_config)
  myReactiveValues$geocodingResult_columnsConfig <-  hot_to_r(input$geocodingResult_config)
  # selectedForFiltering <- myReactiveValues$metaData_columnsConfig$columnNameForMetaData[which(myReactiveValues$metaData_columnsConfig$useForFiltering == TRUE)]
  # textToDisplay <- paste0("Selected for filtering: ", paste0(selectedForFiltering, collapse = ", "))
  # print(textToDisplay)
})

output$metaData_config_TestOutput <- reactive({
  metaData_columnsConfig_here <- myReactiveValues$metaData_columnsConfig
  textToDisplay <- "Please first select the data to load!"
  
  if(!is.na(metaData_columnsConfig_here)){
    selectedForFiltering <- metaData_columnsConfig_here$columnNameForMetaData[which(metaData_columnsConfig_here$useForFiltering == TRUE)]
    textToDisplay <- paste0("Selected for filtering: ", paste0(selectedForFiltering, collapse = ", "))
  }
  return(textToDisplay)
})



#################
