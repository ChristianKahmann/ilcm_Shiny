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

nameEmptyStringInStatsAs = "EMPTY (no value set)"

metaData_prefixFoUniqueIdentificationInInputFilters <- "metaData_"
geocodingResult_prefixFoUniqueIdentificationInInputFilters <- "geocodingResult_"
metaData_columnNameForMatchWithOtherData <- "areaId" # see code below how areaId is composed for documents
geocodingResult_columnNameForMatchWithOtherData <- "areaId"


# TODO: include option: load geolocation from meta data / from georesults


metaData <- NULL
geocodingResult <- NULL

myReactiveValues <- reactiveValues(dataLoaded = FALSE, 
                                   geocodingResult_dataLoaded = NA, 
                                   geocodingResult_dataToUse = NA, 
                                   geocodingResult_columnsConfig = NA,
                                   geocodingResult_columnNames = NA,
                                   geocodingResult_columnNamesToUseForFiltering = NA,
                                   geocodingResult_columnsWithMultiValueData = NA,
                                   geocodingResult_multiValueSeparators = NA,
                                   geocodingResult_columnsToCalcDistributions = NA,
                                   geocodingResult_columnsToCalcNumericInfos = NA,
                                   geocodingResult_availableValues = NA,
                                   
                                   
                                   metaData_dataLoaded = NA, # all data
                                   metaData_dataToUse = NA, # intersect with geocoding data
                                   
                                   metaData_columnsConfig = NA,
                                   metaData_columnNames = NA,
                                   metaData_columnNamesToUseForFiltering = NA,
                                   metaData_columnsWithMultiValueData = NA,
                                   metaData_multiValueSeparators = NA,
                                   metaData_columnsToCalcDistributions = NA,
                                   metaData_columnsToCalcNumericInfos = NA,
                                   metaData_availableValues = NA,
                                   
                                   idsAllDataInCommon = NA
                                   )


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
  geocodingResult_allDataAsList <- list()
  selectedCollection <- input$geoExplorer_selectedCollection
  selectedGeoResult <- input$geocodingResult
  fileNameGeoResult <- availableGeocodingResults[grep(x = availableGeocodingResults, pattern = selectedGeoResult, fixed = T)]
  folderpathWithGeoResults <- paste0(folderpathGeoCodingResults,fileNameGeoResult)
  
  load(file = paste0(folderpathWithGeoResults,"/geocodingResult.RData"))
  geocodingResult_allDataAsList$geocodingResult <- geocodingResult
  
  load(file = paste0(folderpathWithGeoResults,"/geocodingResultForCountries.RData"))
  geocodingResult_allDataAsList$geocodingResultForCountries <- geocodingResultForCountries
  
  load(file = paste0(folderpathWithGeoResults,"/parameters.RData"))
  geocodingResult_allDataAsList$parameters <- parameters
  
  load(file = paste0(folderpathWithGeoResults,"/info.RData")) # info about the used collection
  geocodingResult_allDataAsList$collectionInfoForGeocodingResult <- info
  
  load(file = paste0(folderpathWithGeoResults,"/dtm.RData")) # only includes string entities of geolocations, so shouldn't be used for something else
  geocodingResult_allDataAsList$dtm <- dtm
  
  geocodingResult_allDataAsList$geocodingResult_columnNames <- names(geocodingResult_allDataAsList$geocodingResult)
  
  print(paste0("Georesult loaded: ", dim(geocodingResult_allDataAsList$geocodingResult)[1]))
  
  myReactiveValues$geocodingResult_dataLoaded <- geocodingResult_allDataAsList$geocodingResult # just the geocodingResult
  myReactiveValues$geocodingResult_columnNames <- names(myReactiveValues$geocodingResult_dataLoaded)
  myReactiveValues$geocodingResult_furtherMetaData <- geocodingResult_allDataAsList
  
  
  
  
  #---------------------
  # load meta data 
  #---------------------
  metaData_dataLoaded <- list()
  collectionInfoForGeocodingResult <- myReactiveValues$geocodingResult_furtherMetaData$collectionInfoForGeocodingResult
  metaData <- getMetaData(collectionIDs = collectionInfoForGeocodingResult[[1]], collectionDataSet = collectionInfoForGeocodingResult[[2]], host = host, port = db_port)
  metaData <- metaData$meta # nicer for code to work just with the name metaData
  metaData$areaId <- paste(metaData$dataset, metaData$id_doc, sep = "_") # Attention! this needs to be compliant with the way area IDs are assigned in GeocodingScript.R!
  
  myReactiveValues$metaData_dataLoaded <- metaData
  myReactiveValues$metaData_columnNames <- names(metaData)
  
  
  #---------------------
  # calc ids in common and data to use (intersect)
  #---------------------
  myReactiveValues$idsAllDataInCommon <- intersect(
    myReactiveValues$metaData_dataLoaded[[metaData_columnNameForMatchWithOtherData]], 
    myReactiveValues$geocodingResult_dataLoaded[[geocodingResult_columnNameForMatchWithOtherData]]) 
  
  myReactiveValues$metaData_dataToUse <- metaData[which(metaData[[metaData_columnNameForMatchWithOtherData]] %in% myReactiveValues$idsAllDataInCommon),]
  myReactiveValues$geocodingResult_dataToUse <- myReactiveValues$geocodingResult_dataLoaded[which(myReactiveValues$geocodingResult_dataLoaded[[geocodingResult_columnNameForMatchWithOtherData]] %in% myReactiveValues$idsAllDataInCommon),]
  
  
  #---------------------
  # initialze metaData_columnsConfig
  #---------------------
  
  myReactiveValues$metaData_columnsConfig <- data.frame(
    columnNameForField=myReactiveValues$metaData_columnNames,
    useForFiltering = rep(as.logical(TRUE)),
    useForStatsToCalcDistributions = rep(as.logical(TRUE)),
    useForStatsToCalcNumericInfos = rep(as.logical(TRUE)),
    type = rep(as.String("String")), # c(String,Numeric,Date)
    isMultiValue = rep(as.logical(FALSE)),
    multiValueSeparator = rep(as.String(NA)),
    stringsAsFactors = FALSE
  )
  
  #---------------------
  # initialze geocodingResult_columnsConfig
  #---------------------
  myReactiveValues$geocodingResult_columnsConfig <- data.frame(
    columnNameForField=myReactiveValues$geocodingResult_columnNames,
    useForFiltering = rep(as.logical(TRUE)),
    useForStatsToCalcDistributions = rep(as.logical(TRUE)),
    useForStatsToCalcNumericInfos = rep(as.logical(TRUE)),
    type = rep(as.String("String")), # c(String,Numeric,Date)
    isMultiValue = rep(as.logical(FALSE)),
    multiValueSeparator = rep(as.String(NA)),
    stringsAsFactors = FALSE
  )
  
  # initialize values based on config values (in case the user doesn't configures & doesn't hit apply config button)
  setValuesBasedOnConfig()
  
  myReactiveValues$dataLoaded <- TRUE
  
})


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
  
  #-------------------
  # initialize  needed variables
  #-------------------
  
  myReactiveValues$metaData_columnsConfig <-  hot_to_r(input$metaData_config)
  myReactiveValues$geocodingResult_columnsConfig <-  hot_to_r(input$geocodingResult_config)
  
  setValuesBasedOnConfig()
  
  
  
  # selectedForFiltering <- myReactiveValues$metaData_columnsConfig$columnNameForField[which(myReactiveValues$metaData_columnsConfig$useForFiltering == TRUE)]
  # textToDisplay <- paste0("Selected for filtering: ", paste0(selectedForFiltering, collapse = ", "))
  # print(textToDisplay)
})

setValuesBasedOnConfig <- function(){
  
  metaData_configData <- myReactiveValues$metaData_columnsConfig #  just for better readability of code below
  myReactiveValues$metaData_columnNamesToUseForFiltering <- metaData_configData$columnNameForField[which(metaData_configData$useForFiltering == TRUE)]
  myReactiveValues$metaData_columnsWithMultiValueData <- metaData_configData$columnNameForField[which(metaData_configData$isMultiValue == TRUE)]
  myReactiveValues$metaData_multiValueSeparators <- metaData_configData$multiValueSeparator[which(metaData_configData$isMultiValue == TRUE)]
  myReactiveValues$metaData_columnsToCalcDistributions <- metaData_configData$columnNameForField[which(metaData_configData$useForStatsToCalcDistributions == TRUE)]
  myReactiveValues$metaData_columnsToCalcNumericInfos <- metaData_configData$columnNameForField[which(metaData_configData$useForStatsToCalcNumericInfos == TRUE)]
  myReactiveValues$metaData_availableValues <- getAvailableValuesForGivenColumns(dataToUse = myReactiveValues$metaData_dataToUse, columnNames = myReactiveValues$metaData_columnNames,columnNamesContainingMultiValues = myReactiveValues$metaData_columnsWithMultiValueData,separatorsToUseForColumnsWithMultivalues = myReactiveValues$metaData_multiValueSeparators)
  
  
  geocodingResult_configData <- myReactiveValues$geocodingResult_columnsConfig #  just for better readability of code below
  myReactiveValues$geocodingResult_columnNamesToUseForFiltering <- geocodingResult_configData$columnNameForField[which(geocodingResult_configData$useForFiltering == TRUE)]
  myReactiveValues$geocodingResult_columnsWithMultiValueData <- geocodingResult_configData$columnNameForField[which(geocodingResult_configData$isMultiValue == TRUE)]
  myReactiveValues$geocodingResult_multiValueSeparators <- geocodingResult_configData$multiValueSeparator[which(geocodingResult_configData$isMultiValue == TRUE)]
  myReactiveValues$geocodingResult_columnsToCalcDistributions <- geocodingResult_configData$columnNameForField[which(geocodingResult_configData$useForStatsToCalcDistributions == TRUE)]
  myReactiveValues$geocodingResult_columnsToCalcNumericInfos <- geocodingResult_configData$columnNameForField[which(geocodingResult_configData$useForStatsToCalcNumericInfos == TRUE)]
  myReactiveValues$geocodingResult_availableValues <- getAvailableValuesForGivenColumns(dataToUse = myReactiveValues$geocodingResult_dataToUse, columnNames = myReactiveValues$geocodingResult_columnNames,columnNamesContainingMultiValues = myReactiveValues$geocodingResult_columnsWithMultiValueData,separatorsToUseForColumnsWithMultivalues = myReactiveValues$geocodingResult_multiValueSeparators)
  
}

#####################################
# prepare filtering possibilities
#####################################

# # create selectInputs
selectInputListForMetaData <- reactive({createSelectInputsForColumnsAndValues(myReactiveValues$metaData_columnNamesToUseForFiltering,  myReactiveValues$metaData_availableValues, prefixForUniqueIdentification = metaData_prefixFoUniqueIdentificationInInputFilters)})
output$selectInputListForMetaData <- renderUI(selectInputListForMetaData())
selectInputListForGeocodingResult <- reactive({createSelectInputsForColumnsAndValues(myReactiveValues$geocodingResult_columnNamesToUseForFiltering, myReactiveValues$geocodingResult_availableValues, prefixForUniqueIdentification = geocodingResult_prefixFoUniqueIdentificationInInputFilters)})
output$selectInputListForGeocodingResult <- renderUI(selectInputListForGeocodingResult()) 

metaData_uiInputFilterNames <- reactive({paste(metaData_prefixFoUniqueIdentificationInInputFilters,names(myReactiveValues$metaData_availableValues), sep="")})
geocodingResult_uiInputFilterNames <- reactive({paste(geocodingResult_prefixFoUniqueIdentificationInInputFilters,names(myReactiveValues$geocodingResult_availableValues), sep="")})



#################
