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
geoDataToUse_columnNameForMatchWithOtherData <- "areaId"

statsOfdistributions_sortByValueDesc <- T # TODO: make this configurabke

geocodingResult_columnsToCalcDistributions_default <- c("osm_type","class","type","countryName")
geocodingResult_columnsToCalcNumericInfos_default <- c("frequencyInArea","place_rank","importance")
geocodingResult_columnsToUseForFiltering_default <- c("entityName", "frequencyInArea", "query", "osm_type", "place_rank", "display_name", "class", "type", "importance", "countryName", "countryCode")

# TODO: abfangen wenn keine Ergebnisse
# todo: abfangen wenn keine stats in config ausgewählt
# TODO: check why values not used are shown in stats graph
# TODO: display message when no geolocation result is avaialable for selected collection
# TODO: include option: load geolocation from meta data / from georesults


metaData <- NULL
geocodingResult <- NULL

myReactiveValues <- reactiveValues(dataLoaded = FALSE, 
                                   geocodingResult_dataLoaded = NA, 
                                   geocodingResult_dataToUse = NA,  # intersect with meta data
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
    isMultiValue = rep(as.logical(FALSE)),
    multiValueSeparator = rep(as.String(NA)),
    useForFiltering = rep(as.logical(TRUE)),
    useForStatsToCalcDistributions = rep(as.logical(FALSE)),
    useForStatsToCalcNumericInfos = rep(as.logical(FALSE)),
    #type = rep(as.String("String")), # c(String,Numeric,Date)
    stringsAsFactors = FALSE
  )
  
  #---------------------
  # initialze geocodingResult_columnsConfig
  #---------------------
  myReactiveValues$geocodingResult_columnsConfig <- data.frame(
    columnNameForField=myReactiveValues$geocodingResult_columnNames,
    isMultiValue = rep(as.logical(FALSE)),
    multiValueSeparator = rep(as.String(NA)),
    
    useForFiltering = rep(as.logical(TRUE)),
    useForStatsToCalcDistributions = rep(as.logical(FALSE)),
    useForStatsToCalcNumericInfos = rep(as.logical(FALSE)),
    #type = rep(as.String("String")), # c(String,Numeric,Date)
    stringsAsFactors = FALSE
  )
  
  # set default values (just for convenience)
  myReactiveValues$geocodingResult_columnsConfig <- setConfigForCertainColumnNames(configToUse = myReactiveValues$geocodingResult_columnsConfig, configFieldNameToUse = "useForStatsToCalcDistributions", listOfColumnNamesTheConfigFieldNameShouldChange = geocodingResult_columnsToCalcDistributions_default, valueToUse = TRUE)
  myReactiveValues$geocodingResult_columnsConfig <- setConfigForCertainColumnNames(configToUse = myReactiveValues$geocodingResult_columnsConfig, configFieldNameToUse = "useForStatsToCalcNumericInfos", listOfColumnNamesTheConfigFieldNameShouldChange = geocodingResult_columnsToCalcNumericInfos_default, valueToUse = TRUE)
  myReactiveValues$geocodingResult_columnsConfig <- setConfigForCertainColumnNames(configToUse = myReactiveValues$geocodingResult_columnsConfig, configFieldNameToUse = "useForFiltering", listOfColumnNamesTheConfigFieldNameShouldChange = geocodingResult_columnsToUseForFiltering_default, valueToUse = TRUE)
  
  # initialize values based on config values (in case the user doesn't configures & doesn't hit apply config button)
  setValuesBasedOnConfig()
  
  
  myReactiveValues$dataLoaded <- TRUE
  
})

geoDataToUseForMap <- reactive({
  geoDataToUse <- myReactiveValues$geocodingResult_dataToUse # TODO: make this selectable to be able to select locations from meta data (or other data)
  geoDataToUse$geoDataLat <- myReactiveValues$geocodingResult_dataToUse$lat
  geoDataToUse$geoDataLon <- myReactiveValues$geocodingResult_dataToUse$lon
  geoDataToUse$geoDataLatLon <- paste(geoDataToUse$geoDataLat, geoDataToUse$geoDataLon, sep = " ")
  
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

setConfigForCertainColumnNames <- function(configToUse, configFieldNameToUse,listOfColumnNamesTheConfigFieldNameShouldChange,valueToUse){
  changedConfig <- configToUse
  for(columnName in listOfColumnNamesTheConfigFieldNameShouldChange){
    indexToChange <- which(configToUse$columnNameForField == columnName)
    
    changedConfig[[configFieldNameToUse]][indexToChange] <- valueToUse
  }
  return (changedConfig)
}

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
# filtering
#################

#----------------------------
# check if any filters set
#----------------------------
metaData_isFilterSet <- reactive({
  #print("is meta data filter set?")
  for(filterName in metaData_uiInputFilterNames()){
    if(length(input[[filterName]])>0 && startsWith(x = filterName, prefix = metaData_prefixFoUniqueIdentificationInInputFilters)){
      return (TRUE)
    }
  }
  return(FALSE)
})

geocodingResult_isFilterSet <- reactive({
  #print("is geocodingResult filter set?")
  for(filterName in geocodingResult_uiInputFilterNames()){
    if(length(input[[filterName]])>0 && startsWith(x = filterName, prefix = geocodingResult_prefixFoUniqueIdentificationInInputFilters)){
      return (TRUE)
    }
  }
  return(FALSE)
})

#----------------------------
# perform filtering and return ids in common
#----------------------------
idsAfterFiltering <- reactive({
  result <- myReactiveValues$idsAllDataInCommon
  metaData_filtered <- myReactiveValues$metaData_dataToUse
  geocodingResult_filtered <- myReactiveValues$geocodingResult_dataToUse
  # filter meta data
  if(metaData_isFilterSet()){
    metaData_filtered <- filterDataBasedOnInputFilterFields(dataToFilter = myReactiveValues$metaData_dataToUse, columnNamesOfDataToFilter = myReactiveValues$metaData_columnNames,columnsNamesWithMultiValueData = myReactiveValues$metaData_columnsWithMultiValueData,filterInput = input,prefixInFilterNameForUniqueIdentification = metaData_prefixFoUniqueIdentificationInInputFilters)
  }
  #filter geo coding result
  if(geocodingResult_isFilterSet()){
    geocodingResult_filtered <- filterDataBasedOnInputFilterFields(dataToFilter = myReactiveValues$geocodingResult_dataToUse,columnNamesOfDataToFilter = myReactiveValues$geocodingResult_columnNames,columnsNamesWithMultiValueData = myReactiveValues$geocodingResult_columnsWithMultiValueData,filterInput = input,prefixInFilterNameForUniqueIdentification = geocodingResult_prefixFoUniqueIdentificationInInputFilters)
  }
  
  # take only those ids in common (which means filters are applied to meta data AND geoResult)
  result <- intersect(metaData_filtered[[metaData_columnNameForMatchWithOtherData]], geocodingResult_filtered[[geocodingResult_columnNameForMatchWithOtherData]]) 
  
  return(result)
})

#----------------------------
# Filter data based on idsAfterFiltering
#----------------------------
metaData_filtered <- reactive({
  #print("filter meta data based on filters set.")
  result <- myReactiveValues$metaData_dataToUse
  result <- result[which(result[[metaData_columnNameForMatchWithOtherData]] %in% idsAfterFiltering()),]
})
geocodingResult_filtered <- reactive({
  result <- myReactiveValues$geocodingResult_dataToUse
  result <- result[which(result[[geocodingResult_columnNameForMatchWithOtherData]] %in% idsAfterFiltering()),]
})

geoDataToUseForMap_filtered <- reactive({
  result <- geoDataToUseForMap()
  result <- result[which(result[[geoDataToUse_columnNameForMatchWithOtherData]] %in% idsAfterFiltering()),]
})

#----------------------------
# output
#----------------------------

# number of results
output$metaData_numberOfResults1 <- reactive({
  text <- paste("There are", dim(metaData_filtered())[1], "results for document meta data.")
})
output$geocodingResult_numberOfResults1 <- reactive({
  text <- paste("There are", dim(geocodingResult_filtered())[1], "results for geo location data.")
})
# number of results 2 (needed twice, becasue should be displayed for stats and for map, but shiny doesn't allow displaying same output object twice in UI )
output$metaData_numberOfResults2 <- reactive({
  text <- paste("There are", dim(metaData_filtered())[1], "results for document meta data.")
})
output$geocodingResult_numberOfResults2 <- reactive({
  text <- paste("There are", dim(geocodingResult_filtered())[1], "results for geo location data.")
})

output$geoDataToUse_numberOfResults <- reactive({
  text <- paste("There are", dim(geoDataToUseForMap_filtered())[1], "results for geoData to use.")
})

# stats for filtered data 
metaData_stats <- reactive({
  #print("re-calc stats for meta data")
  dataForStats <- metaData_filtered()
  
  stats <- calcStatsMetaData(metaData = dataForStats, 
                             columnsToCalcDistributions = myReactiveValues$metaData_columnsToCalcDistributions,
                             availableValues = myReactiveValues$metaData_availableValues,
                             includeValuesNotUsedForDistribution = F,
                             columnsWithMultiValues = myReactiveValues$metaData_columnsWithMultiValueData, 
                             separatorsForMultiValues = myReactiveValues$metaData_multiValueSeparators, 
                             nameEmptyStringInStatsAs = nameEmptyStringInStatsAs,
                             columnsToUseForNumericStats = myReactiveValues$metaData_columnsToCalcNumericInfos)
  
  
  })

geocodingResult_stats <- reactive({
  dataForStats <- geocodingResult_filtered()
  stats <- calcStatsGeocodingResult(geocodingResultData = dataForStats, 
                                    columnsToCalcDistributions = myReactiveValues$geocodingResult_columnsToCalcDistributions,
                                    availableValues = myReactiveValues$geocodingResult_availableValues,
                                    includeValuesNotUsedForDistribution = F,
                                    columnsWithMultiValues = myReactiveValues$geocodingResult_columnsWithMultiValueData, 
                                    separatorsForMultiValues = myReactiveValues$geocodingResult_multiValueSeparators, 
                                    nameEmptyStringInStatsAs = nameEmptyStringInStatsAs,
                                    columnsToUseForNumericStats = geocodingResult_columnsToCalcNumericInfos)      
})

# # stats distributions plots
# output$metaData_stats_distributions_plots <- plotDistributionData(metaData_stats()$distributions)
# output$geocodingResult_stats_distributions_plots <- reactive({plotDistributionData(geocodingResult_stats()$distributions)})
# 
# # stats numeric tables
# output$metaData_stats_numeric_table <- reactive({renderTable(metaData_stats()$numericInfos, rownames = T)})
# output$geocodingResult_stats_numeric_table <- reactive({renderTable(geocodingResult_stats()$numericInfos, rownames = T)})

# stats distributions plots
output$metaData_stats_distributions_plots <- renderPlotly(createPlotsForDistributionData(metaData_stats()$distributions, statsOfdistributions_sortByValueDesc))

output$geocodingResult_stats_distributions_plots <- renderPlotly(createPlotsForDistributionData(geocodingResult_stats()$distributions, statsOfdistributions_sortByValueDesc))

# stats numeric tables
output$metaData_stats_numeric_table <- renderTable(metaData_stats()$numericInfos, rownames = T)
output$geocodingResult_stats_numeric_table <- renderTable(geocodingResult_stats()$numericInfos, rownames = T)

