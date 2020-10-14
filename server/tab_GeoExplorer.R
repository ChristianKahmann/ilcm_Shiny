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
library(stringr)

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
regexData_columnNameForMatchWithOtherData <- "areaId"

# geoDataToUse
geoDataToUseForMap_dataSource <- "geocodingResult"
geoDataToUseForMap_columnNameForMatchWithOtherData <- "areaId"
geoDataToUseForMap_columnNameForLat <- "lat"
geoDataToUseForMap_columnNameForLon <- "lon"


statsOfdistributions_sortByValueDesc <- T # TODO: make this configurabke

geocodingResult_columnsToCalcDistributions_default <- c("osm_type","class","type","countryName")
geocodingResult_columnsToCalcNumericInfos_default <- c("frequencyInArea","place_rank","importance")
geocodingResult_columnsToUseForFiltering_default <- c("entityName", "frequencyInArea", "query", "osm_type", "place_rank", "display_name", "class", "type", "importance", "countryName", "countryCode")


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
                                   
                                   regexData_performRegExMatching = FALSE,
                                   regexData_dataLoaded = NA,
                                   regexData_dataToUse = NA, # intersect with meta data and geocoding data 
                                   regexData_availableValues = NA,
                                   regexData_columnsToCalcDistributions = "regexMatch",
                                   regexData_columnsToCalcNumericInfos = "numericValue",
                                   
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
  
  
  if(length(availableGeocodingResultsForGivenCollection)==0){
    updateSelectInput(session, "geocodingResult",
                      label = "select geocoding results",
                      choices = c("Sorry, no geocoding results available for given collection. Please select another collection")
                      #choices = NULL
                      )
  }else{
    updateSelectInput(session, "geocodingResult",
                      label = "select geocoding results",
                      choices = availableGeocodingResultsForGivenCollection
    )
  }
  
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
  geocodingResult$latlon <- paste(geocodingResult$lat, geocodingResult$lon, sep = " ")
  
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
    multiValueSeparator = rep(as.String(",")),
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
    multiValueSeparator = rep(as.String(",")),
    
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

#######################
# optinal Regex process
#######################
# TODO: own own section within Configuration for "RegEx"
# TODO: inside regEx Config: checkbox: transformRegexResultsToNumeric, checkbox useParenthesizedSubexpressions, 
# TODO: idea: show aggregated results of matches (e.g. how many per doc, distribution of values, first 20 docs with matches etc.), use another button to say: yes, apply these regex results
# TODO: idea: in Tab Filtering: beside meta data and geocoding filter possibilities: filter possibilities for regex (only selected values /  within a certain numeric range)
# TODO: implement dealing with parentesized sub expressions
#load(file = "play/temp/fulltextData.RData")

observeEvent(input$performRegexMatching,{
  
  myReactiveValues$regexData_performRegExMatching <- T
  
  # get fulltext data (for regex matching)
  collectionInfoForGeocodingResult <- myReactiveValues$geocodingResult_furtherMetaData$collectionInfoForGeocodingResult
  fullDocData <- getFullDocDataFromDB(collectionIDs = collectionInfoForGeocodingResult[[1]], collectionDataSet = collectionInfoForGeocodingResult[[2]], host = host, port = db_port)
  fullDocData$areaId <- paste(fullDocData$dataset, fullDocData$id_doc, sep = "_") # Attention! this needs to be compliant with the way area IDs are assigned for the other data. will be used to match / intersect between metadata, geocodingResult & fulltext
  fulltextData <- select(fullDocData, id_doc, title, body, areaId)
  
  # params
  regexStr <- input$regexInput # "[0-9]{1,3}(,[0-9]{3})*(\\.[0-9]+)?"   # TODO: show link to regex specifics in R in UI to user: https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
  useParenthesizedSubexpressions <- F # TODO: make this selectable via UI (needs to be implemented first! see below)
  performNumericTransformation <- input$regex_tranformToNumeric
  separatorForThousandsUsedInTextData <- input$regex_separatorForThousandsUsedInTextData
  separatorForDecimalUsedInTextData <- input$regex_separatorForDecimalUsedInTextData
  includeTitleForRegExMatching <- input$regex_includeTitleForRegExMatching
  
  showDistr <- input$regex_filterAndResults_showDistributions
  if(showDistr){
    if(performNumericTransformation){
      myReactiveValues$regexData_columnsToCalcDistributions <- c("numericValue")
    }else{
      myReactiveValues$regexData_columnsToCalcDistributions <- c("regexMatch")
    }
  }else{
    myReactiveValues$regexData_columnsToCalcDistributions <- vector()
  }
  showNumericInfos <- input$regex_filterAndResults_showNumericInfos
  if(showNumericInfos){
    if(performNumericTransformation){
      myReactiveValues$regexData_columnsToCalcNumericInfos <- c("numericValue")
    }else{
      myReactiveValues$regexData_columnsToCalcNumericInfos <- vector()
    }
  }else{
    myReactiveValues$regexData_columnsToCalcNumericInfos <- vector()
  }
  
  
  # escaping for dot
  if(separatorForThousandsUsedInTextData == "."){
    separatorForThousandsUsedInTextData <- "\\."
  }
  if(separatorForDecimalUsedInTextData == "."){
    separatorForDecimalUsedInTextData <- "\\."
  }
  
  # apply only to text or also to title
  dataToApplyRegExOn <- fulltextData$body # TODO: remove restriction "[1:10]"
  if(includeTitleForRegExMatching){
    dataToApplyRegExOn <- paste(fulltextData$title, fulltextData$body, sep = " ")
  }
  
  # match regex
  matchResult <- str_match_all(dataToApplyRegExOn, regexStr)
  
  # prepare resulting data frame
  regexData <- data.frame(areaId=character(), regexMatch=character(), numericValue = numeric(), stringsAsFactors = F)
  
  # is parenthesizedSubExpressionDataAvailable?
  parenthesizedSubExpressionDataAvailable <- F 
  if(dim(matchResult[[1]])[2]>1){
    parenthesizedSubExpressionDataAvailable <- T
  }
  
  for(i in 1:length(matchResult)){
    areaId  <- fulltextData$areaId[i]
    matchResultEntry <- matchResult[[i]]
    if(dim(matchResultEntry)[1]==0){# no match found for given doc
      regexData <- rbind(regexData, data.frame(areaId=areaId, regexMatch=nameEmptyStringInStatsAs, numericValue = NA))
      next
    }
    
    # parenthesized sub-expressions used
    matches <- c()
    if(useParenthesizedSubexpressions & parenthesizedSubExpressionDataAvailable){# parenthesized sub-expressions are used in regex which leads to 2 different results: 1: the full match 2: the match of the parenthesized sub-expressions (for explanation on parenthesized sub-expressions see e.g https://bookdown.org/rdpeng/rprogdatascience/regular-expressions.html => "17.7. regexec()")
      stop("handling of parenthesizedSubExpression not implemented yet! Please implement this first!")
      # TODO: implement: matches <- matchResultEntry[,2:n] excl. NAs
      
    }else{# normal regex (no parenthesized sub-expressions)
      # actualMatches <- matchResultEntry[,1]
      matches <- matchResultEntry[,1]
    }
    
    for(givenMatch in matches){
      matchStringToUse <- givenMatch
      numericValue <- NA
      if(performNumericTransformation){
        # deal with separatorForThousandsUsedInTextData
        if(!is.null(separatorForThousandsUsedInTextData) & !is.na(separatorForThousandsUsedInTextData) & nchar(separatorForThousandsUsedInTextData)>0){
          matchStringToUse <- str_replace_all(matchStringToUse, separatorForThousandsUsedInTextData, "")
        }
        # deal with separatorForDecimalUsedInTextData
        if(!is.null(separatorForDecimalUsedInTextData) & !is.na(separatorForDecimalUsedInTextData) & nchar(separatorForDecimalUsedInTextData)>0){
          numberOfDecimalSeparatorsInMatch <- dim(str_match_all(string = matchStringToUse, pattern = separatorForDecimalUsedInTextData)[[1]])[1]
          if(numberOfDecimalSeparatorsInMatch == 1){ # only replace it when it occurs just once, only in these cases is is a decimal separator
            matchStringToUse <- str_replace(matchStringToUse, separatorForDecimalUsedInTextData, ".")
          }
        }
        
        tryCatch({
          # try to transform to numeric
          numericValue <- as.numeric(matchStringToUse)
        },warning = function(e){
          # do nothing
          warning(paste0("could not convert \"",matchStringToUse,"\" to numeric value. It will be set to NA."))
          numericValue <- NA
        }
        )
      }
      regexData <- rbind(regexData, data.frame(areaId=areaId, regexMatch=givenMatch, numericValue = numericValue))
    }
  }
  
  
  
  myReactiveValues$regexData_dataLoaded <- regexData
  
  #---------------------
  # calc ids in common and data to use (intersect)
  #---------------------
  # the follwing lines are not necessary, as the regexData contains all docs (even if the regex doesn't match. In this case they are listed as "EMPTY (nothing set)"). Thus, the ids in common are not further reduced by the regex data. If docs/areaIds will be exlcuded in future if regex doesn't match, the following lineshave to be performed
  # myReactiveValues$idsAllDataInCommon <- intersect(
  #   myReactiveValues$idsAllDataInCommon, 
  #   myReactiveValues$regexData_dataLoaded[[regexData_columnNameForMatchWithOtherData]]) 
  # 
  # myReactiveValues$metaData_dataToUse <-        myReactiveValues$metaData_dataLoaded[which(myReactiveValues$metaData_dataLoaded[[metaData_columnNameForMatchWithOtherData]] %in% myReactiveValues$idsAllDataInCommon),]
  # myReactiveValues$geocodingResult_dataToUse <- myReactiveValues$geocodingResult_dataLoaded[which(myReactiveValues$geocodingResult_dataLoaded[[geocodingResult_columnNameForMatchWithOtherData]] %in% myReactiveValues$idsAllDataInCommon),]
  
  # reduce regex data to those contained in ids in common
  myReactiveValues$regexData_dataToUse <-   myReactiveValues$regexData_dataLoaded[which(myReactiveValues$regexData_dataLoaded[[regexData_columnNameForMatchWithOtherData]] %in% myReactiveValues$idsAllDataInCommon),]

  # retrieve available values (needed later for stats)
  myReactiveValues$regexData_availableValues <- getAvailableValuesForGivenColumns(dataToUse = myReactiveValues$regexData_dataToUse, columnNames = c("regexMatch", "numericValue"),columnNamesContainingMultiValues = NULL, separatorsToUseForColumnsWithMultivalues = NULL)
  
  
})

# some infos about the regex matches
output$regExInfo <- reactive({
  
  regexData <- myReactiveValues$regexData_dataLoaded
  dataIsAvailable <- (myReactiveValues$regexData_performRegExMatching && dim(regexData)[1]>0)
  validate(need(dataIsAvailable, message = "Enter a regular expression an hit apply."))
  dataWithMatches <- regexData[which(regexData$regexMatch != nameEmptyStringInStatsAs),]
  numberOfDocsWithMatches <- length(unique(dataWithMatches$areaId))
  numberOfMatchesPerDoc <- dataWithMatches %>% count(areaId)
  meanNumberOfMatchesPerDoc <- mean(numberOfMatchesPerDoc$n)
  textToDisplay <- paste0("The given regex has ", length(dataWithMatches$areaId), " matches in ", numberOfDocsWithMatches, " different documents. ",
                            "In average, there are ", meanNumberOfMatchesPerDoc, " matches per doc with a minimum of ", min(numberOfMatchesPerDoc$n), " and a maximum of ", max(numberOfMatchesPerDoc$n))
}) 

# some example output for the regex matches
output$regexResults <- renderRHandsontable({
  
  regexData <- myReactiveValues$regexData_dataLoaded
  dataIsAvaialble <- (myReactiveValues$regexData_performRegExMatching && dim(regexData)[1]>0)
  validate(need(dataIsAvaialble, message = ""))

  #aggregate
  separatorForRegexMatches <- input$regex_separatorForMatchesDisplay
  matchesByDoc <- aggregate(regexData$regexMatch ~ areaId, data = regexData, paste, collapse = separatorForRegexMatches)
  colnames(matchesByDoc) <- c("areaId", "regexMatch")
  
  dataToShow <- matchesByDoc
  
  # aggregate numeric results
  performNumericTransformation <- input$regex_tranformToNumeric
  if(performNumericTransformation){
    numericResultsByDoc <- aggregate(regexData$numericValue ~ areaId, data = regexData, paste, collapse = separatorForRegexMatches, na.action=na.pass) # include NAs to show where the transformation to numeric didn't work 
    colnames(numericResultsByDoc) <- c("areaId", "numericValue")
    dataToShow <- merge(matchesByDoc, numericResultsByDoc, by = "areaId")
  }
  
  
  numberOfMatchesToShow <- input$regex_showMatches_topX
  typeDocsOrDocsWithMatches <- input$regex_showMatches_typeDocsOrDocsWithMatches
  
   
  if(typeDocsOrDocsWithMatches == "documents"){
    dataToShow <- dataToShow
  }
  if(typeDocsOrDocsWithMatches == "documents with matches"){
    dataToShow <- dataToShow[which(dataToShow$matchResults!=nameEmptyStringInStatsAs),]
  }
  dataToShow <- dataToShow[1:numberOfMatchesToShow,]
  rhandsontable(dataToShow)
})



observeEvent(input$resetRegexMatching,{
  myReactiveValues$regexData_performRegExMatching <- F
  myReactiveValues$regexData_dataLoaded <- myReactiveValues$regexData_dataLoaded[0,]
})

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
  result <- result[which(result[[geocodingResult_columnNameForMatchWithOtherData]] %in% idsAfterFiltering()),] # this just filters for certain docs. As a doc might include several geoCodingResults, the filter of geoCodingResult is not applied yet correctly and needs to be re-applied to the given docs
  result <- filterDataBasedOnInputFilterFields(dataToFilter = result, columnNamesOfDataToFilter = myReactiveValues$geocodingResult_columnNames,columnsNamesWithMultiValueData = myReactiveValues$geocodingResult_columnsWithMultiValueData,filterInput = input,prefixInFilterNameForUniqueIdentification = geocodingResult_prefixFoUniqueIdentificationInInputFilters)
})

regexData_filtered <- reactive({
  result <- myReactiveValues$regexData_dataToUse
  result <- result[which(result[[regexData_columnNameForMatchWithOtherData]] %in% idsAfterFiltering()),] 
})

geoDataToUseForMap_filtered <- reactive({
  req(myReactiveValues$dataLoaded)
  result <- NULL
  if(geoDataToUseForMap_dataSource == "geocodingResult"){
    result <- geocodingResult_filtered()
  }else{
    stop("The given geoDataToUseForMap_dataSource \"",geoDataToUseForMap_dataSource,"\" is not implemented")
  }
  
  result$geoDataLat <- result[[geoDataToUseForMap_columnNameForLat]]
  result$geoDataLon <- result[[geoDataToUseForMap_columnNameForLon]]
  result$geoDataLatLon <- paste(result[[geoDataToUseForMap_columnNameForLat]], result[[geoDataToUseForMap_columnNameForLon]], sep = " ")
  
  return (result)
})

##############################
# stats for filtered data 
##############################
metaData_stats <- reactive({
    #print("re-calc stats for meta data")
    validate(
      need(dim(metaData_filtered())[1]>0, message = "There is no data available to calculate stats. You might try to adjust the filters.")
    )
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
  validate(
    need(dim(geocodingResult_filtered())[1]>0, message = "There is no data available to calculate stats. You might try to adjust the filters.")
  )
  
  dataForStats <- geocodingResult_filtered()
  stats <- calcStatsGeocodingResult(geocodingResultData = dataForStats, 
                                    columnsToCalcDistributions = myReactiveValues$geocodingResult_columnsToCalcDistributions,
                                    availableValues = myReactiveValues$geocodingResult_availableValues,
                                    includeValuesNotUsedForDistribution = F,
                                    columnsWithMultiValues = myReactiveValues$geocodingResult_columnsWithMultiValueData, 
                                    separatorsForMultiValues = myReactiveValues$geocodingResult_multiValueSeparators, 
                                    nameEmptyStringInStatsAs = nameEmptyStringInStatsAs,
                                    columnsToUseForNumericStats = myReactiveValues$geocodingResult_columnsToCalcNumericInfos)      
})

regexData_stats <- reactive({
  
  dataIsAvaialable <- myReactiveValues$regexData_performRegExMatching && regexData_filtered()[1]>0 
  validate(need(dataIsAvaialable, message = "There is no data available to calculate stats for regex. You might try to adjust the filters or the regex under configuration."))
  dataForStats <- regexData_filtered()
  stats <- calcStatsGeneralData(inputData = dataForStats, 
                             columnsToCalcDistributions = myReactiveValues$regexData_columnsToCalcDistributions,
                             availableValues = myReactiveValues$regexData_availableValues,
                             includeValuesNotUsedForDistribution = F,
                             columnsWithMultiValues = NULL, 
                             separatorsForMultiValues = NULL, 
                             nameEmptyStringInStatsAs = nameEmptyStringInStatsAs,
                             columnsToUseForNumericStats = myReactiveValues$regexData_columnsToCalcNumericInfos)
  
})
  
##############################
# output number of results & stats
##############################

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



output$metaData_stats_distributions_plots <- renderUI({# this a bit complicated looking construct is used here for correct display of stats AND correct error message when no fields selected (and the error message doesn't slide down over the other outputs). This was the working solution I found, might be improved for nicer/better code
  #browser()
  validate(need(length(myReactiveValues$metaData_columnsToCalcDistributions)>0, message = "Calculation of stats (distributions): There are no fields configured. You can do this under 'Configuration'"))
  tablesToCreate <- c("MetaData_distribution_plots")
  lapply(tablesToCreate, function(nameToUse) {
    id <- paste0("metaData_stats_distributions_plots","_", nameToUse)
    output[[id]] <- renderPlotly(createPlotsForDistributionData(metaData_stats()$distributions, statsOfdistributions_sortByValueDesc))
  })
})

output$geocodingResult_stats_distributions_plots <-  renderUI({# this a bit complicated looking construct is used here for correct display of stats AND correct error message when no fields selected (and the error message doesn't slide down over the other outputs). This was the working solution I found, might be improved for nicer/better code
  #browser()
  validate(need(length(myReactiveValues$geocodingResult_columnsToCalcDistributions)>0, message = "GeocodingResult: Calculation of stats (distributions): There are no fields configured. You can do this under 'Configuration'"))
  tablesToCreate <- c("GeocodingResult_distribution_plots")
  lapply(tablesToCreate, function(nameToUse) {
      id <- paste0("geocodingResult_stats_distributions_plots","_", nameToUse)
      dataToUseHere <- geocodingResult_stats()$distributions
      output[[id]] <- renderPlotly(createPlotsForDistributionData(dataToUseHere, statsOfdistributions_sortByValueDesc))
    })

})

output$regexData_stats_distributions_plots <- renderUI({# this a bit complicated looking construct is used here for correct display of stats AND correct error message when no fields selected (and the error message doesn't slide down over the other outputs). This was the working solution I found, might be improved for nicer/better code
  #browser()
  dataIsAvailable <- myReactiveValues$regexData_performRegExMatching && length(myReactiveValues$regexData_columnsToCalcDistributions)>0
  validate(need(dataIsAvailable, message = "Calculation of stats (distributions): There are no fields configured. You can do this under 'Configuration'"))
  tablesToCreate <- c("RegexData_distribution_plots")
  lapply(tablesToCreate, function(nameToUse) {
    id <- paste0("regexData_stats_distributions_plots","_", nameToUse)
    output[[id]] <- renderPlotly(createPlotsForDistributionData(regexData_stats()$distributions, statsOfdistributions_sortByValueDesc))
  })
})  
  
# stats numeric tables
metaData_stats_numeric <- reactive({# needed for user error message when no columns selected for calculation 
  validate(need(length(myReactiveValues$metaData_columnsToCalcNumericInfos)>0, message = "Calculation of stats (numeric): There are no fields configured. You can do this under 'Configuration'"))
  metaData_stats()$numericInfos
}) 

geocodingResult_stats_numeric <- reactive({# needed for user error message when no columns selected for calculation distributions
  validate(need(length(myReactiveValues$geocodingResult_columnsToCalcNumericInfos)>0, message = "Calculation of stats (numeric): There are no fields configured. You can do this under 'Configuration'"))
  geocodingResult_stats()$numericInfos
})

regexData_stats_numeric <- reactive({# needed for user error message when no columns selected for calculation distributions
  
  dataIsAvailable <- myReactiveValues$regexData_performRegExMatching && length(myReactiveValues$regexData_columnsToCalcNumericInfos)>0
  validate(need(dataIsAvailable, message = "Calculation of stats (numeric): There are no fields configured. You can do this under 'Configuration'"))
  regexData_stats()$numericInfos
})

output$metaData_stats_numeric_table <- renderTable(metaData_stats_numeric(), rownames = T)
output$geocodingResult_stats_numeric_table <- renderTable(geocodingResult_stats_numeric(), rownames = T)
output$regexData_stats_numeric_table <- renderTable(regexData_stats_numeric(), rownames = T)


###################
# map
###################

# show map and update based on (filtered) geoData
output$lmap <- renderLeaflet({
  validate(need(dim(geoDataToUseForMap_filtered()[1]>0), message = "No Geolocation Data available for given filters. You might try to adjust (relax) the filters."))
  leaflet(data=geoDataToUseForMap_filtered()) %>%
    addTiles() %>%
    clearMarkers()%>%
    clearShapes()%>%
    addMarkers(
      lat = ~geoDataLat, lng = ~geoDataLon,
      layerId= ~geoDataLatLon,
      label = ~htmlEscape(paste0( entityName)),
      popup = ~htmlEscape(paste0( entityName, " (", geoDataLat, ",", geoDataLon,")"))
    )
}
)


############################
# when marker is clicked
############################

clickedMarker_infos <- reactive({})
output$clickedMarker_infos <- reactive({clickedMarker_infos()})
clickedMarkerAllOutput <- reactive({})
output$clickedMarkerAllOutput <-  reactive({renderText("click on a marker to display further information about it")})



observe({
  click<-input$lmap_marker_click
  displayClickedMarkerInfos <- F
  # filter data for marker and calc stats for marker
  clickedMarker_geoData <- geoDataToUseForMap_filtered()[which(geoDataToUseForMap_filtered()$geoDataLatLon == click$id),]
  clickedMarker_geocodingResult <- geocodingResult_filtered()[which(geocodingResult_filtered()[["latlon"]] %in% clickedMarker_geoData[["latlon"]]),]
  clickedMarker_metaData <- metaData_filtered()[which(metaData_filtered()[[metaData_columnNameForMatchWithOtherData]] %in% clickedMarker_geoData[[geoDataToUseForMap_columnNameForMatchWithOtherData]]),]
  
  
  if(dim(clickedMarker_metaData)[1]==0 & dim(clickedMarker_geocodingResult)[1]==0){
    displayClickedMarkerInfos <- F
  }else{
    displayClickedMarkerInfos <- T
  }
  
  if(displayClickedMarkerInfos){
    
    clickedMarker_stats <- calcStatsPerMapPoint(geocodingResultReducedToPointData = clickedMarker_geocodingResult, 
                                                geocodingResult_columnsToCalcDistributions = myReactiveValues$geocodingResult_columnsToCalcDistributions, 
                                                geocodingResult_availableValues = myReactiveValues$geocodingResult_availableValues, 
                                                geocodingResult_includeValuesNotUsed = F,
                                                geocodingResult_columnsWithMultiValues = myReactiveValues$geocodingResult_columnsWithMultiValueData, 
                                                geocodingResult_separatorsForMultiValues = myReactiveValues$geocodingResult_multiValueSeparators, 
                                                geocodingResult_nameEmptyStringInStatsAs = nameEmptyStringInStatsAs,
                                                geocodingResult_columnsToCalcNumericInfos = myReactiveValues$geocodingResult_columnsToCalcNumericInfos,
                                                metaDataReducedToPointData = clickedMarker_metaData, 
                                                metaData_columnsToCalcDistributions = myReactiveValues$metaData_columnsToCalcDistributions, 
                                                metaData_availableValues = myReactiveValues$metaData_availableValues, 
                                                metaData_includeValuesNotUsed = F,
                                                metaData_columnsWithMultiValues = myReactiveValues$metaData_columnsWithMultiValueData, 
                                                metaData_separatorsForMultiValues = myReactiveValues$metaData_multiValueSeparators, 
                                                metaData_nameEmptyStringInStatsAs = nameEmptyStringInStatsAs,
                                                metaData_columnsToCalcNumericInfos = myReactiveValues$metaData_columnsToCalcNumericInfos,
                                                metaData_columnNameForMatchWithOtherData = metaData_columnNameForMatchWithOtherData)
    

    # output general infos
    clickedMarker_infos<-renderText({
      heading <- paste0("INFORMATION ABOUT CLICKED MARKER: \"",unique(clickedMarker_geoData$entityName),"\"\n===========================\n")
      part1 <- paste0("Location string: ", unique(clickedMarker_geoData$entityName),"\n",
                      "lat/lon: ", unique(clickedMarker_geoData$latlon), "\n",
                      "\nGEOCODING RESULT\n================\n",
                      "Official location name(s): ", paste(clickedMarker_stats$geoCodingResult_distinctLocationNames,collapse = " | "),"\n",
                      "number of times locations found: ", clickedMarker_stats$geoCodingResult_numberOfTimesLocationsFound,"\n",
                      "number of dictinct docs location found in: ", clickedMarker_stats$geocodingResult_numberOfDictinctDocsLocationFoundIn,"\n"
      )
      textToDisplay <- paste0(heading,part1)
    })
    
    # output in which docs the marker was found (id, frequency, title)
    clickedMarkerDistribution <- renderTable(clickedMarker_stats$geocodingResult_distributionInDocs,rownames = T)
    
    #----------------------------------
    # output of meta data infos (distributions and numeric) - infos about docs the location was found in
    #-----------------------------------
    
    #output$clickedMarker_metaDataStats_plots <- plotDistributionData(clickedMarker_stats$metaData_distributions) # meta data distributions as plots
    clickedMarker_metaData_distributions_tables <- renderUI({ # meta data distributions as tables: better overview if we have just a few docs per marker
      
      validate(need(length(myReactiveValues$metaData_columnsToCalcDistributions)>0, message = "Calculation of stats (distributions): There are no fields configured. You can do this under 'Configuration'"))
      
      clickedMarker_stats_metaData_distributions_aspects <- names(clickedMarker_stats$metaData_distributions)
      
      lapply(clickedMarker_stats_metaData_distributions_aspects, function(aspectName) {# lapply needed because else only last table is used due to: The expressions passed into render functions are captured in closures and not evaluated immediately (See https://community.rstudio.com/t/shiny-app-with-dynamic-number-of-datatables/2405/7)
        
        id <- paste0("clickedMarker_metaData_distributions_tables","_", aspectName)
        tableToUse <- clickedMarker_stats$metaData_distributions[[aspectName]]
        names(tableToUse) <- c(aspectName,"frequency","percent")
        output[[id]] <- renderTable(tableToUse)
      })
    })
    
    clickedMarker_metaData_numeric_tables <- renderUI({
      validate(need(length(myReactiveValues$metaData_columnsToCalcNumericInfos)>0, message = "Calculation of stats (numeric): There are no fields configured. You can do this under 'Configuration'"))
      tablesToCreate <- c("MetaDataNumeric")
      lapply(tablesToCreate, function(nameToUse) {
        id <- paste0("clickedMarker_metaData_numeric_tables","_", nameToUse)
        tableToUse <- clickedMarker_stats$metaData_numericStats
        output[[id]] <- renderTable(tableToUse, rownames = T)
      })
    })
    #clickedMarker_metaData_numeric_tables <- renderTable(clickedMarker_stats$metaData_numericStats)
    #----------------------------------
    # output of geoCodingResult infos (distributions and numeric) - infos about docs the location was found in
    #-----------------------------------
    
    clickedMarker_geocodingResult_distributions_tables <- renderUI({
      validate(need(length(myReactiveValues$geocodingResult_columnsToCalcDistributions)>0, message = "Calculation of stats (distributions): There are no fields configured. You can do this under 'Configuration'"))
      
      clickedMarker_stats_geocodingResult_distributions_aspects <-  names(clickedMarker_stats$geocodingResult_distributions)
      
      lapply(clickedMarker_stats_geocodingResult_distributions_aspects, function(aspectName) {
        id <- paste0("clickedMarker_geocodingResult_distributions_tables","_", aspectName)
        tableToUse <- clickedMarker_stats$geocodingResult_distributions[[aspectName]]
        names(tableToUse) <- c(aspectName,"frequency","percent")
        output[[id]] <- renderTable(tableToUse)
      })
    })
    
    #clickedMarker_geocodingResult_numeric_tables <- renderTable(clickedMarker_stats$geocodingResult_numericStats, rownames = T)
    clickedMarker_geocodingResult_numeric_tables <- renderUI({
      validate(need(length(myReactiveValues$geocodingResult_columnsToCalcNumericInfos)>0, message = "Calculation of stats (numeric): There are no fields configured. You can do this under 'Configuration'"))
      tablesToCreate <- c("geocodingResultNumeric")
      lapply(tablesToCreate, function(nameToUse) {
        id <- paste0("clickedMarker_geocodingResult_numeric_tables","_", nameToUse)
        tableToUse <- clickedMarker_stats$geocodingResult_numericStats
        output[[id]] <- renderTable(tableToUse, rownames = T)
      })
    })
    
    #----------------------------------
    # output of regEx infos (distributions and numeric) infos about regex matches found in doc text
    #-----------------------------------
    
    
    # put all clicked marker infos in one object
    clickedMarkerAllOutput <- renderUI({
      result <- tagList(
        # clicked marker infos is not included here but used separatetely because verbatimTextOutput should be used for nicer output
        h4("In which documents this marker was found:"),
        clickedMarkerDistribution,
        h4("geocodingResult info about this marker"),
        clickedMarker_geocodingResult_distributions_tables,
        clickedMarker_geocodingResult_numeric_tables,
        h4("meta data info for docs containing this marker"),
        clickedMarker_metaData_distributions_tables,
        clickedMarker_metaData_numeric_tables
      )
      return (result)
    }
    )
  }
  else{ # displayClickedMarkerInfos == F
    clickedMarker_infos <- renderText("")
    clickedMarkerAllOutput <- renderText("click on a marker to display further information about it")
  }
  
  output$clickedMarker_infos <- clickedMarker_infos
  output$clickedMarkerAllOutput <- clickedMarkerAllOutput
  
})