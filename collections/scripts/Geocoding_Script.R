source("global/functions_used_in_scripts.R")
source("global/log_to_file.R")
source("global/geo_functions.R")
source("global/text_functions.R")
source("global/rbind_huge_sparse_Matrix.R")
source("config_file.R")


error<-try(expr = {
  library(Matrix)
  library(dplyr)
  library(spacyr)
  
  #load parameters
  load("collections/tmp/tmp.RData")
  parameters_original<-parameters
  

  
  #load collection 
  log_to_file(message = "<b>Step 1/8: Loading collection</b>",file = logfile)
  load(paste("collections/collections/",unlist(parameters[1]),".RData",sep=""))
  log_to_file(message = "  <b style='color:green'> ✔ </b> Finished loading collection",file = logfile)
  
  
  #load data from database
  log_to_file(message = "<b>Step 2/8: Loading data from database now</b>",file = logfile)
  #db_data<-get_token_meta_and_language_from_db(host=host,port=db_port,id=info[[1]],dataset=info[[2]])
  db_data<-get_token_meta_and_language_from_db(get_meta = F,get_language = T,get_global_doc_ids = F,host=host,port=db_port,id=info[[1]],dataset=info[[2]])
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished loading data from database",file = logfile)
  
  
  
  #sanity check
  log_to_file(message = "<b>Step 3/8: Sanity check</b>",file = logfile)
  #token object not empty
  log_to_file(message = "&emsp; token object not empty?",logfile)
  if(dim(db_data$token)[1]>1){
    log_to_file(message = "&emsp; ✔",logfile)
  }else{
    log_to_file(message = "&emsp;<b style='color:red'>&#10008; No documents were found in the database for the specified collection.</b>",logfile)
    stop("Token empty")
  }
  #specified language has quanteda stopwordlist?
  if(parameters$remove_stopwords==T){
    log_to_file(message = "&emsp; stopwords available for specified language?",logfile)
    if(!(db_data$language%in%stopwords::stopwords_getlanguages(source="stopwords-iso"))){
      log_to_file(message = "&emsp;<b style='color:red'>&#10008; The specified language is not included in stop word list. You can maybe specify a stopwordlist using the blacklist functionality.</b>",file = logfile)
      stop("Stopwords not available for found language")
    }
    else{
      log_to_file(message = "&emsp; ✔",logfile)
    }
  }
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished sanity checks",file = logfile)
  
  
  
  #preparing parameters
  log_to_file(message = "<b>Step 4/8: Preparing input parameters</b>",file = logfile)
  parameters<-prepare_input_parameters(parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing input parameters",file = logfile)
  
  
  #preparing token object
  log_to_file(message = "<b>Step 5/8: Preparing token object</b>",file = logfile)
  db_data$token<-prepare_token_object(token = db_data$token,parameters=parameters)
  log_to_file(message = "  <b style='color:green'> ✔ </b>  Finished preparing token object",file = logfile)
  
  
  #calculating dtm (do this also with the reason of filtering)
  log_to_file(message = "<b>Step 6/8: Calculating DTM</b>",file = logfile)
  dtm<-calculate_dtm(token = db_data$token,parameters = parameters,lang = db_data$language)
  log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished pre-processing with",dim(dtm)[1], "documents and ",dim(dtm)[2], "features"),file = logfile)
  
  #######################
  # actual geocoding part
  # see Example_geocoding to see example code on how to perform geocoding with data frame as input instead of dtm
  #######################

  # set some parameters (some might be changed to be selectable via GUI)
  filepathHashtableGeocodingCache <- "collections/geolocations/hashtableGeocodingCacheOSM"
  assignCountryInfoForEachEntry <- T
  if(assignCountryInfoForEachEntry){
    options(geonamesUsername=geonamesUsername) # contained in config_file.R sourced at the beginning
  }
  filenpathHashtableCoordLatLonCountryInfos <- "collections/geolocations/hashtableCoordLatLonCountryInfos.RData"
  
  useWholeDataInsteadOfPerAreaIDToRetrieveLocationFrequenciesAndToApplySecondFilter <- F 
  
  # define functions to retrieve needed data from inputData containing dtm
  inputDataDTM <- list(dtm = dtm)
  functionToGetDistinctLocationStrings <- dtm_getUniqueFeaturesFromDTM # see global/geo_functions.R
  functionToRetrieveUniqueAreaIds <- dtm_getUniqueDocIDs # see global/geo_functions.R
  functionToGetLocationsAndFrequencyForGivenAreaId <- dtm_getTokensAndFrequencyForGivenDocId # see global/geo_functions.R
  inputDataForLocationStringsAndOptionalAreaIds <- inputDataDTM
  
  # define geocoding functions needed using predefined functions for convenience
  functionToGetGeolocationsFromString <- function(inputString, cache){getGeolocationForStringUsingOSMDefault(inputString, cache)}
  functionToUpdateCacheForGeoLocationString <- functionToGetGeolocationsFromString
  
  # define filter 1 (to filter on geoResult(s) for a single String)
  functionToFilterOrSelectGeoResultForALocationString <- function(x){# should return a data frame with at least lat/lon as columns and several rows for multiple results
      result <- selectBasedOnMaxValue(inputData = x, fieldName = "importance")
      result <- selectBasedOnGivenMinMaxValue(inputData = result, fieldName = "importance", minValue = 0.5)
    } 
  
  # define filter 2 (to filter among all locations within one areaID (e.g. within document, paragraph, collection))
  functionToFilterGeoResultsPerArea <- function(x){x} # or e.g. use filterForEntitiesInClusterWithMinDistanceToCenterOfGravity
  functionToFilterGeoResultsPerArea <- filterForEntitiesInClusterWithMinDistanceToCenterOfGravity
  
  # define filter 3
  functionToFilterGeoResultsForWholeData <- function(x){x}
  #functionToFilterGeoResultsForWholeData <- filterForEntitiesInClusterWithMinDistanceToCenterOfGravity
  
  log_to_file(message = "<b>Step 7/8: load cached geo information</b>",file = logfile)
  
  # load cached geo information
  if(!file.exists(filepathHashtableGeocodingCache)){
    cacheForGeocodingData <- new.env(hash=TRUE)
    save(cacheForGeocodingData, file = filepathHashtableGeocodingCache)
  }
  if(!exists("cacheForGeocodingData")){
    load(file = filepathHashtableGeocodingCache)
  }
  log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished loading or initializing cached geo information",dim(dtm)[1], "documents and ",dim(dtm)[2], "features"),file = logfile)
  
  log_to_file(message = "<b>Step 8/8: perform geocoding</b>",file = logfile)
  
  tryCatch(
    {
      # perform geocoding
      geocodingResult <- performGeoCodingWithCacheAndFiltering(
        inputDataForLocationStringsAndOptionalAreaIds, 
        functionToGetDistinctLocationStrings,
        useWholeDataInsteadOfPerAreaIDToRetrieveLocationFrequenciesAndToApplySecondFilter,
        functionToRetrieveUniqueAreaIds,
        functionToGetLocationsAndFrequencyForGivenAreaId,
        cacheForGeocodingData,
        functionToUpdateCacheForGeoLocationString,
        functionToFilterOrSelectGeoResultForALocationString,
        functionToFilterGeoResultsPerArea,
        functionToFilterGeoResultsForWholeData
      )
      
      if(assignCountryInfoForEachEntry){
        applyCountryInfosForLatLonForDataframe(inputDataframe = geocodingResult, filenpathHashtableCoordLatLonCountryInfos = filenpathHashtableCoordLatLonCountryInfos, columnForLat = "latitude", columnForLon = "longitude",targetColumnnameForCountryName = "countryName", targetColumnNameForCountryCode = "countryCode")
      }
      
            
    },
    finally={
      save(cacheForGeocodingData, file = filepathHashtableGeocodingCache)
      
    }
  )
  
  log_to_file(message = paste("  <b style='color:green'> ✔ </b>  Finished geocoding",dim(dtm)[1], "documents and ",dim(dtm)[2], "features"),file = logfile)
  
  ##################
  # save results
  ##################
  log_to_file(message = "<b>Final step: Save results</b>",file = logfile)
  
  save(cacheForGeocodingData, file = filepathHashtableGeocodingCache)
  path<-paste(parameters$id,parameters$collection,sep = "_")
  path0<-paste0("collections/results/geocoding/",path,"/")
  dir.create(path0)
  save(geocodingResult,file = paste0(path0,"geocodingResult.RData"))
  save(dtm,file=paste0(path0,"dtm.RData"))
  save(info,file=paste0(path0,"info.RData"))
  parameters<-parameters_original
  save(parameters,file=paste0(path0,"parameters.RData"))
  log_to_file(message = "   <b style='color:green'> ✔ </b> Finished saving results",logfile)
  
  
  log_to_file(message = " <b style='color:green'>Process finished successfully. You can check the results in Collection Worker &#8594; Results &#8594; Geocoding </b>",logfile)
  system(paste("mv ",logfile," collections/logs/finished/",sep=""))
  
  print("Process of geocoding finished successfully")
})

# if(class(error)=="try-error"){
#   system(paste("mv ",logfile," collections/logs/failed/",sep=""))
#   RMariaDB::dbDisconnect(mydb)
#   log_to_file(message=error[[1]],file = stringr::str_replace(string = logfile,pattern = "running",replacement = "failed"))
# }
#   