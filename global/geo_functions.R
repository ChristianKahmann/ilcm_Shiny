library("geonames")
library("tmaptools")
library("geosphere")
library("stringr")
library("dplyr")
source("global/log_to_file.R")


#' Returns country infos for given lat lon coordinates
#' Uses the given hashtable as cached reference by first querying the hashtable or if not yet existent there directly via geonames service. The result is stored in hashtable as well 
#' Attention! Before executing this function, first a registration at geonames is required as well as setting/executing the following: options(geonamesUsername=geonames.username) In geocodingScript this is done by using the credientials in config_file.R
#' behaviour of querying the geonames API:
#' - A) success: a country was found, the result is also stored in hashtable for future
#' - B) error no country found for given radius: try again by increasing radius in steps of 10 until maxRadiusIfDefaultRadiusLeadsToNoResults
#' - C) other error: This is usally a timeout because of to many requests at a time, wait 10 seconds and try again (just once)
#' return values and hashtable update based on result. If the final result is 
#' - A) success: countryCode, countryName, countryLanguages are returned, and also stored in hashtable
#' - B) no country found even by increasing until maxRadiusIfDefaultRadiusLeadsToNoResults. All values of countryCode, countryName, countryLanguages are set to the string "UNKNOWN_because_of_noCountryFoundInGivenRadius". This is not stored in hash, since increasing maxRadiusIfDefaultRadiusLeadsToNoResults might lead to a result in the future
#' - C) other error: All values of countryCode, countryName, countryLanguages are set to the string == paste0("UNKNOWN_because_of Error: ", error$message). This is not stored in hash, since the error is unknown at it might not happen in the future
#' @param lat The latitude value
#' @param lon The longitude value
#' @param hashtableLatLonCountryInfos The cached info as hashtable containing "latValue lonValue" (separated by space) as key and the countryInfo data provided by geonames as value
#' @param defaultRadius The radius to use to query a country for given lat lon
#' @param maxRadiusIfDefaultRadiusLeadsToNoResults if the radius in defaultRadius doesn't result in a country, further attempts are made (by increasing steps of 10) until maxRadiusIfDefaultRadiusLeadsToNoResults
#' @return a data frame containing lat, lon, countryCode, countryName, countryLanguages
#' 
getCountryInfosForLatLon <- function(lat, lon, hashtableLatLonCountryInfos, defaultRadius, maxRadiusIfDefaultRadiusLeadsToNoResults){
  #print(paste("LAT LON: ", lat,lon))
  
  # check if lat lon are in stored data
  searchKey <- paste(lat, lon, sep=" ")
  if(!is.null(hashtableLatLonCountryInfos[[searchKey]])){
    #print(paste("latlon already saved in hashtable environment, no need to query geonames for ", lat, lon, ": ", hashtableLatLonCountryInfos[[searchKey]]$countryName, sep =" "))
    return(hashtableLatLonCountryInfos[[searchKey]])
  }
  else{# else query geonames
    
    countryInfoEnvironment <- new.env() # needed because inside error-function values of variables ouside of function are not accessible
    
    # create needed variables in environment. necessary because catch block is interpreted as function, which means variables from outside catch block can't be changed. They can be assessed and changed by using environment.
    assign("successType", NULL, env=countryInfoEnvironment)
    assign("radiusToUse", defaultRadius, env=countryInfoEnvironment)
    assign("maxRadiusToUse", maxRadiusIfDefaultRadiusLeadsToNoResults, env=countryInfoEnvironment)
    assign("timeoutAttempts", 0, env=countryInfoEnvironment)
    assign("maxTimeOutAttempts", 1, env=countryInfoEnvironment)
    assign("stepSize", 10, env=countryInfoEnvironment)
    assign("geonamesCountryCode", NULL, env=countryInfoEnvironment)
    
    print(paste("latlon not yet saved in hashtable,  need to query geonames for ", lat, lon, sep =" "))
 
    tryCatch({
      geonamesCountryCode <- GNcountryCode(lat = lat, lng = lon, lang = "en", radius = defaultRadius)
      assign("geonamesCountryCode", geonamesCountryCode, env = countryInfoEnvironment)
      assign("successType", "success", env=countryInfoEnvironment)

    },
    
    error = function(e){

      if(grepl(x = e$message, pattern = "no country code found", fixed = T)){
        assign("successType", "noCountryFoundInGivenRadius", env=countryInfoEnvironment)
        assign("radiusToUse", get("radiusToUse", env=countryInfoEnvironment)+get("stepSize", env=countryInfoEnvironment), env=countryInfoEnvironment)
        
        #print(paste0("- Warning: for lat lon ", lat, ", ", lon, " no country could be found within radius ",get("radiusToUse", env=countryInfoEnvironment), "."))
      
      }else{
        assign("successType", e$message, env=countryInfoEnvironment)
        assign("timeoutAttempts", get("timeoutAttempts", env=countryInfoEnvironment)+1, env=countryInfoEnvironment)
        
        #print(paste0("- Warning: for lat lon ", lat, ", ", lon, " no country could be found based on error: ", e$message))
      }
      
      #while(radiusToUse <= maxRadiusIfDefaultRadiusLeadsToNoResults && timeoutAttempts <= maxTimeOutAttempts){
      while(get("radiusToUse", env=countryInfoEnvironment) <= get("maxRadiusToUse", env=countryInfoEnvironment) && get("timeoutAttempts",env=countryInfoEnvironment) <= get("maxTimeOutAttempts",env=countryInfoEnvironment)){
        tryCatch({
            geonamesCountryCode <- GNcountryCode(lat = lat, lng = lon, lang = "en", radius = get("radiusToUse", env=countryInfoEnvironment))
            assign("geonamesCountryCode", geonamesCountryCode, env=countryInfoEnvironment)
            assign("successType", "success", env=countryInfoEnvironment) #successType <- "success"
            #print(paste0("- Success for lat lon ", lat, lon, " finally found with radius ", get("radiusToUse", env=countryInfoEnvironment),": ",geonamesCountryCode$countryName))
            
            break
            },
            error = function(e){

              if(grepl(x = e$message, pattern = "no country code found", fixed = T)){

                assign("successType", "noCountryFoundInGivenRadius", env=countryInfoEnvironment) # #successType <- "noCountryFoundInGivenRadius"
                assign("radiusToUse", get("radiusToUse", env=countryInfoEnvironment)+get("stepSize", env=countryInfoEnvironment), env=countryInfoEnvironment) #  #radiusToUse <- radiusToUse + stepSize
                #print(paste0("- Warning: for lat lon ", lat, ", ", lon, " no country could be found within radius ",get("radiusToUse", env=countryInfoEnvironment), "."))
                
              }else{
                assign("successType", e$message, env=countryInfoEnvironment) #successType <- e$message
                assign("timeoutAttempts", get("timeoutAttempts", env=countryInfoEnvironment)+1, env=countryInfoEnvironment)  #timeoutAttempts <- timeoutAttempts+1
                #print(paste0("- Warning: for lat lon ", lat, ", ", lon, " no country could be found based on error: ", e$message))
                
              }
            }# end of error function 2
          )
        
      } # end of while loop
      } # end of error function 1
    )# end of try catch
    
    
    # either success or nothing found because of noCountryFoundInGivenRadius or other error
    successType <- get("successType", env=countryInfoEnvironment)
    if(successType == "success"){
      geonamesCountryCode <- get("geonamesCountryCode", NULL, env=countryInfoEnvironment)
      
      createdValue <- data.frame(lat = lat, lon = lon, countryCode = geonamesCountryCode$countryCode, countryName = geonamesCountryCode$countryName, countryLanguages = geonamesCountryCode$languages)
      hashtableLatLonCountryInfos[[searchKey]] <- createdValue
      
    }else if(successType == "noCountryFoundInGivenRadius"){
      
      entryToWrite <- "UNKNOWN_because_of_noCountryFoundInGivenRadius"
      createdValue <- data.frame(lat = lat, lon = lon, countryCode = entryToWrite, countryName = entryToWrite, countryLanguages = entryToWrite)
      
      warningMessage <- paste0("WARNING: for lat lon ", lat, ", ", lon, " no country could be found within radius",get("radiusToUse", env=countryInfoEnvironment), ". Result will be: ", entryToWrite)
      #print(warningMessage)
      log_to_file(message = warningMessage,file = logfile)
      
    }else{
      entryToWrite <- paste0("UNKNOWN_because_of Error: ", successType)
      createdValue <- data.frame(lat = lat, lon = lon, countryCode = entryToWrite, countryName = entryToWrite, countryLanguages = entryToWrite)

      warningMessage <- paste0("WARNING: for lat lon ", lat, ", ", lon, " no country could be found within radius ",get("radiusToUse", env=countryInfoEnvironment), ". Result will be: ", entryToWrite)
      #print(warningMessage)
      log_to_file(message = warningMessage,file = logfile)
     }
    
    return(createdValue)
    

  } # end of else query geonames
  
}

# convenience function
applyCountryInfosForLatLonForDataframe <- function(hashtableLatLonCountryInfos, inputDataframe, columnForLat, columnForLon, targetColumnnameForCountryName, targetColumnNameForCountryCode, defaultRadius, maxRadiusIfDefaultRadiusLeadsToNoResults){
  

  for(i in 1:nrow(inputDataframe)) 
  {
    foundCountryInfo <- getCountryInfosForLatLon(lat = inputDataframe[[columnForLat]][i] , lon = inputDataframe[[columnForLon]][i], hashtableLatLonCountryInfos = hashtableLatLonCountryInfos, defaultRadius, maxRadiusIfDefaultRadiusLeadsToNoResults)
    if(is.data.frame(foundCountryInfo) & nrow(foundCountryInfo)>0){
      inputDataframe[[targetColumnnameForCountryName]][i] <- foundCountryInfo$countryName
      inputDataframe[[targetColumnNameForCountryCode]][i] <- foundCountryInfo$countryCode
    }
    else{
      #print(paste (i, " nothing found for " , ecpmfDataFromCSV$Location.lat[i], " ", ecpmfDataFromCSV$Location.lon[i], ": " , indexesOfFoundCountry, sep = " ", collapse = NULL))
      inputDataframe[[targetColumnnameForCountryName]][i] <- "UNKNOWN"
      inputDataframe[[targetColumnNameForCountryCode]][i] <- "UNKNOWN"

    }
  }
  save(hashtableLatLonCountryInfos, file = filenpathHashtableCoordLatLonCountryInfos)
  return(inputDataframe)
  
}

#' Geocoding function. Returns geolocation coordinates (lat,lon, and other data) for a given location String
#' First, the location is queried from the cache (hashtable) and if not found there queried by the specified geocodingFunction and stored in cache afterwards
#' The returned value is a list of 2 elements: successType and result. The successType can have the values "success" or "noResultsFound". 
#' For "success", the result contains the result (depending on the geocodingfunction), "noResultsFound" will occur when no location could be retrieved for the goiven string (strings being no location). These are cached as well. 
#' In some cases, an error might occur, in theses cases the processing is stopped and the error returned
#' @param inputString The string of the location, e.g. "Leipzig" or "New York"
#' @param hashtableCachedGeoInformation A hashtable with cached information for faster querying of strings queried before
#' @param geocodingFunction The function to retrieve the coordinates when the information is not found in the cache/hashtable. Must be a list with 2 elements: sucessType and result. See description above. SucessType may have additional value "problemOccured" where the error is stored in result. In this case the process is stpooed and the error returned. 
#' @return An list with 2 elements: successType and result: The successType can have the values "success" or "noResultsFound". For "success", the result contains the result (depending on the geocodingfunction), "noResultsFound" will occur when no location could be retrieved for the goiven string (strings being no location). These are cached as well. In some cases, an error might occur, in theses cases the processing is stopped and the error returned
#' @examples getGeolocationForString("New York", hashtableCachedGeoInformation, geoCodingFunction)
#' 
getGeolocationForString <- function(inputString, hashtableCachedGeoInformation, geocodingFunction){
  
  searchKey <- str_trim(inputString, side = "both")
  if(nchar(searchKey) ==0){
    result$successType = "noResultsFound"
    result$result <- NULL
    return(result)
  }
  # already in cache?
  if(!is.null(hashtableCachedGeoInformation[[searchKey]])){
   # print("already in cache")
    log_to_file(message = paste("<b>Geododing string \"", searchKey, "\": already in cache</b>"),file = logfile)
    
    result <- hashtableCachedGeoInformation[[searchKey]]
    return(result)
  }
  
  #print("not in cache. needs to be queried")
  log_to_file(message = paste("<b>Geododing string \"", searchKey, "\": not in cache. needs to be queried</b>"),file = logfile)
  
  result <- geocodingFunction(searchKey)
  if(result$successType == "success"){
    hashtableCachedGeoInformation[[searchKey]] <- result
  }
  else if(result$successType == "noResultsFound"){
    hashtableCachedGeoInformation[[searchKey]] <- result
    #warning("Warning: No results found when trying to find location via geocoding for \"", searchKey, "\"")
  }
  else if(result$successType == "problemOccured"){
    stop(paste("A problem occured when trying to retrieve geolocation for ", searchKey, "This is the Error:" , result$result, sep =" " ))
  }
  else{
    stop(paste("Unknown successtype", result$successType, ". This type needs proper handling in code.", sep =" "))
  }
  
  return (result)
}


#' Geocoding function. Returns geolocation coordinates (lat,lon, and other data) for a given location String. Convenience function to perform @getGeolocationForString using geocode_OSM with default values
#' Executes function @getGeolocationForString with a predefined geocodingfunction. This geocodingFunction queries geoCode_OSM with default values return.first.only = F,details = T, as.data.frame = T and converts results to be conform with function @getGeolocationForString (containing correct successType and result)
#' @param inputString The string of the location, e.g. "Leipzig" or "New York"
#' @param hashtableCachedGeoInformation A hashtable with cached information for faster querying of strings queried before
#' @return An list with 2 elements: successType and result: The successType can have the values "success" or "noResultsFound". For "success", the result contains the result (depending on the geocodingfunction), "noResultsFound" will occur when no location could be retrieved for the goiven string (strings being no location). These are cached as well. In some cases, an error might occur, in theses cases the processing is stopped and the error returned
#' @examples getGeolocationForStringUsingOSMDefault("New York", hashtableCachedGeoInformation)
#' 
getGeolocationForStringUsingOSMDefault <- function(inputString, hashtableCachedGeoInformation){
  
  log_to_file(message = paste("<b>Geocoding: get OSM geolocation for \"", inputString,"\"</b>"),file = logfile)
  result <- list()
  result <- getGeolocationForString(inputString, hashtableCachedGeoInformation, 
                                      geocodingFunction = function(x){ 
                                        result <- NULL
                                        tryCatch({
                                          osmResult <- geocode_OSM(q=inputString, return.first.only = F,details = T, as.data.frame = T)

                                          if(!is.null(osmResult$message)){
                                            if(grepl(x = osmResult$message, pattern = "No results found", fixed = TRUE)){
                                              #no results found
                                              result$successType <- "noResultsFound" #sometimes an warning is thrown when no results were found, this is catched further down
                                              result$result <- NULL
                                            }else{
                                              #other problem
                                              result$successType <- "problemOccured"
                                              result$result <- osmResult
                                            }
                                          }
                                          else{#success
                                            result$successType <- "success"
                                            result$result <- osmResult
                                            return (result)
                                          }
                                        }, error = function(e) {

                                            print(paste("A problem (error) occured when trying to retrieve geolocation for ", inputString, "This is the Error:" , message(e), sep =" " ))
                                            result$successType <- "problemOccured"
                                            result$result <- e
                                            return (result)
                                        }, warning = function(w) {

                                            if(!is.null(w$message)){
                                              if(grepl(x = w$message, pattern = "No results found", fixed = TRUE)){
                                                #no results found
                                                result$successType <- "noResultsFound"
                                                result$result <- NULL
                                              }else{
                                                #other problem
                                                #print(paste("A problem (warning) occured when trying to retrieve geolocation for ", inputString, "This is the Error:" , message(w), sep =" " ))
                                                result$successType <- "problemOccured"
                                                result$result <- osmResult
                                              }
                                            }
                                            return (result)
                                        }
                                      ) # end try catch
                                     } # end function definition
                                    ) # end call getGeolocationForString

  
  return (result)
}




#################
#' Performs geocoding (using cache if possible) and additional filtering for given location strings.
#' @Details 
#' - Uses a geoCoding cache and a function to retrieve lat/lon info for location strings not yet in cache. location strings for which no location could be retrieved are skipped and not contained in result 
#' - a first filter can be applied to the geo result for each location sting. Those filter might only allow locations of certain types (e.g. just cities) but might also sort & filter among multiple results for a given string, e.g. when multiple lat/lon values "Leipzig" found, the filter might go for the most important based on importance value provided by the geoResult.  If after this filter mutliple values exist for the given string, the first one is taken.
#' - a second filter can be applied to further filter a collection of location strings and their geo results (e.g filter just certain types like cities, only include the most specific ones (e.g. keep just Leipzig, but not Germany), only those with certain/top frequency, perform clustering and include only those entities belonging to the cluster closest to center of gravity)
#' - this second filter can be applied to the whole data or to on certain defined "areas" (defined by area ids, which might define a document, paragraph, collection, etc.)
#' @param inputDataForLocationStringsAndOptionalAreaIds 
#' @param functionToGetUniqueLocationStrings the function to get unique location strings from the input data
#' @param useWholeDataInsteadOfPerAreaIDToRetrieveLocationFrequenciesAndToApplySecondFilter if set to true the frequency calcualtion and the second filter is performed on whole data, if false (default) the frequency calcualtion and the second filter is performed separately on the data of each areaId provided by the function @functionToRetrieveUniqueAreaIds (e.g. doc_ids, collection_ids, etc)
#' @param functionToRetrieveUniqueAreaIds a function to return unique area ids from the input data (convenience functions for dataframe and dtm are available). This function is only used if @useWholeDataInsteadOfPerAreaIDToRetrieveLocationFrequenciesAndToApplySecondFilter == F
#' @param functionToGetLocationStringsAndFrequencyForGivenAreaId a function returning unique location strings and their frequency for the given area id. Should return a list or data frame with variables "entityName" and "frequencyInArea". In case of @useWholeDataInsteadOfPerAreaIDToRetrieveLocationFrequenciesAndToApplySecondFilter == T, "all" is used as areaId input. So make sure the function returns location strings and frequencies for whole data for areaId == "all". For convenience, functions for data frame and dtm are availbale. See further below. 
#' @param cacheForGeocodingData The cache where the existing geoCoding data can be found, i.e. a hashtable with cached information for faster querying of location strings queried before
#' @param functionToUpdateGeoCodingCacheForGeoLocationString function to update geocoding cache with given location (check if already in cache if not query geocoding service). For convenience OSM function available
#' @param functionToFilterOrSelectGeoResultForALocationString filter 1:function to filter or select results from potential multiple lat/lon results resulting from a string (e.g. select the one with max importance, filter for cities, exclude ways, etc.). Should deal with the geo result returned by the cache. If after this filtering still multiple exist, the first one is taken. This function will also be applied if just one lat/lon result is there to allow filtering for certain entities like cities etc.
#' @param functionToFilterGeoResultsPerArea filter 2: further filtering of geo results for the given area (potential possibilities e.g. select just the most frequent, the most specific ones, perform clustering and select from these, etc)
#' @param functionToFilterGeoResultsForWholeData filter 3: apllied to all geo results (input is a dataframe with the following fields: entitiyName, areaID, frequencyInArea, latitude, longitude, plus all fields of the geoResult retrieved for a string via @functionToUpdateGeoCodingCacheForGeoLocationString / @cacheForGeocodingData ) 
#' @return data frame with all locations and detailed geo info (columns per location entry: entityName, areaId, frequencyInArea, latitude, longitude, [columnsOfGeoResultReturnByCache])
#' @export
#'
#' @examples
#' # define general params
#' cacheForGeocodingData <- new.env(hash=TRUE)
#' useWholeDataInsteadOfPerAreaIDToRetrieveLocationFrequenciesAndToApplySecondFilter <- T
#'  
#' functionToGetGeolocationsFromString <- function(inputString, cache){getGeolocationForStringUsingOSMDefault(inputString, cache)}
#' functionToUpdateCacheForGeoLocationString <- functionToGetGeolocationsFromString
#' functionToFilterOrSelectGeoResultForALocationString <- function(x){selectOSMGeoResultBasedOn(osmResult = x, selectionType = "importance")} # should return a data frame with at least lat/lon as columns and several rows for multiple results
#' 
#' # depending on having a data frame (A) or dtm (B) as input you can set the following convenience functions
#' # A) define data and functions for data frame input (use convenience functions already defined for data frame - see further below))
#' inputData_dataFrame <- list(tokenData <- db_data$token, columnNameOfLocationString <- "lemma", columnNameOfAreaId <- "doc_id")
#' functionToGetDistinctLocationStrings <- dataframe_getUniqueTokens
#' functionToRetrieveUniqueAreaIdsTo <- dataframe_getUniqueAreaIDs
#' functionToGetLocationStringsAndFrequencyForGivenAreaId <- dataframe_getTokensAndFrequenciesForGivenAreaId
#' 
#' # B) define data and functions for dtm input (use convenience functions already defined - see further below )
#' inputDataDTM <- list(dtm = dtm)
#' inputDataForLocationStringsAndOptionalAreaIds <- inputDataDTM
#' functionToGetDistinctLocationStrings <- dtm_getUniqueFeaturesFromDTM
#' functionToRetrieveUniqueAreaIds <- dtm_getUniqueDocIDs
#' functionToGetLocationStringsAndFrequencyForGivenAreaId <- dtm_getTokensAndFrequencyForGivenDocId
#' 
#' # define second filter
#' functionToFilterGeoResultsPerArea <- filterForEntitiesInClusterWithMinDistanceToCenterOfGravity
#' 
#' # call function
#' x <- performGeoCodingWithCacheAndFiltering(inputDataForLocationStringsAndOptionalAreaIds, functionToGetDistinctLocationStrings, useWholeDataInsteadOfPerAreaIDToRetrieveLocationFrequenciesAndToApplySecondFilter = T, functionToRetrieveUniqueAreaIds, functionToGetLocationsAndFrequencyForGivenAreaId, cacheForGeocodingData, functionToUpdateCacheForGeoLocationString, functionToFilterOrSelectGeoResultForALocationString)
#' 
performGeoCodingWithCacheAndFiltering <- function(inputDataForLocationStringsAndOptionalAreaIds, 
                                                  functionToGetUniqueLocationStrings,
                                                  useWholeDataInsteadOfPerAreaIDToRetrieveLocationFrequenciesAndToApplySecondFilter = F,
                                                  functionToRetrieveUniqueAreaIds,
                                                  functionToGetLocationStringsAndFrequencyForGivenAreaId,
                                                  cacheForGeocodingData,
                                                  functionToUpdateGeoCodingCacheForGeoLocationString,
                                                  functionToFilterOrSelectGeoResultForALocationString,
                                                  functionToFilterGeoResultsPerArea,
                                                  functionToFilterGeoResultsForWholeData
){
  
  log_to_file(message = "<b>Geocoding: get unique location strings</b>",file = logfile)
  allDistinctLocationStrings <- functionToGetUniqueLocationStrings(inputDataForLocationStringsAndOptionalAreaIds)
  
  log_to_file(message = "<b>Geocoding: update cache</b>",file = logfile)
  # update cache so all entities are in cache afterwards
  # check all distinct entities if already in cache, if not query them via geo service and store in cache, result x not needed as chache is used afterwards
  x <- lapply(allDistinctLocationStrings, 
              FUN = function(x){
                entityName <- str_trim(str_replace(x, "_", " "),side = "both")
                functionToUpdateGeoCodingCacheForGeoLocationString(entityName, cacheForGeocodingData)
                return(invisible())
              })
  # now all entities are in cache and can be queried from there
  
  

  log_to_file(message = "<b>Geocoding: get area ids</b>",file = logfile)
  if(useWholeDataInsteadOfPerAreaIDToRetrieveLocationFrequenciesAndToApplySecondFilter){
    areaIds <- c("all")
  }else{
    areaIds <- functionToRetrieveUniqueAreaIds(inputDataForLocationStringsAndOptionalAreaIds)
  }
  log_to_file(message = paste("<b>Geocoding: number of area ids retrieved:", length(areaIds)," </b>"),file = logfile)
  
  initialDataFrameForGeoData <- data.frame(
    areaId=character(),
    entityName = character(),
    frequencyInArea = numeric(),
    lon = numeric(),
    lat = numeric(),
    belongsToClusterWithMinDistanceToCenterOfGravity = logical()
  )
  geoDataAllAreas <- initialDataFrameForGeoData
  
  
  for(areaId in areaIds){ # for each area (document/paragraph/collection,...). If selected "all" the whole data is regarded as same area
    
    #print(areaId)
    
    log_to_file(message = paste("<b>Geocoding: perform geocoding calculation for area id", areaId,"</b>"),file = logfile)
    
    # get location tokens for area
    entitydistributionsInArea <- functionToGetLocationStringsAndFrequencyForGivenAreaId(inputDataForLocationStringsAndOptionalAreaIds, areaId)
    
    if(is.null(entitydistributionsInArea)){
      # skip current area
      next
    }
    
    # get geo info (lat/lon for location entities in area)
    geoDataForArea <- as.data.frame(entitydistributionsInArea)
    geoDataForArea$latitude <- numeric(length = dim(geoDataForArea)[1])
    geoDataForArea$longitude <- numeric(length = dim(geoDataForArea)[1])
    geoResultDetailsForArea <- NULL
    numberOfEntities <- length(geoDataForArea$entityName)
    for(j in 1:numberOfEntities){
      #print(paste0("entityIndex: ",j))
      entityName <- str_trim(str_replace(geoDataForArea$entityName[j], "_", " "),side = "both")
      geoResult <- functionToUpdateGeoCodingCacheForGeoLocationString(entityName, cacheForGeocodingData) # since all are in cache, querying cache would be sufficient as well, but using this function makes the code better readable and internally just the cache is queried inside function as all are in cache already because of the step above
      
      #print(entityName)
      if(geoResult$successType == "success"){
        
        # further select/filter (e.g. if multiple locations found or to allow e.g. only cities)
        filteredGeoResults <- functionToFilterOrSelectGeoResultForALocationString(geoResult$result)
        if(is.null(filteredGeoResults) || dim(filteredGeoResults)[1]==0){
          # skip current entity name
          geoDataForArea$longitude[j] <- NA
          geoDataForArea$latitude[j] <- NA
          #geoResultDetailsForArea[j,] <- NA
          
        }else{
          # take first result, meaning if still multiple available take the first
          geoDataForArea$longitude[j] <- filteredGeoResults[1,]$lon
          geoDataForArea$latitude[j] <- filteredGeoResults[1,]$lat
          
          if(is.null(geoResultDetailsForArea)){
            columnNames <- names(filteredGeoResults)
            geoResultDetailsForArea <- data.frame(matrix(ncol = length(columnNames), nrow = length(entitydistributionsInArea)))
            colnames(geoResultDetailsForArea) <- columnNames
          }
          geoResultDetailsForArea[j,] <- filteredGeoResults[1,]
          
        }
        
        
      }else{# successType != success
        geoDataForArea$longitude[j] <- NA
        geoDataForArea$latitude[j] <- NA
        #geoResultDetailsForArea[j,] <- NA
      }
      
    }
    
    
    # only use those tokens having coordinates (no NAs), meaning only those tokens to which lat/lon coordinates could be assigned
    indicesWithoutNA <- which(!is.na(geoDataForArea$lat))
    geoDataForArea <-  geoDataForArea[indicesWithoutNA,]

    if(dim(geoDataForArea)[1]>0){
      geoResultDetailsForArea <-  geoResultDetailsForArea[indicesWithoutNA,]
      
      # combine both data and assign areaId
      geoDataForAreaInclGeoResultDetails <- cbind(geoDataForArea, geoResultDetailsForArea)
      geoDataForAreaInclGeoResultDetails$areaId <- areaId
      
      # apply filter 2 for this area
      geoDataForAreaInclGeoResultDetails <- functionToFilterGeoResultsPerArea(geoDataForAreaInclGeoResultDetails)
      
      
      geoDataAllAreas <- rbind(geoDataAllAreas, geoDataForAreaInclGeoResultDetails)

    }
    log_to_file(message = paste("<b>Geocoding: finished performing geocoding calculation for area id", areaId,"</b>"),file = logfile)
    
  }# end for each areaID
  
  # filter 3
  geoDataAllAreas <- functionToFilterGeoResultsForWholeData(geoDataAllAreas)
  
  log_to_file(message = "<b>Geocoding: finished performing geocoding calculation for all area ids </b>",file = logfile)
  return (geoDataAllAreas)
  
}

########################################
# convenience function for filtering
########################################



filterForEntitiesInClusterWithMinDistanceToCenterOfGravity <- function(inputData){
  result <- NULL
  indicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity <- performClustering_returnIndicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity(inputData)
  if(!is.null(indicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity) && length(indicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity) >=1){
    result <- inputData[indicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity,]
  }
  return (result)
}

#' - calculates center of gravity for each area based on (frequency weighted) locations
#' - calcualtes distance of each location entity to the center of gravity
#' - performs clustering on these distances
#' - find cluster with minimum distance to center of gravity (distance of cluster to center of gravity is calculated by average distance of entities in cluster to center of gravity)
#' - set belongsToClusterWithMinDistanceToCenterOfGravity=TRUE for all entities/ location strings which belong to the cluster identified above 
# inputData = data frame containing at least columns lat, lon and frequencyInArea
performClustering_returnIndicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity <- function(inputData){
  
  numberOfEntries <- dim(inputData)[1]
  
  if (numberOfEntries == 0){
    
    indicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity <- NULL
    return (indicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity)
    
  }else if(numberOfEntries > 1){
    
    
    # center of gravity
    longitudeValues <- inputData$lon
    latitudeValues <- inputData$lat
    freqencyValues <- inputData$frequencyInArea
    centerOfGravity <-
      c(
        weighted.mean(x = as.numeric(longitudeValues), w = as.numeric(freqencyValues)), # weight dependent on frequency 
        weighted.mean(x = as.numeric(latitudeValues), w = as.numeric(freqencyValues)) # weight dependent on frequency 
      )
    
    # distance of each location entity to the center of gravity
    distances <- matrix(c(0), numberOfEntries, 1)
    for(j in 1:numberOfEntries){
      distances[j, 1] <- distm(c(as.numeric(longitudeValues[j]), as.numeric(latitudeValues[j])), centerOfGravity, fun = distHaversine) / 1000
    }
    
    #clustering
    bestClusterResult <- kmeans(distances, 1)
    if(numberOfEntries-1 >=2){
      for (k in 2:min(5,(numberOfEntries-1))) {
        clusterResult <- kmeans(distances, k)
        if (clusterResult$betweenss < (1.15 * bestClusterResult$betweenss)) {
          break
        }
        else{
          bestClusterResult <- clusterResult
        }
      }
    }
    
    
    # find cluster with minimum distance to center of gravity (distance of cluster to center of gravity is calculated by average distance of entities in cluster to center of gravity)
    clusters <- bestClusterResult$cluster
    numberOfClusters <- length(unique(clusters))
    clusterWithMinDistanceToCenterOfGravity <- 1
    if(numberOfClusters>1){
      minAvgDistanceOfEntitiesToCenterOfGravity <- mean(distances[which(clusters == 1)])
      for (k in 2:numberOfClusters) {
        avgDistanceOfEntitiesToCenterOfGravity <- mean(distances[which(clusters == k)])
        if (avgDistanceOfEntitiesToCenterOfGravity < minAvgDistanceOfEntitiesToCenterOfGravity) {
          clusterWithMinDistanceToCenterOfGravity = k
          minAvgDistanceOfEntitiesToCenterOfGravity <- avgDistanceOfEntitiesToCenterOfGravity
        }
      }
    }
    
    indicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity <- which(clusters==clusterWithMinDistanceToCenterOfGravity)

  }else if (numberOfEntries ==1){
    
    indicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity <- c(1)
    
  }else{# numberOfEntries < 0, this shouldn't happen
    
  }
  return (indicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity)
}


selectBasedOnMaxValue <- function(inputData, fieldName){
  indexOfGeoResultToReturn <- which(inputData[[fieldName]] == max(inputData[[fieldName]]), arr.ind = T)[1] 
  result <- inputData[indexOfGeoResultToReturn,]
}

selectBasedOnMinValue <- function(inputData, fieldName){
  indexOfGeoResultToReturn <- which(inputData[[fieldName]] == min(inputData[[fieldName]]), arr.ind = T)[1] 
  result <- inputData[indexOfGeoResultToReturn,]
}

selectBasedOnGivenMinMaxValue <- function(inputData, fieldName, minValue = NULL, maxValue = NULL){
  result <- inputData
  if(!is.null(minValue)){
    indicesToReturn <- which(result[[fieldName]] >= minValue, arr.ind = T) 
    result <- result[indicesToReturn,]
  }
  if(!is.null(maxValue)){
    indicesToReturn <- which(result[[fieldName]] <= maxValue, arr.ind = T)
    result <- result[indicesToReturn,]
  }
  return (result)
}
################
#
# util functions to get specific data from data frame or dtm via similar functions
# TODO: make this an abstract class e.g. R6 for more convenience
# these functions can be used for geo coding processing to have similar functions regardless if inputData is data_frame or dtm for location strings  
#
##################


####################
# util functions for inputData_dataframe <- list(tokenData = db_data$token, columnNameOfToken <- "token", columnNameOfAreaId <- "doc_id")
########################

dataframe_getUniqueAreaIDs <- function(inputDataDBDataToken){
  columnNameOfAreaId <- inputDataDBDataToken$columnNameOfAreaId
  return (unique(inputDataDBData$tokenData[[columnNameOfAreaId]]))
}

# get unique tokens (types) for whole data
dataframe_getUniqueTokens <- function(inputDataDBDataToken){
  columnNameOfToken <- inputDataDBDataToken$columnNameOfToken
  unique(inputDataDBDataToken$tokenData[[columnNameOfToken]])
}

dataframe_getTokensAndFrequenciesForGivenAreaId <- function(inputDataDBDataToken, areaId){
  dataframeWithLocationTokens <- inputDataDBDataToken$tokenData
  if(docId == "all"){
    columnNameOfToken <- inputDataDBDataToken$columnNameOfToken
    tokensForGivenAreaId <- dataframeWithLocationTokens[[columnNameOfToken]]
  }else{
    columnNameOfAreaId <- inputDataDBDataToken$columnNameOfAreaId
    tokenIndecesWithGivenAreaId <- which(dataframeWithLocationTokens[[columnNameOfAreaId]] == areaId, arr.ind = T )
    if(length(tokenIndecesWithGivenAreaId)==0){
      return (NULL)
    }
    tokensForGivenAreaId <- dataframeWithLocationTokens[[columnNameOfToken]][tokenIndecesWithGivenAreaId]
  }
  entitydistributionsInArea <- as.data.frame(table(tokensForGivenAreaId),stringsAsFactors = FALSE)
  entitydistributionsInArea <- entitydistributionsInArea[order(entitydistributionsInArea$Freq,decreasing = TRUE),]
  colnames(entitydistributionsInArea) <- c("entityName", "frequencyInArea")
  return(entitydistributionsInArea)
}



####################
# util functions for inputDataDTM  <- list(dtm = dtm) being sparse matrix
########################


#' getUniqueDocIDs for given ilcm dtm resulting from filtering via UI
#'
#' @param inputDataDTM 
#'
#' @return docids resulting from inputData$dtm@Dimnames$docs which are composed of [datasetName]_[id_doc], e.g. "MMF-2020-06-15_12345"
#' @export
#'
#' @examples
dtm_getUniqueDocIDs <- function(inputDataDTM){
  dtm <- inputDataDTM$dtm
  return(dtm@Dimnames$docs)
} 

# get unique tokens (types) for whole data
dtm_getUniqueFeaturesFromDTM <- function(inputDataDTM){
  dtm <- inputDataDTM$dtm
  return(dtm@Dimnames$features)
}

dtm_getTokensAndFrequencyForGivenDocId <- function(inputDataDTM, docId){
  
  dtm <- inputDataDTM$dtm
  if(docId == "all"){
    sumFreq <- colSums(dtm)
    indicesOfTokensWithFreqAboveZero <- which(sumFreq>0)
    if(length(indicesOfTokensWithFreqAboveZero)==0){
      return (NULL)
    }
    tokens <- dtm@Dimnames$features[indicesOfTokensWithFreqAboveZero]
    frequencies <- sumFreq[indicesOfTokensWithFreqAboveZero]
    entitydistributionsInArea <- list(entityName = tokens, frequencyInArea = frequencies)
  }else{
    docIndex <- which(dtm@Dimnames$docs == docId)
    indicesOfTokensWithFreqAboveZero <- which(dtm[docIndex,]>0)
    if(length(indicesOfTokensWithFreqAboveZero)==0){
      return (NULL)
    }
    tokens <- dtm@Dimnames$features[indicesOfTokensWithFreqAboveZero]
    frequencies <- dtm[docId,indicesOfTokensWithFreqAboveZero]
    entitydistributionsInArea <- list(entityName = tokens, frequencyInArea = frequencies)
  }
}

############
# convenience function for assigning lat/lon for given countries
##############

functionToGetUniqueLocationStrings_country <- function(x){unique(x$countryName)}

# ignores areaID
functionToGetLocationStringssAndFrequencyForGivenAreaId_country <- function(inputData, areaID){
  result <- aggregate(lat~countryName, data  = inputData, FUN = function(x){NROW(x)})
  names(result) <- c("entityName","frequencyInArea")
  return(result)
}
functionToFilterOrSelectGeoResultForALocationString_country <- function(geocodingResultresult){
  result <- geocodingResultresult %>% filter(type == "administrative") %>% filter(class == "boundary")
  result <- selectBasedOnMaxValue(result, "importance")
  }
