library("geonames")
library("tmaptools")
library("geosphere")

#' Returns country infos for given lat lon coordinates
#' Uses the given hashtable as cached reference by first querying the hashtable or if not yet existent there directly via geonames service. The result is stored in hashtable as well 
#' Attention! Before executing this function, first a registration at geonames is required as well as setting/executing the following: options(geonamesUsername=geonames.username)
#'
#' @param lat The latitude value
#' @param lon The longitude value
#' @param hashtableLatLonCountryInfos The cached info as hashtable containing "latValue lonValue" (separated by space) as key and the countryInfo data provided by geonames as value
#' 
#' @return a data frame containing lat, lon, countryCode, countryName, countryLanguages 
#' 
getCountryInfosForLatLon <- function(lat, lon, hashtableLatLonCountryInfos){
  
  # check if lat lon are in stored data
  searchKey <- paste(lat, lon, sep=" ")
  if(!is.null(hashtableLatLonCountryInfos[[searchKey]])){
    #print(paste("latlon already saved in hashtable environment, no need to query geonames for ", lat, lon, ": ", hashtableLatLonCountryInfos[[searchKey]]$countryName, sep =" "))
    return(hashtableLatLonCountryInfos[[searchKey]])
  }
  else{# else query geonames
    print(paste("latlon not yet saved in hashtable,  need to query geonames for ", lat, lon, sep =" "))
    tryCatch({
      geonamesCountryCode <- GNcountryCode(lat = lat, lng = lon, lang = "en", radius = 20)
    },
    error = function(e){
      print(paste ("For the input ", i, lat, lon, " the follwoing Error happended: ", e, sep = " ", collapse = NULL))
      print("Since errors are mostly timeouts wait a bit and try again")
      Sys.sleep(10)
      tryCatch({
        geonamesCountryCode <- GNcountryCode(lat = lat, lng = lon, lang = "en", radius = 20 )
      },
      error = function(e){
        print(paste (" Also for the second attempt for the input ", i, lat, lon, " the follwoing Error happended: ", e, sep = " ", collapse = NULL))
        return (e)
      })      
    }
    )
    
    createdValue <- data.frame(lat = lat, lon = lon, countryCode = geonamesCountryCode$countryCode, countryName = geonamesCountryCode$countryName, countryLanguages = geonamesCountryCode$languages)
    hashtableLatLonCountryInfos[[searchKey]] <- createdValue
    return(createdValue)
  }
}


#' Geocoding function. Returns geolocation coordinates (lat,lon, and other data) for a given location String
#' First, the location is queried from the cache (hashtable) and if not found there queried by the specified geocodingFunction.
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
    result <- list()
    entryInCache <- hashtableCachedGeoInformation[[searchKey]]
    if(is.character(entryInCache) && entryInCache == "noResultsFound"){
      result$successType = "noResultsFound"
      result$result <- NULL
    }else{
      result$successType <- "success"
      result$result <- hashtableCachedGeoInformation[[searchKey]]
    }
    return(result)
  }
  
  #print("not in cache. needs to be queried")
  
  result <- geocodingFunction(searchKey)
  if(result$successType == "success"){
    hashtableCachedGeoInformation[[searchKey]] <- result$result
  }
  else if(result$successType == "noResultsFound"){
    hashtableCachedGeoInformation[[searchKey]] <- "noResultsFound"
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
                                          browser()
                                          
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



#' Performs geocoding and clustering for given location strings.
#' Details: 
#' - Uses cache and @getGeolocationForStringUsingOSMDefault to retrieve lat/lon info. location strings for which no location could be retrieved are skipped and not contained in result 
#' - calculates center of gravity for each area based on (frequency weighted) locations
#' - calcualtes distance of each location entity to the center of gravity
#' - performs clustering on these distances
#' - find cluster with minimum distance to center of gravity (distance of cluster to center of gravity is calculated by average distance of entities in cluster to center of gravity)
#' - set belongsToClusterWithMinDistanceToCenterOfGravity=TRUE for all entities/ location strings which belong to the cluster identified above 
#' @param dataframeWithLocationTokens a data frame containing the string of the location entities (tokens) and an (optional) area id (see columnNameOfAreaIdToApplyClustering for details) The entities should be already filtered just for locations. The number of entities (types & frequencies) is calculated in subsequent steps internally so the data should include all entities (=tokens, not types) 
#' @param columnNameOfLocationString The column name where to find the location string (e.g. token or lemma)
#' @param columnNameOfAreaIdToApplyClustering The column name where to find the area id to be used for clustering. Clustering can be performed either on whole data (use "all" here) or separately on distinct "areas" (e.g. per document/paragraph/collection,..etc.) Provide the column name where to find the area id (e.g. "doc_id") in the data frame
#' @param hashtableCachedGeoInformation A hashtable with cached information for faster querying of location strings queried before

#' @return A dataframe containing the distinct location entities per defined area with their frequency, lon value, lat value, info if entity belongsToClusterWithMinDistanceToCenterOfGravity and the areaID. Contains only location strings for which a lat/lon value was found
#'
#' @examples
#' locationTokens <- c("New York", "Leipzig", "strangeStringBeingNoLocation", "Berlin", "Berlin", "Leipzig", "Bangkok" )
#' docIds <- c(1,1,1,2,2,2,2)
#' myDataFrame <- data.frame(locationTokens,docIds)
#' hashtableCachedGeoInformation <- new.env(hash=TRUE)
#' x <- performGeoCodingWithClusteringAndRetrivealIfLocationBelongsToClusterWithMinDistanceToCenterOfGravity(myDataFrame, "locationTokens", "docIds", hashtableCachedGeoInformation)
performGeoCodingWithClusteringAndRetrivealIfLocationBelongsToClusterWithMinDistanceToCenterOfGravity <- function(dataframeWithLocationTokens, columnNameOfLocationString, columnNameOfAreaIdToApplyClustering, hashtableCachedGeoInformation ){
  
  # get distinct geo entities for whole data (= all areas)
  entitydistributions <- as.data.frame(table(dataframeWithLocationTokens[[columnNameOfLocationString]]),stringsAsFactors = FALSE)
  entitydistributions <- entitydistributions[order(entitydistributions$Freq,decreasing = TRUE),]
  colnames(entitydistributions) <- c("entityName", "frequency")
  
  # update cache so all entities are in cache afterwards
  # check all distinct entities if already in cache, if not query them via geo service and store in cache
  geoInfo <- as.list(entitydistributions)
  geoInfo$geolocationResult <- lapply(geoInfo$entityName, 
                                      FUN = function(x){
                                        entityName <- str_trim(str_replace(x, "_", " "),side = "both")
                                        getGeolocationForStringUsingOSMDefault(entityName, hashtableCachedGeoInformation)
                                      })
  # now all entities are in cache and can be queried from there
  
  
  # assign locations and calculate clusters for areas
  if(columnNameOfAreaIdToApplyClustering == "all"){
    areaIds <- c("all")
  }else{
    areaIds <- unique(dataframeWithLocationTokens[[columnNameOfAreaIdToApplyClustering]])
  }
  
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
    
    # get location tokens for area
    if(areaId =="all"){
      tokensForGivenAreaId <- dataframeWithLocationTokens[[columnNameOfLocationString]]
    }else{
      tokenIndecesWithGivenAreaId <- which(dataframeWithLocationTokens[[columnNameOfAreaIdToApplyClustering]] == areaId, arr.ind = T )
      if(length(tokenIndecesWithGivenAreaId)==0){
        # skip area
        next
      }
      tokensForGivenAreaId <- dataframeWithLocationTokens[[columnNameOfLocationString]][tokenIndecesWithGivenAreaId]
    }
    
    # only distinct entities in given area with frequency
    entitydistributionsInArea <- as.data.frame(table(tokensForGivenAreaId),stringsAsFactors = FALSE)
    entitydistributionsInArea <- entitydistributionsInArea[order(entitydistributionsInArea$Freq,decreasing = TRUE),]
    colnames(entitydistributionsInArea) <- c("entityName", "frequencyInArea")
    
    # get geo info (lat/lon for location entities in area)
    geoDataForArea <- as.data.frame(entitydistributionsInArea)
    for(j in 1:length(geoDataForArea$entityName)){
      entityName <- str_trim(str_replace(geoDataForArea$entityName[j], "_", " "),side = "both")
      geoResult <- getGeolocationForStringUsingOSMDefault(entityName, hashtableCachedGeoInformation) # since all are in cache, querying cache would be sufficient as well
      if(geoResult$successType == "success"){
        # select the one with maximum importance
        indexOfGeoResultToReturn <- which(geoResult$result$importance == max(geoResult$result$importance), arr.ind = T)[1] 
        lat <- geoResult$result$lat[indexOfGeoResultToReturn]
        lon <- geoResult$result$lon[indexOfGeoResultToReturn]
        geoDataForArea$lon[j] <- lon
        geoDataForArea$lat[j] <- lat
        
      }else{
        geoDataForArea$lon[j] <- NA
        geoDataForArea$lat[j] <- NA
      }
    }
    
    # only use those tokens having coordinates (no NAs), meaning only those tokens to which lat/lon coordinates could be assigned
    indicesWithoutNA <- which(!is.na(geoDataForArea$lat))
    geoDataForArea <-  geoDataForArea[indicesWithoutNA,]
    
    numberOfDistinctEntitiesInArea <- length(geoDataForArea$entityName)
    if (numberOfDistinctEntitiesInArea == 0){
      # skip current area and go to next
      next
      
    }else if(numberOfDistinctEntitiesInArea > 1){
      
      # center of gravity
      centerOfGravity <-
        c(
          weighted.mean(x = as.numeric(geoDataForArea$lon), w = as.numeric(geoDataForArea$frequencyInArea)), # weight dependent on frequency 
          weighted.mean(x = as.numeric(geoDataForArea$lat), w = as.numeric(geoDataForArea$frequencyInArea)) # weight dependent on frequency 
        )
      
      # distance of each location entity to the center of gravity
      distances <- matrix(c(0), dim(geoDataForArea)[1], 1)
      for(j in 1:length(geoDataForArea$entityName)){
        distances[j, 1] <- distm(c(as.numeric(geoDataForArea[j, c("lon", "lat")])), centerOfGravity, fun = distHaversine) / 1000
      }
      
      #clustering
      bestClusterResult <- kmeans(distances, 1)
      for (k in 1:min(5,(dim(geoDataForArea)[1]-1))) {
        clusterResult <- kmeans(distances, k)
        if (clusterResult$betweenss < (1.15 * bestClusterResult$betweenss)) {
          break
        }
        else{
          bestClusterResult <- clusterResult
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
      
      geoDataForArea$belongsToClusterWithMinDistanceToCenterOfGravity <- FALSE
      indicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity <- which(clusters==clusterWithMinDistanceToCenterOfGravity)
      geoDataForArea$belongsToClusterWithMinDistanceToCenterOfGravity[indicesOfEntitiesOfClusterWithMinDistanceToCenterOfGravity] <- TRUE
      geoDataForArea$areaId <- areaId
      
    }else if (numberOfDistinctEntitiesInArea ==1){
      geoDataForArea$belongsToClusterWithMinDistanceToCenterOfGravity <- TRUE
      geoDataForArea$areaId <- areaId
      
    }else{# numberOfDistinctEntitiesInArea < 0, this shouldn't happen
      
    }
    
    
    geoDataAllAreas <- rbind(geoDataAllAreas, geoDataForArea)
  }
  
  return (geoDataAllAreas)
}