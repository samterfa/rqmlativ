tokenDir <- ".tokenCache"
tokenFile <- glue::glue("rqmlativToken")
tokenCacheFile <- glue::glue("{tokenDir}/{tokenFile}")

if(!exists('skyModules')) data('skyModules')
if(!exists('skyObjects')) data('skyObjects')
if(!exists('skyFields')) data('skyFields')
if(!exists('skyRelationships')) data('skyRelationships')

### This function checks obtains a oauth 2.0 token for making Skyward requests. GET(url = requestUrl, config = config(token = checkSkywardAuthentication()))
getSkywardToken <- function(){
  
  requiredEnvVars <- c("SkywardConsumerKey", "SkywardConsumerSecret", "SkywardBaseUrl")
  
  if((Sys.getenv(requiredEnvVars) == '') %>% any()) stop('Skyward API Credentials Not Set. Try Sys.setenv(SkywardConsumerKey = {yourSkywardConsumerKey}, SkywardConsumerSecret = {yourSkywardConsumerSecret}, SkywardBaseUrl = {yourSkywardBaseUrl})')
  
  app <- httr::oauth_app(appname = 'Nightly Scripts', key = Sys.getenv('SkywardConsumerKey'), secret = Sys.getenv('SkywardConsumerSecret'))
  
  if (!dir.exists(tokenDir)) 
    dir.create(tokenDir)
  
  httr::oauth2.0_token(endpoint = httr::oauth_endpoint(authorize = glue::glue("{Sys.getenv('SkywardBaseUrl')}/oauth/authorize"), refresh = glue::glue("{Sys.getenv('SkywardBaseUrl')}/oauth/token"), access = glue::glue("{Sys.getenv('SkywardBaseUrl')}/oauth/token"), validate = glue::glue("{Sys.getenv('SkywardBaseUrl')}/Generic")), app = app, scope = NULL, client_credentials = T, use_oob = F, cache = tokenCacheFile)
}

unlistItems <- function(x){
 
  if(class(x) == 'list'){
   
    x %>% purrr::map(~ifelse(length(.x) == 0, NA, .x))
    
  }else{
  
    x %>% dplyr::mutate_at(.vars = which(unlist(lapply(x, is.list))), .funs = ~unlist(lapply(.x, function(y) ifelse(length(y) == 0, NA, y))))
  }
}

# Creates filtering searches for API list requests.
createSearchObject <- function(SearchConditionsList = NULL, SearchConditionsGroupType = 'And', SearchSortFieldNamesList = NULL, SearchSortFieldNamesDescendingList = rep(F, length(SearchSortFieldNamesList))){
  
  if(is.null(SearchConditionsList) & is.null(SearchSortFieldNamesList)) return(list(SearchCondition = NULL))
  
  # Initialize search object.
  searchObject <- list()
  searchObject$SearchCondition <- list()
  searchObject$SearchCondition$SearchConditionGroup <- list()
  searchObject$SearchCondition$SearchConditionGroup$Conditions <- list()
  searchObject$SearchSort <- list()
  
  searchObject$SearchCondition$SearchConditionGroup$ConditionGroupType <- SearchConditionsGroupType
  
  # Add search conditions each with fieldName, condition, and value/list.
  for(condition in SearchConditionsList){
    
    i <- which(SearchConditionsList == condition)
    
    # Split SearchConditions into FieldName, Condition, and Value/List.
    condition <- stringr::str_split(SearchConditionsList[[i]], ' ') %>% purrr::pluck(1)
    
    if(!condition[[2]] %>% stringr::str_detect('List')){
      searchObject$SearchCondition$SearchConditionGroup$Conditions <- append(
        
        searchObject$SearchCondition$SearchConditionGroup$Conditions,
        
        # Add new search condition.
        list(
        list(
          StringSearchCondition = 
            list(
              FieldName = condition[[1]],
              Conditiontype = condition[[2]],
              Value = paste(condition[3:length(condition)], collapse = ' ')
            )
        )))}else{
          if(condition[[2]] %>% stringr::str_detect('List')){
            
            searchObject$SearchCondition$SearchConditionGroup$Conditions <- append(
              
              searchObject$SearchCondition$SearchConditionGroup$Conditions,
              
              # Add new search condition.
              list(
              list(
                StringSearchCondition = 
                  list(
                    FieldName = condition[[1]],
                    Conditiontype = condition[[2]],
                    List = paste(condition[3:length(condition)], collapse = ' ') %>% stringr::str_split(',') %>% unlist() %>% stringr::str_trim() %>% as.list()
                  )
              )))}
        }}
  
  # Add search conditions each with fieldName, condition, and value/list.
  for(searchSortFieldName in SearchSortFieldNamesList){
    
    i <- which(SearchSortFieldNamesList == searchSortFieldName)
    
    searchObject$SearchSort <- append(
      
      searchObject$SearchSort,
      
      # Add new search sort.
      list(
        list(
          FieldName = SearchSortFieldNamesList[[i]],
          Descending = SearchSortFieldNamesDescendingList[[i]]
        )))}
  
  searchObject
}



loadSkyModules <- function(){
  
  api <- 'Generic'
  entityId <- 1
  module <- 'Security'
  objectName <- 'ProductModulePath'
  SearchConditionsList <- 'Module NotNull'
  pageSize <- 1000
  method <- 'POST'
  queryParams <- list(searchFields = 'Module')
  
  modules <- NULL
  for(page in 1:100000){
    
    endpoint <- glue::glue('/{api}/{entityId}/{module}/{objectName}/{page}/{format(pageSize, scientific = F)}')
    
    searchObject <- createSearchObject(SearchConditionsList = SearchConditionsList)
    
    requestText <- glue::glue('httr::{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}", body = searchObject, encode = "json", query = queryParams, config = httr::config(token = getSkywardToken()))') 
    
    response <- eval(parse(text = requestText))
    
    newResults <- httr::content(response) %>% purrr::pluck('Objects') %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T) %>% unlistItems()
    
    if(length(newResults) == 0){
      break()
    } 
    
    modules <- dplyr::bind_rows(modules, newResults)
  }
  
  productsOwned <- modules %>% dplyr::pull(Module) %>% unique() %>% sort()
  
  
  api <- 'Generic'
  entityId <- 1
  module <- 'SkySys'
  objectName <- 'Module'
  SearchConditionsList <- 'Status Contains Complete'
  pageSize <- 100
  method <- 'POST'
  queryParams <- list(searchFields = 'ModuleID', searchFields = 'CurrentName', searchFields = 'DisplayName', searchFields = 'IsSkywardModule')
  
  modules <- NULL
  for(page in 1:100000){
    
    endpoint <- glue::glue('/{api}/{entityId}/{module}/{objectName}/{page}/{format(pageSize, scientific = F)}')
    
    searchObject <- createSearchObject(SearchConditionsList = SearchConditionsList)
    
    requestText <- glue::glue('httr::{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}", body = searchObject, encode = "json", query = queryParams, config = httr::config(token = getSkywardToken()))') 
    
    response <- eval(parse(text = requestText))
    
    newResults <- httr::content(response) %>% purrr::pluck('Objects') %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T) %>% unlistItems()
    
    if(length(newResults) == 0){
      break()
    } 
    
    modules <- dplyr::bind_rows(modules, newResults)
  }
  
  modules %>% dplyr::filter(CurrentName %in% productsOwned) %>% dplyr::rename(ModuleShortName = CurrentName) %>% dplyr::arrange(ModuleShortName)
}

###skyModules <- loadSkyModules()

loadSkyObjects <- function(){
  
  api <- 'Generic'
  entityId <- 1
  module <- 'SkySys'
  objectName <- 'Object'
  SearchConditionsList <- 'Status Contains Complete'
  searchFields <- c('CurrentName', 'FormattedObjectPath', 'IsSkywardObject', 'ObjectID', 'ModuleID')
  pageSize <- 100
  method <- 'POST'
  queryParams <- list(searchFields = 'CurrentName', searchFields = 'CurrentName', searchFields = 'FormattedObjectPath', searchFields = 'IsSkywardObject', searchFields = 'ObjectID', searchFields = 'ModuleID')
  
  objects <- NULL
  for(page in 1:100000){
    
    endpoint <- glue::glue('/{api}/{entityId}/{module}/{objectName}/{page}/{format(pageSize, scientific = F)}')
    
    searchObject <- createSearchObject(SearchConditionsList = SearchConditionsList)
    
    requestText <- glue::glue('httr::{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}", body = searchObject, encode = "json", query = queryParams, config = httr::config(token = getSkywardToken()))') 
    
    response <- eval(parse(text = requestText))
    
    newResults <- httr::content(response) %>% purrr::pluck('Objects') %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T) %>% unlistItems()
    
    if(length(newResults) == 0){
      break()
    } 
    
    objects <- dplyr::bind_rows(objects, newResults)
  }

  objects <- objects %>% dplyr::filter(ModuleID %in% (skyModules %>% dplyr::pull(ModuleID)))
  
  # Fix non-unique CurrentNames
  nonUniqueObjectNames <- objects %>% dplyr::group_by(CurrentName) %>% dplyr::count() %>% dplyr::filter(n > 1) %>% dplyr::pull(CurrentName)
  objects <- objects %>% dplyr::mutate(ObjectName = ifelse(CurrentName %in% nonUniqueObjectNames, stringr::str_replace_all(FormattedObjectPath, stringr::fixed('/'), ''), CurrentName))
  
  # Fix non-unique ObjectNames caused by the CurrentNames fix.
  nonUniqueObjectNames <- objects %>% dplyr::group_by(ObjectName) %>% dplyr::count() %>% dplyr::filter(n > 1) %>% dplyr::pull(ObjectName)
  objects %>% dplyr::mutate(ObjectName = ifelse(ObjectName %in% nonUniqueObjectNames, stringr::str_replace_all(FormattedObjectPath, stringr::fixed('/'), ''), ObjectName))
}

### skyObjects <- loadSkyObjects()

getSkyObject <- function(module, objectName, objectId, searchFields = 'all', entityId = 1, api = 'Generic', query = NULL, flatten = T, returnResponse = F){
  
  baseUrl <- Sys.getenv('skyward_base_url')
  endpoint <- glue::glue('/Generic/{entityId}/{module}/{objectName}/{objectId}')
  method <- 'GET'
 
  object <- skyObjects %>% dplyr::filter(FormattedObjectPath == glue::glue('{module}/{objectName}'))
  if(all(searchFields == 'all')) searchFields <- skyFields %>% dplyr::filter(ObjectID == object$ObjectID) %>% dplyr::pull(CurrentName)
  
  searchFields <- searchFields %>% purrr::keep(~!.x %in% c("Relationships", "ValidationRules"))
  queryParams <- eval(parse(text = paste0('list(', paste0('searchFields = "', searchFields, '"') %>% stringr::str_flatten(', '), ')'))) %>% append(query %>% purrr::compact())
  
  requestText <- glue::glue('httr::{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}", query = queryParams, config = httr::config(token = getSkywardToken()))') 
  
  response <- eval(parse(text = requestText))
  
  if(response$status_code == 403){
    file.remove(response$request$auth_token$cache_path)
    response <- eval(parse(text = requestText))
  }
  
  if(returnResponse) return(response)
  
  if(response$status_code == 503) stop('Service Unavailable')
  
  if(response$status_code > 300) stop(httr::content(response))
  
  if(!flatten) return(httr::content(response))
  
  httr::content(response) %>% unlistItems() %>% as.data.frame()
}

deleteSkyObject <- function(module, objectName, objectId, ignoreWarnings = F, entityId = 1, api = 'Generic', query = NULL, flatten = T, returnResponse = F){
  
  baseUrl <- Sys.getenv('skyward_base_url')
  endpoint <- glue::glue('/Generic/{entityId}/{module}/{objectName}/{objectId}')
  method <- 'DELETE'
  
  requestText <- glue::glue('httr::{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}", query = list(ignoreWarnings = ignoreWarnings) %>% append(query %>% purrr::compact()), config = httr::config(token = getSkywardToken()))') 
  
  response <- eval(parse(text = requestText))
  
  if(response$status_code == 403){
    file.remove(response$request$auth_token$cache_path)
    response <- eval(parse(text = requestText))
  }
  
  if(returnResponse) return(response)
  
  if(response$status_code == 503) stop('Service Unavailable')
  
  if(response$status_code > 300) stop(httr::content(response))
  
  if(!flatten) return(httr::content(response))
  
  httr::content(response) %>% unlistItems() %>% as.data.frame()
}

createSkyObject <- function(module, objectName, body, searchFields = 'all', entityId = 1, api = 'Generic', query = NULL, flatten = T, returnResponse = F){
  
  baseUrl <- Sys.getenv('skyward_base_url')
  endpoint <- glue::glue('/Generic/{entityId}/{module}/{objectName}')
  method <- 'PUT'
  
  object <- skyObjects %>% dplyr::filter(FormattedObjectPath == glue::glue('{module}/{objectName}'))
  if(all(searchFields == 'all')) searchFields <- skyFields %>% dplyr::filter(ObjectID == object$ObjectID) %>% dplyr::pull(CurrentName)
  
  searchFields <- searchFields %>% purrr::keep(~!.x %in% c("Relationships", "ValidationRules"))
  queryParams <- eval(parse(text = paste0('list(', paste0('searchFields = "', searchFields, '"') %>% stringr::str_flatten(', '), ')'))) %>% append(query %>% purrr::compact())
  
  requestText <- glue::glue('httr::{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}", query = queryParams, body = body, encode = "json", config = httr::config(token = getSkywardToken()))') 
  
  response <- eval(parse(text = requestText))
  
  if(response$status_code == 403){
    file.remove(response$request$auth_token$cache_path)
    response <- eval(parse(text = requestText))
  }
  
  if(returnResponse) return(response)
  
  if(response$status_code == 503) stop('Service Unavailable')
  
  if(response$status_code > 300) stop(httr::content(response))
  
  if(!flatten) return(httr::content(response))
  
  httr::content(response) %>% unlistItems() %>% as.data.frame()
}

modifySkyObject <- function(module, objectName, objectId, body, searchFields = 'all', entityId = 1, api = 'Generic', query = NULL, flatten = T, returnResponse = F){
  
  baseUrl <- Sys.getenv('skyward_base_url')
  endpoint <- glue::glue('/Generic/{entityId}/{module}/{objectName}/{objectId}')
  method <- 'POST'
  
  object <- skyObjects %>% dplyr::filter(FormattedObjectPath == glue::glue('{module}/{objectName}'))
  if(all(searchFields == 'all')) searchFields <- skyFields %>% dplyr::filter(ObjectID == object$ObjectID) %>% dplyr::pull(CurrentName)
  
  searchFields <- searchFields %>% purrr::keep(~!.x %in% c("Relationships", "ValidationRules"))
  queryParams <- eval(parse(text = paste0('list(', paste0('searchFields = "', searchFields, '"') %>% stringr::str_flatten(', '), ')'))) %>% append(query %>% purrr::compact())
  
  requestText <- glue::glue('httr::{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}", query = queryParams, body = body, encode = "json", config = httr::config(token = getSkywardToken()))') 
  
  response <- eval(parse(text = requestText))
  
  if(response$status_code == 403){
    file.remove(response$request$auth_token$cache_path)
    response <- eval(parse(text = requestText))
  }
  
  if(returnResponse) return(response)
  
  if(response$status_code == 503) stop('Service Unavailable')
  
  if(response$status_code > 300) stop(httr::content(response))
  
  if(!flatten) return(httr::content(response))
  
  httr::content(response) %>% unlistItems() %>% as.data.frame()
}

listSkyObjects <- function(module, objectName, searchFields = 'all', page = 1, pageSize = 100, SearchConditionsList = NULL, SearchConditionsGroupType = 'And', SearchSortFieldNamesList = NULL, SearchSortFieldNamesDescendingList = rep(F, length(SearchSortFieldNamesList)), entityId = 1, api = 'Generic', query = NULL, flatten = T, returnResponse = F){
  
  endpoint <- glue::glue('/{api}/{entityId}/{module}/{objectName}/{page}/{format(pageSize, scientific = F)}')
  
  ifelse(is.null(SearchConditionsList) & is.null(SearchSortFieldNamesList), method <- 'GET', method <- 'POST')
  
  object <- skyObjects %>% dplyr::filter(FormattedObjectPath == glue::glue('{module}/{objectName}'))
  if(all(searchFields == 'all')) searchFields <- skyFields %>% dplyr::filter(ObjectID == object$ObjectID) %>% dplyr::pull(CurrentName)
  
  searchFields <- searchFields %>% purrr::keep(~!.x %in% c("Relationships", "ValidationRules"))
  queryParams <- eval(parse(text = paste0('list(', paste0('searchFields = "', searchFields, '"') %>% stringr::str_flatten(', '), ')'))) %>% append(query %>% purrr::compact())
  
  searchObject <- createSearchObject(SearchConditionsList = SearchConditionsList, SearchConditionsGroupType = SearchConditionsGroupType, SearchSortFieldNamesList = SearchSortFieldNamesList, SearchSortFieldNamesDescendingList = SearchSortFieldNamesDescendingList)
 
  requestText <- glue::glue('httr::{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}", body = searchObject, encode = "json", query = queryParams, config = httr::config(token = getSkywardToken()))') 
  
  response <- eval(parse(text = requestText))
  
  if(response$status_code == 403){
    file.remove(response$request$auth_token$cache_path)
    response <- eval(parse(text = requestText))
  }
  
  if(returnResponse) return(response)
  
  if(response$status_code == 503) stop('Service Unavailable')
  
  if(response$status_code > 300) stop(httr::content(response))
  
  if(!flatten) return(httr::content(response))
  
  httr::content(response) %>% purrr::pluck('Objects') %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T) %>% unlistItems()
}


loadSkyFields <- function(){
  
  results <- list()
  for(pg in 1:100000){
    
    newResults <- listSkyObjects(module = 'SkySys', objectName = 'Field', SearchConditionsList = 'Status Contains Complete', searchFields = c('FieldID', 'IsPrimaryKey', 'CurrentIsRequired', 'IsDeniable', 'UserCanEdit', 'CurrentName', 'CurrentType', 'IsSkywardField', 'ObjectID'), flatten = F, page = pg, pageSize = 10000)
    
    if(length(newResults$Objects) == 0){
      break()
    } 
    
    results <- append(results, newResults$Objects)
  }
  
  fields <- results %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T)
  
  fields <- fields %>% dplyr::filter(ObjectID %in% (skyObjects %>% dplyr::pull(ObjectID) %>% unique))
  
  fields %>% dplyr::mutate(FieldName = CurrentName)
}

### skyFields <- loadSkyFields()

loadSkyRelationships <- function(){
  
  results <- list()
  for(page in 1:100000){
    
    newResults <- listSkyObjects(module = 'SkySys', objectName = 'Relationship', SearchConditionsList = c('Status Contains Complete', 'CurrentName NotList UserCreatedBy,UserModifiedBy', 'FieldIDForeignKeyCurrent NotNull'), searchFields = c('CurrentName', 'CurrentType', 'IsSkywardRelationship', 'FieldIDForeignKeyCurrent', 'ObjectIDPrimary', 'ObjectIDForeignCurrent'), flatten = F, page = page, pageSize = 10000)
    
    if(length(newResults$Objects) == 0){
      break()
    }
    
    results <- append(results, newResults$Objects)
  }
  
  relationships <- results %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T)
  
  objectIDs <- skyObjects %>% dplyr::pull(ObjectID) %>% unique()
  
  relationships <- relationships %>% dplyr::filter(ObjectIDPrimary %in% objectIDs, ObjectIDForeignCurrent %in% objectIDs)
  
  relationships <- relationships %>% dplyr::mutate(RelationshipName = CurrentName) %>% dplyr::rename(RelationshipType = CurrentType) %>% dplyr::filter(!RelationshipName %in% c('UserCreatedBy', 'UserModifiedBy')) %>% dplyr::filter(!unlist(lapply(FieldIDForeignKeyCurrent, function(x) length(x) == 0))) 
  
  # Remove relationships with no foreign key field since these are some odd kind of relationship I'm unfamiliar with...
  relationships %>% dplyr::mutate(FieldIDForeignKeyCurrent = unlist(FieldIDForeignKeyCurrent))
}

### skyRelationships <- loadSkyRelationships()


# Function to get last object name in string with object paths separated by .
pluckLastName <- function(objTreeStrings){
  lapply(objTreeStrings, function(objTreeString){
    if(stringr::str_locate_all(objTreeString, stringr::fixed('.')) %>% unlist() %>% length() > 0){
      objTreeString %>% stringr::str_sub(objTreeString %>% stringr::str_locate_all(stringr::fixed('.')) %>% as.data.frame() %>% dplyr::arrange(desc(start)) %>% dplyr::slice(1) %>% purrr::pluck(1) + 1, nchar(.))
    }else{
      objTreeString
    }
  }) %>% unlist()
}

# This function enumerates all related objects up to a maximum depth for input objects. For use with getSchemaForObjects.
generateObjectTree <- function(objTrees, allObjectsList, maxDepth){
  
  objTreeObjects <- objTrees %>% dplyr::filter(Type == 'object')
  if(nrow(objTreeObjects) > 0){
    for(i in 1:nrow(objTreeObjects)){
      
      objTree <- objTreeObjects %>% dplyr::slice(i)
      objName <- objTree %>% purrr::pluck('Name') %>% pluckLastName()
      
      if(objName %in% allObjectsList){
        # Remove object if it already was added.
        objTrees <- objTrees %>% dplyr::slice(-which(objTrees$Name == (objTree %>% purrr::pluck('Name'))))
        next()
      }
      
      allObjectsList <- append(allObjectsList, objName)
      
      objID <- skyObjects %>% dplyr::filter(ObjectName == objName) %>% dplyr::pull(ObjectID)
      
      if(length(objID) == 0) stop(glue::glue('No ObjectID for {objName}'))
      
      # Add fields...
      fieldsToAdd <- skyFields %>% dplyr::filter(ObjectID == objID) %>% dplyr::pull(FieldName)
      
      fieldsToAdd <- tibble::tibble(Name = paste(objTree %>% purrr::pluck('Name'), fieldsToAdd, sep = '.'), Type = 'field')
      
      objTrees <- dplyr::bind_rows(objTrees, fieldsToAdd)
      
      # Add Objects
      rels <- skyRelationships %>% dplyr::filter(ObjectIDPrimary == objID, RelationshipType %in% c('ManyToOne', 'OneToOne'))
  ###    rels <- rels %>% dplyr::select(ObjectIDForeignCurrent) %>% dplyr::distinct() %>% dplyr::left_join(skyObjects, by = c('ObjectIDForeignCurrent' = 'ObjectID'))# %>% dplyr::filter(!stringr::str_detect(objTree$Name, glue::glue(stringr::fixed('{ObjectName}.'))))#, ObjectName != objName)
      rels <- rels %>% dplyr::select(ObjectIDForeignCurrent) %>% dplyr::distinct() %>% dplyr::left_join(skyObjects, by = c('ObjectIDForeignCurrent' = 'ObjectID')) %>% dplyr::filter(!stringr::str_detect(objTree$Name, stringr::fixed(paste0(glue::glue('{ObjectName}.')))), ObjectName != objName)
     
      if(nrow(rels) > 0){
        
        objectsToAdd <- apply(rels, 1, function(rel){
          
          toAdd <- skyObjects %>% dplyr::filter(ObjectID == (rel %>% purrr::pluck('ObjectIDForeignCurrent') %>% trimws())) %>% dplyr::pull(ObjectName)
          
          tibble::tibble(Name = paste(objTree %>% purrr::pluck('Name'), toAdd, sep = '.'), Type = 'object')
          
        })
        
        objTrees <- dplyr::bind_rows(objTrees, objectsToAdd)
      }
      
      objTrees <- objTrees %>% dplyr::slice(-which(objTrees$Name == (objTree %>% purrr::pluck('Name'))))
    }}
  
  objTrees
}

#' Get Schema for Objects
#'
#' This function returns fields and objects directly related to the given objects.
#'
#' @param seedObjectNames A list of object names to find fields and relationships for.
#' @param maxDepth The number of layers of relationships to trace along before stopping. Default is 2.
#' @concept General
#' @return A nested list of fields and relationships for the given seedObjectNames.
#' @section References:
#' \{yourApiUrl\}/swagger\cr\cr
#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
#' @export
getSchemaForObjects <- function(seedObjectNames, maxDepth = 2){
  
  lapply(seedObjectNames, function(seedObjectName){
  objTrees <- dplyr::tibble(Name = seedObjectName, Type = 'object')
  depth <- 1
  
  allObjectsList <- seedObjectName %>% stringr::str_split(stringr::fixed('.')) %>% purrr::pluck(1)
  allObjectsList <- ifelse(length(allObjectsList) > 1, allObjectsList[1:(length(allObjectsList) - 1)], list())
 
  while((nrow(objTrees %>% dplyr::filter(Type == 'object')) > 0) & depth <= maxDepth){
    objTrees <- generateObjectTree(objTrees, allObjectsList)
    depth <- depth + 1
  }
 
  textToList <- function(myTextTreeDF, seedName){
    
    allTexts <- c('')
    
    for(i in 1:nrow(myTextTreeDF)){
      
      myText <- myTextTreeDF$Name[[i]]
      myTextType <- myTextTreeDF$Type[[i]]
      
      myTextList <- append('returnObj', myText %>% stringr::str_split(stringr::fixed('.')) %>% purrr::pluck(1))
      
      for(i in 1:length(myTextList)){
        
        if(i < length(myTextList)){
          
          potentialNewList <- paste(myTextList[1:i], collapse = '.') %>% stringr::str_replace_all(stringr::fixed('.'), '$')
          
          if(!any(stringr::str_detect(allTexts, stringr::fixed(potentialNewList)))){
            allTexts <- append(allTexts, potentialNewList)
            toEval <- paste0(potentialNewList, ' <- list()')
            eval(parse(text = toEval))
            
          }}else{
            
            if(myTextType == 'field'){
              toEval <- glue::glue("returnObj${myText %>% stringr::str_replace_all(stringr::fixed('.'), '$')} <- myText %>% stringr::str_sub((myText %>% stringr::str_locate(paste0(seedName, stringr::fixed('.'))))[,'end'] + 1)")
            }else{
              toEval <- glue::glue("returnObj${myText %>% stringr::str_replace_all(stringr::fixed('.'), '$')} <- list(ObjectPath = myText %>% stringr::str_sub((myText %>% stringr::str_locate(paste0(seedName, stringr::fixed('.'))))[,'end'] + 1), NextDepth = depth)")
            }
            
            eval(parse(text = toEval))
          }
      }}
    
    returnObj
  }
  
  # Remove initial object from path before creating list objects.
  objTrees %>% textToList(seedObjectName)
  
  }) %>% unlist(recursive = F)
}

#' List Search Conditions Types for use in Filtering
#'
#' This function returns the Search Conditions that can be used to filter API list requests.
#'
#' @concept General
#' @return All search condition types for filtering.
#' @section References:
#' \{yourApiUrl\}/swagger\cr\cr
#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
#' @export
listSearchConditionTypes <- function(){
  
  c('Less', 'LessEqual', 'Equal', 'NotEqual', 'GreaterEqual', 'Greater', 'BetweenInclusive', 'BetweenExclusive', 'Null', 'NotNull', 'List', 'NotList', 'Like', 'NotLike', 'Begins', 'NotBegins', 'Contains', 'NotContains', 'Ends', 'NotEnds')
}


generateObjectFunctions <- function(modules = skyModules, deleteAllFiles = T){
  
  if(deleteAllFiles) for(file in list.files('R') %>% purrr::discard(~.x %in% c('data.R', 'zzz.R', 'utils-pipe.R'))) file.remove(glue::glue('R/{file}'))
  
  for(i in 1:nrow(modules)){
    
    module <- modules %>% dplyr::slice(i)
    
    print(glue::glue('Module {i} of {nrow(modules)}: {module$ModuleShortName}'))
  
    # Create {module}.R file
    filepath <- glue::glue('R/{module$ModuleShortName}.R')
    if(!dir.exists('R')) dir.create('R')
    
    objects <- skyObjects %>% dplyr::filter(ModuleID == module$ModuleID)
    if(nrow(objects) == 0) next()
    readr::write_lines(NULL, path = filepath, append = F)
    
    for(j in 1:nrow(objects)){
      
      object <- objects %>% dplyr::slice(j)
      
      fields <- skyFields %>% dplyr::filter(ObjectID == object$ObjectID) %>% dplyr::pull(FieldName) %>% unique()
      editableFields <- skyFields %>% dplyr::filter(ObjectID == object$ObjectID, UserCanEdit == T) %>% dplyr::pull(FieldName) %>% unique()
      
      #### Generate LIST functions
      functionName <- glue::glue('list{pluralize::pluralize(object$ObjectName)}')
      
      # Create documentation text
      docText <- glue::glue("\n\t#' List {functionName %>% stringr::str_replace('^list', '')}", .trim = F)
      docText <- paste0(docText, "\n\t#'")
      docText <- paste0(docText, glue::glue("\n\t#' This function returns a dataframe or json object of {functionName %>% stringr::str_replace('^list', '')}", .trim = F))
      docText <- paste0(docText, glue::glue("\n\t#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given {functionName %>% stringr::str_replace('^list', '')}. Defaults to FALSE for all return fields which, for convenience, returns all fields for the {functionName %>% stringr::str_replace('^list', '')}.", .trim = F))
      docText <- paste0(docText, glue::glue("\n\t#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \\code{{\\link{{getSchemaForObjects}}}}('{object$ObjectName}') to get more field paths.", .trim = F))
      docText <- paste0(docText, "\n\t#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\\%'). Run \\code{\\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).")
      docText <- paste0(docText, "\n\t#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.")
      docText <- paste0(docText, "\n\t#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).")
      docText <- paste0(docText, "\n\t#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.")
      docText <- paste0(docText, "\n\t#' @param entityId The id of the entity (school). Run \\code{\\link{listEntities}} for a list of entities. Defaults to 1 (district).")
      docText <- paste0(docText, "\n\t#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.")
      docText <- paste0(docText, "\n\t#' @param page Results are paginated. The page of results to return. Default is 1.")
      docText <- paste0(docText, "\n\t#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).")
      docText <- paste0(docText, "\n\t#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).")
      docText <- paste0(docText, "\n\t#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.")
      docText <- paste0(docText, glue::glue("\n\t#' @concept {module$ModuleShortName}", .trim = F))
      docText <- paste0(docText, glue::glue("\n\t#' @return A list of {functionName %>% stringr::str_replace('^list', '')}", .trim = F))
      docText <- paste0(docText, "\n\t#' \\href{https://help.skyward.com/}{Skyward's Knowledge Hub}")
      docText <- paste0(docText, "\n\t#' @export")
      
      readr::write_lines(docText, filepath, append = T)
      
      functionText <- glue::glue('\t{functionName} <- function(searchConditionsList = NULL, {fields %>% paste(collapse = " = F, ")} = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){{', .trim = F)
      functionText <- paste0(functionText, '\n\n\t\tparams <- as.list(environment())')
      functionText <- paste0(functionText, glue::glue('\n\n\t\tsearchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())', .trim = F))
      functionText <- paste0(functionText, '\n\n\t\tifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())')
      functionText <- paste0(functionText, glue::glue('\n\n\t\tlistSkyObjects(module = "{module$ModuleShortName}", objectName = "{object$CurrentName}", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)', .trim = F))
      functionText <- paste0(functionText, '\n\t}')
    
      readr::write_lines(functionText, filepath, append = T)
      
      #### Generate GET functions
      functionName <- glue::glue('get{object$ObjectName}')
      aORan <- ifelse(stringr::str_sub(functionName %>% stringr::str_replace('^get', ''), 1, 1) %in% c('A', 'E', 'I', 'O', 'U'), 'an', 'a')
      
      # Create documentation text
      docText <- glue::glue("\n\t#' Get {aORan} {object$ObjectName}", .trim = F)
      docText <- paste0(docText, "\n\t#'")
      docText <- paste0(docText, glue::glue("\n\t#' This function returns a dataframe or json object of {aORan} {functionName %>% stringr::str_replace('^get', '')}", .trim = F))
      docText <- paste0(docText, glue::glue("\n\t#' @param {object$ObjectName}ID The ID of the {object$ObjectName} to return.", .trim = F))
      docText <- paste0(docText, glue::glue("\n\t#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given {object$ObjectName}. Defaults to FALSE for all return fields which, for convenience, returns all fields for the {object$ObjectName}.", .trim = F))
      docText <- paste0(docText, glue::glue("\n\t#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \\code{{\\link{{getSchemaForObjects}}}}('{object$ObjectName}') to get more field paths.", .trim = F))
      docText <- paste0(docText, "\n\t#' @param entityId The id of the entity (school). Run \\code{\\link{listEntities}} for a list of entities. Defaults to 1 (district).")
      docText <- paste0(docText, "\n\t#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.")
      docText <- paste0(docText, "\n\t#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).")
      docText <- paste0(docText, "\n\t#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.")
      docText <- paste0(docText, glue::glue("\n\t#' @concept {module$ModuleShortName}", .trim = F))
      docText <- paste0(docText, glue::glue("\n\t#' @return A dataframe or of {functionName %>% stringr::str_replace('^get', '')}", .trim = F))
      docText <- paste0(docText, "\n\t#' \\href{https://help.skyward.com/}{Skyward's Knowledge Hub}")
      docText <- paste0(docText, "\n\t#' @export")
      
      readr::write_lines(docText, filepath, append = T)
      
      functionText <- glue::glue('\t{functionName} <- function({object$ObjectName}ID, {fields %>% purrr::keep(~.x != paste0(object$ObjectName, "ID")) %>% paste(collapse = " = F, ")} = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){{', .trim = F)
      functionText <- paste0(functionText, glue::glue('\n\n\t\tparams <- as.list(environment()) %>% purrr::keep(names(.) != "{object$ObjectName}ID")', .trim = F))
      functionText <- paste0(functionText, glue::glue('\n\n\t\tsearchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())', .trim = F))
      functionText <- paste0(functionText, '\n\n\t\tifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())')
      functionText <- paste0(functionText, glue::glue('\n\n\t\tgetSkyObject(module = "{module$ModuleShortName}", objectName = "{object$CurrentName}", objectId = {object$ObjectName}ID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)', .trim = F))
      functionText <- paste0(functionText, '\n\t}')
      
      readr::write_lines(functionText, filepath, append = T)
      
      #### Generate DELETE functions
      functionName <- glue::glue('delete{object$ObjectName}')
      
      # Create documentation text
      docText <- glue::glue("\n\t#' Delete {aORan} {object$ObjectName}", .trim = F)
      docText <- paste0(docText, "\n\t#'")
      docText <- paste0(docText, glue::glue("\n\t#' This function deletes {aORan} {object$ObjectName}", .trim = F))
      docText <- paste0(docText, glue::glue("\n\t#' @param {object$ObjectName}ID The ID of the {object$ObjectName} to delete", .trim = F))
      docText <- paste0(docText, "\n\t#' @param entityId The id of the entity (school). Run \\code{\\link{listEntities}} for a list of entities. Defaults to 1 (district).")
      docText <- paste0(docText, "\n\t#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.")
      docText <- paste0(docText, "\n\t#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).")
      docText <- paste0(docText, "\n\t#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.")
      docText <- paste0(docText, glue::glue("\n\t#' @concept {module$ModuleShortName}", .trim = F))
      docText <- paste0(docText, glue::glue("\n\t#' @return The {object$ObjectName}ID of the deleted {object$ObjectName}.", .trim = F))
      docText <- paste0(docText, "\n\t#' \\href{https://help.skyward.com/}{Skyward's Knowledge Hub}")
      docText <- paste0(docText, "\n\t#' @export")
      
      readr::write_lines(docText, filepath, append = T)
      
      functionText <- glue::glue('\t{functionName} <- function({object$ObjectName}ID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){{', .trim = F)
      functionText <- paste0(functionText, glue::glue('\n\n\t\tdeleteSkyObject(module = "{module$ModuleShortName}", objectName = "{object$CurrentName}", objectId = {object$ObjectName}ID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)', .trim = F))
      functionText <- paste0(functionText, '\n\t}')
      
      readr::write_lines(functionText, filepath, append = T)
      
      # Create and Modify only work if there are editable fields for the object.
      if(length(editableFields) > 0){
        
        #### Generate CREATE functions
        functionName <- glue::glue('create{object$ObjectName}')
        
        # Create documentation text
        docText <- glue::glue("\n\t#' Create {aORan} {object$ObjectName}", .trim = F)
        docText <- paste0(docText, "\n\t#'")
        docText <- paste0(docText, glue::glue("\n\t#' This function creates {aORan} {object$ObjectName}", .trim = F))
        docText <- paste0(docText, glue::glue("\n\t#' @param fieldNames The field values to give the created {object$ObjectName}. Each defaults to NULL.", .trim = F))
        docText <- paste0(docText, "\n\t#' @param entityId The id of the entity (school). Run \\code{\\link{listEntities}} for a list of entities. Defaults to 1 (district).")
        docText <- paste0(docText, "\n\t#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.")
        docText <- paste0(docText, "\n\t#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).")
        docText <- paste0(docText, "\n\t#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.")
        docText <- paste0(docText, glue::glue("\n\t#' @concept {module$ModuleShortName}", .trim = F))
        docText <- paste0(docText, glue::glue("\n\t#' @return A newly created {object$ObjectName}", .trim = F))
        docText <- paste0(docText, "\n\t#' \\href{https://help.skyward.com/}{Skyward's Knowledge Hub}")
        docText <- paste0(docText, "\n\t#' @export")
        
        readr::write_lines(docText, filepath, append = T)
        
        functionText <- glue::glue('\t{functionName} <- function({editableFields %>% paste(collapse = " = NULL, ")} = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){{', .trim = F)
        functionText <- paste0(functionText, glue::glue('\n\n\t\tparams <- as.list(environment())', .trim = F))
        functionText <- paste0(functionText, glue::glue('\n\n\t\tbody <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()', .trim = F))
        functionText <- paste0(functionText, glue::glue('\n\n\t\tcreateSkyObject(module = "{module$ModuleShortName}", objectName = "{object$CurrentName}", body = list(DataObject = body), searchFields = append("{object$CurrentName}ID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)', .trim = F))
        functionText <- paste0(functionText, '\n\t}')
        
        readr::write_lines(functionText, filepath, append = T)
        
        
        #### Generate MODIFY functions
        functionName <- glue::glue('modify{object$ObjectName}')
        
        # Create documentation text
        docText <- glue::glue("\n\t#' Modify {aORan} {object$ObjectName}", .trim = F)
        docText <- paste0(docText, "\n\t#'")
        docText <- paste0(docText, glue::glue("\n\t#' This function modifies {aORan} {object$ObjectName}", .trim = F))
        docText <- paste0(docText, glue::glue("\n\t#' @param fieldNames The field values to give the modified {object$ObjectName}. Each defaults to NULL.", .trim = F))
        docText <- paste0(docText, "\n\t#' @param entityId The id of the entity (school). Run \\code{\\link{listEntities}} for a list of entities. Defaults to 1 (district).")
        docText <- paste0(docText, "\n\t#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.")
        docText <- paste0(docText, "\n\t#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).")
        docText <- paste0(docText, "\n\t#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.")
        docText <- paste0(docText, glue::glue("\n\t#' @concept {module$ModuleShortName}", .trim = F))
        docText <- paste0(docText, glue::glue("\n\t#' @return The modified {object$ObjectName}", .trim = F))
        docText <- paste0(docText, "\n\t#' \\href{https://help.skyward.com/}{Skyward's Knowledge Hub}")
        docText <- paste0(docText, "\n\t#' @export")
        
        readr::write_lines(docText, filepath, append = T)
        
        functionText <- glue::glue('\t{functionName} <- function({object$CurrentName}ID, {editableFields %>% paste(collapse = " = NULL, ")} = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){{', .trim = F)
        functionText <- paste0(functionText, glue::glue('\n\n\t\tparams <- as.list(environment())', .trim = F))
        functionText <- paste0(functionText, glue::glue('\n\n\t\tbody <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()', .trim = F))
        functionText <- paste0(functionText, glue::glue('\n\n\t\tmodifySkyObject(module = "{module$ModuleShortName}", objectName = "{object$CurrentName}", objectId = {object$CurrentName}ID, body = list(DataObject = body), searchFields = append("{object$CurrentName}ID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)', .trim = F))
        functionText <- paste0(functionText, '\n\t}')
        
        readr::write_lines(functionText, filepath, append = T)
      }
    }
  }
}


generatePackageDownSite <- function(){
  
  functionCategories <- rqmlativ::skyModules$ModuleShortName %>% sort()
  
  pkgdownYaml <- 
    "url: https://samterfa.github.io/rqmlativ/
                    
author: Sam Terfa

reference:"
  
  for(functionCategory in functionCategories){
    
    pkgdownYaml <- paste0(
      
      pkgdownYaml,
      
      glue::glue(
        
        '\n- title: {functionCategory}
  desc:  Functions involving {functionCategory}.
  contents:
  - has_concept("{functionCategory}")', .trim = F)
      
    )
  }
  
  if(!dir.exists('pkgdown')) dir.create('pkgdown')
  file.remove('pkgdown/_pkgdown.yml')
  readr::write_lines(pkgdownYaml, 'pkgdown/_pkgdown.yml')
  pkgdown::build_site(lazy = T)
}