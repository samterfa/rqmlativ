### This function checks obtains a oauth 2.0 token for making Skyward requests. GET(url = requestUrl, config = config(token = checkSkywardAuthentication()))
getSkywardToken <- function(){
  
  suppressMessages(suppressWarnings({require(dplyr); require(glue)}))
  
  requiredEnvVars <- c("SkywardConsumerKey", "SkywardConsumerSecret", "SkywardBaseUrl")
  
  if((Sys.getenv(requiredEnvVars) == '') %>% any()) stop('Skyward API Credentials Not Set. Try Sys.setenv(SkywardConsumerKey = {yourSkywardConsumerKey}, SkywardConsumerSecret = {yourSkywardConsumerSecret}, SkywardBaseUrl = {yourSkywardBaseUrl})')
  
  app <- oauth_app(appname = 'Nightly Scripts', key = Sys.getenv('SkywardConsumerKey'), secret = Sys.getenv('SkywardConsumerSecret'))
  
  token <- oauth2.0_token(endpoint = oauth_endpoint(authorize = glue("{Sys.getenv('SkywardBaseUrl')}/oauth/authorize"), refresh = glue("{Sys.getenv('SkywardBaseUrl')}/oauth/token"), access = glue("{Sys.getenv('SkywardBaseUrl')}/oauth/token"), validate = glue("{Sys.getenv('SkywardBaseUrl')}/Generic")), app = app, scope = NULL, client_credentials = T, cache = T)
  
  token
}
# 
# getGenericApiDocs <- function(){
#   
#   require(httr)
#   
#   response <- GET('https://skyward.iscorp.com/MinnehahaAcaMNStuAPI/swagger/docs/Generic')
#   
#   if(response$status_code > 300) stop(content(response))
#   
#   content(response)
# }
# 
# generateGenericApiFunctions <- function(functionNames = NULL){
#   
#   require(glue)
#   require(tidyverse)
#   
#   apiDocs <- getGenericApiDocs()
#   
#   packagePath <- 'skywardPackage'
#   
#   if(dir.exists(packagePath)) dir.create(packagePath)
#   
#   genericApiFunctions <- tibble()
#   
#   paths <- apiDocs$paths %>% names()
#   for(path in paths){
#     
#     pathInfo <- apiDocs$paths %>% pluck(path)
#     
#     methods <- pathInfo %>% names()
#     for(method in methods){
#       
#       methodInfo <- pathInfo %>% pluck(method)
#       
#       tag <- methodInfo %>% pluck('tags')
#       
#       functionName <- methodInfo %>% pluck('operationId') %>% str_remove(glue('{tag}_'))
#       
#       if(is.null(functionNames)){
#         genericApiFunctions <- bind_rows(genericApiFunctions, tibble(Method = method %>% str_to_upper(), Endpoint = path))
#       }else{
#         genericApiFunctions <- bind_rows(genericApiFunctions, tibble(Method = method %>% str_to_upper(), Endpoint = path, FunctionNames = functionNames))
#       }
#     }
#   }
#   
#   genericApiFunctions
# }
# 
# getFieldsForObject <- function(module, objectName, entityId = 1, api = 'Generic'){
#   
#   endpoint <- glue('/Generic/{entityId}/{module}/{objectName}')
#   method <- 'GET'
#   
#   requestText <- glue('{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}", config(token = getSkywardToken()))') 
#   
#   response <- eval(parse(text = requestText))
#   
#   if(response$status_code == 403){
#     print('Refreshing auth token')
#     file.remove(response$request$auth_token$cache_path)
#     response <- eval(parse(text = requestText))
#   }
#   
#   if(response$status_code > 300) stop(content(response))
#   
#   content(response)
# }

# Creates filtering searches for API list requests.
createSearchObject <- function(SearchConditionsList = NULL, SearchConditionsGroupType = 'And', SearchSortFieldNamesList = NULL, SearchSortFieldNamesDescendingList = rep(F, length(SearchSortFieldNamesList))){
  
  if(is.null(SearchConditionsList)  & is.null(SearchSortFieldNamesList)) return(NULL)
  
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
    condition <- str_split(SearchConditionsList[[i]], ' ') %>% pluck(1)
    
    if(!condition[[2]] %>% str_detect('List')){
      searchObject$SearchCondition$SearchConditionGroup$Conditions <- append(
        
        searchObject$SearchCondition$SearchConditionGroup$Conditions,
        
        # Add new search condition.
        list(
          StringSearchCondition = 
            list(
              FieldName = condition[[1]],
              Conditiontype = condition[[2]],
              Value = paste(condition[3:length(condition)], collapse = ' ')
            )
        ))}else{
          if(condition[[2]] %>% str_detect('List')){
            
            searchObject$SearchCondition$SearchConditionGroup$Conditions <- append(
              
              searchObject$SearchCondition$SearchConditionGroup$Conditions,
              
              # Add new search condition.
              list(
                StringSearchCondition = 
                  list(
                    FieldName = condition[[1]],
                    Conditiontype = condition[[2]],
                    List = paste(condition[3:length(condition)], collapse = ' ') %>% str_split(',') %>% unlist() %>% str_trim() %>% as.list()
                  )
              ))}
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

# 
# getObjectMetadata <- function(module, objectName, entityId = 1){
#   
#   endpoint <- glue('/Generic/{entityId}/{module}/{objectName}')
#   method <- 'GET'
#   
#   requestText <- glue('{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}", config = config(token = getSkywardToken()))') 
#   
#   response <- eval(parse(text = requestText))
#   
#   if(response$status_code == 403){
#     print('Refreshing auth token')
#     file.remove(response$request$auth_token$cache_path)
#     response <- eval(parse(text = requestText))
#   }
#   
#   if(response$status_code > 300) return(response)
#   
#   Fields <- content(response) %>% keep(!names(.) %in% c('Relationships', 'ValidationRules'))
#   Relationships <- content(response) %>% keep(names(.) == 'Relationships') %>% pluck('Relationships')
#   ValidationRules <- content(response) %>% keep(names(.) == 'ValidationRules') %>% pluck('ValidationRules')
#   
#   list_merge(Fields = Fields, Relationships = Relationships, ValidationRules = ValidationRules)
# }


getSkyObject <- function(module, objectName, objectId, searchFields = 'all', entityId = 1, api = 'Generic', flatten = T){
  
  baseUrl <- Sys.getenv('skyward_base_url')
  endpoint <- glue('/Generic/{entityId}/{module}/{objectName}/{objectId}')
  method <- 'GET'
 
  object <- skyObjects %>% filter(FormattedObjectPath == glue('{module}/{objectName}'))
  allSearchFields <- skyFields %>% filter(ObjectID == object$ObjectID) %>% pull(CurrentName)
  ifelse(searchFields == 'all', searchFields <- allSearchFields, searchFields <- searchFields)
  
  searchFields <- searchFields %>% keep(~!.x %in% c("Relationships", "ValidationRules"))
  queryParams <- eval(parse(text = paste0('list(', paste0('searchFields = "', searchFields, '"') %>% str_flatten(', '), ')')))
  
  requestText <- glue('{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}", query = queryParams, config = config(token = getSkywardToken()))') 
  
  response <- eval(parse(text = requestText))
  
  if(response$status_code == 403){
    print('Refreshing auth token')
    file.remove(response$request$auth_token$cache_path)
    response <- eval(parse(text = requestText))
  }
  
  if(response$status_code > 300) return(response)
  
  if(!flatten) return(content(response))
  
  content(response) %>% map(~ifelse(length(.x) == 0, NA, .x)) %>% as.data.frame()
}



getAllSkyObjects <- function(module, objectName, schoolYearID = NULL, searchFields = 'all', page = 1, pageSize = 100, SearchConditionsList = NULL, SearchConditionsGroupType = 'And', SearchSortFieldNamesList = NULL, SearchSortFieldNamesDescendingList = rep(F, length(SearchSortFieldNamesList)), entityId = 1, api = 'Generic', flatten = T){
  
  endpoint <- glue('/Generic/{entityId}/{module}/{objectName}')
  method <- ifelse(is.null(SearchConditionsList) & is.null(SearchSortFieldNamesList), 'GET', 'POST')
  
  if(all(searchFields == 'all')){
    if(exists('skyObjects') & exists('skyFields')){
      object <- skyObjects %>% filter(FormattedObjectPath == glue('{module}/{objectName}'))
      allSearchFields <- skyFields %>% filter(ObjectID == object$ObjectID) %>% pull(CurrentName)
      searchFields <- allSearchFields
    }else{
      stop('Must either load skyObjects and skyFields, or specificy searchFields.')
    }
  }
  
  searchFields <- searchFields %>% keep(~!.x %in% c("Relationships", "ValidationRules"))
  queryParams <- eval(parse(text = paste0('list(', paste0('searchFields = "', searchFields, '"') %>% str_flatten(', '), ')')))
  
  if(!is.null(schoolYearID)) queryParams <- queryParams %>% list_merge(SchoolYearID = schoolYearID)
  
  searchObject <- createSearchObject(SearchConditionsList = SearchConditionsList, SearchConditionsGroupType = SearchConditionsGroupType, SearchSortFieldNamesList = SearchSortFieldNamesList, SearchSortFieldNamesDescendingList = SearchSortFieldNamesDescendingList)
  
  requestText <- glue('{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}/{page}/{pageSize}", body = searchObject %>% toJSON(auto_unbox = T), content_type("application/json"), query = queryParams, config = config(token = getSkywardToken()))') 
  
  response <- eval(parse(text = requestText))
  
  if(response$status_code == 403){
    print('Refreshing auth token')
    file.remove(response$request$auth_token$cache_path)
    response <- eval(parse(text = requestText))
  }
  
  if(response$status_code > 300) return(response)
  
  if(!flatten) return(content(response))
  
  content(response) %>% pluck('Objects') %>% toJSON(auto_unbox = T) %>% fromJSON(flatten = T)
}


loadSkyRelationships <- function(){
  
  require(tidyverse)
  require(httr)
  require(jsonlite)
  require(glue)
  
  results <- list()
  for(page in 1:100000){
    
    newResults <- getAllSkyObjects(module = 'SkySys', objectName = 'Relationship', SearchConditionsList = 'Status Contains Complete', searchFields = c('CurrentName', 'CurrentType', 'IsSkywardRelationship', 'ObjectIDPrimary', 'ObjectIDForeignCurrent'), flatten = F, page = page, pageSize = 10000)
    
    if(length(newResults$Objects) == 0){
      break()
    } 
    
    results <- append(results, newResults$Objects)
  }
  
  relationships <- results %>% toJSON(auto_unbox = T) %>% fromJSON(flatten = T)
  
  relationships %>% mutate(RelationshipName = CurrentName) %>% rename(RelationshipType = CurrentType)
}


loadSkyModules <- function(){
  
  require(tidyverse)
  require(httr)
  require(jsonlite)
  require(glue)
  
  results <- getAllSkyObjects(module = 'SkySys', objectName = 'Module', SearchConditionsList = 'Status Contains Complete', searchFields = c('ModuleID', 'CurrentName', 'DisplayName', 'IsSkywardModule'), pageSize = 500, flatten = F) %>% pluck('Objects')
   
  results %>% toJSON(auto_unbox = T) %>% fromJSON(flatten = T) %>% rename(ModuleShortName = CurrentName)
}


loadSkyObjects <- function(){
  
  require(tidyverse)
  require(httr)
  require(jsonlite)
  require(glue)
  
  results <- list()
  for(page in 1:100000){
    
    newResults <- getAllSkyObjects(module = 'SkySys', objectName = 'Object', SearchConditionsList = 'Status Contains Complete', searchFields = c('CurrentName', 'FormattedObjectPath', 'IsSkywardObject', 'ObjectID', 'ModuleID'), flatten = F, page = page, pageSize = 10000)
    
    if(length(newResults$Objects) == 0){
      break()
    } 
    
    results <- append(results, newResults$Objects)
  }
  
  objects <- results %>% toJSON(auto_unbox = T) %>% fromJSON(flatten = T)
  
  nonUniqueObjectNames <- objects %>% group_by(CurrentName) %>% count() %>% filter(n > 1) %>% pull(CurrentName)
  
  # Fix objects with non-unique names.
  objects %>% mutate(ObjectName = ifelse(CurrentName %in% nonUniqueObjectNames, str_replace_all(FormattedObjectPath, fixed('/'), ''), CurrentName))
}


loadSkyFields <- function(){
  
  require(tidyverse)
  require(httr)
  require(jsonlite)
  require(glue)
  
  results <- list()
  for(pg in 1:100000){
    
    newResults <- getAllSkyObjects(module = 'SkySys', objectName = 'Field', SearchConditionsList = 'Status Contains Complete', searchFields = c('FieldID', 'IsPrimaryKey', 'CurrentIsRequired', 'IsDeniable', 'UserCanEdit', 'CurrentName', 'CurrentType', 'IsSkywardField', 'ObjectID'), flatten = F, page = pg, pageSize = 10000)
    
    if(length(newResults$Objects) == 0){
      break()
    } 
    
    results <- append(results, newResults$Objects)
  }
  
  fields <- results %>% toJSON(auto_unbox = T) %>% fromJSON(flatten = T)
  
  fields %>% mutate(FieldName = CurrentName)
}

# Function to get last object name in string with object paths separated by .
pluckLastName <- function(objTreeStrings){
  lapply(objTreeStrings, function(objTreeString){
    if(str_locate_all(objTreeString, fixed('.')) %>% unlist() %>% length() > 0){
      objTreeString %>% str_sub(objTreeString %>% str_locate_all(fixed('.')) %>% as.data.frame() %>% arrange(desc(start)) %>% slice(1) %>% pluck(1) + 1, nchar(.))
    }else{
      objTreeString
    }
  }) %>% unlist()
}

# This function enumerates all related objects up to a maximum depth for input objects. For use with getSchemaForObjects.
generateObjectTree <- function(objTrees, allObjectsList, maxDepth){
  
  require(tidyverse)
  require(glue)
  
  if(!exists('skyFields', inherits = T)) fields <- getFieldsInfo()
  if(!exists('skyObjects', inherits = T)) objects <- getObjectsInfo()
  if(!exists('skyRelationships', inherits = T)) relationships <- getRelationshipsInfo()
  
  objTreeObjects <- objTrees %>% filter(Type == 'object')
  if(nrow(objTreeObjects) > 0){
    for(i in 1:nrow(objTreeObjects)){
      
      objTree <- objTreeObjects %>% slice(i)
      
      objName <- objTree %>% pluck('Name') %>% pluckLastName()
      
      if(objName %in% allObjectsList){
        # Remove object if it already was added.
        objTrees <- objTrees %>% slice(-which(objTrees$Name == (objTree %>% pluck('Name'))))
        next()
      }
      
      allObjectsList <- append(allObjectsList, objName)
      
      objID <- objects %>% filter(ObjectName == objName) %>% pull(ObjectID)
      
      # Add fields...
      fieldsToAdd <- fields %>% filter(ObjectID == objID) %>% pull(FieldName)
      
      fieldsToAdd <- tibble(Name = paste(objTree %>% pluck('Name'), fieldsToAdd, sep = '.'), Type = 'field')
      
      objTrees <- bind_rows(objTrees, fieldsToAdd)
      
      # Add Objects
      rels <- relationships %>% filter(ObjectIDPrimary == objID, RelationshipType %in% c('ManyToOne', 'OneToOne'))
      
      rels <- rels %>% select(ObjectIDForeignCurrent) %>% distinct() %>% left_join(objects, by = c('ObjectIDForeignCurrent' = 'ObjectID')) %>% filter(!str_detect(objTree$Name, glue('{ObjectName}.')), ObjectName != objName)
      
      if(nrow(rels) > 0){
        
        objectsToAdd <- apply(rels, 1, function(rel){
          
          toAdd <- objects %>% filter(ObjectID == (rel %>% pluck('ObjectIDForeignCurrent') %>% trimws())) %>% pull(ObjectName)
          
          tibble(Name = paste(objTree %>% pluck('Name'), toAdd, sep = '.'), Type = 'object')
          
        })
        
        objTrees <- bind_rows(objTrees, objectsToAdd)
      }
      
      objTrees <- objTrees %>% slice(-which(objTrees$Name == (objTree %>% pluck('Name'))))
    }}
  
  objTrees
}


# This function returns a list representation of object and field relationships.
getSchemaForObjects <- function(seedObjectName, maxDepth = 4){
  
  objTrees <- tibble(Name = seedObjectName, Type = 'object')
  depth <- 1
  
  allObjectsList <- seedObjectName %>% str_split(fixed('.')) %>% pluck(1)
  allObjectsList <- ifelse(length(allObjectsList) > 1, allObjectsList[1:(length(allObjectsList) - 1)], list())
  
  while((nrow(objTrees %>% filter(Type == 'object')) > 0) & depth <= maxDepth){
    
    objTrees <- generateObjectTree(objTrees, allObjectsList)
    depth <- depth + 1
  }
  
  textToList <- function(myTextTreeDF){
    
    allTexts <- c('')
    
    #   for(myText in myTexts){
    for(i in 1:nrow(myTextTreeDF)){
      
      myText <- myTextTreeDF$Name[[i]]
      myTextType <- myTextTreeDF$Type[[i]]
      
      myTextList <- append('returnObj', myText %>% str_split(fixed('.')) %>% pluck(1))
      
      for(i in 1:length(myTextList)){
        
        if(i < length(myTextList)){
          
          potentialNewList <- paste(myTextList[1:i], collapse = '.') %>% str_replace_all(fixed('.'), '$')
          
          if(!any(str_detect(allTexts, fixed(potentialNewList)))){
            allTexts <- append(allTexts, potentialNewList)
            toEval <- paste0(potentialNewList, ' <- list()')
            eval(parse(text = toEval))
            
          }}else{
            
            if(myTextType == 'field'){
              toEval <- glue("returnObj${myText %>% str_replace_all(fixed('.'), '$')} <- myText")
            }else{
              toEval <- glue("returnObj${myText %>% str_replace_all(fixed('.'), '$')} <- list(ObjectPath = myText, NextDepth = depth)")
            }
            
            eval(parse(text = toEval))
          }
      }}
    
    returnObj
  }
  
  objTrees %>% textToList()
}

#' Get all Search Conditions Types for use in Filtering
#'
#' This function returns the Search Conditions that can be used to filter API GET requests.
#'
#' @concept General
#' @return All search condition types for filtering.
#' @section References:
#' \{yourApiUrl\}/swagger\cr\cr
#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
#' @export
getAllSearchConditionTypes <- function(){
  
  ConditionTypes <- c('Less', 'LessEqual', 'Equal', 'NotEqual', 'GreaterEqual', 'Greater', 'BetweenInclusive', 'BetweenExclusive', 'Null', 'NotNull', 'List', 'NotList', 'Like', 'NotLike', 'Begins', 'NotBegins', 'Contains', 'NotContains', 'Ends', 'NotEnds')
  
  return(ConditionTypes)
}


generateObjectFunctions <- function(modules = loadSkyModules() %>% arrange(ModuleShortName)){
  
  require(tidyverse)
  require(httr)
  require(jsonlite)
  require(glue)
  
  for(file in list.files('R')) file.remove(glue('R/{file}'))
  
  for(i in 1:nrow(modules)){
    
    module <- modules %>% slice(i)
    
    print(glue('Module {i} of {nrow(modules)}: {module$DisplayName}'))
    
    # Create {module}.R file
    if(!dir.exists('R')) dir.create('R')
    write_lines(NULL, path = glue('R/{module$DisplayName}.R'), append = F)
    
    next()
    objects <- skyObjects %>% filter(ModuleID == module$ModuleID)
    for(object in objects){
      
      #### Generate LIST functions
      
      
      #### Generate GET functions
      
      
      #### Generate CREATE functions
      
      
      #### Generate MODIFY functions
      
      
      #### Generate DELETE functions
      
    }
  }
}


