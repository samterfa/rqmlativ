usethis::use_pipe()

### This function checks obtains a oauth 2.0 token for making Skyward requests. GET(url = requestUrl, config = config(token = checkSkywardAuthentication()))
getSkywardToken <- function(){
  
  requiredEnvVars <- c("SkywardConsumerKey", "SkywardConsumerSecret", "SkywardBaseUrl")
  
  if((Sys.getenv(requiredEnvVars) == '') %>% any()) stop('Skyward API Credentials Not Set. Try Sys.setenv(SkywardConsumerKey = {yourSkywardConsumerKey}, SkywardConsumerSecret = {yourSkywardConsumerSecret}, SkywardBaseUrl = {yourSkywardBaseUrl})')
  
  app <- httr::oauth_app(appname = 'Nightly Scripts', key = Sys.getenv('SkywardConsumerKey'), secret = Sys.getenv('SkywardConsumerSecret'))
  
  httr::oauth2.0_token(endpoint = httr::oauth_endpoint(authorize = glue::glue("{Sys.getenv('SkywardBaseUrl')}/oauth/authorize"), refresh = glue::glue("{Sys.getenv('SkywardBaseUrl')}/oauth/token"), access = glue::glue("{Sys.getenv('SkywardBaseUrl')}/oauth/token"), validate = glue::glue("{Sys.getenv('SkywardBaseUrl')}/Generic")), app = app, scope = NULL, client_credentials = T, cache = T)
}

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
    condition <- stringr::str_split(SearchConditionsList[[i]], ' ') %>% purrr::pluck(1)
    
    if(!condition[[2]] %>% stringr::str_detect('List')){
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
                    List = paste(condition[3:length(condition)], collapse = ' ') %>% stringr::str_split(',') %>% unlist() %>% stringr::str_trim() %>% as.list()
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

getSkyObject <- function(module, objectName, objectId, searchFields = 'all', entityId = 1, api = 'Generic', flatten = T){
  
  baseUrl <- Sys.getenv('skyward_base_url')
  endpoint <- glue::glue('/Generic/{entityId}/{module}/{objectName}/{objectId}')
  method <- 'GET'
 
  object <- skyObjects %>% filter(FormattedObjectPath == glue::glue('{module}/{objectName}'))
  allSearchFields <- skyFields %>% filter(ObjectID == object$ObjectID) %>% pull(CurrentName)
  ifelse(searchFields == 'all', searchFields <- allSearchFields, searchFields <- searchFields)
  
  searchFields <- searchFields %>% keep(~!.x %in% c("Relationships", "ValidationRules"))
  queryParams <- eval(parse(text = paste0('list(', paste0('searchFields = "', searchFields, '"') %>% str_flatten(', '), ')')))
  
  requestText <- glue::glue('{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}", query = queryParams, config = config(token = getSkywardToken()))') 
  
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

listSkyObjects <- function(module, objectName, schoolYearID = NULL, searchFields = 'all', page = 1, pageSize = 100, SearchConditionsList = NULL, SearchConditionsGroupType = 'And', SearchSortFieldNamesList = NULL, SearchSortFieldNamesDescendingList = rep(F, length(SearchSortFieldNamesList)), entityId = 1, api = 'Generic', flatten = T){
  
  endpoint <- glue::glue('/Generic/{entityId}/{module}/{objectName}')
  
  method <- ifelse(is.null(SearchConditionsList) & is.null(SearchSortFieldNamesList), 'GET', 'POST')
  
  if(all(searchFields == 'all')){
    if(exists('skyObjects') & exists('skyFields')){
      object <- skyObjects %>% dplyr::filter(FormattedObjectPath == glue::glue('{module}/{objectName}'))
      allSearchFields <- skyFields %>% dplyr::filter(ObjectID == object$ObjectID) %>% dplyr::pull(CurrentName)
      searchFields <- allSearchFields
    }else{
      stop('Must either load skyObjects and skyFields, or specificy searchFields.')
    }
  }
  
  searchFields <- searchFields %>% purrr::keep(~!.x %in% c("Relationships", "ValidationRules"))
  queryParams <- eval(parse(text = paste0('list(', paste0('searchFields = "', searchFields, '"') %>% stringr::str_flatten(', '), ')')))
  
  if(!is.null(schoolYearID)) queryParams <- queryParams %>% purrr::list_merge(SchoolYearID = schoolYearID)
  
  searchObject <- createSearchObject(SearchConditionsList = SearchConditionsList, SearchConditionsGroupType = SearchConditionsGroupType, SearchSortFieldNamesList = SearchSortFieldNamesList, SearchSortFieldNamesDescendingList = SearchSortFieldNamesDescendingList)
  
  requestText <- glue::glue('httr::{method}("{Sys.getenv("SkywardBaseUrl")}{endpoint}/{page}/{format(pageSize, scientific = F)}", body = searchObject %>% jsonlite::toJSON(auto_unbox = T), httr::content_type("application/json"), query = queryParams, config = httr::config(token = getSkywardToken()))') 
  
  response <- eval(parse(text = requestText))
  
  if(response$status_code == 403){
    print('Refreshing auth token')
    file.remove(response$request$auth_token$cache_path)
    response <- eval(parse(text = requestText))
  }
  
  if(response$status_code > 300) return(response)
  
  if(!flatten) return(httr::content(response))
  
  httr::content(response) %>% purrr::pluck('Objects') %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T)
}


loadSkyRelationships <- function(){
  
  results <- list()
  for(page in 1:100000){
    
    newResults <- listSkyObjects(module = 'SkySys', objectName = 'Relationship', SearchConditionsList = 'Status Contains Complete', searchFields = c('CurrentName', 'CurrentType', 'IsSkywardRelationship', 'ObjectIDPrimary', 'ObjectIDForeignCurrent'), flatten = F, page = page, pageSize = 10000)
    
    if(length(newResults$Objects) == 0){
      break()
    } 
    
    results <- append(results, newResults$Objects)
  }
  
  relationships <- results %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T)
  
  relationships %>% dplyr::mutate(RelationshipName = CurrentName) %>% dplyr::rename(RelationshipType = CurrentType) %>% dplyr::filter(!RelationshipName %in% c('UserCreatedBy', 'UserModifiedBy'))
}


loadSkyModules <- function(){
  
  results <- listSkyObjects(module = 'SkySys', objectName = 'Module', SearchConditionsList = 'Status Contains Complete', searchFields = c('ModuleID', 'CurrentName', 'DisplayName', 'IsSkywardModule'), pageSize = 500, flatten = F) %>% purrr::pluck('Objects')
   
  results %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T) %>% dplyr::rename(ModuleShortName = CurrentName)
}


loadSkyObjects <- function(){
  
  results <- list()
  for(page in 1:100000){
    
    newResults <- listSkyObjects(module = 'SkySys', objectName = 'Object', SearchConditionsList = 'Status Contains Complete', searchFields = c('CurrentName', 'FormattedObjectPath', 'IsSkywardObject', 'ObjectID', 'ModuleID'), flatten = F, page = page, pageSize = 10000)
    
    if(length(newResults$Objects) == 0){
      break()
    } 
    
    results <- append(results, newResults$Objects)
  }
  
  objects <- results %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::fromJSON(flatten = T)
  
  # Fix non-unique CurrentNames
  nonUniqueObjectNames <- objects %>% dplyr::group_by(CurrentName) %>% dplyr::count() %>% dplyr::filter(n > 1) %>% dplyr::pull(CurrentName)
  objects <- objects %>% dplyr::mutate(ObjectName = ifelse(CurrentName %in% nonUniqueObjectNames, stringr::str_replace_all(FormattedObjectPath, stringr::fixed('/'), ''), CurrentName))
  
  # Fix non-unique ObjectNames caused by the CurrentNames fix.
  nonUniqueObjectNames <- objects %>% dplyr::group_by(ObjectName) %>% dplyr::count() %>% dplyr::filter(n > 1) %>% dplyr::pull(ObjectName)
  objects %>% dplyr::mutate(ObjectName = ifelse(ObjectName %in% nonUniqueObjectNames, stringr::str_replace_all(FormattedObjectPath, stringr::fixed('/'), ''), ObjectName))
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
  
  fields %>% dplyr::mutate(FieldName = CurrentName)
}

# Function to get last object name in string with object paths separated by .
pluckLastName <- function(objTreeStrings){
  lapply(objTreeStrings, function(objTreeString){
    if(stringr::str_locate_all(objTreeString, stringr::fixed('.')) %>% unlist() %>% length() > 0){
      objTreeString %>% stringr::str_sub(objTreeString %>% stringr::str_locate_all(fixed('.')) %>% as.data.frame() %>% dplyr::arrange(desc(start)) %>% dplyr::slice(1) %>% purrr::pluck(1) + 1, nchar(.))
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
        objTrees <- objTrees %>% dplyr::slice(-which(objTrees$Name == (objTree %>% pluck('Name'))))
        next()
      }
      
      allObjectsList <- append(allObjectsList, objName)
      
      objID <- skyObjects %>% dplyr::filter(ObjectName == objName) %>% dplyr::pull(ObjectID)
      
      # Add fields...
      fieldsToAdd <- skyFields %>% dplyr::filter(ObjectID == objID) %>% dplyr::pull(FieldName)
      
      fieldsToAdd <- tibble(Name = paste(objTree %>% purrr::pluck('Name'), fieldsToAdd, sep = '.'), Type = 'field')
      
      objTrees <- dplyr::bind_rows(objTrees, fieldsToAdd)
      
      # Add Objects
      rels <- skyRelationships %>% dplyr::filter(ObjectIDPrimary == objID, RelationshipType %in% c('ManyToOne', 'OneToOne'))
     
      rels <- rels %>% dplyr::select(ObjectIDForeignCurrent) %>% dplyr::distinct() %>% dplyr::left_join(skyObjects, by = c('ObjectIDForeignCurrent' = 'ObjectID')) %>% dplyr::filter(!str_detect(objTree$Name, glue::glue('{ObjectName}.')), ObjectName != objName)
      
      if(nrow(rels) > 0){
        
        objectsToAdd <- apply(rels, 1, function(rel){
          
          toAdd <- skyObjects %>% dplyr::filter(ObjectID == (rel %>% purrr::pluck('ObjectIDForeignCurrent') %>% trimws())) %>% dplyr::pull(ObjectName)
          
          tibble(Name = paste(objTree %>% purrr::pluck('Name'), toAdd, sep = '.'), Type = 'object')
          
        })
        
        objTrees <- dplyr::bind_rows(objTrees, objectsToAdd)
      }
      
      objTrees <- objTrees %>% dplyr::slice(-which(objTrees$Name == (objTree %>% purrr::pluck('Name'))))
    }}
  
  objTrees
}


# This function returns a list representation of object and field relationships.
getSchemaForObjects <- function(seedObjectName, maxDepth = 1){
  
  objTrees <- dplyr::tibble(Name = seedObjectName, Type = 'object')
  depth <- 1
  
  allObjectsList <- seedObjectName %>% stringr::str_split(fixed('.')) %>% pluck(1)
  allObjectsList <- ifelse(length(allObjectsList) > 1, allObjectsList[1:(length(allObjectsList) - 1)], list())
 
  while((nrow(objTrees %>% dplyr::filter(Type == 'object')) > 0) & depth <= maxDepth){
    
    objTrees <- generateObjectTree(objTrees, allObjectsList)
    depth <- depth + 1
  }
 
  textToList <- function(myTextTreeDF){
    
    allTexts <- c('')
    
    for(i in 1:nrow(myTextTreeDF)){
      
      myText <- myTextTreeDF$Name[[i]]
      myTextType <- myTextTreeDF$Type[[i]]
      
      myTextList <- append('returnObj', myText %>% stringr::str_split(fixed('.')) %>% pluck(1))
      
      for(i in 1:length(myTextList)){
        
        if(i < length(myTextList)){
          
          potentialNewList <- paste(myTextList[1:i], collapse = '.') %>% stringr::str_replace_all(fixed('.'), '$')
          
          if(!any(stringr::str_detect(allTexts, stringr::fixed(potentialNewList)))){
            allTexts <- append(allTexts, potentialNewList)
            toEval <- paste0(potentialNewList, ' <- list()')
            eval(parse(text = toEval))
            
          }}else{
            
            if(myTextType == 'field'){
              toEval <- glue::glue("returnObj${myText %>% stringr::str_replace_all(fixed('.'), '$')} <- myText")
            }else{
              toEval <- glue::glue("returnObj${myText %>% stringr::str_replace_all(fixed('.'), '$')} <- list(ObjectPath = myText, NextDepth = depth)")
            }
            
            eval(parse(text = toEval))
          }
      }}
    
    returnObj
  }
  
  objTrees %>% dplyr::mutate(Name = Name %>% stringr::str_replace(glue('{seedObjectName}.'), '')) %>% textToList()
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
  
  c('Less', 'LessEqual', 'Equal', 'NotEqual', 'GreaterEqual', 'Greater', 'BetweenInclusive', 'BetweenExclusive', 'Null', 'NotNull', 'List', 'NotList', 'Like', 'NotLike', 'Begins', 'NotBegins', 'Contains', 'NotContains', 'Ends', 'NotEnds')
}


generateObjectFunctions <- function(modules = loadSkyModules() %>% dplyr::arrange(ModuleShortName), deleteAllFiles = T){
  
  if(deleteAllFiles) for(file in list.files('R') %>% purrr::discard(~.x %in% c('Utilities.R', 'utils-pipe.R'))) file.remove(glue::glue('R/{file}'))
  
  for(i in 1:nrow(modules)){
    
    module <- modules %>% dplyr::slice(i)
    
    print(glue::glue('Module {i} of {nrow(modules)}: {module$DisplayName}'))
  
    # Create {module}.R file
    filepath <- glue::glue('R/{module$DisplayName}.R')
    if(!dir.exists('R')) dir.create('R')
    
    objects <- skyObjects %>% dplyr::filter(ModuleID == module$ModuleID)
    if(nrow(objects) == 0) next()
    readr::write_lines(NULL, path = filepath, append = F)
    
    for(j in 1:nrow(objects)){
      
      object <- objects %>% dplyr::slice(j)
      
      fields <- skyFields %>% dplyr::filter(ObjectID == object$ObjectID) %>% dplyr::pull(FieldName) %>% unique()
      
      #### Generate LIST functions
      functionName <- glue::glue('list{pluralize::pluralize(object$ObjectName)}')
      
      functionText <- glue::glue('\n\n\t{functionName} <- function(searchConditionsList = NULL, {fields %>% paste(collapse = " = F, ")} = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){{', .trim = F)
      functionText <- paste0(functionText, '\n\n\t\tparams <- as.list(environment()) %>% append(list(...))')
      functionText <- paste0(functionText, glue::glue('\n\n\t\tsearchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())', .trim = F))
      functionText <- paste0(functionText, '\n\n\t\tifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())')
      functionText <- paste0(functionText, glue::glue('\n\n\t\tlistSkyObjects(module = "{module$ModuleShortName}", objectName = "{object$CurrentName}", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)', .trim = F))
      functionText <- paste0(functionText, '\n\t}')
    
      readr::write_lines(functionText, filepath, append = T)
      
      next()
      #### Generate GET functions
      functionName <- glue::glue('get{object$ObjectName}')
      
      #### Generate CREATE functions
      functionName <- glue::glue('create{object$ObjectName}')
      
      #### Generate MODIFY functions
      functionName <- glue::glue('modify{object$ObjectName}')
      
      #### Generate DELETE functions
      functionName <- glue::glue('delete{object$ObjectName}')
      
    }
  }
}


