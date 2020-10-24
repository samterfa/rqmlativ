
	#' List DataMigrationManagers
	#'
	#' This function returns a dataframe or json object of DataMigrationManagers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMigrationManagers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMigrationManagers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMigrationManager') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept DataMigrationManager
	#' @return A list of DataMigrationManagers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDataMigrationManagers <- function(searchConditionsList = NULL, DataMigrationManagerID = F, ProcessedTime = F, MigrationType = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Label = F, SecondaryType = F, MediaID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "DataMigrationManager", objectName = "DataMigrationManager", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DataMigrationManager
	#'
	#' This function returns a dataframe or json object of a DataMigrationManager
	#' @param DataMigrationManagerID The ID of the DataMigrationManager to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMigrationManager. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMigrationManager.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMigrationManager') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept DataMigrationManager
	#' @return A dataframe or of DataMigrationManager
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDataMigrationManager <- function(DataMigrationManagerID, ProcessedTime = F, MigrationType = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Label = F, SecondaryType = F, MediaID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DataMigrationManagerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "DataMigrationManager", objectName = "DataMigrationManager", objectId = DataMigrationManagerID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DataMigrationManager
	#'
	#' This function deletes a DataMigrationManager
	#' @param DataMigrationManagerID The ID of the DataMigrationManager to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept DataMigrationManager
	#' @return The DataMigrationManagerID of the deleted DataMigrationManager.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDataMigrationManager <- function(DataMigrationManagerID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "DataMigrationManager", objectName = "DataMigrationManager", objectId = DataMigrationManagerID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DataMigrationManager
	#'
	#' This function creates a DataMigrationManager
	#' @param fieldNames The field values to give the created DataMigrationManager. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept DataMigrationManager
	#' @return A newly created DataMigrationManager
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDataMigrationManager <- function(ProcessedTime = NULL, MigrationType = NULL, ProcessType = NULL, Label = NULL, SecondaryType = NULL, MediaID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "DataMigrationManager", objectName = "DataMigrationManager", body = list(DataObject = body), searchFields = append("DataMigrationManagerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DataMigrationManager
	#'
	#' This function modifies a DataMigrationManager
	#' @param fieldNames The field values to give the modified DataMigrationManager. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept DataMigrationManager
	#' @return The modified DataMigrationManager
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDataMigrationManager <- function(DataMigrationManagerID, ProcessedTime = NULL, MigrationType = NULL, ProcessType = NULL, Label = NULL, SecondaryType = NULL, MediaID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "DataMigrationManager", objectName = "DataMigrationManager", objectId = DataMigrationManagerID, body = list(DataObject = body), searchFields = append("DataMigrationManagerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DataMigrationManagerExceptions
	#'
	#' This function returns a dataframe or json object of DataMigrationManagerExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMigrationManagerExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMigrationManagerExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMigrationManagerException') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept DataMigrationManager
	#' @return A list of DataMigrationManagerExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDataMigrationManagerExceptions <- function(searchConditionsList = NULL, DataMigrationManagerExceptionID = F, DataMigrationManagerID = F, SourcePrimaryKey = F, Object = F, Identifier = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExceptionType = F, Category = F, Col1 = F, Col2 = F, Col3 = F, Col4 = F, Col5 = F, Col6 = F, Col7 = F, Col8 = F, Col9 = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "DataMigrationManager", objectName = "DataMigrationManagerException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DataMigrationManagerException
	#'
	#' This function returns a dataframe or json object of a DataMigrationManagerException
	#' @param DataMigrationManagerExceptionID The ID of the DataMigrationManagerException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMigrationManagerException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMigrationManagerException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMigrationManagerException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept DataMigrationManager
	#' @return A dataframe or of DataMigrationManagerException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDataMigrationManagerException <- function(DataMigrationManagerExceptionID, DataMigrationManagerID = F, SourcePrimaryKey = F, Object = F, Identifier = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExceptionType = F, Category = F, Col1 = F, Col2 = F, Col3 = F, Col4 = F, Col5 = F, Col6 = F, Col7 = F, Col8 = F, Col9 = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DataMigrationManagerExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "DataMigrationManager", objectName = "DataMigrationManagerException", objectId = DataMigrationManagerExceptionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DataMigrationManagerException
	#'
	#' This function deletes a DataMigrationManagerException
	#' @param DataMigrationManagerExceptionID The ID of the DataMigrationManagerException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept DataMigrationManager
	#' @return The DataMigrationManagerExceptionID of the deleted DataMigrationManagerException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDataMigrationManagerException <- function(DataMigrationManagerExceptionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "DataMigrationManager", objectName = "DataMigrationManagerException", objectId = DataMigrationManagerExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DataMigrationManagerException
	#'
	#' This function creates a DataMigrationManagerException
	#' @param fieldNames The field values to give the created DataMigrationManagerException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept DataMigrationManager
	#' @return A newly created DataMigrationManagerException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDataMigrationManagerException <- function(DataMigrationManagerID = NULL, SourcePrimaryKey = NULL, Object = NULL, Identifier = NULL, Message = NULL, ExceptionType = NULL, Category = NULL, Col1 = NULL, Col2 = NULL, Col3 = NULL, Col4 = NULL, Col5 = NULL, Col6 = NULL, Col7 = NULL, Col8 = NULL, Col9 = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "DataMigrationManager", objectName = "DataMigrationManagerException", body = list(DataObject = body), searchFields = append("DataMigrationManagerExceptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DataMigrationManagerException
	#'
	#' This function modifies a DataMigrationManagerException
	#' @param fieldNames The field values to give the modified DataMigrationManagerException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept DataMigrationManager
	#' @return The modified DataMigrationManagerException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDataMigrationManagerException <- function(DataMigrationManagerExceptionID, DataMigrationManagerID = NULL, SourcePrimaryKey = NULL, Object = NULL, Identifier = NULL, Message = NULL, ExceptionType = NULL, Category = NULL, Col1 = NULL, Col2 = NULL, Col3 = NULL, Col4 = NULL, Col5 = NULL, Col6 = NULL, Col7 = NULL, Col8 = NULL, Col9 = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "DataMigrationManager", objectName = "DataMigrationManagerException", objectId = DataMigrationManagerExceptionID, body = list(DataObject = body), searchFields = append("DataMigrationManagerExceptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
