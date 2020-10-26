
	#' List TempIDListItems
	#'
	#' This function returns a dataframe or json object of TempIDListItems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIDListItems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIDListItems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIDListItem') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Common
	#' @return A list of TempIDListItems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIDListItems <- function(searchConditionsList = NULL, TempIDListItemID = F, ID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Common", objectName = "TempIDListItem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIDListItem
	#'
	#' This function returns a dataframe or json object of a TempIDListItem
	#' @param TempIDListItemID The ID of the TempIDListItem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIDListItem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIDListItem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIDListItem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Common
	#' @return A dataframe or of TempIDListItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIDListItem <- function(TempIDListItemID, ID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIDListItemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Common", objectName = "TempIDListItem", objectId = TempIDListItemID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIDListItem
	#'
	#' This function deletes a TempIDListItem
	#' @param TempIDListItemID The ID of the TempIDListItem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Common
	#' @return The TempIDListItemID of the deleted TempIDListItem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIDListItem <- function(TempIDListItemID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Common", objectName = "TempIDListItem", objectId = TempIDListItemID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CommonTempErrors
	#'
	#' This function returns a dataframe or json object of CommonTempErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CommonTempErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CommonTempErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CommonTempError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Common
	#' @return A list of CommonTempErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCommonTempErrors <- function(searchConditionsList = NULL, TempErrorID = F, ParentObjectID = F, ErrorNumber = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Common", objectName = "TempError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CommonTempError
	#'
	#' This function returns a dataframe or json object of a CommonTempError
	#' @param CommonTempErrorID The ID of the CommonTempError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CommonTempError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CommonTempError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CommonTempError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Common
	#' @return A dataframe or of CommonTempError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCommonTempError <- function(CommonTempErrorID, TempErrorID = F, ParentObjectID = F, ErrorNumber = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CommonTempErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Common", objectName = "TempError", objectId = CommonTempErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CommonTempError
	#'
	#' This function deletes a CommonTempError
	#' @param CommonTempErrorID The ID of the CommonTempError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Common
	#' @return The CommonTempErrorID of the deleted CommonTempError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCommonTempError <- function(CommonTempErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Common", objectName = "TempError", objectId = CommonTempErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CommonTempError
	#'
	#' This function creates a CommonTempError
	#' @param fieldNames The field values to give the created CommonTempError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Common
	#' @return A newly created CommonTempError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCommonTempError <- function(ParentObjectID = NULL, ErrorNumber = NULL, Message = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Common", objectName = "TempError", body = list(DataObject = body), searchFields = append("TempErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CommonTempError
	#'
	#' This function modifies a CommonTempError
	#' @param fieldNames The field values to give the modified CommonTempError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Common
	#' @return The modified CommonTempError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCommonTempError <- function(TempErrorID, ParentObjectID = NULL, ErrorNumber = NULL, Message = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Common", objectName = "TempError", objectId = TempErrorID, body = list(DataObject = body), searchFields = append("TempErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
