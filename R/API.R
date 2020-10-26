
	#' List RestAPIRequests
	#'
	#' This function returns a dataframe or json object of RestAPIRequests
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RestAPIRequests. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RestAPIRequests.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RestAPIRequest') to get more field paths.
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
	#' @concept API
	#' @return A list of RestAPIRequests
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRestAPIRequests <- function(searchConditionsList = NULL, RestAPIRequestID = F, Nonce = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "RestAPIRequest", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RestAPIRequest
	#'
	#' This function returns a dataframe or json object of a RestAPIRequest
	#' @param RestAPIRequestID The ID of the RestAPIRequest to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RestAPIRequest. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RestAPIRequest.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RestAPIRequest') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of RestAPIRequest
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRestAPIRequest <- function(RestAPIRequestID, Nonce = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RestAPIRequestID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "RestAPIRequest", objectId = RestAPIRequestID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RestAPIRequest
	#'
	#' This function deletes a RestAPIRequest
	#' @param RestAPIRequestID The ID of the RestAPIRequest to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The RestAPIRequestID of the deleted RestAPIRequest.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRestAPIRequest <- function(RestAPIRequestID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "RestAPIRequest", objectId = RestAPIRequestID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RestAPIRequest
	#'
	#' This function creates a RestAPIRequest
	#' @param fieldNames The field values to give the created RestAPIRequest. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created RestAPIRequest
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRestAPIRequest <- function(Nonce = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "RestAPIRequest", body = list(DataObject = body), searchFields = append("RestAPIRequestID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RestAPIRequest
	#'
	#' This function modifies a RestAPIRequest
	#' @param fieldNames The field values to give the modified RestAPIRequest. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified RestAPIRequest
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRestAPIRequest <- function(RestAPIRequestID, Nonce = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "RestAPIRequest", objectId = RestAPIRequestID, body = list(DataObject = body), searchFields = append("RestAPIRequestID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserAccesses
	#'
	#' This function returns a dataframe or json object of UserAccesses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserAccesses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserAccesses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserAccess') to get more field paths.
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
	#' @concept API
	#' @return A list of UserAccesses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserAccesses <- function(searchConditionsList = NULL, UserAccessID = F, Description = F, IsActive = F, EffectiveDate = F, ExpirationDate = F, APIType = F, AuthenticationTypeOverride = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UserID = F, DisplayText = F, EffectiveAuthenticationTypeCode = F, TimeTrackingConfigID = F, IdentificationConfigID = F, OneRosterConfigID = F, AttendanceConfigID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "UserAccess", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserAccess
	#'
	#' This function returns a dataframe or json object of an UserAccess
	#' @param UserAccessID The ID of the UserAccess to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserAccess. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserAccess.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserAccess') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of UserAccess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserAccess <- function(UserAccessID, Description = F, IsActive = F, EffectiveDate = F, ExpirationDate = F, APIType = F, AuthenticationTypeOverride = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UserID = F, DisplayText = F, EffectiveAuthenticationTypeCode = F, TimeTrackingConfigID = F, IdentificationConfigID = F, OneRosterConfigID = F, AttendanceConfigID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserAccessID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "UserAccess", objectId = UserAccessID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserAccess
	#'
	#' This function deletes an UserAccess
	#' @param UserAccessID The ID of the UserAccess to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The UserAccessID of the deleted UserAccess.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserAccess <- function(UserAccessID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "UserAccess", objectId = UserAccessID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserAccess
	#'
	#' This function creates an UserAccess
	#' @param fieldNames The field values to give the created UserAccess. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created UserAccess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserAccess <- function(Description = NULL, IsActive = NULL, EffectiveDate = NULL, ExpirationDate = NULL, APIType = NULL, AuthenticationTypeOverride = NULL, UserID = NULL, TimeTrackingConfigID = NULL, IdentificationConfigID = NULL, OneRosterConfigID = NULL, AttendanceConfigID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "UserAccess", body = list(DataObject = body), searchFields = append("UserAccessID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserAccess
	#'
	#' This function modifies an UserAccess
	#' @param fieldNames The field values to give the modified UserAccess. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified UserAccess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserAccess <- function(UserAccessID, Description = NULL, IsActive = NULL, EffectiveDate = NULL, ExpirationDate = NULL, APIType = NULL, AuthenticationTypeOverride = NULL, UserID = NULL, TimeTrackingConfigID = NULL, IdentificationConfigID = NULL, OneRosterConfigID = NULL, AttendanceConfigID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "UserAccess", objectId = UserAccessID, body = list(DataObject = body), searchFields = append("UserAccessID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserAuthorizations
	#'
	#' This function returns a dataframe or json object of UserAuthorizations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserAuthorizations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserAuthorizations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserAuthorization') to get more field paths.
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
	#' @concept API
	#' @return A list of UserAuthorizations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserAuthorizations <- function(searchConditionsList = NULL, UserAuthorizationID = F, AccessCode = F, AccessCodeExpirationTime = F, State = F, Scope = F, RedirectURI = F, Revoke = F, AccessToken = F, AccessTokenExpirationTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UserID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "UserAuthorization", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserAuthorization
	#'
	#' This function returns a dataframe or json object of an UserAuthorization
	#' @param UserAuthorizationID The ID of the UserAuthorization to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserAuthorization. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserAuthorization.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserAuthorization') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of UserAuthorization
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserAuthorization <- function(UserAuthorizationID, AccessCode = F, AccessCodeExpirationTime = F, State = F, Scope = F, RedirectURI = F, Revoke = F, AccessToken = F, AccessTokenExpirationTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UserID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserAuthorizationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "UserAuthorization", objectId = UserAuthorizationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserAuthorization
	#'
	#' This function deletes an UserAuthorization
	#' @param UserAuthorizationID The ID of the UserAuthorization to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The UserAuthorizationID of the deleted UserAuthorization.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserAuthorization <- function(UserAuthorizationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "UserAuthorization", objectId = UserAuthorizationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserAuthorization
	#'
	#' This function creates an UserAuthorization
	#' @param fieldNames The field values to give the created UserAuthorization. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created UserAuthorization
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserAuthorization <- function(AccessCode = NULL, AccessCodeExpirationTime = NULL, State = NULL, Scope = NULL, RedirectURI = NULL, Revoke = NULL, AccessToken = NULL, AccessTokenExpirationTime = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "UserAuthorization", body = list(DataObject = body), searchFields = append("UserAuthorizationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserAuthorization
	#'
	#' This function modifies an UserAuthorization
	#' @param fieldNames The field values to give the modified UserAuthorization. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified UserAuthorization
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserAuthorization <- function(UserAuthorizationID, AccessCode = NULL, AccessCodeExpirationTime = NULL, State = NULL, Scope = NULL, RedirectURI = NULL, Revoke = NULL, AccessToken = NULL, AccessTokenExpirationTime = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "UserAuthorization", objectId = UserAuthorizationID, body = list(DataObject = body), searchFields = append("UserAuthorizationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List APIUsageHistories
	#'
	#' This function returns a dataframe or json object of APIUsageHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given APIUsageHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the APIUsageHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('APIUsageHistory') to get more field paths.
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
	#' @concept API
	#' @return A list of APIUsageHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAPIUsageHistories <- function(searchConditionsList = NULL, UsageHistoryID = F, UserAccessID = F, APIType = F, Url = F, HttpMethod = F, EntityID = F, SchoolYearID = F, FiscalYearID = F, HostAddress = F, ServerResponseTimeInMilliseconds = F, ResponseSize = F, HttpStatusCode = F, LogID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "UsageHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an APIUsageHistory
	#'
	#' This function returns a dataframe or json object of an APIUsageHistory
	#' @param APIUsageHistoryID The ID of the APIUsageHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given APIUsageHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the APIUsageHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('APIUsageHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of APIUsageHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAPIUsageHistory <- function(APIUsageHistoryID, UsageHistoryID = F, UserAccessID = F, APIType = F, Url = F, HttpMethod = F, EntityID = F, SchoolYearID = F, FiscalYearID = F, HostAddress = F, ServerResponseTimeInMilliseconds = F, ResponseSize = F, HttpStatusCode = F, LogID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "APIUsageHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "UsageHistory", objectId = APIUsageHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an APIUsageHistory
	#'
	#' This function deletes an APIUsageHistory
	#' @param APIUsageHistoryID The ID of the APIUsageHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The APIUsageHistoryID of the deleted APIUsageHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAPIUsageHistory <- function(APIUsageHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "UsageHistory", objectId = APIUsageHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an APIUsageHistory
	#'
	#' This function creates an APIUsageHistory
	#' @param fieldNames The field values to give the created APIUsageHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created APIUsageHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAPIUsageHistory <- function(UserAccessID = NULL, APIType = NULL, Url = NULL, HttpMethod = NULL, EntityID = NULL, SchoolYearID = NULL, FiscalYearID = NULL, HostAddress = NULL, ServerResponseTimeInMilliseconds = NULL, ResponseSize = NULL, HttpStatusCode = NULL, LogID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "UsageHistory", body = list(DataObject = body), searchFields = append("UsageHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an APIUsageHistory
	#'
	#' This function modifies an APIUsageHistory
	#' @param fieldNames The field values to give the modified APIUsageHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified APIUsageHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAPIUsageHistory <- function(UsageHistoryID, UserAccessID = NULL, APIType = NULL, Url = NULL, HttpMethod = NULL, EntityID = NULL, SchoolYearID = NULL, FiscalYearID = NULL, HostAddress = NULL, ServerResponseTimeInMilliseconds = NULL, ResponseSize = NULL, HttpStatusCode = NULL, LogID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "UsageHistory", objectId = UsageHistoryID, body = list(DataObject = body), searchFields = append("UsageHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List APIUsers
	#'
	#' This function returns a dataframe or json object of APIUsers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given APIUsers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the APIUsers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('APIUser') to get more field paths.
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
	#' @concept API
	#' @return A list of APIUsers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAPIUsers <- function(searchConditionsList = NULL, UserID = F, Name = F, ConsumerKey = F, UserIDSecurity = F, ConsumerSecret = F, IsActive = F, AuthenticationType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "User", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an APIUser
	#'
	#' This function returns a dataframe or json object of an APIUser
	#' @param APIUserID The ID of the APIUser to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given APIUser. Defaults to FALSE for all return fields which, for convenience, returns all fields for the APIUser.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('APIUser') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of APIUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAPIUser <- function(APIUserID, UserID = F, Name = F, ConsumerKey = F, UserIDSecurity = F, ConsumerSecret = F, IsActive = F, AuthenticationType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "APIUserID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "User", objectId = APIUserID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an APIUser
	#'
	#' This function deletes an APIUser
	#' @param APIUserID The ID of the APIUser to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The APIUserID of the deleted APIUser.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAPIUser <- function(APIUserID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "User", objectId = APIUserID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an APIUser
	#'
	#' This function creates an APIUser
	#' @param fieldNames The field values to give the created APIUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created APIUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAPIUser <- function(Name = NULL, ConsumerKey = NULL, UserIDSecurity = NULL, IsActive = NULL, AuthenticationType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "User", body = list(DataObject = body), searchFields = append("UserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an APIUser
	#'
	#' This function modifies an APIUser
	#' @param fieldNames The field values to give the modified APIUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified APIUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAPIUser <- function(UserID, Name = NULL, ConsumerKey = NULL, UserIDSecurity = NULL, IsActive = NULL, AuthenticationType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "User", objectId = UserID, body = list(DataObject = body), searchFields = append("UserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IdentificationEnrollments
	#'
	#' This function returns a dataframe or json object of IdentificationEnrollments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IdentificationEnrollments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IdentificationEnrollments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IdentificationEnrollment') to get more field paths.
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
	#' @concept API
	#' @return A list of IdentificationEnrollments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIdentificationEnrollments <- function(searchConditionsList = NULL, IdentificationEnrollmentID = F, UserID = F, IdentificationApplicationID = F, Type = F, Group = F, DataType = F, AuthenticationData = F, BiometricData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "IdentificationEnrollment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IdentificationEnrollment
	#'
	#' This function returns a dataframe or json object of an IdentificationEnrollment
	#' @param IdentificationEnrollmentID The ID of the IdentificationEnrollment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IdentificationEnrollment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IdentificationEnrollment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IdentificationEnrollment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of IdentificationEnrollment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIdentificationEnrollment <- function(IdentificationEnrollmentID, UserID = F, IdentificationApplicationID = F, Type = F, Group = F, DataType = F, AuthenticationData = F, BiometricData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IdentificationEnrollmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "IdentificationEnrollment", objectId = IdentificationEnrollmentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IdentificationEnrollment
	#'
	#' This function deletes an IdentificationEnrollment
	#' @param IdentificationEnrollmentID The ID of the IdentificationEnrollment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The IdentificationEnrollmentID of the deleted IdentificationEnrollment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIdentificationEnrollment <- function(IdentificationEnrollmentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "IdentificationEnrollment", objectId = IdentificationEnrollmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IdentificationEnrollment
	#'
	#' This function creates an IdentificationEnrollment
	#' @param fieldNames The field values to give the created IdentificationEnrollment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created IdentificationEnrollment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIdentificationEnrollment <- function(UserID = NULL, IdentificationApplicationID = NULL, Type = NULL, Group = NULL, DataType = NULL, AuthenticationData = NULL, BiometricData = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "IdentificationEnrollment", body = list(DataObject = body), searchFields = append("IdentificationEnrollmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IdentificationEnrollment
	#'
	#' This function modifies an IdentificationEnrollment
	#' @param fieldNames The field values to give the modified IdentificationEnrollment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified IdentificationEnrollment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIdentificationEnrollment <- function(IdentificationEnrollmentID, UserID = NULL, IdentificationApplicationID = NULL, Type = NULL, Group = NULL, DataType = NULL, AuthenticationData = NULL, BiometricData = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "IdentificationEnrollment", objectId = IdentificationEnrollmentID, body = list(DataObject = body), searchFields = append("IdentificationEnrollmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IdentificationApplications
	#'
	#' This function returns a dataframe or json object of IdentificationApplications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IdentificationApplications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IdentificationApplications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IdentificationApplication') to get more field paths.
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
	#' @concept API
	#' @return A list of IdentificationApplications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIdentificationApplications <- function(searchConditionsList = NULL, IdentificationApplicationID = F, Name = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "IdentificationApplication", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IdentificationApplication
	#'
	#' This function returns a dataframe or json object of an IdentificationApplication
	#' @param IdentificationApplicationID The ID of the IdentificationApplication to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IdentificationApplication. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IdentificationApplication.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IdentificationApplication') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of IdentificationApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIdentificationApplication <- function(IdentificationApplicationID, Name = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IdentificationApplicationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "IdentificationApplication", objectId = IdentificationApplicationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IdentificationApplication
	#'
	#' This function deletes an IdentificationApplication
	#' @param IdentificationApplicationID The ID of the IdentificationApplication to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The IdentificationApplicationID of the deleted IdentificationApplication.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIdentificationApplication <- function(IdentificationApplicationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "IdentificationApplication", objectId = IdentificationApplicationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IdentificationApplication
	#'
	#' This function creates an IdentificationApplication
	#' @param fieldNames The field values to give the created IdentificationApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created IdentificationApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIdentificationApplication <- function(Name = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "IdentificationApplication", body = list(DataObject = body), searchFields = append("IdentificationApplicationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IdentificationApplication
	#'
	#' This function modifies an IdentificationApplication
	#' @param fieldNames The field values to give the modified IdentificationApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified IdentificationApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIdentificationApplication <- function(IdentificationApplicationID, Name = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "IdentificationApplication", objectId = IdentificationApplicationID, body = list(DataObject = body), searchFields = append("IdentificationApplicationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IdentificationConfigs
	#'
	#' This function returns a dataframe or json object of IdentificationConfigs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IdentificationConfigs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IdentificationConfigs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IdentificationConfig') to get more field paths.
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
	#' @concept API
	#' @return A list of IdentificationConfigs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIdentificationConfigs <- function(searchConditionsList = NULL, IdentificationConfigID = F, IdentificationApplicationID = F, BiometricDataMaxBytes = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Code = F, Description = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "IdentificationConfig", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IdentificationConfig
	#'
	#' This function returns a dataframe or json object of an IdentificationConfig
	#' @param IdentificationConfigID The ID of the IdentificationConfig to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IdentificationConfig. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IdentificationConfig.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IdentificationConfig') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of IdentificationConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIdentificationConfig <- function(IdentificationConfigID, IdentificationApplicationID = F, BiometricDataMaxBytes = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Code = F, Description = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IdentificationConfigID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "IdentificationConfig", objectId = IdentificationConfigID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IdentificationConfig
	#'
	#' This function deletes an IdentificationConfig
	#' @param IdentificationConfigID The ID of the IdentificationConfig to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The IdentificationConfigID of the deleted IdentificationConfig.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIdentificationConfig <- function(IdentificationConfigID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "IdentificationConfig", objectId = IdentificationConfigID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IdentificationConfig
	#'
	#' This function creates an IdentificationConfig
	#' @param fieldNames The field values to give the created IdentificationConfig. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created IdentificationConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIdentificationConfig <- function(IdentificationApplicationID = NULL, BiometricDataMaxBytes = NULL, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "IdentificationConfig", body = list(DataObject = body), searchFields = append("IdentificationConfigID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IdentificationConfig
	#'
	#' This function modifies an IdentificationConfig
	#' @param fieldNames The field values to give the modified IdentificationConfig. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified IdentificationConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIdentificationConfig <- function(IdentificationConfigID, IdentificationApplicationID = NULL, BiometricDataMaxBytes = NULL, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "IdentificationConfig", objectId = IdentificationConfigID, body = list(DataObject = body), searchFields = append("IdentificationConfigID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IdentificationUserParameters
	#'
	#' This function returns a dataframe or json object of IdentificationUserParameters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IdentificationUserParameters. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IdentificationUserParameters.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IdentificationUserParameter') to get more field paths.
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
	#' @concept API
	#' @return A list of IdentificationUserParameters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIdentificationUserParameters <- function(searchConditionsList = NULL, IdentificationUserParameterID = F, UserID = F, IdentificationApplicationID = F, Name = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "IdentificationUserParameter", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IdentificationUserParameter
	#'
	#' This function returns a dataframe or json object of an IdentificationUserParameter
	#' @param IdentificationUserParameterID The ID of the IdentificationUserParameter to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IdentificationUserParameter. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IdentificationUserParameter.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IdentificationUserParameter') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of IdentificationUserParameter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIdentificationUserParameter <- function(IdentificationUserParameterID, UserID = F, IdentificationApplicationID = F, Name = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IdentificationUserParameterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "IdentificationUserParameter", objectId = IdentificationUserParameterID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IdentificationUserParameter
	#'
	#' This function deletes an IdentificationUserParameter
	#' @param IdentificationUserParameterID The ID of the IdentificationUserParameter to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The IdentificationUserParameterID of the deleted IdentificationUserParameter.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIdentificationUserParameter <- function(IdentificationUserParameterID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "IdentificationUserParameter", objectId = IdentificationUserParameterID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IdentificationUserParameter
	#'
	#' This function creates an IdentificationUserParameter
	#' @param fieldNames The field values to give the created IdentificationUserParameter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created IdentificationUserParameter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIdentificationUserParameter <- function(UserID = NULL, IdentificationApplicationID = NULL, Name = NULL, Value = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "IdentificationUserParameter", body = list(DataObject = body), searchFields = append("IdentificationUserParameterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IdentificationUserParameter
	#'
	#' This function modifies an IdentificationUserParameter
	#' @param fieldNames The field values to give the modified IdentificationUserParameter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified IdentificationUserParameter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIdentificationUserParameter <- function(IdentificationUserParameterID, UserID = NULL, IdentificationApplicationID = NULL, Name = NULL, Value = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "IdentificationUserParameter", objectId = IdentificationUserParameterID, body = list(DataObject = body), searchFields = append("IdentificationUserParameterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeTrackingConfigs
	#'
	#' This function returns a dataframe or json object of TimeTrackingConfigs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTrackingConfigs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTrackingConfigs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTrackingConfig') to get more field paths.
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
	#' @concept API
	#' @return A list of TimeTrackingConfigs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeTrackingConfigs <- function(searchConditionsList = NULL, TimeTrackingConfigID = F, DistrictID = F, BuildingID = F, IPAddress = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Code = F, Description = F, CodeDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "TimeTrackingConfig", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeTrackingConfig
	#'
	#' This function returns a dataframe or json object of a TimeTrackingConfig
	#' @param TimeTrackingConfigID The ID of the TimeTrackingConfig to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTrackingConfig. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTrackingConfig.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTrackingConfig') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of TimeTrackingConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeTrackingConfig <- function(TimeTrackingConfigID, DistrictID = F, BuildingID = F, IPAddress = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Code = F, Description = F, CodeDescription = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeTrackingConfigID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "TimeTrackingConfig", objectId = TimeTrackingConfigID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeTrackingConfig
	#'
	#' This function deletes a TimeTrackingConfig
	#' @param TimeTrackingConfigID The ID of the TimeTrackingConfig to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The TimeTrackingConfigID of the deleted TimeTrackingConfig.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeTrackingConfig <- function(TimeTrackingConfigID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "TimeTrackingConfig", objectId = TimeTrackingConfigID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeTrackingConfig
	#'
	#' This function creates a TimeTrackingConfig
	#' @param fieldNames The field values to give the created TimeTrackingConfig. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created TimeTrackingConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeTrackingConfig <- function(DistrictID = NULL, BuildingID = NULL, IPAddress = NULL, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "TimeTrackingConfig", body = list(DataObject = body), searchFields = append("TimeTrackingConfigID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeTrackingConfig
	#'
	#' This function modifies a TimeTrackingConfig
	#' @param fieldNames The field values to give the modified TimeTrackingConfig. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified TimeTrackingConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeTrackingConfig <- function(TimeTrackingConfigID, DistrictID = NULL, BuildingID = NULL, IPAddress = NULL, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "TimeTrackingConfig", objectId = TimeTrackingConfigID, body = list(DataObject = body), searchFields = append("TimeTrackingConfigID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OneRosterConfigs
	#'
	#' This function returns a dataframe or json object of OneRosterConfigs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OneRosterConfigs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OneRosterConfigs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OneRosterConfig') to get more field paths.
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
	#' @concept API
	#' @return A list of OneRosterConfigs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOneRosterConfigs <- function(searchConditionsList = NULL, OneRosterConfigID = F, OneRosterVendorID = F, AllowGradePassBack = F, UserAccessCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Code = F, Description = F, CodeDescription = F, DistrictID = F, DefaultCategoryCode = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "OneRosterConfig", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OneRosterConfig
	#'
	#' This function returns a dataframe or json object of an OneRosterConfig
	#' @param OneRosterConfigID The ID of the OneRosterConfig to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OneRosterConfig. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OneRosterConfig.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OneRosterConfig') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of OneRosterConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOneRosterConfig <- function(OneRosterConfigID, OneRosterVendorID = F, AllowGradePassBack = F, UserAccessCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Code = F, Description = F, CodeDescription = F, DistrictID = F, DefaultCategoryCode = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OneRosterConfigID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "OneRosterConfig", objectId = OneRosterConfigID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OneRosterConfig
	#'
	#' This function deletes an OneRosterConfig
	#' @param OneRosterConfigID The ID of the OneRosterConfig to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The OneRosterConfigID of the deleted OneRosterConfig.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOneRosterConfig <- function(OneRosterConfigID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "OneRosterConfig", objectId = OneRosterConfigID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OneRosterConfig
	#'
	#' This function creates an OneRosterConfig
	#' @param fieldNames The field values to give the created OneRosterConfig. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created OneRosterConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOneRosterConfig <- function(OneRosterVendorID = NULL, AllowGradePassBack = NULL, Code = NULL, Description = NULL, DistrictID = NULL, DefaultCategoryCode = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "OneRosterConfig", body = list(DataObject = body), searchFields = append("OneRosterConfigID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OneRosterConfig
	#'
	#' This function modifies an OneRosterConfig
	#' @param fieldNames The field values to give the modified OneRosterConfig. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified OneRosterConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOneRosterConfig <- function(OneRosterConfigID, OneRosterVendorID = NULL, AllowGradePassBack = NULL, Code = NULL, Description = NULL, DistrictID = NULL, DefaultCategoryCode = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "OneRosterConfig", objectId = OneRosterConfigID, body = list(DataObject = body), searchFields = append("OneRosterConfigID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OneRosterVendors
	#'
	#' This function returns a dataframe or json object of OneRosterVendors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OneRosterVendors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OneRosterVendors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OneRosterVendor') to get more field paths.
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
	#' @concept API
	#' @return A list of OneRosterVendors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOneRosterVendors <- function(searchConditionsList = NULL, OneRosterVendorID = F, Name = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "OneRosterVendor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OneRosterVendor
	#'
	#' This function returns a dataframe or json object of an OneRosterVendor
	#' @param OneRosterVendorID The ID of the OneRosterVendor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OneRosterVendor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OneRosterVendor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OneRosterVendor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of OneRosterVendor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOneRosterVendor <- function(OneRosterVendorID, Name = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OneRosterVendorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "OneRosterVendor", objectId = OneRosterVendorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OneRosterVendor
	#'
	#' This function deletes an OneRosterVendor
	#' @param OneRosterVendorID The ID of the OneRosterVendor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The OneRosterVendorID of the deleted OneRosterVendor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOneRosterVendor <- function(OneRosterVendorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "OneRosterVendor", objectId = OneRosterVendorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OneRosterVendor
	#'
	#' This function creates an OneRosterVendor
	#' @param fieldNames The field values to give the created OneRosterVendor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created OneRosterVendor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOneRosterVendor <- function(Name = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "OneRosterVendor", body = list(DataObject = body), searchFields = append("OneRosterVendorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OneRosterVendor
	#'
	#' This function modifies an OneRosterVendor
	#' @param fieldNames The field values to give the modified OneRosterVendor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified OneRosterVendor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOneRosterVendor <- function(OneRosterVendorID, Name = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "OneRosterVendor", objectId = OneRosterVendorID, body = list(DataObject = body), searchFields = append("OneRosterVendorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IdentificationApplicationAdmins
	#'
	#' This function returns a dataframe or json object of IdentificationApplicationAdmins
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IdentificationApplicationAdmins. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IdentificationApplicationAdmins.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IdentificationApplicationAdmin') to get more field paths.
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
	#' @concept API
	#' @return A list of IdentificationApplicationAdmins
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIdentificationApplicationAdmins <- function(searchConditionsList = NULL, IdentificationApplicationAdminID = F, UserID = F, IdentificationApplicationID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "IdentificationApplicationAdmin", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IdentificationApplicationAdmin
	#'
	#' This function returns a dataframe or json object of an IdentificationApplicationAdmin
	#' @param IdentificationApplicationAdminID The ID of the IdentificationApplicationAdmin to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IdentificationApplicationAdmin. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IdentificationApplicationAdmin.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IdentificationApplicationAdmin') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of IdentificationApplicationAdmin
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIdentificationApplicationAdmin <- function(IdentificationApplicationAdminID, UserID = F, IdentificationApplicationID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IdentificationApplicationAdminID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "IdentificationApplicationAdmin", objectId = IdentificationApplicationAdminID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IdentificationApplicationAdmin
	#'
	#' This function deletes an IdentificationApplicationAdmin
	#' @param IdentificationApplicationAdminID The ID of the IdentificationApplicationAdmin to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The IdentificationApplicationAdminID of the deleted IdentificationApplicationAdmin.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIdentificationApplicationAdmin <- function(IdentificationApplicationAdminID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "IdentificationApplicationAdmin", objectId = IdentificationApplicationAdminID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IdentificationApplicationAdmin
	#'
	#' This function creates an IdentificationApplicationAdmin
	#' @param fieldNames The field values to give the created IdentificationApplicationAdmin. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created IdentificationApplicationAdmin
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIdentificationApplicationAdmin <- function(UserID = NULL, IdentificationApplicationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "IdentificationApplicationAdmin", body = list(DataObject = body), searchFields = append("IdentificationApplicationAdminID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IdentificationApplicationAdmin
	#'
	#' This function modifies an IdentificationApplicationAdmin
	#' @param fieldNames The field values to give the modified IdentificationApplicationAdmin. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified IdentificationApplicationAdmin
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIdentificationApplicationAdmin <- function(IdentificationApplicationAdminID, UserID = NULL, IdentificationApplicationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "IdentificationApplicationAdmin", objectId = IdentificationApplicationAdminID, body = list(DataObject = body), searchFields = append("IdentificationApplicationAdminID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiApplications
	#'
	#' This function returns a dataframe or json object of EdFiApplications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiApplications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiApplications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiApplication') to get more field paths.
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
	#' @concept API
	#' @return A list of EdFiApplications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiApplications <- function(searchConditionsList = NULL, EdFiApplicationID = F, DistrictID = F, Name = F, NumericYear = F, Key = F, Secret = F, IsEnabled = F, ProcessEvents = F, ReportingType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasRunningMassSend = F, RetainMessageQueueRecordsFor = F, LiveURI = F, SandboxURI = F, GPAMethodID = F, GPABucketID = F, EarnedCreditsMethodIDOverride = F, GradeSendOption = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "EdFiApplication", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiApplication
	#'
	#' This function returns a dataframe or json object of an EdFiApplication
	#' @param EdFiApplicationID The ID of the EdFiApplication to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiApplication. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiApplication.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiApplication') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of EdFiApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiApplication <- function(EdFiApplicationID, DistrictID = F, Name = F, NumericYear = F, Key = F, Secret = F, IsEnabled = F, ProcessEvents = F, ReportingType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasRunningMassSend = F, RetainMessageQueueRecordsFor = F, LiveURI = F, SandboxURI = F, GPAMethodID = F, GPABucketID = F, EarnedCreditsMethodIDOverride = F, GradeSendOption = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiApplicationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "EdFiApplication", objectId = EdFiApplicationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiApplication
	#'
	#' This function deletes an EdFiApplication
	#' @param EdFiApplicationID The ID of the EdFiApplication to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The EdFiApplicationID of the deleted EdFiApplication.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiApplication <- function(EdFiApplicationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "EdFiApplication", objectId = EdFiApplicationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiApplication
	#'
	#' This function creates an EdFiApplication
	#' @param fieldNames The field values to give the created EdFiApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created EdFiApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiApplication <- function(DistrictID = NULL, Name = NULL, NumericYear = NULL, IsEnabled = NULL, ProcessEvents = NULL, ReportingType = NULL, RetainMessageQueueRecordsFor = NULL, LiveURI = NULL, SandboxURI = NULL, GPAMethodID = NULL, GPABucketID = NULL, EarnedCreditsMethodIDOverride = NULL, GradeSendOption = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "EdFiApplication", body = list(DataObject = body), searchFields = append("EdFiApplicationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiApplication
	#'
	#' This function modifies an EdFiApplication
	#' @param fieldNames The field values to give the modified EdFiApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified EdFiApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiApplication <- function(EdFiApplicationID, DistrictID = NULL, Name = NULL, NumericYear = NULL, IsEnabled = NULL, ProcessEvents = NULL, ReportingType = NULL, RetainMessageQueueRecordsFor = NULL, LiveURI = NULL, SandboxURI = NULL, GPAMethodID = NULL, GPABucketID = NULL, EarnedCreditsMethodIDOverride = NULL, GradeSendOption = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "EdFiApplication", objectId = EdFiApplicationID, body = list(DataObject = body), searchFields = append("EdFiApplicationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiMessageQueues
	#'
	#' This function returns a dataframe or json object of EdFiMessageQueues
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiMessageQueues. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiMessageQueues.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiMessageQueue') to get more field paths.
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
	#' @concept API
	#' @return A list of EdFiMessageQueues
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiMessageQueues <- function(searchConditionsList = NULL, EdFiMessageQueueID = F, EdFiApplicationID = F, EdFiLogID = F, IsFromSyncProcess = F, EdFiRecordTypeName = F, Status = F, RequestHTTPMethodType = F, Payload = F, RequestSentTime = F, ResponseHTTPStatus = F, ResponseReceivedTime = F, ResponsePayload = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkipEdFiLogUpdateOnResponse = F, EdFiRecordID = F, SentToSandbox = F, SortOrder = F, DisablePooling = F, PayloadDisplay = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "EdFiMessageQueue", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiMessageQueue
	#'
	#' This function returns a dataframe or json object of an EdFiMessageQueue
	#' @param EdFiMessageQueueID The ID of the EdFiMessageQueue to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiMessageQueue. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiMessageQueue.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiMessageQueue') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of EdFiMessageQueue
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiMessageQueue <- function(EdFiMessageQueueID, EdFiApplicationID = F, EdFiLogID = F, IsFromSyncProcess = F, EdFiRecordTypeName = F, Status = F, RequestHTTPMethodType = F, Payload = F, RequestSentTime = F, ResponseHTTPStatus = F, ResponseReceivedTime = F, ResponsePayload = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkipEdFiLogUpdateOnResponse = F, EdFiRecordID = F, SentToSandbox = F, SortOrder = F, DisablePooling = F, PayloadDisplay = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiMessageQueueID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "EdFiMessageQueue", objectId = EdFiMessageQueueID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiMessageQueue
	#'
	#' This function deletes an EdFiMessageQueue
	#' @param EdFiMessageQueueID The ID of the EdFiMessageQueue to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The EdFiMessageQueueID of the deleted EdFiMessageQueue.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiMessageQueue <- function(EdFiMessageQueueID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "EdFiMessageQueue", objectId = EdFiMessageQueueID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiMessageQueue
	#'
	#' This function creates an EdFiMessageQueue
	#' @param fieldNames The field values to give the created EdFiMessageQueue. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created EdFiMessageQueue
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiMessageQueue <- function(EdFiApplicationID = NULL, EdFiLogID = NULL, IsFromSyncProcess = NULL, EdFiRecordTypeName = NULL, Status = NULL, RequestHTTPMethodType = NULL, RequestSentTime = NULL, ResponseHTTPStatus = NULL, ResponseReceivedTime = NULL, ResponsePayload = NULL, SkipEdFiLogUpdateOnResponse = NULL, EdFiRecordID = NULL, SentToSandbox = NULL, DisablePooling = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "EdFiMessageQueue", body = list(DataObject = body), searchFields = append("EdFiMessageQueueID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiMessageQueue
	#'
	#' This function modifies an EdFiMessageQueue
	#' @param fieldNames The field values to give the modified EdFiMessageQueue. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified EdFiMessageQueue
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiMessageQueue <- function(EdFiMessageQueueID, EdFiApplicationID = NULL, EdFiLogID = NULL, IsFromSyncProcess = NULL, EdFiRecordTypeName = NULL, Status = NULL, RequestHTTPMethodType = NULL, RequestSentTime = NULL, ResponseHTTPStatus = NULL, ResponseReceivedTime = NULL, ResponsePayload = NULL, SkipEdFiLogUpdateOnResponse = NULL, EdFiRecordID = NULL, SentToSandbox = NULL, DisablePooling = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "EdFiMessageQueue", objectId = EdFiMessageQueueID, body = list(DataObject = body), searchFields = append("EdFiMessageQueueID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiLogs
	#'
	#' This function returns a dataframe or json object of EdFiLogs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiLogs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiLogs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiLog') to get more field paths.
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
	#' @concept API
	#' @return A list of EdFiLogs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiLogs <- function(searchConditionsList = NULL, EdFiLogID = F, EdFiApplicationID = F, ModuleID = F, ObjectID = F, EdFiRecordID = F, EdFiRecordTypeName = F, IsDeleted = F, Payload = F, Status = F, StatusDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SourceKey = F, StudentID = F, NameIDStaff = F, EntityID = F, CourseID = F, SectionID = F, IncidentID = F, ErroredPayload = F, ErrorMessage = F, DBSStatusCode = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "EdFiLog", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiLog
	#'
	#' This function returns a dataframe or json object of an EdFiLog
	#' @param EdFiLogID The ID of the EdFiLog to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiLog. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiLog.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiLog') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of EdFiLog
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiLog <- function(EdFiLogID, EdFiApplicationID = F, ModuleID = F, ObjectID = F, EdFiRecordID = F, EdFiRecordTypeName = F, IsDeleted = F, Payload = F, Status = F, StatusDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SourceKey = F, StudentID = F, NameIDStaff = F, EntityID = F, CourseID = F, SectionID = F, IncidentID = F, ErroredPayload = F, ErrorMessage = F, DBSStatusCode = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiLogID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "EdFiLog", objectId = EdFiLogID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiLog
	#'
	#' This function deletes an EdFiLog
	#' @param EdFiLogID The ID of the EdFiLog to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The EdFiLogID of the deleted EdFiLog.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiLog <- function(EdFiLogID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "EdFiLog", objectId = EdFiLogID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiLog
	#'
	#' This function creates an EdFiLog
	#' @param fieldNames The field values to give the created EdFiLog. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created EdFiLog
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiLog <- function(EdFiApplicationID = NULL, ModuleID = NULL, ObjectID = NULL, EdFiRecordID = NULL, EdFiRecordTypeName = NULL, IsDeleted = NULL, Payload = NULL, Status = NULL, StatusDescription = NULL, SourceKey = NULL, StudentID = NULL, NameIDStaff = NULL, EntityID = NULL, CourseID = NULL, SectionID = NULL, IncidentID = NULL, ErrorMessage = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "EdFiLog", body = list(DataObject = body), searchFields = append("EdFiLogID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiLog
	#'
	#' This function modifies an EdFiLog
	#' @param fieldNames The field values to give the modified EdFiLog. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified EdFiLog
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiLog <- function(EdFiLogID, EdFiApplicationID = NULL, ModuleID = NULL, ObjectID = NULL, EdFiRecordID = NULL, EdFiRecordTypeName = NULL, IsDeleted = NULL, Payload = NULL, Status = NULL, StatusDescription = NULL, SourceKey = NULL, StudentID = NULL, NameIDStaff = NULL, EntityID = NULL, CourseID = NULL, SectionID = NULL, IncidentID = NULL, ErrorMessage = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "EdFiLog", objectId = EdFiLogID, body = list(DataObject = body), searchFields = append("EdFiLogID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TableWatchApplications
	#'
	#' This function returns a dataframe or json object of TableWatchApplications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TableWatchApplications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TableWatchApplications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TableWatchApplication') to get more field paths.
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
	#' @concept API
	#' @return A list of TableWatchApplications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTableWatchApplications <- function(searchConditionsList = NULL, AllowsMultipleInstances = F, TableWatchApplicationID = F, IsEnabled = F, ProcessEvents = F, Schema = F, Table = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "TableWatchApplication", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TableWatchApplication
	#'
	#' This function returns a dataframe or json object of a TableWatchApplication
	#' @param TableWatchApplicationID The ID of the TableWatchApplication to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TableWatchApplication. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TableWatchApplication.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TableWatchApplication') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of TableWatchApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTableWatchApplication <- function(TableWatchApplicationID, AllowsMultipleInstances = F, IsEnabled = F, ProcessEvents = F, Schema = F, Table = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TableWatchApplicationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "TableWatchApplication", objectId = TableWatchApplicationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TableWatchApplication
	#'
	#' This function deletes a TableWatchApplication
	#' @param TableWatchApplicationID The ID of the TableWatchApplication to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The TableWatchApplicationID of the deleted TableWatchApplication.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTableWatchApplication <- function(TableWatchApplicationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "TableWatchApplication", objectId = TableWatchApplicationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TableWatchApplication
	#'
	#' This function creates a TableWatchApplication
	#' @param fieldNames The field values to give the created TableWatchApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created TableWatchApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTableWatchApplication <- function(IsEnabled = NULL, ProcessEvents = NULL, Schema = NULL, Table = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "TableWatchApplication", body = list(DataObject = body), searchFields = append("TableWatchApplicationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TableWatchApplication
	#'
	#' This function modifies a TableWatchApplication
	#' @param fieldNames The field values to give the modified TableWatchApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified TableWatchApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTableWatchApplication <- function(TableWatchApplicationID, IsEnabled = NULL, ProcessEvents = NULL, Schema = NULL, Table = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "TableWatchApplication", objectId = TableWatchApplicationID, body = list(DataObject = body), searchFields = append("TableWatchApplicationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendanceConfigs
	#'
	#' This function returns a dataframe or json object of AttendanceConfigs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceConfigs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceConfigs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceConfig') to get more field paths.
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
	#' @concept API
	#' @return A list of AttendanceConfigs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceConfigs <- function(searchConditionsList = NULL, AttendanceConfigID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Description = F, Entities = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "AttendanceConfig", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceConfig
	#'
	#' This function returns a dataframe or json object of an AttendanceConfig
	#' @param AttendanceConfigID The ID of the AttendanceConfig to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceConfig. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceConfig.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceConfig') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of AttendanceConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceConfig <- function(AttendanceConfigID, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Description = F, Entities = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceConfigID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "AttendanceConfig", objectId = AttendanceConfigID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceConfig
	#'
	#' This function deletes an AttendanceConfig
	#' @param AttendanceConfigID The ID of the AttendanceConfig to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The AttendanceConfigID of the deleted AttendanceConfig.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceConfig <- function(AttendanceConfigID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "AttendanceConfig", objectId = AttendanceConfigID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceConfig
	#'
	#' This function creates an AttendanceConfig
	#' @param fieldNames The field values to give the created AttendanceConfig. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created AttendanceConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceConfig <- function(Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "AttendanceConfig", body = list(DataObject = body), searchFields = append("AttendanceConfigID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceConfig
	#'
	#' This function modifies an AttendanceConfig
	#' @param fieldNames The field values to give the modified AttendanceConfig. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified AttendanceConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceConfig <- function(AttendanceConfigID, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "AttendanceConfig", objectId = AttendanceConfigID, body = list(DataObject = body), searchFields = append("AttendanceConfigID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendanceConfigEntities
	#'
	#' This function returns a dataframe or json object of AttendanceConfigEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceConfigEntities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceConfigEntities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceConfigEntity') to get more field paths.
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
	#' @concept API
	#' @return A list of AttendanceConfigEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceConfigEntities <- function(searchConditionsList = NULL, AttendanceConfigEntityID = F, AttendanceConfigID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "AttendanceConfigEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceConfigEntity
	#'
	#' This function returns a dataframe or json object of an AttendanceConfigEntity
	#' @param AttendanceConfigEntityID The ID of the AttendanceConfigEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceConfigEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceConfigEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceConfigEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of AttendanceConfigEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceConfigEntity <- function(AttendanceConfigEntityID, AttendanceConfigID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceConfigEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "AttendanceConfigEntity", objectId = AttendanceConfigEntityID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceConfigEntity
	#'
	#' This function deletes an AttendanceConfigEntity
	#' @param AttendanceConfigEntityID The ID of the AttendanceConfigEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The AttendanceConfigEntityID of the deleted AttendanceConfigEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceConfigEntity <- function(AttendanceConfigEntityID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "AttendanceConfigEntity", objectId = AttendanceConfigEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceConfigEntity
	#'
	#' This function creates an AttendanceConfigEntity
	#' @param fieldNames The field values to give the created AttendanceConfigEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created AttendanceConfigEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceConfigEntity <- function(AttendanceConfigID = NULL, EntityID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "AttendanceConfigEntity", body = list(DataObject = body), searchFields = append("AttendanceConfigEntityID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceConfigEntity
	#'
	#' This function modifies an AttendanceConfigEntity
	#' @param fieldNames The field values to give the modified AttendanceConfigEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified AttendanceConfigEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceConfigEntity <- function(AttendanceConfigEntityID, AttendanceConfigID = NULL, EntityID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "AttendanceConfigEntity", objectId = AttendanceConfigEntityID, body = list(DataObject = body), searchFields = append("AttendanceConfigEntityID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiMassSendRunHistories
	#'
	#' This function returns a dataframe or json object of EdFiMassSendRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiMassSendRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiMassSendRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiMassSendRunHistory') to get more field paths.
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
	#' @concept API
	#' @return A list of EdFiMassSendRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiMassSendRunHistories <- function(searchConditionsList = NULL, EdFiMassSendRunHistoryID = F, EdFiApplicationID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "EdFiMassSendRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiMassSendRunHistory
	#'
	#' This function returns a dataframe or json object of an EdFiMassSendRunHistory
	#' @param EdFiMassSendRunHistoryID The ID of the EdFiMassSendRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiMassSendRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiMassSendRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiMassSendRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of EdFiMassSendRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiMassSendRunHistory <- function(EdFiMassSendRunHistoryID, EdFiApplicationID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiMassSendRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "EdFiMassSendRunHistory", objectId = EdFiMassSendRunHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiMassSendRunHistory
	#'
	#' This function deletes an EdFiMassSendRunHistory
	#' @param EdFiMassSendRunHistoryID The ID of the EdFiMassSendRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The EdFiMassSendRunHistoryID of the deleted EdFiMassSendRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiMassSendRunHistory <- function(EdFiMassSendRunHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "EdFiMassSendRunHistory", objectId = EdFiMassSendRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiMassSendRunHistory
	#'
	#' This function creates an EdFiMassSendRunHistory
	#' @param fieldNames The field values to give the created EdFiMassSendRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created EdFiMassSendRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiMassSendRunHistory <- function(EdFiApplicationID = NULL, StartDateTime = NULL, EndDateTime = NULL, Type = NULL, MediaID = NULL, Status = NULL, UserIDCanceledBy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "EdFiMassSendRunHistory", body = list(DataObject = body), searchFields = append("EdFiMassSendRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiMassSendRunHistory
	#'
	#' This function modifies an EdFiMassSendRunHistory
	#' @param fieldNames The field values to give the modified EdFiMassSendRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified EdFiMassSendRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiMassSendRunHistory <- function(EdFiMassSendRunHistoryID, EdFiApplicationID = NULL, StartDateTime = NULL, EndDateTime = NULL, Type = NULL, MediaID = NULL, Status = NULL, UserIDCanceledBy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "EdFiMassSendRunHistory", objectId = EdFiMassSendRunHistoryID, body = list(DataObject = body), searchFields = append("EdFiMassSendRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEdFiAPISpecialEducationEnrollments
	#'
	#' This function returns a dataframe or json object of TempEdFiAPISpecialEducationEnrollments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEdFiAPISpecialEducationEnrollments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEdFiAPISpecialEducationEnrollments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEdFiAPISpecialEducationEnrollment') to get more field paths.
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
	#' @concept API
	#' @return A list of TempEdFiAPISpecialEducationEnrollments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEdFiAPISpecialEducationEnrollments <- function(searchConditionsList = NULL, TempEdFiAPISpecialEducationEnrollmentID = F, StudentID = F, DistrictID = F, SchoolYearID = F, LastName = F, FirstName = F, MiddleName = F, DisabilityCodes = F, UpdateType = F, StartDate = F, EndDate = F, LastEvaluationDate = F, LatestServiceStartDate = F, WISEIDNumber = F, EnrollmentWIID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "TempEdFiAPISpecialEducationEnrollment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEdFiAPISpecialEducationEnrollment
	#'
	#' This function returns a dataframe or json object of a TempEdFiAPISpecialEducationEnrollment
	#' @param TempEdFiAPISpecialEducationEnrollmentID The ID of the TempEdFiAPISpecialEducationEnrollment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEdFiAPISpecialEducationEnrollment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEdFiAPISpecialEducationEnrollment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEdFiAPISpecialEducationEnrollment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of TempEdFiAPISpecialEducationEnrollment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEdFiAPISpecialEducationEnrollment <- function(TempEdFiAPISpecialEducationEnrollmentID, StudentID = F, DistrictID = F, SchoolYearID = F, LastName = F, FirstName = F, MiddleName = F, DisabilityCodes = F, UpdateType = F, StartDate = F, EndDate = F, LastEvaluationDate = F, LatestServiceStartDate = F, WISEIDNumber = F, EnrollmentWIID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEdFiAPISpecialEducationEnrollmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "TempEdFiAPISpecialEducationEnrollment", objectId = TempEdFiAPISpecialEducationEnrollmentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEdFiAPISpecialEducationEnrollment
	#'
	#' This function deletes a TempEdFiAPISpecialEducationEnrollment
	#' @param TempEdFiAPISpecialEducationEnrollmentID The ID of the TempEdFiAPISpecialEducationEnrollment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The TempEdFiAPISpecialEducationEnrollmentID of the deleted TempEdFiAPISpecialEducationEnrollment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEdFiAPISpecialEducationEnrollment <- function(TempEdFiAPISpecialEducationEnrollmentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "TempEdFiAPISpecialEducationEnrollment", objectId = TempEdFiAPISpecialEducationEnrollmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEdFiAPISpecialEducationEnrollment
	#'
	#' This function creates a TempEdFiAPISpecialEducationEnrollment
	#' @param fieldNames The field values to give the created TempEdFiAPISpecialEducationEnrollment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created TempEdFiAPISpecialEducationEnrollment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEdFiAPISpecialEducationEnrollment <- function(LastName = NULL, FirstName = NULL, MiddleName = NULL, DisabilityCodes = NULL, UpdateType = NULL, StartDate = NULL, EndDate = NULL, LastEvaluationDate = NULL, LatestServiceStartDate = NULL, WISEIDNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "TempEdFiAPISpecialEducationEnrollment", body = list(DataObject = body), searchFields = append("TempEdFiAPISpecialEducationEnrollmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEdFiAPISpecialEducationEnrollment
	#'
	#' This function modifies a TempEdFiAPISpecialEducationEnrollment
	#' @param fieldNames The field values to give the modified TempEdFiAPISpecialEducationEnrollment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified TempEdFiAPISpecialEducationEnrollment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEdFiAPISpecialEducationEnrollment <- function(TempEdFiAPISpecialEducationEnrollmentID, LastName = NULL, FirstName = NULL, MiddleName = NULL, DisabilityCodes = NULL, UpdateType = NULL, StartDate = NULL, EndDate = NULL, LastEvaluationDate = NULL, LatestServiceStartDate = NULL, WISEIDNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "TempEdFiAPISpecialEducationEnrollment", objectId = TempEdFiAPISpecialEducationEnrollmentID, body = list(DataObject = body), searchFields = append("TempEdFiAPISpecialEducationEnrollmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEdFiAPISpecialEducationIEPServices
	#'
	#' This function returns a dataframe or json object of TempEdFiAPISpecialEducationIEPServices
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEdFiAPISpecialEducationIEPServices. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEdFiAPISpecialEducationIEPServices.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEdFiAPISpecialEducationIEPService') to get more field paths.
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
	#' @concept API
	#' @return A list of TempEdFiAPISpecialEducationIEPServices
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEdFiAPISpecialEducationIEPServices <- function(searchConditionsList = NULL, TempEdFiAPISpecialEducationIEPServiceID = F, TempEdFiAPISpecialEducationEnrollmentID = F, UpdateType = F, StartDate = F, EndDate = F, SpecialEducationSettingDescriptor = F, ReasonExitedDescriptor = F, FapeResponsibleSchool = F, IEPServiceWIID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "TempEdFiAPISpecialEducationIEPService", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEdFiAPISpecialEducationIEPService
	#'
	#' This function returns a dataframe or json object of a TempEdFiAPISpecialEducationIEPService
	#' @param TempEdFiAPISpecialEducationIEPServiceID The ID of the TempEdFiAPISpecialEducationIEPService to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEdFiAPISpecialEducationIEPService. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEdFiAPISpecialEducationIEPService.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEdFiAPISpecialEducationIEPService') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of TempEdFiAPISpecialEducationIEPService
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEdFiAPISpecialEducationIEPService <- function(TempEdFiAPISpecialEducationIEPServiceID, TempEdFiAPISpecialEducationEnrollmentID = F, UpdateType = F, StartDate = F, EndDate = F, SpecialEducationSettingDescriptor = F, ReasonExitedDescriptor = F, FapeResponsibleSchool = F, IEPServiceWIID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEdFiAPISpecialEducationIEPServiceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "TempEdFiAPISpecialEducationIEPService", objectId = TempEdFiAPISpecialEducationIEPServiceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEdFiAPISpecialEducationIEPService
	#'
	#' This function deletes a TempEdFiAPISpecialEducationIEPService
	#' @param TempEdFiAPISpecialEducationIEPServiceID The ID of the TempEdFiAPISpecialEducationIEPService to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The TempEdFiAPISpecialEducationIEPServiceID of the deleted TempEdFiAPISpecialEducationIEPService.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEdFiAPISpecialEducationIEPService <- function(TempEdFiAPISpecialEducationIEPServiceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "TempEdFiAPISpecialEducationIEPService", objectId = TempEdFiAPISpecialEducationIEPServiceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEdFiAPISpecialEducationIEPService
	#'
	#' This function creates a TempEdFiAPISpecialEducationIEPService
	#' @param fieldNames The field values to give the created TempEdFiAPISpecialEducationIEPService. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created TempEdFiAPISpecialEducationIEPService
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEdFiAPISpecialEducationIEPService <- function(UpdateType = NULL, StartDate = NULL, EndDate = NULL, SpecialEducationSettingDescriptor = NULL, ReasonExitedDescriptor = NULL, FapeResponsibleSchool = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "TempEdFiAPISpecialEducationIEPService", body = list(DataObject = body), searchFields = append("TempEdFiAPISpecialEducationIEPServiceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEdFiAPISpecialEducationIEPService
	#'
	#' This function modifies a TempEdFiAPISpecialEducationIEPService
	#' @param fieldNames The field values to give the modified TempEdFiAPISpecialEducationIEPService. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified TempEdFiAPISpecialEducationIEPService
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEdFiAPISpecialEducationIEPService <- function(TempEdFiAPISpecialEducationIEPServiceID, UpdateType = NULL, StartDate = NULL, EndDate = NULL, SpecialEducationSettingDescriptor = NULL, ReasonExitedDescriptor = NULL, FapeResponsibleSchool = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "TempEdFiAPISpecialEducationIEPService", objectId = TempEdFiAPISpecialEducationIEPServiceID, body = list(DataObject = body), searchFields = append("TempEdFiAPISpecialEducationIEPServiceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEdFiLoadSpecialEducationFromAPIErrors
	#'
	#' This function returns a dataframe or json object of TempEdFiLoadSpecialEducationFromAPIErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEdFiLoadSpecialEducationFromAPIErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEdFiLoadSpecialEducationFromAPIErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEdFiLoadSpecialEducationFromAPIError') to get more field paths.
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
	#' @concept API
	#' @return A list of TempEdFiLoadSpecialEducationFromAPIErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEdFiLoadSpecialEducationFromAPIErrors <- function(searchConditionsList = NULL, TempEdFiLoadSpecialEducationFromAPIErrorID = F, StudentID = F, LastName = F, FirstName = F, MiddleName = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "TempEdFiLoadSpecialEducationFromAPIError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEdFiLoadSpecialEducationFromAPIError
	#'
	#' This function returns a dataframe or json object of a TempEdFiLoadSpecialEducationFromAPIError
	#' @param TempEdFiLoadSpecialEducationFromAPIErrorID The ID of the TempEdFiLoadSpecialEducationFromAPIError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEdFiLoadSpecialEducationFromAPIError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEdFiLoadSpecialEducationFromAPIError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEdFiLoadSpecialEducationFromAPIError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of TempEdFiLoadSpecialEducationFromAPIError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEdFiLoadSpecialEducationFromAPIError <- function(TempEdFiLoadSpecialEducationFromAPIErrorID, StudentID = F, LastName = F, FirstName = F, MiddleName = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEdFiLoadSpecialEducationFromAPIErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "TempEdFiLoadSpecialEducationFromAPIError", objectId = TempEdFiLoadSpecialEducationFromAPIErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEdFiLoadSpecialEducationFromAPIError
	#'
	#' This function deletes a TempEdFiLoadSpecialEducationFromAPIError
	#' @param TempEdFiLoadSpecialEducationFromAPIErrorID The ID of the TempEdFiLoadSpecialEducationFromAPIError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The TempEdFiLoadSpecialEducationFromAPIErrorID of the deleted TempEdFiLoadSpecialEducationFromAPIError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEdFiLoadSpecialEducationFromAPIError <- function(TempEdFiLoadSpecialEducationFromAPIErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "TempEdFiLoadSpecialEducationFromAPIError", objectId = TempEdFiLoadSpecialEducationFromAPIErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEdFiLoadSpecialEducationFromAPIError
	#'
	#' This function creates a TempEdFiLoadSpecialEducationFromAPIError
	#' @param fieldNames The field values to give the created TempEdFiLoadSpecialEducationFromAPIError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created TempEdFiLoadSpecialEducationFromAPIError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEdFiLoadSpecialEducationFromAPIError <- function(StudentID = NULL, LastName = NULL, FirstName = NULL, MiddleName = NULL, FailureReason = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "TempEdFiLoadSpecialEducationFromAPIError", body = list(DataObject = body), searchFields = append("TempEdFiLoadSpecialEducationFromAPIErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEdFiLoadSpecialEducationFromAPIError
	#'
	#' This function modifies a TempEdFiLoadSpecialEducationFromAPIError
	#' @param fieldNames The field values to give the modified TempEdFiLoadSpecialEducationFromAPIError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified TempEdFiLoadSpecialEducationFromAPIError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEdFiLoadSpecialEducationFromAPIError <- function(TempEdFiLoadSpecialEducationFromAPIErrorID, StudentID = NULL, LastName = NULL, FirstName = NULL, MiddleName = NULL, FailureReason = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "TempEdFiLoadSpecialEducationFromAPIError", objectId = TempEdFiLoadSpecialEducationFromAPIErrorID, body = list(DataObject = body), searchFields = append("TempEdFiLoadSpecialEducationFromAPIErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ETranscriptApplications
	#'
	#' This function returns a dataframe or json object of ETranscriptApplications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ETranscriptApplications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ETranscriptApplications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ETranscriptApplication') to get more field paths.
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
	#' @concept API
	#' @return A list of ETranscriptApplications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listETranscriptApplications <- function(searchConditionsList = NULL, ETranscriptApplicationID = F, DistrictID = F, Key = F, Secret = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "ETranscriptApplication", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ETranscriptApplication
	#'
	#' This function returns a dataframe or json object of an ETranscriptApplication
	#' @param ETranscriptApplicationID The ID of the ETranscriptApplication to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ETranscriptApplication. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ETranscriptApplication.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ETranscriptApplication') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of ETranscriptApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getETranscriptApplication <- function(ETranscriptApplicationID, DistrictID = F, Key = F, Secret = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ETranscriptApplicationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "ETranscriptApplication", objectId = ETranscriptApplicationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ETranscriptApplication
	#'
	#' This function deletes an ETranscriptApplication
	#' @param ETranscriptApplicationID The ID of the ETranscriptApplication to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The ETranscriptApplicationID of the deleted ETranscriptApplication.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteETranscriptApplication <- function(ETranscriptApplicationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "ETranscriptApplication", objectId = ETranscriptApplicationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ETranscriptApplication
	#'
	#' This function creates an ETranscriptApplication
	#' @param fieldNames The field values to give the created ETranscriptApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created ETranscriptApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createETranscriptApplication <- function(DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "ETranscriptApplication", body = list(DataObject = body), searchFields = append("ETranscriptApplicationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ETranscriptApplication
	#'
	#' This function modifies an ETranscriptApplication
	#' @param fieldNames The field values to give the modified ETranscriptApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified ETranscriptApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyETranscriptApplication <- function(ETranscriptApplicationID, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "ETranscriptApplication", objectId = ETranscriptApplicationID, body = list(DataObject = body), searchFields = append("ETranscriptApplicationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiLocalEducationAgencySchoolProcesses
	#'
	#' This function returns a dataframe or json object of EdFiLocalEducationAgencySchoolProcesses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiLocalEducationAgencySchoolProcesses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiLocalEducationAgencySchoolProcesses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiLocalEducationAgencySchoolProcess') to get more field paths.
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
	#' @concept API
	#' @return A list of EdFiLocalEducationAgencySchoolProcesses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiLocalEducationAgencySchoolProcesses <- function(searchConditionsList = NULL, EdFiLocalEducationAgencySchoolProcessID = F, EdFiApplicationID = F, Process = F, SendOption = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "EdFiLocalEducationAgencySchoolProcess", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiLocalEducationAgencySchoolProcess
	#'
	#' This function returns a dataframe or json object of an EdFiLocalEducationAgencySchoolProcess
	#' @param EdFiLocalEducationAgencySchoolProcessID The ID of the EdFiLocalEducationAgencySchoolProcess to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiLocalEducationAgencySchoolProcess. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiLocalEducationAgencySchoolProcess.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiLocalEducationAgencySchoolProcess') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of EdFiLocalEducationAgencySchoolProcess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiLocalEducationAgencySchoolProcess <- function(EdFiLocalEducationAgencySchoolProcessID, EdFiApplicationID = F, Process = F, SendOption = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiLocalEducationAgencySchoolProcessID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "EdFiLocalEducationAgencySchoolProcess", objectId = EdFiLocalEducationAgencySchoolProcessID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiLocalEducationAgencySchoolProcess
	#'
	#' This function deletes an EdFiLocalEducationAgencySchoolProcess
	#' @param EdFiLocalEducationAgencySchoolProcessID The ID of the EdFiLocalEducationAgencySchoolProcess to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The EdFiLocalEducationAgencySchoolProcessID of the deleted EdFiLocalEducationAgencySchoolProcess.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiLocalEducationAgencySchoolProcess <- function(EdFiLocalEducationAgencySchoolProcessID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "EdFiLocalEducationAgencySchoolProcess", objectId = EdFiLocalEducationAgencySchoolProcessID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiLocalEducationAgencySchoolProcess
	#'
	#' This function creates an EdFiLocalEducationAgencySchoolProcess
	#' @param fieldNames The field values to give the created EdFiLocalEducationAgencySchoolProcess. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created EdFiLocalEducationAgencySchoolProcess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiLocalEducationAgencySchoolProcess <- function(EdFiApplicationID = NULL, Process = NULL, SendOption = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "EdFiLocalEducationAgencySchoolProcess", body = list(DataObject = body), searchFields = append("EdFiLocalEducationAgencySchoolProcessID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiLocalEducationAgencySchoolProcess
	#'
	#' This function modifies an EdFiLocalEducationAgencySchoolProcess
	#' @param fieldNames The field values to give the modified EdFiLocalEducationAgencySchoolProcess. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified EdFiLocalEducationAgencySchoolProcess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiLocalEducationAgencySchoolProcess <- function(EdFiLocalEducationAgencySchoolProcessID, EdFiApplicationID = NULL, Process = NULL, SendOption = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "EdFiLocalEducationAgencySchoolProcess", objectId = EdFiLocalEducationAgencySchoolProcessID, body = list(DataObject = body), searchFields = append("EdFiLocalEducationAgencySchoolProcessID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
