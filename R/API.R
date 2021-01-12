
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
	listRestAPIRequests <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, Nonce = F, RestAPIRequestID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getRestAPIRequest <- function(RestAPIRequestID, CreatedTime = F, ModifiedTime = F, Nonce = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listUserAccesses <- function(searchConditionsList = NULL, APIType = F, AttendanceConfigID = F, AuthenticationTypeOverride = F, CreatedTime = F, Description = F, DisplayText = F, EffectiveAuthenticationTypeCode = F, EffectiveDate = F, ExpirationDate = F, IdentificationConfigID = F, IsActive = F, ModifiedTime = F, OneRosterConfigID = F, TimeTrackingConfigID = F, UserAccessID = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getUserAccess <- function(UserAccessID, APIType = F, AttendanceConfigID = F, AuthenticationTypeOverride = F, CreatedTime = F, Description = F, DisplayText = F, EffectiveAuthenticationTypeCode = F, EffectiveDate = F, ExpirationDate = F, IdentificationConfigID = F, IsActive = F, ModifiedTime = F, OneRosterConfigID = F, TimeTrackingConfigID = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createUserAccess <- function(APIType = NULL, AttendanceConfigID = NULL, AuthenticationTypeOverride = NULL, Description = NULL, EffectiveDate = NULL, ExpirationDate = NULL, IdentificationConfigID = NULL, IsActive = NULL, OneRosterConfigID = NULL, TimeTrackingConfigID = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyUserAccess <- function(UserAccessID, APIType = NULL, AttendanceConfigID = NULL, AuthenticationTypeOverride = NULL, Description = NULL, EffectiveDate = NULL, ExpirationDate = NULL, IdentificationConfigID = NULL, IsActive = NULL, OneRosterConfigID = NULL, TimeTrackingConfigID = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listUserAuthorizations <- function(searchConditionsList = NULL, AccessCode = F, AccessCodeExpirationTime = F, AccessToken = F, AccessTokenExpirationTime = F, CreatedTime = F, ModifiedTime = F, RedirectURI = F, Revoke = F, Scope = F, State = F, UserAuthorizationID = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getUserAuthorization <- function(UserAuthorizationID, AccessCode = F, AccessCodeExpirationTime = F, AccessToken = F, AccessTokenExpirationTime = F, CreatedTime = F, ModifiedTime = F, RedirectURI = F, Revoke = F, Scope = F, State = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createUserAuthorization <- function(AccessCode = NULL, AccessCodeExpirationTime = NULL, AccessToken = NULL, AccessTokenExpirationTime = NULL, RedirectURI = NULL, Revoke = NULL, Scope = NULL, State = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyUserAuthorization <- function(UserAuthorizationID, AccessCode = NULL, AccessCodeExpirationTime = NULL, AccessToken = NULL, AccessTokenExpirationTime = NULL, RedirectURI = NULL, Revoke = NULL, Scope = NULL, State = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listAPIUsageHistories <- function(searchConditionsList = NULL, APIType = F, CreatedTime = F, EntityID = F, FiscalYearID = F, HostAddress = F, HttpMethod = F, HttpStatusCode = F, LogID = F, ModifiedTime = F, ResponseSize = F, SchoolYearID = F, ServerResponseTimeInMilliseconds = F, Url = F, UsageHistoryID = F, UserAccessID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getAPIUsageHistory <- function(APIUsageHistoryID, APIType = F, CreatedTime = F, EntityID = F, FiscalYearID = F, HostAddress = F, HttpMethod = F, HttpStatusCode = F, LogID = F, ModifiedTime = F, ResponseSize = F, SchoolYearID = F, ServerResponseTimeInMilliseconds = F, Url = F, UsageHistoryID = F, UserAccessID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createAPIUsageHistory <- function(APIType = NULL, EntityID = NULL, FiscalYearID = NULL, HostAddress = NULL, HttpMethod = NULL, HttpStatusCode = NULL, LogID = NULL, ResponseSize = NULL, SchoolYearID = NULL, ServerResponseTimeInMilliseconds = NULL, Url = NULL, UserAccessID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyAPIUsageHistory <- function(UsageHistoryID, APIType = NULL, EntityID = NULL, FiscalYearID = NULL, HostAddress = NULL, HttpMethod = NULL, HttpStatusCode = NULL, LogID = NULL, ResponseSize = NULL, SchoolYearID = NULL, ServerResponseTimeInMilliseconds = NULL, Url = NULL, UserAccessID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listAPIUsers <- function(searchConditionsList = NULL, AuthenticationType = F, ConsumerKey = F, ConsumerSecret = F, CreatedTime = F, IsActive = F, ModifiedTime = F, Name = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDSecurity = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getAPIUser <- function(APIUserID, AuthenticationType = F, ConsumerKey = F, ConsumerSecret = F, CreatedTime = F, IsActive = F, ModifiedTime = F, Name = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDSecurity = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createAPIUser <- function(AuthenticationType = NULL, ConsumerKey = NULL, IsActive = NULL, Name = NULL, UserIDSecurity = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyAPIUser <- function(UserID, AuthenticationType = NULL, ConsumerKey = NULL, IsActive = NULL, Name = NULL, UserIDSecurity = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listIdentificationEnrollments <- function(searchConditionsList = NULL, AuthenticationData = F, BiometricData = F, CreatedTime = F, DataType = F, Group = F, IdentificationApplicationID = F, IdentificationEnrollmentID = F, ModifiedTime = F, Type = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getIdentificationEnrollment <- function(IdentificationEnrollmentID, AuthenticationData = F, BiometricData = F, CreatedTime = F, DataType = F, Group = F, IdentificationApplicationID = F, ModifiedTime = F, Type = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createIdentificationEnrollment <- function(AuthenticationData = NULL, BiometricData = NULL, DataType = NULL, Group = NULL, IdentificationApplicationID = NULL, Type = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyIdentificationEnrollment <- function(IdentificationEnrollmentID, AuthenticationData = NULL, BiometricData = NULL, DataType = NULL, Group = NULL, IdentificationApplicationID = NULL, Type = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listIdentificationApplications <- function(searchConditionsList = NULL, CreatedTime = F, Description = F, IdentificationApplicationID = F, ModifiedTime = F, Name = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getIdentificationApplication <- function(IdentificationApplicationID, CreatedTime = F, Description = F, ModifiedTime = F, Name = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createIdentificationApplication <- function(Description = NULL, Name = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyIdentificationApplication <- function(IdentificationApplicationID, Description = NULL, Name = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listIdentificationConfigs <- function(searchConditionsList = NULL, BiometricDataMaxBytes = F, Code = F, CreatedTime = F, Description = F, IdentificationApplicationID = F, IdentificationConfigID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getIdentificationConfig <- function(IdentificationConfigID, BiometricDataMaxBytes = F, Code = F, CreatedTime = F, Description = F, IdentificationApplicationID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createIdentificationConfig <- function(BiometricDataMaxBytes = NULL, Code = NULL, Description = NULL, IdentificationApplicationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyIdentificationConfig <- function(IdentificationConfigID, BiometricDataMaxBytes = NULL, Code = NULL, Description = NULL, IdentificationApplicationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listIdentificationUserParameters <- function(searchConditionsList = NULL, CreatedTime = F, IdentificationApplicationID = F, IdentificationUserParameterID = F, ModifiedTime = F, Name = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Value = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getIdentificationUserParameter <- function(IdentificationUserParameterID, CreatedTime = F, IdentificationApplicationID = F, ModifiedTime = F, Name = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Value = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createIdentificationUserParameter <- function(IdentificationApplicationID = NULL, Name = NULL, UserID = NULL, Value = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyIdentificationUserParameter <- function(IdentificationUserParameterID, IdentificationApplicationID = NULL, Name = NULL, UserID = NULL, Value = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listTimeTrackingConfigs <- function(searchConditionsList = NULL, BuildingID = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictID = F, IPAddress = F, ModifiedTime = F, TimeTrackingConfigID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getTimeTrackingConfig <- function(TimeTrackingConfigID, BuildingID = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictID = F, IPAddress = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createTimeTrackingConfig <- function(BuildingID = NULL, Code = NULL, Description = NULL, DistrictID = NULL, IPAddress = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyTimeTrackingConfig <- function(TimeTrackingConfigID, BuildingID = NULL, Code = NULL, Description = NULL, DistrictID = NULL, IPAddress = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listOneRosterConfigs <- function(searchConditionsList = NULL, AllowGradePassBack = F, Code = F, CodeDescription = F, CreatedTime = F, DefaultCategoryCode = F, Description = F, DistrictID = F, ModifiedTime = F, OneRosterConfigID = F, OneRosterVendorID = F, UserAccessCount = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getOneRosterConfig <- function(OneRosterConfigID, AllowGradePassBack = F, Code = F, CodeDescription = F, CreatedTime = F, DefaultCategoryCode = F, Description = F, DistrictID = F, ModifiedTime = F, OneRosterVendorID = F, UserAccessCount = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createOneRosterConfig <- function(AllowGradePassBack = NULL, Code = NULL, DefaultCategoryCode = NULL, Description = NULL, DistrictID = NULL, OneRosterVendorID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyOneRosterConfig <- function(OneRosterConfigID, AllowGradePassBack = NULL, Code = NULL, DefaultCategoryCode = NULL, Description = NULL, DistrictID = NULL, OneRosterVendorID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listOneRosterVendors <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, Name = F, OneRosterVendorID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getOneRosterVendor <- function(OneRosterVendorID, CreatedTime = F, ModifiedTime = F, Name = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listIdentificationApplicationAdmins <- function(searchConditionsList = NULL, CreatedTime = F, IdentificationApplicationAdminID = F, IdentificationApplicationID = F, ModifiedTime = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getIdentificationApplicationAdmin <- function(IdentificationApplicationAdminID, CreatedTime = F, IdentificationApplicationID = F, ModifiedTime = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createIdentificationApplicationAdmin <- function(IdentificationApplicationID = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyIdentificationApplicationAdmin <- function(IdentificationApplicationAdminID, IdentificationApplicationID = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listEdFiApplications <- function(searchConditionsList = NULL, AddressIDESC = F, AddressIDLEA = F, CreatedTime = F, DistrictID = F, EarnedCreditsMethodIDOverride = F, EdFiApplicationID = F, EducationServiceCenterId = F, FaxNumberESC = F, FaxNumberLEA = F, GPABucketID = F, GPAMethodID = F, GradeSendOption = F, HasRunningMassSend = F, IsEnabled = F, Key = F, LiveURI = F, LocalEducationAgencyId = F, LogDurationTimes = F, ModifiedTime = F, Name = F, NameOfInstitutionESC = F, NameOfInstitutionLEA = F, Namespace = F, NumericYear = F, PhoneNumberESC = F, PhoneNumberLEA = F, ProcessEvents = F, ReportingType = F, RetainMessageQueueRecordsFor = F, SandboxURI = F, Secret = F, StateOrganizationIdESC = F, StateOrganizationIdLEA = F, UseDistrictData = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getEdFiApplication <- function(EdFiApplicationID, AddressIDESC = F, AddressIDLEA = F, CreatedTime = F, DistrictID = F, EarnedCreditsMethodIDOverride = F, EducationServiceCenterId = F, FaxNumberESC = F, FaxNumberLEA = F, GPABucketID = F, GPAMethodID = F, GradeSendOption = F, HasRunningMassSend = F, IsEnabled = F, Key = F, LiveURI = F, LocalEducationAgencyId = F, LogDurationTimes = F, ModifiedTime = F, Name = F, NameOfInstitutionESC = F, NameOfInstitutionLEA = F, Namespace = F, NumericYear = F, PhoneNumberESC = F, PhoneNumberLEA = F, ProcessEvents = F, ReportingType = F, RetainMessageQueueRecordsFor = F, SandboxURI = F, Secret = F, StateOrganizationIdESC = F, StateOrganizationIdLEA = F, UseDistrictData = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createEdFiApplication <- function(AddressIDESC = NULL, AddressIDLEA = NULL, DistrictID = NULL, EarnedCreditsMethodIDOverride = NULL, EducationServiceCenterId = NULL, FaxNumberESC = NULL, FaxNumberLEA = NULL, GPABucketID = NULL, GPAMethodID = NULL, GradeSendOption = NULL, IsEnabled = NULL, Key = NULL, LiveURI = NULL, LocalEducationAgencyId = NULL, LogDurationTimes = NULL, Name = NULL, NameOfInstitutionESC = NULL, NameOfInstitutionLEA = NULL, Namespace = NULL, NumericYear = NULL, PhoneNumberESC = NULL, PhoneNumberLEA = NULL, ProcessEvents = NULL, ReportingType = NULL, RetainMessageQueueRecordsFor = NULL, SandboxURI = NULL, StateOrganizationIdESC = NULL, StateOrganizationIdLEA = NULL, UseDistrictData = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyEdFiApplication <- function(EdFiApplicationID, AddressIDESC = NULL, AddressIDLEA = NULL, DistrictID = NULL, EarnedCreditsMethodIDOverride = NULL, EducationServiceCenterId = NULL, FaxNumberESC = NULL, FaxNumberLEA = NULL, GPABucketID = NULL, GPAMethodID = NULL, GradeSendOption = NULL, IsEnabled = NULL, Key = NULL, LiveURI = NULL, LocalEducationAgencyId = NULL, LogDurationTimes = NULL, Name = NULL, NameOfInstitutionESC = NULL, NameOfInstitutionLEA = NULL, Namespace = NULL, NumericYear = NULL, PhoneNumberESC = NULL, PhoneNumberLEA = NULL, ProcessEvents = NULL, ReportingType = NULL, RetainMessageQueueRecordsFor = NULL, SandboxURI = NULL, StateOrganizationIdESC = NULL, StateOrganizationIdLEA = NULL, UseDistrictData = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "EdFiApplication", objectId = EdFiApplicationID, body = list(DataObject = body), searchFields = append("EdFiApplicationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
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
	listEdFiLogs <- function(searchConditionsList = NULL, CourseID = F, CreatedTime = F, DBSStatusCode = F, EdFiApplicationID = F, EdFiLogID = F, EdFiRecordID = F, EdFiRecordTypeName = F, EntityID = F, ErroredPayload = F, ErrorMessage = F, IncidentID = F, IsDeleted = F, ModifiedTime = F, ModuleID = F, NameIDStaff = F, ObjectID = F, Payload = F, SectionID = F, SourceKey = F, Status = F, StatusDescription = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getEdFiLog <- function(EdFiLogID, CourseID = F, CreatedTime = F, DBSStatusCode = F, EdFiApplicationID = F, EdFiRecordID = F, EdFiRecordTypeName = F, EntityID = F, ErroredPayload = F, ErrorMessage = F, IncidentID = F, IsDeleted = F, ModifiedTime = F, ModuleID = F, NameIDStaff = F, ObjectID = F, Payload = F, SectionID = F, SourceKey = F, Status = F, StatusDescription = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createEdFiLog <- function(CourseID = NULL, EdFiApplicationID = NULL, EdFiRecordID = NULL, EdFiRecordTypeName = NULL, EntityID = NULL, ErrorMessage = NULL, IncidentID = NULL, IsDeleted = NULL, ModuleID = NULL, NameIDStaff = NULL, ObjectID = NULL, Payload = NULL, SectionID = NULL, SourceKey = NULL, Status = NULL, StatusDescription = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyEdFiLog <- function(EdFiLogID, CourseID = NULL, EdFiApplicationID = NULL, EdFiRecordID = NULL, EdFiRecordTypeName = NULL, EntityID = NULL, ErrorMessage = NULL, IncidentID = NULL, IsDeleted = NULL, ModuleID = NULL, NameIDStaff = NULL, ObjectID = NULL, Payload = NULL, SectionID = NULL, SourceKey = NULL, Status = NULL, StatusDescription = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listTableWatchApplications <- function(searchConditionsList = NULL, AllowsMultipleInstances = F, CreatedTime = F, IsEnabled = F, ModifiedTime = F, ProcessEvents = F, Schema = F, Table = F, TableWatchApplicationID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getTableWatchApplication <- function(TableWatchApplicationID, AllowsMultipleInstances = F, CreatedTime = F, IsEnabled = F, ModifiedTime = F, ProcessEvents = F, Schema = F, Table = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listAttendanceConfigs <- function(searchConditionsList = NULL, AttendanceConfigID = F, CreatedTime = F, Description = F, Entities = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getAttendanceConfig <- function(AttendanceConfigID, CreatedTime = F, Description = F, Entities = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listAttendanceConfigEntities <- function(searchConditionsList = NULL, AttendanceConfigEntityID = F, AttendanceConfigID = F, CreatedTime = F, EntityID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getAttendanceConfigEntity <- function(AttendanceConfigEntityID, AttendanceConfigID = F, CreatedTime = F, EntityID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listEdFiMassSendRunHistories <- function(searchConditionsList = NULL, CanAccessMedia = F, CreatedTime = F, EdFiApplicationID = F, EdFiMassSendRunHistoryID = F, EndDateTime = F, HasValidMedia = F, IsExport = F, IsLocked = F, MediaID = F, ModifiedTime = F, RunData = F, RunParameters = F, ScopeAccessAllowed = F, StartDateTime = F, Status = F, Type = F, UserIDCanceledBy = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getEdFiMassSendRunHistory <- function(EdFiMassSendRunHistoryID, CanAccessMedia = F, CreatedTime = F, EdFiApplicationID = F, EndDateTime = F, HasValidMedia = F, IsExport = F, IsLocked = F, MediaID = F, ModifiedTime = F, RunData = F, RunParameters = F, ScopeAccessAllowed = F, StartDateTime = F, Status = F, Type = F, UserIDCanceledBy = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createEdFiMassSendRunHistory <- function(EdFiApplicationID = NULL, EndDateTime = NULL, MediaID = NULL, StartDateTime = NULL, Status = NULL, Type = NULL, UserIDCanceledBy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyEdFiMassSendRunHistory <- function(EdFiMassSendRunHistoryID, EdFiApplicationID = NULL, EndDateTime = NULL, MediaID = NULL, StartDateTime = NULL, Status = NULL, Type = NULL, UserIDCanceledBy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listTempEdFiAPISpecialEducationEnrollments <- function(searchConditionsList = NULL, CreatedTime = F, DisabilityCodes = F, DistrictID = F, EndDate = F, EnrollmentWIID = F, FirstName = F, LastEvaluationDate = F, LastName = F, LatestServiceStartDate = F, MiddleName = F, ModifiedTime = F, SchoolYearID = F, StartDate = F, StudentID = F, TempEdFiAPISpecialEducationEnrollmentID = F, UpdateType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WISEIDNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getTempEdFiAPISpecialEducationEnrollment <- function(TempEdFiAPISpecialEducationEnrollmentID, CreatedTime = F, DisabilityCodes = F, DistrictID = F, EndDate = F, EnrollmentWIID = F, FirstName = F, LastEvaluationDate = F, LastName = F, LatestServiceStartDate = F, MiddleName = F, ModifiedTime = F, SchoolYearID = F, StartDate = F, StudentID = F, UpdateType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WISEIDNumber = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createTempEdFiAPISpecialEducationEnrollment <- function(DisabilityCodes = NULL, EndDate = NULL, FirstName = NULL, LastEvaluationDate = NULL, LastName = NULL, LatestServiceStartDate = NULL, MiddleName = NULL, StartDate = NULL, UpdateType = NULL, WISEIDNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyTempEdFiAPISpecialEducationEnrollment <- function(TempEdFiAPISpecialEducationEnrollmentID, DisabilityCodes = NULL, EndDate = NULL, FirstName = NULL, LastEvaluationDate = NULL, LastName = NULL, LatestServiceStartDate = NULL, MiddleName = NULL, StartDate = NULL, UpdateType = NULL, WISEIDNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listTempEdFiAPISpecialEducationIEPServices <- function(searchConditionsList = NULL, CreatedTime = F, EndDate = F, FapeResponsibleSchool = F, IEPServiceWIID = F, ModifiedTime = F, ReasonExitedDescriptor = F, SpecialEducationSettingDescriptor = F, StartDate = F, TempEdFiAPISpecialEducationEnrollmentID = F, TempEdFiAPISpecialEducationIEPServiceID = F, UpdateType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getTempEdFiAPISpecialEducationIEPService <- function(TempEdFiAPISpecialEducationIEPServiceID, CreatedTime = F, EndDate = F, FapeResponsibleSchool = F, IEPServiceWIID = F, ModifiedTime = F, ReasonExitedDescriptor = F, SpecialEducationSettingDescriptor = F, StartDate = F, TempEdFiAPISpecialEducationEnrollmentID = F, UpdateType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createTempEdFiAPISpecialEducationIEPService <- function(EndDate = NULL, FapeResponsibleSchool = NULL, ReasonExitedDescriptor = NULL, SpecialEducationSettingDescriptor = NULL, StartDate = NULL, UpdateType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyTempEdFiAPISpecialEducationIEPService <- function(TempEdFiAPISpecialEducationIEPServiceID, EndDate = NULL, FapeResponsibleSchool = NULL, ReasonExitedDescriptor = NULL, SpecialEducationSettingDescriptor = NULL, StartDate = NULL, UpdateType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listTempEdFiLoadSpecialEducationFromAPIErrors <- function(searchConditionsList = NULL, CreatedTime = F, FailureReason = F, FirstName = F, LastName = F, MiddleName = F, ModifiedTime = F, StudentID = F, TempEdFiLoadSpecialEducationFromAPIErrorID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getTempEdFiLoadSpecialEducationFromAPIError <- function(TempEdFiLoadSpecialEducationFromAPIErrorID, CreatedTime = F, FailureReason = F, FirstName = F, LastName = F, MiddleName = F, ModifiedTime = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	createTempEdFiLoadSpecialEducationFromAPIError <- function(FailureReason = NULL, FirstName = NULL, LastName = NULL, MiddleName = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	modifyTempEdFiLoadSpecialEducationFromAPIError <- function(TempEdFiLoadSpecialEducationFromAPIErrorID, FailureReason = NULL, FirstName = NULL, LastName = NULL, MiddleName = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listETranscriptApplications <- function(searchConditionsList = NULL, CreatedTime = F, DistrictID = F, ETranscriptApplicationID = F, Key = F, ModifiedTime = F, Secret = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getETranscriptApplication <- function(ETranscriptApplicationID, CreatedTime = F, DistrictID = F, Key = F, ModifiedTime = F, Secret = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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
	listEdFiLocalEducationAgencySchoolProcesses <- function(searchConditionsList = NULL, CreatedTime = F, EdFiApplicationID = F, EdFiLocalEducationAgencySchoolProcessID = F, ModifiedTime = F, Process = F, SendOption = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

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
	getEdFiLocalEducationAgencySchoolProcess <- function(EdFiLocalEducationAgencySchoolProcessID, CreatedTime = F, EdFiApplicationID = F, ModifiedTime = F, Process = F, SendOption = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

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

	#' List EdFiProcessedMessages
	#'
	#' This function returns a dataframe or json object of EdFiProcessedMessages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiProcessedMessages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiProcessedMessages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiProcessedMessage') to get more field paths.
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
	#' @return A list of EdFiProcessedMessages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiProcessedMessages <- function(searchConditionsList = NULL, CreatedTime = F, EdFiApplicationID = F, EdFiLogID = F, EdFiProcessedMessageID = F, EventSourceID = F, EventType = F, ModifiedTime = F, ObjectID = F, QueuedTime = F, RequestHTTPMethodType = F, RequestSentTime = F, ResponseHTTPStatus = F, ResponsePayload = F, ResponseReceivedTime = F, ResponseTime = F, SentPayload = F, SentPayloadDisplay = F, Status = F, URLUsed = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "EdFiProcessedMessage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiProcessedMessage
	#'
	#' This function returns a dataframe or json object of an EdFiProcessedMessage
	#' @param EdFiProcessedMessageID The ID of the EdFiProcessedMessage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiProcessedMessage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiProcessedMessage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiProcessedMessage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of EdFiProcessedMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiProcessedMessage <- function(EdFiProcessedMessageID, CreatedTime = F, EdFiApplicationID = F, EdFiLogID = F, EventSourceID = F, EventType = F, ModifiedTime = F, ObjectID = F, QueuedTime = F, RequestHTTPMethodType = F, RequestSentTime = F, ResponseHTTPStatus = F, ResponsePayload = F, ResponseReceivedTime = F, ResponseTime = F, SentPayload = F, SentPayloadDisplay = F, Status = F, URLUsed = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiProcessedMessageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "EdFiProcessedMessage", objectId = EdFiProcessedMessageID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiProcessedMessage
	#'
	#' This function deletes an EdFiProcessedMessage
	#' @param EdFiProcessedMessageID The ID of the EdFiProcessedMessage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The EdFiProcessedMessageID of the deleted EdFiProcessedMessage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiProcessedMessage <- function(EdFiProcessedMessageID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "EdFiProcessedMessage", objectId = EdFiProcessedMessageID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiProcessedMessage
	#'
	#' This function creates an EdFiProcessedMessage
	#' @param fieldNames The field values to give the created EdFiProcessedMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created EdFiProcessedMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiProcessedMessage <- function(EdFiApplicationID = NULL, EdFiLogID = NULL, EventSourceID = NULL, EventType = NULL, ObjectID = NULL, QueuedTime = NULL, RequestHTTPMethodType = NULL, RequestSentTime = NULL, ResponseHTTPStatus = NULL, ResponsePayload = NULL, ResponseReceivedTime = NULL, Status = NULL, URLUsed = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "EdFiProcessedMessage", body = list(DataObject = body), searchFields = append("EdFiProcessedMessageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiProcessedMessage
	#'
	#' This function modifies an EdFiProcessedMessage
	#' @param fieldNames The field values to give the modified EdFiProcessedMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified EdFiProcessedMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiProcessedMessage <- function(EdFiProcessedMessageID, EdFiApplicationID = NULL, EdFiLogID = NULL, EventSourceID = NULL, EventType = NULL, ObjectID = NULL, QueuedTime = NULL, RequestHTTPMethodType = NULL, RequestSentTime = NULL, ResponseHTTPStatus = NULL, ResponsePayload = NULL, ResponseReceivedTime = NULL, Status = NULL, URLUsed = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "EdFiProcessedMessage", objectId = EdFiProcessedMessageID, body = list(DataObject = body), searchFields = append("EdFiProcessedMessageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiQueuedMessages
	#'
	#' This function returns a dataframe or json object of EdFiQueuedMessages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiQueuedMessages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiQueuedMessages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiQueuedMessage') to get more field paths.
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
	#' @return A list of EdFiQueuedMessages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiQueuedMessages <- function(searchConditionsList = NULL, CreatedTime = F, DisablePooling = F, EdFiApplicationID = F, EdFiLogID = F, EdFiQueuedMessageID = F, EventSourceID = F, EventType = F, ModifiedTime = F, ObjectID = F, Payload = F, PayloadDisplay = F, RequestHTTPMethodType = F, Status = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "API", objectName = "EdFiQueuedMessage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiQueuedMessage
	#'
	#' This function returns a dataframe or json object of an EdFiQueuedMessage
	#' @param EdFiQueuedMessageID The ID of the EdFiQueuedMessage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiQueuedMessage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiQueuedMessage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiQueuedMessage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A dataframe or of EdFiQueuedMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiQueuedMessage <- function(EdFiQueuedMessageID, CreatedTime = F, DisablePooling = F, EdFiApplicationID = F, EdFiLogID = F, EventSourceID = F, EventType = F, ModifiedTime = F, ObjectID = F, Payload = F, PayloadDisplay = F, RequestHTTPMethodType = F, Status = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiQueuedMessageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "API", objectName = "EdFiQueuedMessage", objectId = EdFiQueuedMessageID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiQueuedMessage
	#'
	#' This function deletes an EdFiQueuedMessage
	#' @param EdFiQueuedMessageID The ID of the EdFiQueuedMessage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The EdFiQueuedMessageID of the deleted EdFiQueuedMessage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiQueuedMessage <- function(EdFiQueuedMessageID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "API", objectName = "EdFiQueuedMessage", objectId = EdFiQueuedMessageID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiQueuedMessage
	#'
	#' This function creates an EdFiQueuedMessage
	#' @param fieldNames The field values to give the created EdFiQueuedMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return A newly created EdFiQueuedMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiQueuedMessage <- function(DisablePooling = NULL, EdFiApplicationID = NULL, EdFiLogID = NULL, EventSourceID = NULL, EventType = NULL, ObjectID = NULL, RequestHTTPMethodType = NULL, Status = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "API", objectName = "EdFiQueuedMessage", body = list(DataObject = body), searchFields = append("EdFiQueuedMessageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiQueuedMessage
	#'
	#' This function modifies an EdFiQueuedMessage
	#' @param fieldNames The field values to give the modified EdFiQueuedMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept API
	#' @return The modified EdFiQueuedMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiQueuedMessage <- function(EdFiQueuedMessageID, DisablePooling = NULL, EdFiApplicationID = NULL, EdFiLogID = NULL, EventSourceID = NULL, EventType = NULL, ObjectID = NULL, RequestHTTPMethodType = NULL, Status = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "API", objectName = "EdFiQueuedMessage", objectId = EdFiQueuedMessageID, body = list(DataObject = body), searchFields = append("EdFiQueuedMessageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
