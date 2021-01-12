
	#' List UserCalendarPreferences
	#'
	#' This function returns a dataframe or json object of UserCalendarPreferences
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserCalendarPreferences. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserCalendarPreferences.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserCalendarPreference') to get more field paths.
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
	#' @concept Security
	#' @return A list of UserCalendarPreferences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserCalendarPreferences <- function(searchConditionsList = NULL, ApprovedTimeOffEventBackgroundColor = F, BirthdayEventBackgroundColor = F, CalendarCommentsBackgroundColor = F, CalendarEventBackgroundColor = F, CalendarType = F, CreatedTime = F, DistrictActivityEventBackgroundColor = F, ModifiedTime = F, PayDayEventBackgroundColor = F, SelectedView = F, ShowAllMyEmployeesTimeOff = F, ShowAllocatedTimeOff = F, ShowBirthdays = F, ShowCalendarComments = F, ShowCalendarEvents = F, ShowDistrictActivityEvents = F, ShowMyDirectEmployeesTimeOff = F, ShowMyTimeOff = F, ShowTransactionsIHadASubFor = F, ShowTransactionsISubbedFor = F, ShowWeekends = F, TransactionsIHadASubForEventBackgroundColor = F, TransactionsISubbedForEventBackgroundColor = F, UnapprovedTimeOffEventBackgroundColor = F, UserCalendarPreferenceID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDOwner = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "UserCalendarPreference", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserCalendarPreference
	#'
	#' This function returns a dataframe or json object of an UserCalendarPreference
	#' @param UserCalendarPreferenceID The ID of the UserCalendarPreference to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserCalendarPreference. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserCalendarPreference.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserCalendarPreference') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of UserCalendarPreference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserCalendarPreference <- function(UserCalendarPreferenceID, ApprovedTimeOffEventBackgroundColor = F, BirthdayEventBackgroundColor = F, CalendarCommentsBackgroundColor = F, CalendarEventBackgroundColor = F, CalendarType = F, CreatedTime = F, DistrictActivityEventBackgroundColor = F, ModifiedTime = F, PayDayEventBackgroundColor = F, SelectedView = F, ShowAllMyEmployeesTimeOff = F, ShowAllocatedTimeOff = F, ShowBirthdays = F, ShowCalendarComments = F, ShowCalendarEvents = F, ShowDistrictActivityEvents = F, ShowMyDirectEmployeesTimeOff = F, ShowMyTimeOff = F, ShowTransactionsIHadASubFor = F, ShowTransactionsISubbedFor = F, ShowWeekends = F, TransactionsIHadASubForEventBackgroundColor = F, TransactionsISubbedForEventBackgroundColor = F, UnapprovedTimeOffEventBackgroundColor = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDOwner = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserCalendarPreferenceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "UserCalendarPreference", objectId = UserCalendarPreferenceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserCalendarPreference
	#'
	#' This function deletes an UserCalendarPreference
	#' @param UserCalendarPreferenceID The ID of the UserCalendarPreference to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The UserCalendarPreferenceID of the deleted UserCalendarPreference.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserCalendarPreference <- function(UserCalendarPreferenceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "UserCalendarPreference", objectId = UserCalendarPreferenceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserCalendarPreference
	#'
	#' This function creates an UserCalendarPreference
	#' @param fieldNames The field values to give the created UserCalendarPreference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created UserCalendarPreference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserCalendarPreference <- function(ApprovedTimeOffEventBackgroundColor = NULL, BirthdayEventBackgroundColor = NULL, CalendarCommentsBackgroundColor = NULL, CalendarEventBackgroundColor = NULL, CalendarType = NULL, DistrictActivityEventBackgroundColor = NULL, PayDayEventBackgroundColor = NULL, SelectedView = NULL, ShowAllMyEmployeesTimeOff = NULL, ShowAllocatedTimeOff = NULL, ShowBirthdays = NULL, ShowCalendarComments = NULL, ShowCalendarEvents = NULL, ShowDistrictActivityEvents = NULL, ShowMyDirectEmployeesTimeOff = NULL, ShowMyTimeOff = NULL, ShowTransactionsIHadASubFor = NULL, ShowTransactionsISubbedFor = NULL, ShowWeekends = NULL, TransactionsIHadASubForEventBackgroundColor = NULL, TransactionsISubbedForEventBackgroundColor = NULL, UnapprovedTimeOffEventBackgroundColor = NULL, UserIDOwner = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "UserCalendarPreference", body = list(DataObject = body), searchFields = append("UserCalendarPreferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserCalendarPreference
	#'
	#' This function modifies an UserCalendarPreference
	#' @param fieldNames The field values to give the modified UserCalendarPreference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified UserCalendarPreference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserCalendarPreference <- function(UserCalendarPreferenceID, ApprovedTimeOffEventBackgroundColor = NULL, BirthdayEventBackgroundColor = NULL, CalendarCommentsBackgroundColor = NULL, CalendarEventBackgroundColor = NULL, CalendarType = NULL, DistrictActivityEventBackgroundColor = NULL, PayDayEventBackgroundColor = NULL, SelectedView = NULL, ShowAllMyEmployeesTimeOff = NULL, ShowAllocatedTimeOff = NULL, ShowBirthdays = NULL, ShowCalendarComments = NULL, ShowCalendarEvents = NULL, ShowDistrictActivityEvents = NULL, ShowMyDirectEmployeesTimeOff = NULL, ShowMyTimeOff = NULL, ShowTransactionsIHadASubFor = NULL, ShowTransactionsISubbedFor = NULL, ShowWeekends = NULL, TransactionsIHadASubForEventBackgroundColor = NULL, TransactionsISubbedForEventBackgroundColor = NULL, UnapprovedTimeOffEventBackgroundColor = NULL, UserIDOwner = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "UserCalendarPreference", objectId = UserCalendarPreferenceID, body = list(DataObject = body), searchFields = append("UserCalendarPreferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDeletedPortalAccessSecurityUsers
	#'
	#' This function returns a dataframe or json object of TempDeletedPortalAccessSecurityUsers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDeletedPortalAccessSecurityUsers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDeletedPortalAccessSecurityUsers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDeletedPortalAccessSecurityUser') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempDeletedPortalAccessSecurityUsers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDeletedPortalAccessSecurityUsers <- function(searchConditionsList = NULL, CreatedTime = F, FullNameLFM = F, ModifiedTime = F, TempDeletedPortalAccessSecurityUserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempDeletedPortalAccessSecurityUser", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDeletedPortalAccessSecurityUser
	#'
	#' This function returns a dataframe or json object of a TempDeletedPortalAccessSecurityUser
	#' @param TempDeletedPortalAccessSecurityUserID The ID of the TempDeletedPortalAccessSecurityUser to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDeletedPortalAccessSecurityUser. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDeletedPortalAccessSecurityUser.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDeletedPortalAccessSecurityUser') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempDeletedPortalAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDeletedPortalAccessSecurityUser <- function(TempDeletedPortalAccessSecurityUserID, CreatedTime = F, FullNameLFM = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserName = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDeletedPortalAccessSecurityUserID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempDeletedPortalAccessSecurityUser", objectId = TempDeletedPortalAccessSecurityUserID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDeletedPortalAccessSecurityUser
	#'
	#' This function deletes a TempDeletedPortalAccessSecurityUser
	#' @param TempDeletedPortalAccessSecurityUserID The ID of the TempDeletedPortalAccessSecurityUser to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempDeletedPortalAccessSecurityUserID of the deleted TempDeletedPortalAccessSecurityUser.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDeletedPortalAccessSecurityUser <- function(TempDeletedPortalAccessSecurityUserID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempDeletedPortalAccessSecurityUser", objectId = TempDeletedPortalAccessSecurityUserID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDeletedPortalAccessSecurityUser
	#'
	#' This function creates a TempDeletedPortalAccessSecurityUser
	#' @param fieldNames The field values to give the created TempDeletedPortalAccessSecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempDeletedPortalAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDeletedPortalAccessSecurityUser <- function(FullNameLFM = NULL, UserName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempDeletedPortalAccessSecurityUser", body = list(DataObject = body), searchFields = append("TempDeletedPortalAccessSecurityUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDeletedPortalAccessSecurityUser
	#'
	#' This function modifies a TempDeletedPortalAccessSecurityUser
	#' @param fieldNames The field values to give the modified TempDeletedPortalAccessSecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempDeletedPortalAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDeletedPortalAccessSecurityUser <- function(TempDeletedPortalAccessSecurityUserID, FullNameLFM = NULL, UserName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempDeletedPortalAccessSecurityUser", objectId = TempDeletedPortalAccessSecurityUserID, body = list(DataObject = body), searchFields = append("TempDeletedPortalAccessSecurityUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedPortalAccessSecurityUsers
	#'
	#' This function returns a dataframe or json object of TempFailedPortalAccessSecurityUsers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFailedPortalAccessSecurityUsers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFailedPortalAccessSecurityUsers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFailedPortalAccessSecurityUser') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempFailedPortalAccessSecurityUsers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedPortalAccessSecurityUsers <- function(searchConditionsList = NULL, CreatedTime = F, FullNameLFM = F, ModifiedTime = F, Note = F, TempFailedPortalAccessSecurityUserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempFailedPortalAccessSecurityUser", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempFailedPortalAccessSecurityUser
	#'
	#' This function returns a dataframe or json object of a TempFailedPortalAccessSecurityUser
	#' @param TempFailedPortalAccessSecurityUserID The ID of the TempFailedPortalAccessSecurityUser to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFailedPortalAccessSecurityUser. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFailedPortalAccessSecurityUser.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFailedPortalAccessSecurityUser') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempFailedPortalAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedPortalAccessSecurityUser <- function(TempFailedPortalAccessSecurityUserID, CreatedTime = F, FullNameLFM = F, ModifiedTime = F, Note = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserName = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedPortalAccessSecurityUserID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempFailedPortalAccessSecurityUser", objectId = TempFailedPortalAccessSecurityUserID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempFailedPortalAccessSecurityUser
	#'
	#' This function deletes a TempFailedPortalAccessSecurityUser
	#' @param TempFailedPortalAccessSecurityUserID The ID of the TempFailedPortalAccessSecurityUser to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempFailedPortalAccessSecurityUserID of the deleted TempFailedPortalAccessSecurityUser.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempFailedPortalAccessSecurityUser <- function(TempFailedPortalAccessSecurityUserID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempFailedPortalAccessSecurityUser", objectId = TempFailedPortalAccessSecurityUserID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempFailedPortalAccessSecurityUser
	#'
	#' This function creates a TempFailedPortalAccessSecurityUser
	#' @param fieldNames The field values to give the created TempFailedPortalAccessSecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempFailedPortalAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempFailedPortalAccessSecurityUser <- function(FullNameLFM = NULL, Note = NULL, UserName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempFailedPortalAccessSecurityUser", body = list(DataObject = body), searchFields = append("TempFailedPortalAccessSecurityUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempFailedPortalAccessSecurityUser
	#'
	#' This function modifies a TempFailedPortalAccessSecurityUser
	#' @param fieldNames The field values to give the modified TempFailedPortalAccessSecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempFailedPortalAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempFailedPortalAccessSecurityUser <- function(TempFailedPortalAccessSecurityUserID, FullNameLFM = NULL, Note = NULL, UserName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempFailedPortalAccessSecurityUser", objectId = TempFailedPortalAccessSecurityUserID, body = list(DataObject = body), searchFields = append("TempFailedPortalAccessSecurityUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IPRanges
	#'
	#' This function returns a dataframe or json object of IPRanges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IPRanges. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IPRanges.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IPRange') to get more field paths.
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
	#' @concept Security
	#' @return A list of IPRanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIPRanges <- function(searchConditionsList = NULL, CreatedTime = F, Description = F, High = F, IPRangeID = F, Low = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "IPRange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IPRange
	#'
	#' This function returns a dataframe or json object of an IPRange
	#' @param IPRangeID The ID of the IPRange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IPRange. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IPRange.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IPRange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of IPRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIPRange <- function(IPRangeID, CreatedTime = F, Description = F, High = F, Low = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IPRangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "IPRange", objectId = IPRangeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IPRange
	#'
	#' This function deletes an IPRange
	#' @param IPRangeID The ID of the IPRange to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The IPRangeID of the deleted IPRange.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIPRange <- function(IPRangeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "IPRange", objectId = IPRangeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IPRange
	#'
	#' This function creates an IPRange
	#' @param fieldNames The field values to give the created IPRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created IPRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIPRange <- function(Description = NULL, High = NULL, Low = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "IPRange", body = list(DataObject = body), searchFields = append("IPRangeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IPRange
	#'
	#' This function modifies an IPRange
	#' @param fieldNames The field values to give the modified IPRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified IPRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIPRange <- function(IPRangeID, Description = NULL, High = NULL, Low = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "IPRange", objectId = IPRangeID, body = list(DataObject = body), searchFields = append("IPRangeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RoleIPRanges
	#'
	#' This function returns a dataframe or json object of RoleIPRanges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoleIPRanges. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoleIPRanges.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoleIPRange') to get more field paths.
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
	#' @concept Security
	#' @return A list of RoleIPRanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoleIPRanges <- function(searchConditionsList = NULL, CreatedTime = F, IPRangeID = F, ModifiedTime = F, RoleID = F, RoleIPRangeID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "RoleIPRange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RoleIPRange
	#'
	#' This function returns a dataframe or json object of a RoleIPRange
	#' @param RoleIPRangeID The ID of the RoleIPRange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoleIPRange. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoleIPRange.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoleIPRange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of RoleIPRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoleIPRange <- function(RoleIPRangeID, CreatedTime = F, IPRangeID = F, ModifiedTime = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoleIPRangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "RoleIPRange", objectId = RoleIPRangeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RoleIPRange
	#'
	#' This function deletes a RoleIPRange
	#' @param RoleIPRangeID The ID of the RoleIPRange to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The RoleIPRangeID of the deleted RoleIPRange.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoleIPRange <- function(RoleIPRangeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "RoleIPRange", objectId = RoleIPRangeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RoleIPRange
	#'
	#' This function creates a RoleIPRange
	#' @param fieldNames The field values to give the created RoleIPRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created RoleIPRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoleIPRange <- function(IPRangeID = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "RoleIPRange", body = list(DataObject = body), searchFields = append("RoleIPRangeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RoleIPRange
	#'
	#' This function modifies a RoleIPRange
	#' @param fieldNames The field values to give the modified RoleIPRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified RoleIPRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoleIPRange <- function(RoleIPRangeID, IPRangeID = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "RoleIPRange", objectId = RoleIPRangeID, body = list(DataObject = body), searchFields = append("RoleIPRangeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ElectronicSignatures
	#'
	#' This function returns a dataframe or json object of ElectronicSignatures
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ElectronicSignatures. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ElectronicSignatures.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ElectronicSignature') to get more field paths.
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
	#' @concept Security
	#' @return A list of ElectronicSignatures
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listElectronicSignatures <- function(searchConditionsList = NULL, Code = F, CreatedTime = F, Description = F, DistrictID = F, ElectronicSignatureID = F, EntityID = F, EntityName = F, IsForGrading = F, IsForPurchasing = F, IsForStateReporting = F, MediaID = F, ModifiedTime = F, SignatureLocationKey = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "ElectronicSignature", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ElectronicSignature
	#'
	#' This function returns a dataframe or json object of an ElectronicSignature
	#' @param ElectronicSignatureID The ID of the ElectronicSignature to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ElectronicSignature. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ElectronicSignature.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ElectronicSignature') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of ElectronicSignature
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getElectronicSignature <- function(ElectronicSignatureID, Code = F, CreatedTime = F, Description = F, DistrictID = F, EntityID = F, EntityName = F, IsForGrading = F, IsForPurchasing = F, IsForStateReporting = F, MediaID = F, ModifiedTime = F, SignatureLocationKey = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ElectronicSignatureID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "ElectronicSignature", objectId = ElectronicSignatureID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ElectronicSignature
	#'
	#' This function deletes an ElectronicSignature
	#' @param ElectronicSignatureID The ID of the ElectronicSignature to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The ElectronicSignatureID of the deleted ElectronicSignature.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteElectronicSignature <- function(ElectronicSignatureID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "ElectronicSignature", objectId = ElectronicSignatureID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ElectronicSignature
	#'
	#' This function creates an ElectronicSignature
	#' @param fieldNames The field values to give the created ElectronicSignature. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created ElectronicSignature
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createElectronicSignature <- function(Code = NULL, Description = NULL, DistrictID = NULL, EntityID = NULL, IsForGrading = NULL, IsForPurchasing = NULL, IsForStateReporting = NULL, MediaID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "ElectronicSignature", body = list(DataObject = body), searchFields = append("ElectronicSignatureID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ElectronicSignature
	#'
	#' This function modifies an ElectronicSignature
	#' @param fieldNames The field values to give the modified ElectronicSignature. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified ElectronicSignature
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyElectronicSignature <- function(ElectronicSignatureID, Code = NULL, Description = NULL, DistrictID = NULL, EntityID = NULL, IsForGrading = NULL, IsForPurchasing = NULL, IsForStateReporting = NULL, MediaID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "ElectronicSignature", objectId = ElectronicSignatureID, body = list(DataObject = body), searchFields = append("ElectronicSignatureID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ProductModulePaths
	#'
	#' This function returns a dataframe or json object of ProductModulePaths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ProductModulePaths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ProductModulePaths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ProductModulePath') to get more field paths.
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
	#' @concept Security
	#' @return A list of ProductModulePaths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listProductModulePaths <- function(searchConditionsList = NULL, Controller = F, CreatedTime = F, ModifiedTime = F, Module = F, ProductID = F, ProductModulePathID = F, RMSSecurityLocationSkywardID = F, Screen = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "ProductModulePath", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ProductModulePath
	#'
	#' This function returns a dataframe or json object of a ProductModulePath
	#' @param ProductModulePathID The ID of the ProductModulePath to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ProductModulePath. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ProductModulePath.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ProductModulePath') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of ProductModulePath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getProductModulePath <- function(ProductModulePathID, Controller = F, CreatedTime = F, ModifiedTime = F, Module = F, ProductID = F, RMSSecurityLocationSkywardID = F, Screen = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ProductModulePathID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "ProductModulePath", objectId = ProductModulePathID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ProductModulePath
	#'
	#' This function deletes a ProductModulePath
	#' @param ProductModulePathID The ID of the ProductModulePath to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The ProductModulePathID of the deleted ProductModulePath.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteProductModulePath <- function(ProductModulePathID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "ProductModulePath", objectId = ProductModulePathID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ProductModulePath
	#'
	#' This function creates a ProductModulePath
	#' @param fieldNames The field values to give the created ProductModulePath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created ProductModulePath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createProductModulePath <- function(Controller = NULL, Module = NULL, ProductID = NULL, RMSSecurityLocationSkywardID = NULL, Screen = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "ProductModulePath", body = list(DataObject = body), searchFields = append("ProductModulePathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ProductModulePath
	#'
	#' This function modifies a ProductModulePath
	#' @param fieldNames The field values to give the modified ProductModulePath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified ProductModulePath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyProductModulePath <- function(ProductModulePathID, Controller = NULL, Module = NULL, ProductID = NULL, RMSSecurityLocationSkywardID = NULL, Screen = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "ProductModulePath", objectId = ProductModulePathID, body = list(DataObject = body), searchFields = append("ProductModulePathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ProductOwneds
	#'
	#' This function returns a dataframe or json object of ProductOwneds
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ProductOwneds. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ProductOwneds.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ProductOwned') to get more field paths.
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
	#' @concept Security
	#' @return A list of ProductOwneds
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listProductOwneds <- function(searchConditionsList = NULL, CreatedTime = F, EndDate = F, ExpirationDate = F, ModifiedTime = F, ProductID = F, ProductOwnedID = F, RMSID = F, StartDate = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "ProductOwned", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ProductOwned
	#'
	#' This function returns a dataframe or json object of a ProductOwned
	#' @param ProductOwnedID The ID of the ProductOwned to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ProductOwned. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ProductOwned.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ProductOwned') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of ProductOwned
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getProductOwned <- function(ProductOwnedID, CreatedTime = F, EndDate = F, ExpirationDate = F, ModifiedTime = F, ProductID = F, RMSID = F, StartDate = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ProductOwnedID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "ProductOwned", objectId = ProductOwnedID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ProductOwned
	#'
	#' This function deletes a ProductOwned
	#' @param ProductOwnedID The ID of the ProductOwned to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The ProductOwnedID of the deleted ProductOwned.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteProductOwned <- function(ProductOwnedID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "ProductOwned", objectId = ProductOwnedID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ProductOwned
	#'
	#' This function creates a ProductOwned
	#' @param fieldNames The field values to give the created ProductOwned. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created ProductOwned
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createProductOwned <- function(EndDate = NULL, ExpirationDate = NULL, ProductID = NULL, RMSID = NULL, StartDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "ProductOwned", body = list(DataObject = body), searchFields = append("ProductOwnedID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ProductOwned
	#'
	#' This function modifies a ProductOwned
	#' @param fieldNames The field values to give the modified ProductOwned. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified ProductOwned
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyProductOwned <- function(ProductOwnedID, EndDate = NULL, ExpirationDate = NULL, ProductID = NULL, RMSID = NULL, StartDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "ProductOwned", objectId = ProductOwnedID, body = list(DataObject = body), searchFields = append("ProductOwnedID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Impersonations
	#'
	#' This function returns a dataframe or json object of Impersonations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Impersonations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Impersonations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Impersonation') to get more field paths.
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
	#' @concept Security
	#' @return A list of Impersonations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listImpersonations <- function(searchConditionsList = NULL, CreatedTime = F, ImpersonationEnded = F, ImpersonationID = F, ImpersonationStarted = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDImpersonated = F, UserIDImpersonator = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "Impersonation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Impersonation
	#'
	#' This function returns a dataframe or json object of an Impersonation
	#' @param ImpersonationID The ID of the Impersonation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Impersonation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Impersonation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Impersonation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of Impersonation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getImpersonation <- function(ImpersonationID, CreatedTime = F, ImpersonationEnded = F, ImpersonationStarted = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDImpersonated = F, UserIDImpersonator = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ImpersonationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "Impersonation", objectId = ImpersonationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Impersonation
	#'
	#' This function deletes an Impersonation
	#' @param ImpersonationID The ID of the Impersonation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The ImpersonationID of the deleted Impersonation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteImpersonation <- function(ImpersonationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "Impersonation", objectId = ImpersonationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Impersonation
	#'
	#' This function creates an Impersonation
	#' @param fieldNames The field values to give the created Impersonation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created Impersonation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createImpersonation <- function(ImpersonationEnded = NULL, UserIDImpersonated = NULL, UserIDImpersonator = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "Impersonation", body = list(DataObject = body), searchFields = append("ImpersonationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Impersonation
	#'
	#' This function modifies an Impersonation
	#' @param fieldNames The field values to give the modified Impersonation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified Impersonation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyImpersonation <- function(ImpersonationID, ImpersonationEnded = NULL, UserIDImpersonated = NULL, UserIDImpersonator = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "Impersonation", objectId = ImpersonationID, body = list(DataObject = body), searchFields = append("ImpersonationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SecurityUsers
	#'
	#' This function returns a dataframe or json object of SecurityUsers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityUsers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityUsers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityUser') to get more field paths.
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
	#' @concept Security
	#' @return A list of SecurityUsers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSecurityUsers <- function(searchConditionsList = NULL, AccessCode = F, AuthenticationRoleID = F, CreatedTime = F, CurrentPortal = F, CustomerAccessID = F, DatabaseUsername = F, DockDisplayOpen = F, EffectiveAuthenticationRoleID = F, EffectiveAuthenticationRoleName = F, EffectiveCachedAuthenticationRole = F, EffectiveMultifactorAuthenticationCode = F, EffectiveMultifactorAuthenticationID = F, EmulatingMobile = F, EntityIDCurrent = F, FailedMultifactorAuthenticationCount = F, FailedSignInCount = F, FiscalYearIDCurrent = F, ForcePasswordChange = F, FullNameFL = F, FullNameFML = F, FullNameLFM = F, GroupMembershipCount = F, IsActive = F, IsDeleted = F, IsExpired = F, IsLockedOut = F, IsSuperUser = F, LastPasswordChangeTime = F, LockedOutTime = F, MessageCount = F, ModifiedTime = F, MultifactorAuthenticationID = F, NameID = F, PasswordExpirationDate = F, PasswordHash = F, PasswordSalt = F, PasswordStrategy = F, RolesAuthenticationRoleID = F, RolesMultifactorAuthenticationID = F, UserHasFamilyAccess = F, UserHasStudentAccess = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Username = F, UserUncachedID = F, UsesSkywardAuthentication = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "User", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SecurityUser
	#'
	#' This function returns a dataframe or json object of a SecurityUser
	#' @param SecurityUserID The ID of the SecurityUser to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityUser. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityUser.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityUser') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of SecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSecurityUser <- function(SecurityUserID, AccessCode = F, AuthenticationRoleID = F, CreatedTime = F, CurrentPortal = F, CustomerAccessID = F, DatabaseUsername = F, DockDisplayOpen = F, EffectiveAuthenticationRoleID = F, EffectiveAuthenticationRoleName = F, EffectiveCachedAuthenticationRole = F, EffectiveMultifactorAuthenticationCode = F, EffectiveMultifactorAuthenticationID = F, EmulatingMobile = F, EntityIDCurrent = F, FailedMultifactorAuthenticationCount = F, FailedSignInCount = F, FiscalYearIDCurrent = F, ForcePasswordChange = F, FullNameFL = F, FullNameFML = F, FullNameLFM = F, GroupMembershipCount = F, IsActive = F, IsDeleted = F, IsExpired = F, IsLockedOut = F, IsSuperUser = F, LastPasswordChangeTime = F, LockedOutTime = F, MessageCount = F, ModifiedTime = F, MultifactorAuthenticationID = F, NameID = F, PasswordExpirationDate = F, PasswordHash = F, PasswordSalt = F, PasswordStrategy = F, RolesAuthenticationRoleID = F, RolesMultifactorAuthenticationID = F, UserHasFamilyAccess = F, UserHasStudentAccess = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Username = F, UserUncachedID = F, UsesSkywardAuthentication = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SecurityUserID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "User", objectId = SecurityUserID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SecurityUser
	#'
	#' This function deletes a SecurityUser
	#' @param SecurityUserID The ID of the SecurityUser to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The SecurityUserID of the deleted SecurityUser.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSecurityUser <- function(SecurityUserID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "User", objectId = SecurityUserID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SecurityUser
	#'
	#' This function creates a SecurityUser
	#' @param fieldNames The field values to give the created SecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created SecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSecurityUser <- function(AccessCode = NULL, AuthenticationRoleID = NULL, CurrentPortal = NULL, CustomerAccessID = NULL, DatabaseUsername = NULL, DockDisplayOpen = NULL, EmulatingMobile = NULL, EntityIDCurrent = NULL, FailedMultifactorAuthenticationCount = NULL, FailedSignInCount = NULL, FiscalYearIDCurrent = NULL, ForcePasswordChange = NULL, IsActive = NULL, IsDeleted = NULL, IsExpired = NULL, IsSuperUser = NULL, LastPasswordChangeTime = NULL, LockedOutTime = NULL, MultifactorAuthenticationID = NULL, NameID = NULL, PasswordStrategy = NULL, Username = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "User", body = list(DataObject = body), searchFields = append("UserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SecurityUser
	#'
	#' This function modifies a SecurityUser
	#' @param fieldNames The field values to give the modified SecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified SecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySecurityUser <- function(UserID, AccessCode = NULL, AuthenticationRoleID = NULL, CurrentPortal = NULL, CustomerAccessID = NULL, DatabaseUsername = NULL, DockDisplayOpen = NULL, EmulatingMobile = NULL, EntityIDCurrent = NULL, FailedMultifactorAuthenticationCount = NULL, FailedSignInCount = NULL, FiscalYearIDCurrent = NULL, ForcePasswordChange = NULL, IsActive = NULL, IsDeleted = NULL, IsExpired = NULL, IsSuperUser = NULL, LastPasswordChangeTime = NULL, LockedOutTime = NULL, MultifactorAuthenticationID = NULL, NameID = NULL, PasswordStrategy = NULL, Username = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "User", objectId = UserID, body = list(DataObject = body), searchFields = append("UserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFamilyAccessSecurityUsers
	#'
	#' This function returns a dataframe or json object of TempFamilyAccessSecurityUsers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFamilyAccessSecurityUsers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFamilyAccessSecurityUsers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFamilyAccessSecurityUser') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempFamilyAccessSecurityUsers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFamilyAccessSecurityUsers <- function(searchConditionsList = NULL, AddToFamilyAccess = F, CreatedTime = F, EmailAddress = F, EntityCodeName = F, ForUserCreation = F, GuardianNameLFM = F, IsAuditFamilyAccessSecurity = F, IsException = F, IsSelected = F, ModifiedTime = F, RemoveFromFamilyAccess = F, StudentGuardianID = F, StudentNameLFM = F, TempFamilyAccessSecurityUserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempFamilyAccessSecurityUser", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempFamilyAccessSecurityUser
	#'
	#' This function returns a dataframe or json object of a TempFamilyAccessSecurityUser
	#' @param TempFamilyAccessSecurityUserID The ID of the TempFamilyAccessSecurityUser to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFamilyAccessSecurityUser. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFamilyAccessSecurityUser.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFamilyAccessSecurityUser') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempFamilyAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFamilyAccessSecurityUser <- function(TempFamilyAccessSecurityUserID, AddToFamilyAccess = F, CreatedTime = F, EmailAddress = F, EntityCodeName = F, ForUserCreation = F, GuardianNameLFM = F, IsAuditFamilyAccessSecurity = F, IsException = F, IsSelected = F, ModifiedTime = F, RemoveFromFamilyAccess = F, StudentGuardianID = F, StudentNameLFM = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserName = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFamilyAccessSecurityUserID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempFamilyAccessSecurityUser", objectId = TempFamilyAccessSecurityUserID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempFamilyAccessSecurityUser
	#'
	#' This function deletes a TempFamilyAccessSecurityUser
	#' @param TempFamilyAccessSecurityUserID The ID of the TempFamilyAccessSecurityUser to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempFamilyAccessSecurityUserID of the deleted TempFamilyAccessSecurityUser.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempFamilyAccessSecurityUser <- function(TempFamilyAccessSecurityUserID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempFamilyAccessSecurityUser", objectId = TempFamilyAccessSecurityUserID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempFamilyAccessSecurityUser
	#'
	#' This function creates a TempFamilyAccessSecurityUser
	#' @param fieldNames The field values to give the created TempFamilyAccessSecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempFamilyAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempFamilyAccessSecurityUser <- function(AddToFamilyAccess = NULL, EmailAddress = NULL, EntityCodeName = NULL, GuardianNameLFM = NULL, IsSelected = NULL, RemoveFromFamilyAccess = NULL, StudentGuardianID = NULL, StudentNameLFM = NULL, UserName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempFamilyAccessSecurityUser", body = list(DataObject = body), searchFields = append("TempFamilyAccessSecurityUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempFamilyAccessSecurityUser
	#'
	#' This function modifies a TempFamilyAccessSecurityUser
	#' @param fieldNames The field values to give the modified TempFamilyAccessSecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempFamilyAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempFamilyAccessSecurityUser <- function(TempFamilyAccessSecurityUserID, AddToFamilyAccess = NULL, EmailAddress = NULL, EntityCodeName = NULL, GuardianNameLFM = NULL, IsSelected = NULL, RemoveFromFamilyAccess = NULL, StudentGuardianID = NULL, StudentNameLFM = NULL, UserName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempFamilyAccessSecurityUser", objectId = TempFamilyAccessSecurityUserID, body = list(DataObject = body), searchFields = append("TempFamilyAccessSecurityUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentAccessSecurityUsers
	#'
	#' This function returns a dataframe or json object of TempStudentAccessSecurityUsers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentAccessSecurityUsers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentAccessSecurityUsers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentAccessSecurityUser') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempStudentAccessSecurityUsers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentAccessSecurityUsers <- function(searchConditionsList = NULL, AddToStudentAccess = F, CreatedTime = F, DeleteUserAfterAudit = F, EmailAddress = F, ForUserCreation = F, Group = F, IsAuditStudentAccessSecurity = F, IsException = F, IsSelected = F, ModifiedTime = F, RemoveFromStudentAccess = F, StudentID = F, StudentNameLFM = F, TempStudentAccessSecurityUserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempStudentAccessSecurityUser", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentAccessSecurityUser
	#'
	#' This function returns a dataframe or json object of a TempStudentAccessSecurityUser
	#' @param TempStudentAccessSecurityUserID The ID of the TempStudentAccessSecurityUser to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentAccessSecurityUser. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentAccessSecurityUser.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentAccessSecurityUser') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempStudentAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentAccessSecurityUser <- function(TempStudentAccessSecurityUserID, AddToStudentAccess = F, CreatedTime = F, DeleteUserAfterAudit = F, EmailAddress = F, ForUserCreation = F, Group = F, IsAuditStudentAccessSecurity = F, IsException = F, IsSelected = F, ModifiedTime = F, RemoveFromStudentAccess = F, StudentID = F, StudentNameLFM = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserName = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentAccessSecurityUserID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempStudentAccessSecurityUser", objectId = TempStudentAccessSecurityUserID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentAccessSecurityUser
	#'
	#' This function deletes a TempStudentAccessSecurityUser
	#' @param TempStudentAccessSecurityUserID The ID of the TempStudentAccessSecurityUser to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempStudentAccessSecurityUserID of the deleted TempStudentAccessSecurityUser.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentAccessSecurityUser <- function(TempStudentAccessSecurityUserID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempStudentAccessSecurityUser", objectId = TempStudentAccessSecurityUserID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentAccessSecurityUser
	#'
	#' This function creates a TempStudentAccessSecurityUser
	#' @param fieldNames The field values to give the created TempStudentAccessSecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempStudentAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentAccessSecurityUser <- function(AddToStudentAccess = NULL, DeleteUserAfterAudit = NULL, EmailAddress = NULL, Group = NULL, IsSelected = NULL, RemoveFromStudentAccess = NULL, StudentID = NULL, StudentNameLFM = NULL, UserName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempStudentAccessSecurityUser", body = list(DataObject = body), searchFields = append("TempStudentAccessSecurityUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentAccessSecurityUser
	#'
	#' This function modifies a TempStudentAccessSecurityUser
	#' @param fieldNames The field values to give the modified TempStudentAccessSecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempStudentAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentAccessSecurityUser <- function(TempStudentAccessSecurityUserID, AddToStudentAccess = NULL, DeleteUserAfterAudit = NULL, EmailAddress = NULL, Group = NULL, IsSelected = NULL, RemoveFromStudentAccess = NULL, StudentID = NULL, StudentNameLFM = NULL, UserName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempStudentAccessSecurityUser", objectId = TempStudentAccessSecurityUserID, body = list(DataObject = body), searchFields = append("TempStudentAccessSecurityUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeeAccessSecurityUsers
	#'
	#' This function returns a dataframe or json object of TempEmployeeAccessSecurityUsers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeAccessSecurityUsers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeAccessSecurityUsers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeAccessSecurityUser') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempEmployeeAccessSecurityUsers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeeAccessSecurityUsers <- function(searchConditionsList = NULL, AddToEmployeeAccess = F, AllowEmployeeAccess = F, CreatedTime = F, EmailAddress = F, EmployeeID = F, EmployeeNameLFM = F, EmployeeNumber = F, ForUserCreation = F, Group = F, IsAuditEmployeeAccessSecurity = F, IsException = F, IsSelected = F, ModifiedTime = F, RemoveFromEmployeeAccess = F, TempEmployeeAccessSecurityUserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempEmployeeAccessSecurityUser", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeeAccessSecurityUser
	#'
	#' This function returns a dataframe or json object of a TempEmployeeAccessSecurityUser
	#' @param TempEmployeeAccessSecurityUserID The ID of the TempEmployeeAccessSecurityUser to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeAccessSecurityUser. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeAccessSecurityUser.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeAccessSecurityUser') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempEmployeeAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeeAccessSecurityUser <- function(TempEmployeeAccessSecurityUserID, AddToEmployeeAccess = F, AllowEmployeeAccess = F, CreatedTime = F, EmailAddress = F, EmployeeID = F, EmployeeNameLFM = F, EmployeeNumber = F, ForUserCreation = F, Group = F, IsAuditEmployeeAccessSecurity = F, IsException = F, IsSelected = F, ModifiedTime = F, RemoveFromEmployeeAccess = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserName = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeeAccessSecurityUserID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempEmployeeAccessSecurityUser", objectId = TempEmployeeAccessSecurityUserID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeeAccessSecurityUser
	#'
	#' This function deletes a TempEmployeeAccessSecurityUser
	#' @param TempEmployeeAccessSecurityUserID The ID of the TempEmployeeAccessSecurityUser to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempEmployeeAccessSecurityUserID of the deleted TempEmployeeAccessSecurityUser.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeeAccessSecurityUser <- function(TempEmployeeAccessSecurityUserID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempEmployeeAccessSecurityUser", objectId = TempEmployeeAccessSecurityUserID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeeAccessSecurityUser
	#'
	#' This function creates a TempEmployeeAccessSecurityUser
	#' @param fieldNames The field values to give the created TempEmployeeAccessSecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempEmployeeAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeeAccessSecurityUser <- function(AllowEmployeeAccess = NULL, EmailAddress = NULL, EmployeeID = NULL, EmployeeNameLFM = NULL, EmployeeNumber = NULL, Group = NULL, UserName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempEmployeeAccessSecurityUser", body = list(DataObject = body), searchFields = append("TempEmployeeAccessSecurityUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeeAccessSecurityUser
	#'
	#' This function modifies a TempEmployeeAccessSecurityUser
	#' @param fieldNames The field values to give the modified TempEmployeeAccessSecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempEmployeeAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeeAccessSecurityUser <- function(TempEmployeeAccessSecurityUserID, AllowEmployeeAccess = NULL, EmailAddress = NULL, EmployeeID = NULL, EmployeeNameLFM = NULL, EmployeeNumber = NULL, Group = NULL, UserName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempEmployeeAccessSecurityUser", objectId = TempEmployeeAccessSecurityUserID, body = list(DataObject = body), searchFields = append("TempEmployeeAccessSecurityUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SecurityUserSettings
	#'
	#' This function returns a dataframe or json object of SecurityUserSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityUserSettings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityUserSettings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityUserSetting') to get more field paths.
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
	#' @concept Security
	#' @return A list of SecurityUserSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSecurityUserSettings <- function(searchConditionsList = NULL, Action = F, Area = F, Code = F, Controller = F, CreatedTime = F, ModifiedTime = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserSettingID = F, Value = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "UserSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SecurityUserSetting
	#'
	#' This function returns a dataframe or json object of a SecurityUserSetting
	#' @param SecurityUserSettingID The ID of the SecurityUserSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityUserSetting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityUserSetting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityUserSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of SecurityUserSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSecurityUserSetting <- function(SecurityUserSettingID, Action = F, Area = F, Code = F, Controller = F, CreatedTime = F, ModifiedTime = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserSettingID = F, Value = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SecurityUserSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "UserSetting", objectId = SecurityUserSettingID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SecurityUserSetting
	#'
	#' This function deletes a SecurityUserSetting
	#' @param SecurityUserSettingID The ID of the SecurityUserSetting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The SecurityUserSettingID of the deleted SecurityUserSetting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSecurityUserSetting <- function(SecurityUserSettingID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "UserSetting", objectId = SecurityUserSettingID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SecurityUserSetting
	#'
	#' This function creates a SecurityUserSetting
	#' @param fieldNames The field values to give the created SecurityUserSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created SecurityUserSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSecurityUserSetting <- function(Action = NULL, Area = NULL, Code = NULL, Controller = NULL, UserID = NULL, Value = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "UserSetting", body = list(DataObject = body), searchFields = append("UserSettingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SecurityUserSetting
	#'
	#' This function modifies a SecurityUserSetting
	#' @param fieldNames The field values to give the modified SecurityUserSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified SecurityUserSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySecurityUserSetting <- function(UserSettingID, Action = NULL, Area = NULL, Code = NULL, Controller = NULL, UserID = NULL, Value = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "UserSetting", objectId = UserSettingID, body = list(DataObject = body), searchFields = append("UserSettingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserPasswordResets
	#'
	#' This function returns a dataframe or json object of UserPasswordResets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserPasswordResets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserPasswordResets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserPasswordReset') to get more field paths.
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
	#' @concept Security
	#' @return A list of UserPasswordResets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserPasswordResets <- function(searchConditionsList = NULL, CreatedTime = F, ExpirationTime = F, HostAddressRequestedFrom = F, ModifiedTime = F, ResetGuid = F, ResetSalt = F, Used = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserPasswordResetID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "UserPasswordReset", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserPasswordReset
	#'
	#' This function returns a dataframe or json object of an UserPasswordReset
	#' @param UserPasswordResetID The ID of the UserPasswordReset to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserPasswordReset. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserPasswordReset.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserPasswordReset') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of UserPasswordReset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserPasswordReset <- function(UserPasswordResetID, CreatedTime = F, ExpirationTime = F, HostAddressRequestedFrom = F, ModifiedTime = F, ResetGuid = F, ResetSalt = F, Used = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserPasswordResetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "UserPasswordReset", objectId = UserPasswordResetID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserPasswordReset
	#'
	#' This function deletes an UserPasswordReset
	#' @param UserPasswordResetID The ID of the UserPasswordReset to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The UserPasswordResetID of the deleted UserPasswordReset.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserPasswordReset <- function(UserPasswordResetID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "UserPasswordReset", objectId = UserPasswordResetID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserPasswordReset
	#'
	#' This function creates an UserPasswordReset
	#' @param fieldNames The field values to give the created UserPasswordReset. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created UserPasswordReset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserPasswordReset <- function(ExpirationTime = NULL, HostAddressRequestedFrom = NULL, Used = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "UserPasswordReset", body = list(DataObject = body), searchFields = append("UserPasswordResetID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserPasswordReset
	#'
	#' This function modifies an UserPasswordReset
	#' @param fieldNames The field values to give the modified UserPasswordReset. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified UserPasswordReset
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserPasswordReset <- function(UserPasswordResetID, ExpirationTime = NULL, HostAddressRequestedFrom = NULL, Used = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "UserPasswordReset", objectId = UserPasswordResetID, body = list(DataObject = body), searchFields = append("UserPasswordResetID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BrowseFieldPaths
	#'
	#' This function returns a dataframe or json object of BrowseFieldPaths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BrowseFieldPaths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BrowseFieldPaths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BrowseFieldPath') to get more field paths.
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
	#' @concept Security
	#' @return A list of BrowseFieldPaths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBrowseFieldPaths <- function(searchConditionsList = NULL, BrowseFieldPathID = F, BrowseID = F, CreatedTime = F, FieldPath = F, GuidFieldPath = F, IsSkywardDefined = F, ModifiedTime = F, RoleID = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "BrowseFieldPath", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BrowseFieldPath
	#'
	#' This function returns a dataframe or json object of a BrowseFieldPath
	#' @param BrowseFieldPathID The ID of the BrowseFieldPath to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BrowseFieldPath. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BrowseFieldPath.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BrowseFieldPath') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of BrowseFieldPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBrowseFieldPath <- function(BrowseFieldPathID, BrowseID = F, CreatedTime = F, FieldPath = F, GuidFieldPath = F, IsSkywardDefined = F, ModifiedTime = F, RoleID = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BrowseFieldPathID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "BrowseFieldPath", objectId = BrowseFieldPathID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BrowseFieldPath
	#'
	#' This function deletes a BrowseFieldPath
	#' @param BrowseFieldPathID The ID of the BrowseFieldPath to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The BrowseFieldPathID of the deleted BrowseFieldPath.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBrowseFieldPath <- function(BrowseFieldPathID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "BrowseFieldPath", objectId = BrowseFieldPathID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BrowseFieldPath
	#'
	#' This function creates a BrowseFieldPath
	#' @param fieldNames The field values to give the created BrowseFieldPath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created BrowseFieldPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBrowseFieldPath <- function(BrowseID = NULL, FieldPath = NULL, GuidFieldPath = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "BrowseFieldPath", body = list(DataObject = body), searchFields = append("BrowseFieldPathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BrowseFieldPath
	#'
	#' This function modifies a BrowseFieldPath
	#' @param fieldNames The field values to give the modified BrowseFieldPath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified BrowseFieldPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBrowseFieldPath <- function(BrowseFieldPathID, BrowseID = NULL, FieldPath = NULL, GuidFieldPath = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "BrowseFieldPath", objectId = BrowseFieldPathID, body = list(DataObject = body), searchFields = append("BrowseFieldPathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DataObjectFieldPaths
	#'
	#' This function returns a dataframe or json object of DataObjectFieldPaths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataObjectFieldPaths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataObjectFieldPaths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataObjectFieldPath') to get more field paths.
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
	#' @concept Security
	#' @return A list of DataObjectFieldPaths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDataObjectFieldPaths <- function(searchConditionsList = NULL, CreatedTime = F, DataObjectFieldPathID = F, ExactSystemTypeName = F, FieldIDSkySys = F, FieldPath = F, GuidFieldPath = F, IsSkywardDefined = F, ModifiedTime = F, ObjectID = F, RoleID = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "DataObjectFieldPath", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DataObjectFieldPath
	#'
	#' This function returns a dataframe or json object of a DataObjectFieldPath
	#' @param DataObjectFieldPathID The ID of the DataObjectFieldPath to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataObjectFieldPath. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataObjectFieldPath.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataObjectFieldPath') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of DataObjectFieldPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDataObjectFieldPath <- function(DataObjectFieldPathID, CreatedTime = F, ExactSystemTypeName = F, FieldIDSkySys = F, FieldPath = F, GuidFieldPath = F, IsSkywardDefined = F, ModifiedTime = F, ObjectID = F, RoleID = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DataObjectFieldPathID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "DataObjectFieldPath", objectId = DataObjectFieldPathID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DataObjectFieldPath
	#'
	#' This function deletes a DataObjectFieldPath
	#' @param DataObjectFieldPathID The ID of the DataObjectFieldPath to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The DataObjectFieldPathID of the deleted DataObjectFieldPath.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDataObjectFieldPath <- function(DataObjectFieldPathID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "DataObjectFieldPath", objectId = DataObjectFieldPathID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DataObjectFieldPath
	#'
	#' This function creates a DataObjectFieldPath
	#' @param fieldNames The field values to give the created DataObjectFieldPath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created DataObjectFieldPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDataObjectFieldPath <- function(FieldIDSkySys = NULL, FieldPath = NULL, GuidFieldPath = NULL, ObjectID = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "DataObjectFieldPath", body = list(DataObject = body), searchFields = append("DataObjectFieldPathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DataObjectFieldPath
	#'
	#' This function modifies a DataObjectFieldPath
	#' @param fieldNames The field values to give the modified DataObjectFieldPath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified DataObjectFieldPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDataObjectFieldPath <- function(DataObjectFieldPathID, FieldIDSkySys = NULL, FieldPath = NULL, GuidFieldPath = NULL, ObjectID = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "DataObjectFieldPath", objectId = DataObjectFieldPathID, body = list(DataObject = body), searchFields = append("DataObjectFieldPathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RoleFields
	#'
	#' This function returns a dataframe or json object of RoleFields
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoleFields. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoleFields.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoleField') to get more field paths.
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
	#' @concept Security
	#' @return A list of RoleFields
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoleFields <- function(searchConditionsList = NULL, AllowRead = F, CreatedTime = F, Field = F, FullField = F, ModifiedTime = F, Module = F, Object = F, RoleFieldID = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "RoleField", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RoleField
	#'
	#' This function returns a dataframe or json object of a RoleField
	#' @param RoleFieldID The ID of the RoleField to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoleField. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoleField.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoleField') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of RoleField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoleField <- function(RoleFieldID, AllowRead = F, CreatedTime = F, Field = F, FullField = F, ModifiedTime = F, Module = F, Object = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoleFieldID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "RoleField", objectId = RoleFieldID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RoleField
	#'
	#' This function deletes a RoleField
	#' @param RoleFieldID The ID of the RoleField to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The RoleFieldID of the deleted RoleField.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoleField <- function(RoleFieldID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "RoleField", objectId = RoleFieldID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RoleField
	#'
	#' This function creates a RoleField
	#' @param fieldNames The field values to give the created RoleField. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created RoleField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoleField <- function(AllowRead = NULL, Field = NULL, Module = NULL, Object = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "RoleField", body = list(DataObject = body), searchFields = append("RoleFieldID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RoleField
	#'
	#' This function modifies a RoleField
	#' @param fieldNames The field values to give the modified RoleField. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified RoleField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoleField <- function(RoleFieldID, AllowRead = NULL, Field = NULL, Module = NULL, Object = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "RoleField", objectId = RoleFieldID, body = list(DataObject = body), searchFields = append("RoleFieldID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserPreferences
	#'
	#' This function returns a dataframe or json object of UserPreferences
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserPreferences. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserPreferences.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserPreference') to get more field paths.
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
	#' @concept Security
	#' @return A list of UserPreferences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserPreferences <- function(searchConditionsList = NULL, AccountSelection = F, CreatedTime = F, ModifiedTime = F, ThemeID = F, UseEmailMultifactorAuthentication = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDSecurity = F, UserPreferenceID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "UserPreference", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserPreference
	#'
	#' This function returns a dataframe or json object of an UserPreference
	#' @param UserPreferenceID The ID of the UserPreference to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserPreference. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserPreference.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserPreference') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of UserPreference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserPreference <- function(UserPreferenceID, AccountSelection = F, CreatedTime = F, ModifiedTime = F, ThemeID = F, UseEmailMultifactorAuthentication = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDSecurity = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserPreferenceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "UserPreference", objectId = UserPreferenceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserPreference
	#'
	#' This function deletes an UserPreference
	#' @param UserPreferenceID The ID of the UserPreference to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The UserPreferenceID of the deleted UserPreference.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserPreference <- function(UserPreferenceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "UserPreference", objectId = UserPreferenceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserPreference
	#'
	#' This function creates an UserPreference
	#' @param fieldNames The field values to give the created UserPreference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created UserPreference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserPreference <- function(AccountSelection = NULL, ThemeID = NULL, UseEmailMultifactorAuthentication = NULL, UserIDSecurity = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "UserPreference", body = list(DataObject = body), searchFields = append("UserPreferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserPreference
	#'
	#' This function modifies an UserPreference
	#' @param fieldNames The field values to give the modified UserPreference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified UserPreference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserPreference <- function(UserPreferenceID, AccountSelection = NULL, ThemeID = NULL, UseEmailMultifactorAuthentication = NULL, UserIDSecurity = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "UserPreference", objectId = UserPreferenceID, body = list(DataObject = body), searchFields = append("UserPreferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SessionFileUploads
	#'
	#' This function returns a dataframe or json object of SessionFileUploads
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SessionFileUploads. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SessionFileUploads.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SessionFileUpload') to get more field paths.
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
	#' @concept Security
	#' @return A list of SessionFileUploads
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSessionFileUploads <- function(searchConditionsList = NULL, Bytes = F, CreatedTime = F, FileContents = F, FileExtension = F, FileName = F, FilePath = F, MetaDataXml = F, ModifiedTime = F, SessionFileUploadID = F, SessionID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, XDimension = F, YDimension = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "SessionFileUpload", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SessionFileUpload
	#'
	#' This function returns a dataframe or json object of a SessionFileUpload
	#' @param SessionFileUploadID The ID of the SessionFileUpload to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SessionFileUpload. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SessionFileUpload.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SessionFileUpload') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of SessionFileUpload
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSessionFileUpload <- function(SessionFileUploadID, Bytes = F, CreatedTime = F, FileContents = F, FileExtension = F, FileName = F, FilePath = F, MetaDataXml = F, ModifiedTime = F, SessionID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, XDimension = F, YDimension = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SessionFileUploadID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "SessionFileUpload", objectId = SessionFileUploadID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SessionFileUpload
	#'
	#' This function deletes a SessionFileUpload
	#' @param SessionFileUploadID The ID of the SessionFileUpload to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The SessionFileUploadID of the deleted SessionFileUpload.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSessionFileUpload <- function(SessionFileUploadID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "SessionFileUpload", objectId = SessionFileUploadID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SessionFileUpload
	#'
	#' This function creates a SessionFileUpload
	#' @param fieldNames The field values to give the created SessionFileUpload. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created SessionFileUpload
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSessionFileUpload <- function(FilePath = NULL, SessionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "SessionFileUpload", body = list(DataObject = body), searchFields = append("SessionFileUploadID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SessionFileUpload
	#'
	#' This function modifies a SessionFileUpload
	#' @param fieldNames The field values to give the modified SessionFileUpload. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified SessionFileUpload
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySessionFileUpload <- function(SessionFileUploadID, FilePath = NULL, SessionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "SessionFileUpload", objectId = SessionFileUploadID, body = list(DataObject = body), searchFields = append("SessionFileUploadID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AuthenticationMethods
	#'
	#' This function returns a dataframe or json object of AuthenticationMethods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AuthenticationMethods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AuthenticationMethods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AuthenticationMethod') to get more field paths.
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
	#' @concept Security
	#' @return A list of AuthenticationMethods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAuthenticationMethods <- function(searchConditionsList = NULL, AuthenticationMethodID = F, AuthenticationRequestsSigned = F, CacheDurationInSecondsOverride = F, Certificate = F, CompareNameIDAsNumeric = F, CreatedTime = F, IsSkywardMaintained = F, Metadata = F, MetadataURL = F, ModifiedTime = F, Name = F, NameIdentifierSkywardField = F, NameIdentifierType = F, SkywardHash = F, SkywardID = F, SSOURL = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WantAssertionsSigned = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "AuthenticationMethod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AuthenticationMethod
	#'
	#' This function returns a dataframe or json object of an AuthenticationMethod
	#' @param AuthenticationMethodID The ID of the AuthenticationMethod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AuthenticationMethod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AuthenticationMethod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AuthenticationMethod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of AuthenticationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAuthenticationMethod <- function(AuthenticationMethodID, AuthenticationRequestsSigned = F, CacheDurationInSecondsOverride = F, Certificate = F, CompareNameIDAsNumeric = F, CreatedTime = F, IsSkywardMaintained = F, Metadata = F, MetadataURL = F, ModifiedTime = F, Name = F, NameIdentifierSkywardField = F, NameIdentifierType = F, SkywardHash = F, SkywardID = F, SSOURL = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WantAssertionsSigned = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AuthenticationMethodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "AuthenticationMethod", objectId = AuthenticationMethodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AuthenticationMethod
	#'
	#' This function deletes an AuthenticationMethod
	#' @param AuthenticationMethodID The ID of the AuthenticationMethod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The AuthenticationMethodID of the deleted AuthenticationMethod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAuthenticationMethod <- function(AuthenticationMethodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "AuthenticationMethod", objectId = AuthenticationMethodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AuthenticationMethod
	#'
	#' This function creates an AuthenticationMethod
	#' @param fieldNames The field values to give the created AuthenticationMethod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created AuthenticationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAuthenticationMethod <- function(AuthenticationRequestsSigned = NULL, CacheDurationInSecondsOverride = NULL, Certificate = NULL, CompareNameIDAsNumeric = NULL, IsSkywardMaintained = NULL, Metadata = NULL, MetadataURL = NULL, Name = NULL, NameIdentifierType = NULL, SSOURL = NULL, WantAssertionsSigned = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "AuthenticationMethod", body = list(DataObject = body), searchFields = append("AuthenticationMethodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AuthenticationMethod
	#'
	#' This function modifies an AuthenticationMethod
	#' @param fieldNames The field values to give the modified AuthenticationMethod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified AuthenticationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAuthenticationMethod <- function(AuthenticationMethodID, AuthenticationRequestsSigned = NULL, CacheDurationInSecondsOverride = NULL, Certificate = NULL, CompareNameIDAsNumeric = NULL, IsSkywardMaintained = NULL, Metadata = NULL, MetadataURL = NULL, Name = NULL, NameIdentifierType = NULL, SSOURL = NULL, WantAssertionsSigned = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "AuthenticationMethod", objectId = AuthenticationMethodID, body = list(DataObject = body), searchFields = append("AuthenticationMethodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SecurityConfigSystems
	#'
	#' This function returns a dataframe or json object of SecurityConfigSystems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityConfigSystems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityConfigSystems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityConfigSystem') to get more field paths.
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
	#' @concept Security
	#' @return A list of SecurityConfigSystems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSecurityConfigSystems <- function(searchConditionsList = NULL, AccessCodeLength = F, AdminEmployeeTeacherActivityAllowUsernameChange = F, AdminMissedSessionPingCountLimit = F, AdminSessionClientPingSeconds = F, AdminSessionTimeoutSeconds = F, AdminSessionWarnSeconds = F, AuthenticationRoleIDActivity = F, AuthenticationRoleIDAdministrative = F, AuthenticationRoleIDEmployee = F, AuthenticationRoleIDFamilyNewStudentEnrollment = F, AuthenticationRoleIDStudent = F, AuthenticationRoleIDTeacher = F, AutogenerateEmployeeAccessCodes = F, AutogenerateStaffAccessCodes = F, AutogenerateStudentAccessCodes = F, CombineAuthenticationRolesOnSignIn = F, ConfigSystemID = F, CreatedTime = F, DaysUntilPasswordExpires = F, FailedSignInCountLimit = F, FamilyAllowUsernameChange = F, FamilyStudentEmployeeMissedSessionPingCountLimit = F, FamilyStudentEmployeeSessionClientPingSeconds = F, FamilyStudentEmployeeSessionTimeoutSeconds = F, FamilyStudentEmployeeSessionWarnSeconds = F, FileDestinationIDUserImport = F, ForcePasswordExpirationOnSkywardLoginIfPasswordRequirementsNotMet = F, LoginLockRetryDelayMinutes = F, MaximumPasswordLength = F, MinimumPasswordLength = F, ModifiedTime = F, MultifactorAuthenticationIDActivity = F, MultifactorAuthenticationIDAdministrative = F, MultifactorAuthenticationIDEmployee = F, MultifactorAuthenticationIDFamilyNewStudentEnrollment = F, MultifactorAuthenticationIDStudent = F, MultifactorAuthenticationIDTeacher = F, OrganizationDisplayName = F, OrganizationName = F, OrganizationUrl = F, RequiredNumericCharacters = F, RequiredSpecialCharacters = F, SessionAccessDeniedLimit = F, StudentAllowUsernameChange = F, TeacherMissedSessionPingCountLimit = F, TeacherSessionClientPingSeconds = F, TeacherSessionTimeoutSeconds = F, TeacherSessionWarnSeconds = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserImportChangeThreshold = F, UserImportDeleteThreshold = F, UserImportFileName = F, UserImportFileType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "ConfigSystem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SecurityConfigSystem
	#'
	#' This function returns a dataframe or json object of a SecurityConfigSystem
	#' @param SecurityConfigSystemID The ID of the SecurityConfigSystem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityConfigSystem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityConfigSystem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityConfigSystem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of SecurityConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSecurityConfigSystem <- function(SecurityConfigSystemID, AccessCodeLength = F, AdminEmployeeTeacherActivityAllowUsernameChange = F, AdminMissedSessionPingCountLimit = F, AdminSessionClientPingSeconds = F, AdminSessionTimeoutSeconds = F, AdminSessionWarnSeconds = F, AuthenticationRoleIDActivity = F, AuthenticationRoleIDAdministrative = F, AuthenticationRoleIDEmployee = F, AuthenticationRoleIDFamilyNewStudentEnrollment = F, AuthenticationRoleIDStudent = F, AuthenticationRoleIDTeacher = F, AutogenerateEmployeeAccessCodes = F, AutogenerateStaffAccessCodes = F, AutogenerateStudentAccessCodes = F, CombineAuthenticationRolesOnSignIn = F, ConfigSystemID = F, CreatedTime = F, DaysUntilPasswordExpires = F, FailedSignInCountLimit = F, FamilyAllowUsernameChange = F, FamilyStudentEmployeeMissedSessionPingCountLimit = F, FamilyStudentEmployeeSessionClientPingSeconds = F, FamilyStudentEmployeeSessionTimeoutSeconds = F, FamilyStudentEmployeeSessionWarnSeconds = F, FileDestinationIDUserImport = F, ForcePasswordExpirationOnSkywardLoginIfPasswordRequirementsNotMet = F, LoginLockRetryDelayMinutes = F, MaximumPasswordLength = F, MinimumPasswordLength = F, ModifiedTime = F, MultifactorAuthenticationIDActivity = F, MultifactorAuthenticationIDAdministrative = F, MultifactorAuthenticationIDEmployee = F, MultifactorAuthenticationIDFamilyNewStudentEnrollment = F, MultifactorAuthenticationIDStudent = F, MultifactorAuthenticationIDTeacher = F, OrganizationDisplayName = F, OrganizationName = F, OrganizationUrl = F, RequiredNumericCharacters = F, RequiredSpecialCharacters = F, SessionAccessDeniedLimit = F, StudentAllowUsernameChange = F, TeacherMissedSessionPingCountLimit = F, TeacherSessionClientPingSeconds = F, TeacherSessionTimeoutSeconds = F, TeacherSessionWarnSeconds = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserImportChangeThreshold = F, UserImportDeleteThreshold = F, UserImportFileName = F, UserImportFileType = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SecurityConfigSystemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "ConfigSystem", objectId = SecurityConfigSystemID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SecurityConfigSystem
	#'
	#' This function deletes a SecurityConfigSystem
	#' @param SecurityConfigSystemID The ID of the SecurityConfigSystem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The SecurityConfigSystemID of the deleted SecurityConfigSystem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSecurityConfigSystem <- function(SecurityConfigSystemID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "ConfigSystem", objectId = SecurityConfigSystemID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SecurityConfigSystem
	#'
	#' This function creates a SecurityConfigSystem
	#' @param fieldNames The field values to give the created SecurityConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created SecurityConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSecurityConfigSystem <- function(AccessCodeLength = NULL, AdminEmployeeTeacherActivityAllowUsernameChange = NULL, AdminMissedSessionPingCountLimit = NULL, AdminSessionClientPingSeconds = NULL, AdminSessionTimeoutSeconds = NULL, AdminSessionWarnSeconds = NULL, AuthenticationRoleIDActivity = NULL, AuthenticationRoleIDAdministrative = NULL, AuthenticationRoleIDEmployee = NULL, AuthenticationRoleIDFamilyNewStudentEnrollment = NULL, AuthenticationRoleIDStudent = NULL, AuthenticationRoleIDTeacher = NULL, AutogenerateEmployeeAccessCodes = NULL, AutogenerateStaffAccessCodes = NULL, AutogenerateStudentAccessCodes = NULL, CombineAuthenticationRolesOnSignIn = NULL, DaysUntilPasswordExpires = NULL, FailedSignInCountLimit = NULL, FamilyAllowUsernameChange = NULL, FamilyStudentEmployeeMissedSessionPingCountLimit = NULL, FamilyStudentEmployeeSessionClientPingSeconds = NULL, FamilyStudentEmployeeSessionTimeoutSeconds = NULL, FamilyStudentEmployeeSessionWarnSeconds = NULL, FileDestinationIDUserImport = NULL, ForcePasswordExpirationOnSkywardLoginIfPasswordRequirementsNotMet = NULL, LoginLockRetryDelayMinutes = NULL, MaximumPasswordLength = NULL, MinimumPasswordLength = NULL, MultifactorAuthenticationIDActivity = NULL, MultifactorAuthenticationIDAdministrative = NULL, MultifactorAuthenticationIDEmployee = NULL, MultifactorAuthenticationIDFamilyNewStudentEnrollment = NULL, MultifactorAuthenticationIDStudent = NULL, MultifactorAuthenticationIDTeacher = NULL, OrganizationDisplayName = NULL, OrganizationName = NULL, OrganizationUrl = NULL, RequiredNumericCharacters = NULL, RequiredSpecialCharacters = NULL, SessionAccessDeniedLimit = NULL, StudentAllowUsernameChange = NULL, TeacherMissedSessionPingCountLimit = NULL, TeacherSessionClientPingSeconds = NULL, TeacherSessionTimeoutSeconds = NULL, TeacherSessionWarnSeconds = NULL, UserImportChangeThreshold = NULL, UserImportDeleteThreshold = NULL, UserImportFileName = NULL, UserImportFileType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "ConfigSystem", body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SecurityConfigSystem
	#'
	#' This function modifies a SecurityConfigSystem
	#' @param fieldNames The field values to give the modified SecurityConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified SecurityConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySecurityConfigSystem <- function(ConfigSystemID, AccessCodeLength = NULL, AdminEmployeeTeacherActivityAllowUsernameChange = NULL, AdminMissedSessionPingCountLimit = NULL, AdminSessionClientPingSeconds = NULL, AdminSessionTimeoutSeconds = NULL, AdminSessionWarnSeconds = NULL, AuthenticationRoleIDActivity = NULL, AuthenticationRoleIDAdministrative = NULL, AuthenticationRoleIDEmployee = NULL, AuthenticationRoleIDFamilyNewStudentEnrollment = NULL, AuthenticationRoleIDStudent = NULL, AuthenticationRoleIDTeacher = NULL, AutogenerateEmployeeAccessCodes = NULL, AutogenerateStaffAccessCodes = NULL, AutogenerateStudentAccessCodes = NULL, CombineAuthenticationRolesOnSignIn = NULL, DaysUntilPasswordExpires = NULL, FailedSignInCountLimit = NULL, FamilyAllowUsernameChange = NULL, FamilyStudentEmployeeMissedSessionPingCountLimit = NULL, FamilyStudentEmployeeSessionClientPingSeconds = NULL, FamilyStudentEmployeeSessionTimeoutSeconds = NULL, FamilyStudentEmployeeSessionWarnSeconds = NULL, FileDestinationIDUserImport = NULL, ForcePasswordExpirationOnSkywardLoginIfPasswordRequirementsNotMet = NULL, LoginLockRetryDelayMinutes = NULL, MaximumPasswordLength = NULL, MinimumPasswordLength = NULL, MultifactorAuthenticationIDActivity = NULL, MultifactorAuthenticationIDAdministrative = NULL, MultifactorAuthenticationIDEmployee = NULL, MultifactorAuthenticationIDFamilyNewStudentEnrollment = NULL, MultifactorAuthenticationIDStudent = NULL, MultifactorAuthenticationIDTeacher = NULL, OrganizationDisplayName = NULL, OrganizationName = NULL, OrganizationUrl = NULL, RequiredNumericCharacters = NULL, RequiredSpecialCharacters = NULL, SessionAccessDeniedLimit = NULL, StudentAllowUsernameChange = NULL, TeacherMissedSessionPingCountLimit = NULL, TeacherSessionClientPingSeconds = NULL, TeacherSessionTimeoutSeconds = NULL, TeacherSessionWarnSeconds = NULL, UserImportChangeThreshold = NULL, UserImportDeleteThreshold = NULL, UserImportFileName = NULL, UserImportFileType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "ConfigSystem", objectId = ConfigSystemID, body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LDAPProviders
	#'
	#' This function returns a dataframe or json object of LDAPProviders
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LDAPProviders. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LDAPProviders.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LDAPProvider') to get more field paths.
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
	#' @concept Security
	#' @return A list of LDAPProviders
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLDAPProviders <- function(searchConditionsList = NULL, CreatedTime = F, DisableReferrals = F, DomainName = F, GroupBaseDN = F, GroupFilter = F, GroupMemberFilter = F, Host = F, IgnoreCertificationErrors = F, LDAPProviderID = F, ModifiedTime = F, Name = F, Port = F, Protocol = F, SearchBaseDN = F, SearchFilter = F, SearchPassword = F, SearchUserDN = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UsernameAttribute = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "LDAPProvider", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LDAPProvider
	#'
	#' This function returns a dataframe or json object of a LDAPProvider
	#' @param LDAPProviderID The ID of the LDAPProvider to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LDAPProvider. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LDAPProvider.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LDAPProvider') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of LDAPProvider
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLDAPProvider <- function(LDAPProviderID, CreatedTime = F, DisableReferrals = F, DomainName = F, GroupBaseDN = F, GroupFilter = F, GroupMemberFilter = F, Host = F, IgnoreCertificationErrors = F, ModifiedTime = F, Name = F, Port = F, Protocol = F, SearchBaseDN = F, SearchFilter = F, SearchPassword = F, SearchUserDN = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UsernameAttribute = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LDAPProviderID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "LDAPProvider", objectId = LDAPProviderID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LDAPProvider
	#'
	#' This function deletes a LDAPProvider
	#' @param LDAPProviderID The ID of the LDAPProvider to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The LDAPProviderID of the deleted LDAPProvider.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLDAPProvider <- function(LDAPProviderID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "LDAPProvider", objectId = LDAPProviderID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LDAPProvider
	#'
	#' This function creates a LDAPProvider
	#' @param fieldNames The field values to give the created LDAPProvider. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created LDAPProvider
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLDAPProvider <- function(DisableReferrals = NULL, DomainName = NULL, GroupBaseDN = NULL, GroupFilter = NULL, GroupMemberFilter = NULL, Host = NULL, IgnoreCertificationErrors = NULL, Name = NULL, Port = NULL, Protocol = NULL, SearchBaseDN = NULL, SearchFilter = NULL, SearchUserDN = NULL, UsernameAttribute = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "LDAPProvider", body = list(DataObject = body), searchFields = append("LDAPProviderID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LDAPProvider
	#'
	#' This function modifies a LDAPProvider
	#' @param fieldNames The field values to give the modified LDAPProvider. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified LDAPProvider
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLDAPProvider <- function(LDAPProviderID, DisableReferrals = NULL, DomainName = NULL, GroupBaseDN = NULL, GroupFilter = NULL, GroupMemberFilter = NULL, Host = NULL, IgnoreCertificationErrors = NULL, Name = NULL, Port = NULL, Protocol = NULL, SearchBaseDN = NULL, SearchFilter = NULL, SearchUserDN = NULL, UsernameAttribute = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "LDAPProvider", objectId = LDAPProviderID, body = list(DataObject = body), searchFields = append("LDAPProviderID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserAuthenticationMethods
	#'
	#' This function returns a dataframe or json object of UserAuthenticationMethods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserAuthenticationMethods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserAuthenticationMethods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserAuthenticationMethod') to get more field paths.
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
	#' @concept Security
	#' @return A list of UserAuthenticationMethods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserAuthenticationMethods <- function(searchConditionsList = NULL, AuthenticationMethodID = F, CreatedTime = F, ModifiedTime = F, ProviderUserIdentity = F, UserAuthenticationMethodID = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "UserAuthenticationMethod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserAuthenticationMethod
	#'
	#' This function returns a dataframe or json object of an UserAuthenticationMethod
	#' @param UserAuthenticationMethodID The ID of the UserAuthenticationMethod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserAuthenticationMethod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserAuthenticationMethod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserAuthenticationMethod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of UserAuthenticationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserAuthenticationMethod <- function(UserAuthenticationMethodID, AuthenticationMethodID = F, CreatedTime = F, ModifiedTime = F, ProviderUserIdentity = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserAuthenticationMethodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "UserAuthenticationMethod", objectId = UserAuthenticationMethodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserAuthenticationMethod
	#'
	#' This function deletes an UserAuthenticationMethod
	#' @param UserAuthenticationMethodID The ID of the UserAuthenticationMethod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The UserAuthenticationMethodID of the deleted UserAuthenticationMethod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserAuthenticationMethod <- function(UserAuthenticationMethodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "UserAuthenticationMethod", objectId = UserAuthenticationMethodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserAuthenticationMethod
	#'
	#' This function creates an UserAuthenticationMethod
	#' @param fieldNames The field values to give the created UserAuthenticationMethod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created UserAuthenticationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserAuthenticationMethod <- function(AuthenticationMethodID = NULL, ProviderUserIdentity = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "UserAuthenticationMethod", body = list(DataObject = body), searchFields = append("UserAuthenticationMethodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserAuthenticationMethod
	#'
	#' This function modifies an UserAuthenticationMethod
	#' @param fieldNames The field values to give the modified UserAuthenticationMethod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified UserAuthenticationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserAuthenticationMethod <- function(UserAuthenticationMethodID, AuthenticationMethodID = NULL, ProviderUserIdentity = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "UserAuthenticationMethod", objectId = UserAuthenticationMethodID, body = list(DataObject = body), searchFields = append("UserAuthenticationMethodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserProfileData
	#'
	#' This function returns a dataframe or json object of UserProfileData
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserProfileData. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserProfileData.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserProfileData') to get more field paths.
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
	#' @concept Security
	#' @return A list of UserProfileData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserProfileData <- function(searchConditionsList = NULL, BrowseObject = F, CreatedTime = F, ModifiedTime = F, Module = F, RelatedRecord = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserProfileDataID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "UserProfileData", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserProfileData
	#'
	#' This function returns a dataframe or json object of an UserProfileData
	#' @param UserProfileDataID The ID of the UserProfileData to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserProfileData. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserProfileData.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserProfileData') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of UserProfileData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserProfileData <- function(UserProfileDataID, BrowseObject = F, CreatedTime = F, ModifiedTime = F, Module = F, RelatedRecord = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserProfileDataID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "UserProfileData", objectId = UserProfileDataID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserProfileData
	#'
	#' This function deletes an UserProfileData
	#' @param UserProfileDataID The ID of the UserProfileData to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The UserProfileDataID of the deleted UserProfileData.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserProfileData <- function(UserProfileDataID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "UserProfileData", objectId = UserProfileDataID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserProfileData
	#'
	#' This function creates an UserProfileData
	#' @param fieldNames The field values to give the created UserProfileData. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created UserProfileData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserProfileData <- function(BrowseObject = NULL, Module = NULL, RelatedRecord = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "UserProfileData", body = list(DataObject = body), searchFields = append("UserProfileDataID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserProfileData
	#'
	#' This function modifies an UserProfileData
	#' @param fieldNames The field values to give the modified UserProfileData. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified UserProfileData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserProfileData <- function(UserProfileDataID, BrowseObject = NULL, Module = NULL, RelatedRecord = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "UserProfileData", objectId = UserProfileDataID, body = list(DataObject = body), searchFields = append("UserProfileDataID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Products
	#'
	#' This function returns a dataframe or json object of Products
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Products. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Products.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Product') to get more field paths.
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
	#' @concept Security
	#' @return A list of Products
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listProducts <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, Name = F, ProductID = F, RMSID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "Product", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Product
	#'
	#' This function returns a dataframe or json object of a Product
	#' @param ProductID The ID of the Product to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Product. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Product.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Product') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of Product
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getProduct <- function(ProductID, CreatedTime = F, ModifiedTime = F, Name = F, RMSID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ProductID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "Product", objectId = ProductID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Product
	#'
	#' This function deletes a Product
	#' @param ProductID The ID of the Product to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The ProductID of the deleted Product.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteProduct <- function(ProductID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "Product", objectId = ProductID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Product
	#'
	#' This function creates a Product
	#' @param fieldNames The field values to give the created Product. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created Product
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createProduct <- function(Name = NULL, RMSID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "Product", body = list(DataObject = body), searchFields = append("ProductID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Product
	#'
	#' This function modifies a Product
	#' @param fieldNames The field values to give the modified Product. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified Product
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyProduct <- function(ProductID, Name = NULL, RMSID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "Product", objectId = ProductID, body = list(DataObject = body), searchFields = append("ProductID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Roles
	#'
	#' This function returns a dataframe or json object of Roles
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Roles. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Roles.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Role') to get more field paths.
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
	#' @concept Security
	#' @return A list of Roles
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoles <- function(searchConditionsList = NULL, AuthenticationRoleID = F, CreatedTime = F, Description = F, DocumentationPersona = F, IsActive = F, ModifiedTime = F, MultifactorAuthenticationID = F, Name = F, ReportCount = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "Role", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Role
	#'
	#' This function returns a dataframe or json object of a Role
	#' @param RoleID The ID of the Role to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Role. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Role.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Role') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of Role
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRole <- function(RoleID, AuthenticationRoleID = F, CreatedTime = F, Description = F, DocumentationPersona = F, IsActive = F, ModifiedTime = F, MultifactorAuthenticationID = F, Name = F, ReportCount = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "Role", objectId = RoleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Role
	#'
	#' This function deletes a Role
	#' @param RoleID The ID of the Role to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The RoleID of the deleted Role.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRole <- function(RoleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "Role", objectId = RoleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Role
	#'
	#' This function creates a Role
	#' @param fieldNames The field values to give the created Role. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created Role
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRole <- function(AuthenticationRoleID = NULL, Description = NULL, DocumentationPersona = NULL, IsActive = NULL, MultifactorAuthenticationID = NULL, Name = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "Role", body = list(DataObject = body), searchFields = append("RoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Role
	#'
	#' This function modifies a Role
	#' @param fieldNames The field values to give the modified Role. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified Role
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRole <- function(RoleID, AuthenticationRoleID = NULL, Description = NULL, DocumentationPersona = NULL, IsActive = NULL, MultifactorAuthenticationID = NULL, Name = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "Role", objectId = RoleID, body = list(DataObject = body), searchFields = append("RoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RoleModulePaths
	#'
	#' This function returns a dataframe or json object of RoleModulePaths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoleModulePaths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoleModulePaths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoleModulePath') to get more field paths.
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
	#' @concept Security
	#' @return A list of RoleModulePaths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoleModulePaths <- function(searchConditionsList = NULL, AllowCreate = F, AllowDelete = F, AllowMassCreate = F, AllowMassDelete = F, AllowMassUpdate = F, AllowRead = F, AllowUpdate = F, CreatedTime = F, ModifiedTime = F, ModulePath = F, RoleID = F, RoleModulePathID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "RoleModulePath", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RoleModulePath
	#'
	#' This function returns a dataframe or json object of a RoleModulePath
	#' @param RoleModulePathID The ID of the RoleModulePath to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoleModulePath. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoleModulePath.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoleModulePath') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of RoleModulePath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoleModulePath <- function(RoleModulePathID, AllowCreate = F, AllowDelete = F, AllowMassCreate = F, AllowMassDelete = F, AllowMassUpdate = F, AllowRead = F, AllowUpdate = F, CreatedTime = F, ModifiedTime = F, ModulePath = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoleModulePathID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "RoleModulePath", objectId = RoleModulePathID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RoleModulePath
	#'
	#' This function deletes a RoleModulePath
	#' @param RoleModulePathID The ID of the RoleModulePath to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The RoleModulePathID of the deleted RoleModulePath.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoleModulePath <- function(RoleModulePathID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "RoleModulePath", objectId = RoleModulePathID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RoleModulePath
	#'
	#' This function creates a RoleModulePath
	#' @param fieldNames The field values to give the created RoleModulePath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created RoleModulePath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoleModulePath <- function(AllowCreate = NULL, AllowDelete = NULL, AllowMassCreate = NULL, AllowMassDelete = NULL, AllowMassUpdate = NULL, AllowRead = NULL, AllowUpdate = NULL, ModulePath = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "RoleModulePath", body = list(DataObject = body), searchFields = append("RoleModulePathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RoleModulePath
	#'
	#' This function modifies a RoleModulePath
	#' @param fieldNames The field values to give the modified RoleModulePath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified RoleModulePath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoleModulePath <- function(RoleModulePathID, AllowCreate = NULL, AllowDelete = NULL, AllowMassCreate = NULL, AllowMassDelete = NULL, AllowMassUpdate = NULL, AllowRead = NULL, AllowUpdate = NULL, ModulePath = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "RoleModulePath", objectId = RoleModulePathID, body = list(DataObject = body), searchFields = append("RoleModulePathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Groups
	#'
	#' This function returns a dataframe or json object of Groups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Groups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Groups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Group') to get more field paths.
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
	#' @concept Security
	#' @return A list of Groups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGroups <- function(searchConditionsList = NULL, AutoAddToUserType = F, CreatedTime = F, Description = F, EdFiStaffClassificationID = F, GroupID = F, IsActive = F, ModifiedTime = F, Name = F, NameDescription = F, PositionTitleOverride = F, UsedForEmployeeAccess = F, UsedForFamilyAccess = F, UsedForNewStudentEnrollment = F, UsedForStudentAccess = F, UsedForTeacherAccess = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "Group", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Group
	#'
	#' This function returns a dataframe or json object of a Group
	#' @param GroupID The ID of the Group to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Group. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Group.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Group') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of Group
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGroup <- function(GroupID, AutoAddToUserType = F, CreatedTime = F, Description = F, EdFiStaffClassificationID = F, IsActive = F, ModifiedTime = F, Name = F, NameDescription = F, PositionTitleOverride = F, UsedForEmployeeAccess = F, UsedForFamilyAccess = F, UsedForNewStudentEnrollment = F, UsedForStudentAccess = F, UsedForTeacherAccess = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "Group", objectId = GroupID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Group
	#'
	#' This function deletes a Group
	#' @param GroupID The ID of the Group to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The GroupID of the deleted Group.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGroup <- function(GroupID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "Group", objectId = GroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Group
	#'
	#' This function creates a Group
	#' @param fieldNames The field values to give the created Group. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created Group
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGroup <- function(AutoAddToUserType = NULL, Description = NULL, EdFiStaffClassificationID = NULL, IsActive = NULL, Name = NULL, PositionTitleOverride = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "Group", body = list(DataObject = body), searchFields = append("GroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Group
	#'
	#' This function modifies a Group
	#' @param fieldNames The field values to give the modified Group. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified Group
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGroup <- function(GroupID, AutoAddToUserType = NULL, Description = NULL, EdFiStaffClassificationID = NULL, IsActive = NULL, Name = NULL, PositionTitleOverride = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "Group", objectId = GroupID, body = list(DataObject = body), searchFields = append("GroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GroupMemberships
	#'
	#' This function returns a dataframe or json object of GroupMemberships
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GroupMemberships. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GroupMemberships.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GroupMembership') to get more field paths.
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
	#' @concept Security
	#' @return A list of GroupMemberships
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGroupMemberships <- function(searchConditionsList = NULL, CreatedTime = F, EntityID = F, ExternalUniqueIdentifier = F, GroupIDParent = F, GroupMembershipID = F, MembershipSource = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDMember = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "GroupMembership", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GroupMembership
	#'
	#' This function returns a dataframe or json object of a GroupMembership
	#' @param GroupMembershipID The ID of the GroupMembership to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GroupMembership. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GroupMembership.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GroupMembership') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of GroupMembership
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGroupMembership <- function(GroupMembershipID, CreatedTime = F, EntityID = F, ExternalUniqueIdentifier = F, GroupIDParent = F, MembershipSource = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDMember = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GroupMembershipID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "GroupMembership", objectId = GroupMembershipID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GroupMembership
	#'
	#' This function deletes a GroupMembership
	#' @param GroupMembershipID The ID of the GroupMembership to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The GroupMembershipID of the deleted GroupMembership.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGroupMembership <- function(GroupMembershipID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "GroupMembership", objectId = GroupMembershipID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GroupMembership
	#'
	#' This function creates a GroupMembership
	#' @param fieldNames The field values to give the created GroupMembership. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created GroupMembership
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGroupMembership <- function(EntityID = NULL, ExternalUniqueIdentifier = NULL, GroupIDParent = NULL, MembershipSource = NULL, UserIDMember = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "GroupMembership", body = list(DataObject = body), searchFields = append("GroupMembershipID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GroupMembership
	#'
	#' This function modifies a GroupMembership
	#' @param fieldNames The field values to give the modified GroupMembership. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified GroupMembership
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGroupMembership <- function(GroupMembershipID, EntityID = NULL, ExternalUniqueIdentifier = NULL, GroupIDParent = NULL, MembershipSource = NULL, UserIDMember = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "GroupMembership", objectId = GroupMembershipID, body = list(DataObject = body), searchFields = append("GroupMembershipID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GroupRoles
	#'
	#' This function returns a dataframe or json object of GroupRoles
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GroupRoles. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GroupRoles.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GroupRole') to get more field paths.
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
	#' @concept Security
	#' @return A list of GroupRoles
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGroupRoles <- function(searchConditionsList = NULL, CreatedTime = F, GroupID = F, GroupRoleID = F, ModifiedTime = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "GroupRole", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GroupRole
	#'
	#' This function returns a dataframe or json object of a GroupRole
	#' @param GroupRoleID The ID of the GroupRole to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GroupRole. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GroupRole.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GroupRole') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of GroupRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGroupRole <- function(GroupRoleID, CreatedTime = F, GroupID = F, ModifiedTime = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GroupRoleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "GroupRole", objectId = GroupRoleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GroupRole
	#'
	#' This function deletes a GroupRole
	#' @param GroupRoleID The ID of the GroupRole to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The GroupRoleID of the deleted GroupRole.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGroupRole <- function(GroupRoleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "GroupRole", objectId = GroupRoleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GroupRole
	#'
	#' This function creates a GroupRole
	#' @param fieldNames The field values to give the created GroupRole. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created GroupRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGroupRole <- function(GroupID = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "GroupRole", body = list(DataObject = body), searchFields = append("GroupRoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GroupRole
	#'
	#' This function modifies a GroupRole
	#' @param fieldNames The field values to give the modified GroupRole. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified GroupRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGroupRole <- function(GroupRoleID, GroupID = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "GroupRole", objectId = GroupRoleID, body = list(DataObject = body), searchFields = append("GroupRoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RolePortals
	#'
	#' This function returns a dataframe or json object of RolePortals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RolePortals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RolePortals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RolePortal') to get more field paths.
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
	#' @concept Security
	#' @return A list of RolePortals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRolePortals <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, Portal = F, RoleID = F, RolePortalID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "RolePortal", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RolePortal
	#'
	#' This function returns a dataframe or json object of a RolePortal
	#' @param RolePortalID The ID of the RolePortal to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RolePortal. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RolePortal.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RolePortal') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of RolePortal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRolePortal <- function(RolePortalID, CreatedTime = F, ModifiedTime = F, Portal = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RolePortalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "RolePortal", objectId = RolePortalID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RolePortal
	#'
	#' This function deletes a RolePortal
	#' @param RolePortalID The ID of the RolePortal to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The RolePortalID of the deleted RolePortal.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRolePortal <- function(RolePortalID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "RolePortal", objectId = RolePortalID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RolePortal
	#'
	#' This function creates a RolePortal
	#' @param fieldNames The field values to give the created RolePortal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created RolePortal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRolePortal <- function(Portal = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "RolePortal", body = list(DataObject = body), searchFields = append("RolePortalID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RolePortal
	#'
	#' This function modifies a RolePortal
	#' @param fieldNames The field values to give the modified RolePortal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified RolePortal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRolePortal <- function(RolePortalID, Portal = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "RolePortal", objectId = RolePortalID, body = list(DataObject = body), searchFields = append("RolePortalID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserImports
	#'
	#' This function returns a dataframe or json object of UserImports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserImports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserImports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserImport') to get more field paths.
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
	#' @concept Security
	#' @return A list of UserImports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserImports <- function(searchConditionsList = NULL, AboveChangeThreshold = F, AboveDeleteThreshold = F, CreatedTime = F, DateExecuted = F, HasErrors = F, MediaID = F, ModifiedTime = F, Status = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserImportID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "UserImport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserImport
	#'
	#' This function returns a dataframe or json object of an UserImport
	#' @param UserImportID The ID of the UserImport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserImport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserImport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserImport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of UserImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserImport <- function(UserImportID, AboveChangeThreshold = F, AboveDeleteThreshold = F, CreatedTime = F, DateExecuted = F, HasErrors = F, MediaID = F, ModifiedTime = F, Status = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserImportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "UserImport", objectId = UserImportID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserImport
	#'
	#' This function deletes an UserImport
	#' @param UserImportID The ID of the UserImport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The UserImportID of the deleted UserImport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserImport <- function(UserImportID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "UserImport", objectId = UserImportID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserImport
	#'
	#' This function creates an UserImport
	#' @param fieldNames The field values to give the created UserImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created UserImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserImport <- function(AboveChangeThreshold = NULL, AboveDeleteThreshold = NULL, DateExecuted = NULL, MediaID = NULL, Status = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "UserImport", body = list(DataObject = body), searchFields = append("UserImportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserImport
	#'
	#' This function modifies an UserImport
	#' @param fieldNames The field values to give the modified UserImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified UserImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserImport <- function(UserImportID, AboveChangeThreshold = NULL, AboveDeleteThreshold = NULL, DateExecuted = NULL, MediaID = NULL, Status = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "UserImport", objectId = UserImportID, body = list(DataObject = body), searchFields = append("UserImportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserImportResultErrors
	#'
	#' This function returns a dataframe or json object of UserImportResultErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserImportResultErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserImportResultErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserImportResultError') to get more field paths.
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
	#' @concept Security
	#' @return A list of UserImportResultErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserImportResultErrors <- function(searchConditionsList = NULL, CreatedTime = F, ErrorMessage = F, FieldName = F, FromPreview = F, ModifiedTime = F, ObjectName = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserImportResultErrorID = F, UserImportResultID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "UserImportResultError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserImportResultError
	#'
	#' This function returns a dataframe or json object of an UserImportResultError
	#' @param UserImportResultErrorID The ID of the UserImportResultError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserImportResultError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserImportResultError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserImportResultError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of UserImportResultError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserImportResultError <- function(UserImportResultErrorID, CreatedTime = F, ErrorMessage = F, FieldName = F, FromPreview = F, ModifiedTime = F, ObjectName = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserImportResultID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserImportResultErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "UserImportResultError", objectId = UserImportResultErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserImportResultError
	#'
	#' This function deletes an UserImportResultError
	#' @param UserImportResultErrorID The ID of the UserImportResultError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The UserImportResultErrorID of the deleted UserImportResultError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserImportResultError <- function(UserImportResultErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "UserImportResultError", objectId = UserImportResultErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserImportResultError
	#'
	#' This function creates an UserImportResultError
	#' @param fieldNames The field values to give the created UserImportResultError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created UserImportResultError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserImportResultError <- function(ErrorMessage = NULL, FieldName = NULL, FromPreview = NULL, ObjectName = NULL, UserImportResultID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "UserImportResultError", body = list(DataObject = body), searchFields = append("UserImportResultErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserImportResultError
	#'
	#' This function modifies an UserImportResultError
	#' @param fieldNames The field values to give the modified UserImportResultError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified UserImportResultError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserImportResultError <- function(UserImportResultErrorID, ErrorMessage = NULL, FieldName = NULL, FromPreview = NULL, ObjectName = NULL, UserImportResultID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "UserImportResultError", objectId = UserImportResultErrorID, body = list(DataObject = body), searchFields = append("UserImportResultErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserImportResults
	#'
	#' This function returns a dataframe or json object of UserImportResults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserImportResults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserImportResults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserImportResult') to get more field paths.
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
	#' @concept Security
	#' @return A list of UserImportResults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserImportResults <- function(searchConditionsList = NULL, CreatedTime = F, EntityCode = F, GroupMembershipChangeType = F, GroupName = F, HasErrors = F, IsStaff = F, LineNumber = F, ModifiedTime = F, NameChangeType = F, SchoolYear = F, StaffChangeType = F, UserChangeType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserImportID = F, UserImportResultID = F, Username = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "UserImportResult", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserImportResult
	#'
	#' This function returns a dataframe or json object of an UserImportResult
	#' @param UserImportResultID The ID of the UserImportResult to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserImportResult. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserImportResult.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserImportResult') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of UserImportResult
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserImportResult <- function(UserImportResultID, CreatedTime = F, EntityCode = F, GroupMembershipChangeType = F, GroupName = F, HasErrors = F, IsStaff = F, LineNumber = F, ModifiedTime = F, NameChangeType = F, SchoolYear = F, StaffChangeType = F, UserChangeType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserImportID = F, Username = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserImportResultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "UserImportResult", objectId = UserImportResultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserImportResult
	#'
	#' This function deletes an UserImportResult
	#' @param UserImportResultID The ID of the UserImportResult to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The UserImportResultID of the deleted UserImportResult.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserImportResult <- function(UserImportResultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "UserImportResult", objectId = UserImportResultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserImportResult
	#'
	#' This function creates an UserImportResult
	#' @param fieldNames The field values to give the created UserImportResult. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created UserImportResult
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserImportResult <- function(EntityCode = NULL, GroupMembershipChangeType = NULL, GroupName = NULL, IsStaff = NULL, LineNumber = NULL, NameChangeType = NULL, SchoolYear = NULL, StaffChangeType = NULL, UserChangeType = NULL, UserImportID = NULL, Username = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "UserImportResult", body = list(DataObject = body), searchFields = append("UserImportResultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserImportResult
	#'
	#' This function modifies an UserImportResult
	#' @param fieldNames The field values to give the modified UserImportResult. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified UserImportResult
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserImportResult <- function(UserImportResultID, EntityCode = NULL, GroupMembershipChangeType = NULL, GroupName = NULL, IsStaff = NULL, LineNumber = NULL, NameChangeType = NULL, SchoolYear = NULL, StaffChangeType = NULL, UserChangeType = NULL, UserImportID = NULL, Username = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "UserImportResult", objectId = UserImportResultID, body = list(DataObject = body), searchFields = append("UserImportResultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SecurityLocationMenuSecurityItems
	#'
	#' This function returns a dataframe or json object of SecurityLocationMenuSecurityItems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityLocationMenuSecurityItems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityLocationMenuSecurityItems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityLocationMenuSecurityItem') to get more field paths.
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
	#' @concept Security
	#' @return A list of SecurityLocationMenuSecurityItems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSecurityLocationMenuSecurityItems <- function(searchConditionsList = NULL, CreatedTime = F, MenuSecurityItemID = F, ModifiedTime = F, SecurityLocationID = F, SecurityLocationMenuSecurityItemID = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "SecurityLocationMenuSecurityItem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SecurityLocationMenuSecurityItem
	#'
	#' This function returns a dataframe or json object of a SecurityLocationMenuSecurityItem
	#' @param SecurityLocationMenuSecurityItemID The ID of the SecurityLocationMenuSecurityItem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityLocationMenuSecurityItem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityLocationMenuSecurityItem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityLocationMenuSecurityItem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of SecurityLocationMenuSecurityItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSecurityLocationMenuSecurityItem <- function(SecurityLocationMenuSecurityItemID, CreatedTime = F, MenuSecurityItemID = F, ModifiedTime = F, SecurityLocationID = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SecurityLocationMenuSecurityItemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "SecurityLocationMenuSecurityItem", objectId = SecurityLocationMenuSecurityItemID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SecurityLocationMenuSecurityItem
	#'
	#' This function deletes a SecurityLocationMenuSecurityItem
	#' @param SecurityLocationMenuSecurityItemID The ID of the SecurityLocationMenuSecurityItem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The SecurityLocationMenuSecurityItemID of the deleted SecurityLocationMenuSecurityItem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSecurityLocationMenuSecurityItem <- function(SecurityLocationMenuSecurityItemID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "SecurityLocationMenuSecurityItem", objectId = SecurityLocationMenuSecurityItemID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SecurityLocationMenuSecurityItem
	#'
	#' This function creates a SecurityLocationMenuSecurityItem
	#' @param fieldNames The field values to give the created SecurityLocationMenuSecurityItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created SecurityLocationMenuSecurityItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSecurityLocationMenuSecurityItem <- function(MenuSecurityItemID = NULL, SecurityLocationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "SecurityLocationMenuSecurityItem", body = list(DataObject = body), searchFields = append("SecurityLocationMenuSecurityItemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SecurityLocationMenuSecurityItem
	#'
	#' This function modifies a SecurityLocationMenuSecurityItem
	#' @param fieldNames The field values to give the modified SecurityLocationMenuSecurityItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified SecurityLocationMenuSecurityItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySecurityLocationMenuSecurityItem <- function(SecurityLocationMenuSecurityItemID, MenuSecurityItemID = NULL, SecurityLocationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "SecurityLocationMenuSecurityItem", objectId = SecurityLocationMenuSecurityItemID, body = list(DataObject = body), searchFields = append("SecurityLocationMenuSecurityItemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SecurityLocations
	#'
	#' This function returns a dataframe or json object of SecurityLocations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityLocations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityLocations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityLocation') to get more field paths.
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
	#' @concept Security
	#' @return A list of SecurityLocations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSecurityLocations <- function(searchConditionsList = NULL, AttachmentTypeGUID = F, CanAllowCreate = F, CanAllowDelete = F, CanAllowMassCreate = F, CanAllowMassDelete = F, CanAllowMassUpdate = F, CanAllowRead = F, CanAllowUpdate = F, CreatedTime = F, MobileCanAllowCreate = F, MobileCanAllowDelete = F, MobileCanAllowMassCreate = F, MobileCanAllowMassDelete = F, MobileCanAllowMassUpdate = F, MobileCanAllowRead = F, MobileCanAllowUpdate = F, ModifiedTime = F, ModulePathID = F, Path = F, Portal = F, ReportID = F, SecurityLocationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "SecurityLocation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SecurityLocation
	#'
	#' This function returns a dataframe or json object of a SecurityLocation
	#' @param SecurityLocationID The ID of the SecurityLocation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityLocation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityLocation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityLocation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of SecurityLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSecurityLocation <- function(SecurityLocationID, AttachmentTypeGUID = F, CanAllowCreate = F, CanAllowDelete = F, CanAllowMassCreate = F, CanAllowMassDelete = F, CanAllowMassUpdate = F, CanAllowRead = F, CanAllowUpdate = F, CreatedTime = F, MobileCanAllowCreate = F, MobileCanAllowDelete = F, MobileCanAllowMassCreate = F, MobileCanAllowMassDelete = F, MobileCanAllowMassUpdate = F, MobileCanAllowRead = F, MobileCanAllowUpdate = F, ModifiedTime = F, ModulePathID = F, Path = F, Portal = F, ReportID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SecurityLocationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "SecurityLocation", objectId = SecurityLocationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SecurityLocation
	#'
	#' This function deletes a SecurityLocation
	#' @param SecurityLocationID The ID of the SecurityLocation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The SecurityLocationID of the deleted SecurityLocation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSecurityLocation <- function(SecurityLocationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "SecurityLocation", objectId = SecurityLocationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SecurityLocation
	#'
	#' This function creates a SecurityLocation
	#' @param fieldNames The field values to give the created SecurityLocation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created SecurityLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSecurityLocation <- function(AttachmentTypeGUID = NULL, CanAllowCreate = NULL, CanAllowDelete = NULL, CanAllowMassCreate = NULL, CanAllowMassDelete = NULL, CanAllowMassUpdate = NULL, CanAllowRead = NULL, CanAllowUpdate = NULL, MobileCanAllowCreate = NULL, MobileCanAllowDelete = NULL, MobileCanAllowMassCreate = NULL, MobileCanAllowMassDelete = NULL, MobileCanAllowMassUpdate = NULL, MobileCanAllowRead = NULL, MobileCanAllowUpdate = NULL, ModulePathID = NULL, Portal = NULL, ReportID = NULL, WorkflowID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "SecurityLocation", body = list(DataObject = body), searchFields = append("SecurityLocationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SecurityLocation
	#'
	#' This function modifies a SecurityLocation
	#' @param fieldNames The field values to give the modified SecurityLocation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified SecurityLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySecurityLocation <- function(SecurityLocationID, AttachmentTypeGUID = NULL, CanAllowCreate = NULL, CanAllowDelete = NULL, CanAllowMassCreate = NULL, CanAllowMassDelete = NULL, CanAllowMassUpdate = NULL, CanAllowRead = NULL, CanAllowUpdate = NULL, MobileCanAllowCreate = NULL, MobileCanAllowDelete = NULL, MobileCanAllowMassCreate = NULL, MobileCanAllowMassDelete = NULL, MobileCanAllowMassUpdate = NULL, MobileCanAllowRead = NULL, MobileCanAllowUpdate = NULL, ModulePathID = NULL, Portal = NULL, ReportID = NULL, WorkflowID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "SecurityLocation", objectId = SecurityLocationID, body = list(DataObject = body), searchFields = append("SecurityLocationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RoleMenuSecurityItems
	#'
	#' This function returns a dataframe or json object of RoleMenuSecurityItems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoleMenuSecurityItems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoleMenuSecurityItems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoleMenuSecurityItem') to get more field paths.
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
	#' @concept Security
	#' @return A list of RoleMenuSecurityItems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoleMenuSecurityItems <- function(searchConditionsList = NULL, AllowCreate = F, AllowDelete = F, AllowMassCreate = F, AllowMassDelete = F, AllowMassUpdate = F, AllowRead = F, AllowUpdate = F, CreatedTime = F, MenuSecurityItemID = F, ModifiedTime = F, RoleID = F, RoleMenuSecurityItemID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "RoleMenuSecurityItem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RoleMenuSecurityItem
	#'
	#' This function returns a dataframe or json object of a RoleMenuSecurityItem
	#' @param RoleMenuSecurityItemID The ID of the RoleMenuSecurityItem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoleMenuSecurityItem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoleMenuSecurityItem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoleMenuSecurityItem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of RoleMenuSecurityItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoleMenuSecurityItem <- function(RoleMenuSecurityItemID, AllowCreate = F, AllowDelete = F, AllowMassCreate = F, AllowMassDelete = F, AllowMassUpdate = F, AllowRead = F, AllowUpdate = F, CreatedTime = F, MenuSecurityItemID = F, ModifiedTime = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoleMenuSecurityItemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "RoleMenuSecurityItem", objectId = RoleMenuSecurityItemID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RoleMenuSecurityItem
	#'
	#' This function deletes a RoleMenuSecurityItem
	#' @param RoleMenuSecurityItemID The ID of the RoleMenuSecurityItem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The RoleMenuSecurityItemID of the deleted RoleMenuSecurityItem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoleMenuSecurityItem <- function(RoleMenuSecurityItemID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "RoleMenuSecurityItem", objectId = RoleMenuSecurityItemID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RoleMenuSecurityItem
	#'
	#' This function creates a RoleMenuSecurityItem
	#' @param fieldNames The field values to give the created RoleMenuSecurityItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created RoleMenuSecurityItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoleMenuSecurityItem <- function(AllowCreate = NULL, AllowDelete = NULL, AllowMassCreate = NULL, AllowMassDelete = NULL, AllowMassUpdate = NULL, AllowRead = NULL, AllowUpdate = NULL, MenuSecurityItemID = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "RoleMenuSecurityItem", body = list(DataObject = body), searchFields = append("RoleMenuSecurityItemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RoleMenuSecurityItem
	#'
	#' This function modifies a RoleMenuSecurityItem
	#' @param fieldNames The field values to give the modified RoleMenuSecurityItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified RoleMenuSecurityItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoleMenuSecurityItem <- function(RoleMenuSecurityItemID, AllowCreate = NULL, AllowDelete = NULL, AllowMassCreate = NULL, AllowMassDelete = NULL, AllowMassUpdate = NULL, AllowRead = NULL, AllowUpdate = NULL, MenuSecurityItemID = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "RoleMenuSecurityItem", objectId = RoleMenuSecurityItemID, body = list(DataObject = body), searchFields = append("RoleMenuSecurityItemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MenuSecurityItems
	#'
	#' This function returns a dataframe or json object of MenuSecurityItems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MenuSecurityItems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MenuSecurityItems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MenuSecurityItem') to get more field paths.
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
	#' @concept Security
	#' @return A list of MenuSecurityItems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMenuSecurityItems <- function(searchConditionsList = NULL, CreatedTime = F, MenuScreenID = F, MenuSecurityItemID = F, ModifiedTime = F, ProfileScreenID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "MenuSecurityItem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MenuSecurityItem
	#'
	#' This function returns a dataframe or json object of a MenuSecurityItem
	#' @param MenuSecurityItemID The ID of the MenuSecurityItem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MenuSecurityItem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MenuSecurityItem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MenuSecurityItem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of MenuSecurityItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMenuSecurityItem <- function(MenuSecurityItemID, CreatedTime = F, MenuScreenID = F, ModifiedTime = F, ProfileScreenID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MenuSecurityItemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "MenuSecurityItem", objectId = MenuSecurityItemID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MenuSecurityItem
	#'
	#' This function deletes a MenuSecurityItem
	#' @param MenuSecurityItemID The ID of the MenuSecurityItem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The MenuSecurityItemID of the deleted MenuSecurityItem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMenuSecurityItem <- function(MenuSecurityItemID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "MenuSecurityItem", objectId = MenuSecurityItemID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MenuSecurityItem
	#'
	#' This function creates a MenuSecurityItem
	#' @param fieldNames The field values to give the created MenuSecurityItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created MenuSecurityItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMenuSecurityItem <- function(MenuScreenID = NULL, ProfileScreenID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "MenuSecurityItem", body = list(DataObject = body), searchFields = append("MenuSecurityItemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MenuSecurityItem
	#'
	#' This function modifies a MenuSecurityItem
	#' @param fieldNames The field values to give the modified MenuSecurityItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified MenuSecurityItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMenuSecurityItem <- function(MenuSecurityItemID, MenuScreenID = NULL, ProfileScreenID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "MenuSecurityItem", objectId = MenuSecurityItemID, body = list(DataObject = body), searchFields = append("MenuSecurityItemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempTeacherAccessSecurityUsers
	#'
	#' This function returns a dataframe or json object of TempTeacherAccessSecurityUsers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTeacherAccessSecurityUsers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTeacherAccessSecurityUsers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTeacherAccessSecurityUser') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempTeacherAccessSecurityUsers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempTeacherAccessSecurityUsers <- function(searchConditionsList = NULL, AddToTeacherAccess = F, AllowTeacherAccess = F, CreatedTime = F, DeleteUserAfterAudit = F, EmailAddress = F, ForUserCreation = F, Group = F, IsAuditTeacherAccessSecurity = F, IsException = F, IsSelected = F, ModifiedTime = F, Note = F, RemoveFromTeacherAccess = F, StaffID = F, StaffNameLFM = F, StaffNumber = F, TempTeacherAccessSecurityUserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempTeacherAccessSecurityUser", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempTeacherAccessSecurityUser
	#'
	#' This function returns a dataframe or json object of a TempTeacherAccessSecurityUser
	#' @param TempTeacherAccessSecurityUserID The ID of the TempTeacherAccessSecurityUser to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTeacherAccessSecurityUser. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTeacherAccessSecurityUser.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTeacherAccessSecurityUser') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempTeacherAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempTeacherAccessSecurityUser <- function(TempTeacherAccessSecurityUserID, AddToTeacherAccess = F, AllowTeacherAccess = F, CreatedTime = F, DeleteUserAfterAudit = F, EmailAddress = F, ForUserCreation = F, Group = F, IsAuditTeacherAccessSecurity = F, IsException = F, IsSelected = F, ModifiedTime = F, Note = F, RemoveFromTeacherAccess = F, StaffID = F, StaffNameLFM = F, StaffNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserName = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempTeacherAccessSecurityUserID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempTeacherAccessSecurityUser", objectId = TempTeacherAccessSecurityUserID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempTeacherAccessSecurityUser
	#'
	#' This function deletes a TempTeacherAccessSecurityUser
	#' @param TempTeacherAccessSecurityUserID The ID of the TempTeacherAccessSecurityUser to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempTeacherAccessSecurityUserID of the deleted TempTeacherAccessSecurityUser.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempTeacherAccessSecurityUser <- function(TempTeacherAccessSecurityUserID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempTeacherAccessSecurityUser", objectId = TempTeacherAccessSecurityUserID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempTeacherAccessSecurityUser
	#'
	#' This function creates a TempTeacherAccessSecurityUser
	#' @param fieldNames The field values to give the created TempTeacherAccessSecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempTeacherAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempTeacherAccessSecurityUser <- function(AllowTeacherAccess = NULL, EmailAddress = NULL, Group = NULL, StaffID = NULL, StaffNameLFM = NULL, StaffNumber = NULL, UserName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempTeacherAccessSecurityUser", body = list(DataObject = body), searchFields = append("TempTeacherAccessSecurityUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempTeacherAccessSecurityUser
	#'
	#' This function modifies a TempTeacherAccessSecurityUser
	#' @param fieldNames The field values to give the modified TempTeacherAccessSecurityUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempTeacherAccessSecurityUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempTeacherAccessSecurityUser <- function(TempTeacherAccessSecurityUserID, AllowTeacherAccess = NULL, EmailAddress = NULL, Group = NULL, StaffID = NULL, StaffNameLFM = NULL, StaffNumber = NULL, UserName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempTeacherAccessSecurityUser", objectId = TempTeacherAccessSecurityUserID, body = list(DataObject = body), searchFields = append("TempTeacherAccessSecurityUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GroupEntityAutoAdds
	#'
	#' This function returns a dataframe or json object of GroupEntityAutoAdds
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GroupEntityAutoAdds. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GroupEntityAutoAdds.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GroupEntityAutoAdd') to get more field paths.
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
	#' @concept Security
	#' @return A list of GroupEntityAutoAdds
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGroupEntityAutoAdds <- function(searchConditionsList = NULL, CreatedTime = F, EntityID = F, GroupEntityAutoAddID = F, GroupID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "GroupEntityAutoAdd", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GroupEntityAutoAdd
	#'
	#' This function returns a dataframe or json object of a GroupEntityAutoAdd
	#' @param GroupEntityAutoAddID The ID of the GroupEntityAutoAdd to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GroupEntityAutoAdd. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GroupEntityAutoAdd.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GroupEntityAutoAdd') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of GroupEntityAutoAdd
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGroupEntityAutoAdd <- function(GroupEntityAutoAddID, CreatedTime = F, EntityID = F, GroupID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GroupEntityAutoAddID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "GroupEntityAutoAdd", objectId = GroupEntityAutoAddID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GroupEntityAutoAdd
	#'
	#' This function deletes a GroupEntityAutoAdd
	#' @param GroupEntityAutoAddID The ID of the GroupEntityAutoAdd to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The GroupEntityAutoAddID of the deleted GroupEntityAutoAdd.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGroupEntityAutoAdd <- function(GroupEntityAutoAddID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "GroupEntityAutoAdd", objectId = GroupEntityAutoAddID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GroupEntityAutoAdd
	#'
	#' This function creates a GroupEntityAutoAdd
	#' @param fieldNames The field values to give the created GroupEntityAutoAdd. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created GroupEntityAutoAdd
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGroupEntityAutoAdd <- function(EntityID = NULL, GroupID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "GroupEntityAutoAdd", body = list(DataObject = body), searchFields = append("GroupEntityAutoAddID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GroupEntityAutoAdd
	#'
	#' This function modifies a GroupEntityAutoAdd
	#' @param fieldNames The field values to give the modified GroupEntityAutoAdd. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified GroupEntityAutoAdd
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGroupEntityAutoAdd <- function(GroupEntityAutoAddID, EntityID = NULL, GroupID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "GroupEntityAutoAdd", objectId = GroupEntityAutoAddID, body = list(DataObject = body), searchFields = append("GroupEntityAutoAddID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSecurityImportErrors
	#'
	#' This function returns a dataframe or json object of TempSecurityImportErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSecurityImportErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSecurityImportErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSecurityImportError') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempSecurityImportErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSecurityImportErrors <- function(searchConditionsList = NULL, CreatedTime = F, ErrorMessage = F, ErrorObject = F, ModifiedTime = F, TempSecurityImportErrorID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempSecurityImportError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSecurityImportError
	#'
	#' This function returns a dataframe or json object of a TempSecurityImportError
	#' @param TempSecurityImportErrorID The ID of the TempSecurityImportError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSecurityImportError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSecurityImportError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSecurityImportError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempSecurityImportError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSecurityImportError <- function(TempSecurityImportErrorID, CreatedTime = F, ErrorMessage = F, ErrorObject = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSecurityImportErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempSecurityImportError", objectId = TempSecurityImportErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSecurityImportError
	#'
	#' This function deletes a TempSecurityImportError
	#' @param TempSecurityImportErrorID The ID of the TempSecurityImportError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempSecurityImportErrorID of the deleted TempSecurityImportError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSecurityImportError <- function(TempSecurityImportErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempSecurityImportError", objectId = TempSecurityImportErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSecurityImportError
	#'
	#' This function creates a TempSecurityImportError
	#' @param fieldNames The field values to give the created TempSecurityImportError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempSecurityImportError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSecurityImportError <- function(ErrorMessage = NULL, ErrorObject = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempSecurityImportError", body = list(DataObject = body), searchFields = append("TempSecurityImportErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSecurityImportError
	#'
	#' This function modifies a TempSecurityImportError
	#' @param fieldNames The field values to give the modified TempSecurityImportError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempSecurityImportError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSecurityImportError <- function(TempSecurityImportErrorID, ErrorMessage = NULL, ErrorObject = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempSecurityImportError", objectId = TempSecurityImportErrorID, body = list(DataObject = body), searchFields = append("TempSecurityImportErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserProfileTabStatuses
	#'
	#' This function returns a dataframe or json object of UserProfileTabStatuses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserProfileTabStatuses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserProfileTabStatuses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserProfileTabStatus') to get more field paths.
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
	#' @concept Security
	#' @return A list of UserProfileTabStatuses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserProfileTabStatuses <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, OpenTabs = F, ProfileID = F, ProfileScreenIDLastTab = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserProfileTabStatusID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "UserProfileTabStatus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserProfileTabStatus
	#'
	#' This function returns a dataframe or json object of an UserProfileTabStatus
	#' @param UserProfileTabStatusID The ID of the UserProfileTabStatus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserProfileTabStatus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserProfileTabStatus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserProfileTabStatus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of UserProfileTabStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserProfileTabStatus <- function(UserProfileTabStatusID, CreatedTime = F, ModifiedTime = F, OpenTabs = F, ProfileID = F, ProfileScreenIDLastTab = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserProfileTabStatusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "UserProfileTabStatus", objectId = UserProfileTabStatusID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserProfileTabStatus
	#'
	#' This function deletes an UserProfileTabStatus
	#' @param UserProfileTabStatusID The ID of the UserProfileTabStatus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The UserProfileTabStatusID of the deleted UserProfileTabStatus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserProfileTabStatus <- function(UserProfileTabStatusID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "UserProfileTabStatus", objectId = UserProfileTabStatusID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserProfileTabStatus
	#'
	#' This function creates an UserProfileTabStatus
	#' @param fieldNames The field values to give the created UserProfileTabStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created UserProfileTabStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserProfileTabStatus <- function(ProfileID = NULL, ProfileScreenIDLastTab = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "UserProfileTabStatus", body = list(DataObject = body), searchFields = append("UserProfileTabStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserProfileTabStatus
	#'
	#' This function modifies an UserProfileTabStatus
	#' @param fieldNames The field values to give the modified UserProfileTabStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified UserProfileTabStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserProfileTabStatus <- function(UserProfileTabStatusID, ProfileID = NULL, ProfileScreenIDLastTab = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "UserProfileTabStatus", objectId = UserProfileTabStatusID, body = list(DataObject = body), searchFields = append("UserProfileTabStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserStudentCalendarPreferences
	#'
	#' This function returns a dataframe or json object of UserStudentCalendarPreferences
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserStudentCalendarPreferences. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserStudentCalendarPreferences.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserStudentCalendarPreference') to get more field paths.
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
	#' @concept Security
	#' @return A list of UserStudentCalendarPreferences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserStudentCalendarPreferences <- function(searchConditionsList = NULL, AbsencesAndTardiesBackgroundColor = F, AssignmentBackgroundColor = F, CreatedTime = F, ModifiedTime = F, ShowAbsencesAndTardies = F, ShowAssignments = F, ShowStudentActivityEvents = F, ShowStudentNotes = F, StudentActivityEventBackgroundColor = F, StudentID = F, StudentNoteBackgroundColor = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDOwner = F, UserStudentCalendarPreferenceID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "UserStudentCalendarPreference", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserStudentCalendarPreference
	#'
	#' This function returns a dataframe or json object of an UserStudentCalendarPreference
	#' @param UserStudentCalendarPreferenceID The ID of the UserStudentCalendarPreference to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserStudentCalendarPreference. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserStudentCalendarPreference.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserStudentCalendarPreference') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of UserStudentCalendarPreference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserStudentCalendarPreference <- function(UserStudentCalendarPreferenceID, AbsencesAndTardiesBackgroundColor = F, AssignmentBackgroundColor = F, CreatedTime = F, ModifiedTime = F, ShowAbsencesAndTardies = F, ShowAssignments = F, ShowStudentActivityEvents = F, ShowStudentNotes = F, StudentActivityEventBackgroundColor = F, StudentID = F, StudentNoteBackgroundColor = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDOwner = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserStudentCalendarPreferenceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "UserStudentCalendarPreference", objectId = UserStudentCalendarPreferenceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserStudentCalendarPreference
	#'
	#' This function deletes an UserStudentCalendarPreference
	#' @param UserStudentCalendarPreferenceID The ID of the UserStudentCalendarPreference to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The UserStudentCalendarPreferenceID of the deleted UserStudentCalendarPreference.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserStudentCalendarPreference <- function(UserStudentCalendarPreferenceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "UserStudentCalendarPreference", objectId = UserStudentCalendarPreferenceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserStudentCalendarPreference
	#'
	#' This function creates an UserStudentCalendarPreference
	#' @param fieldNames The field values to give the created UserStudentCalendarPreference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created UserStudentCalendarPreference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserStudentCalendarPreference <- function(AbsencesAndTardiesBackgroundColor = NULL, AssignmentBackgroundColor = NULL, ShowAbsencesAndTardies = NULL, ShowAssignments = NULL, ShowStudentActivityEvents = NULL, ShowStudentNotes = NULL, StudentActivityEventBackgroundColor = NULL, StudentID = NULL, StudentNoteBackgroundColor = NULL, UserIDOwner = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "UserStudentCalendarPreference", body = list(DataObject = body), searchFields = append("UserStudentCalendarPreferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserStudentCalendarPreference
	#'
	#' This function modifies an UserStudentCalendarPreference
	#' @param fieldNames The field values to give the modified UserStudentCalendarPreference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified UserStudentCalendarPreference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserStudentCalendarPreference <- function(UserStudentCalendarPreferenceID, AbsencesAndTardiesBackgroundColor = NULL, AssignmentBackgroundColor = NULL, ShowAbsencesAndTardies = NULL, ShowAssignments = NULL, ShowStudentActivityEvents = NULL, ShowStudentNotes = NULL, StudentActivityEventBackgroundColor = NULL, StudentID = NULL, StudentNoteBackgroundColor = NULL, UserIDOwner = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "UserStudentCalendarPreference", objectId = UserStudentCalendarPreferenceID, body = list(DataObject = body), searchFields = append("UserStudentCalendarPreferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSecurityImportPreviews
	#'
	#' This function returns a dataframe or json object of TempSecurityImportPreviews
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSecurityImportPreviews. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSecurityImportPreviews.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSecurityImportPreview') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempSecurityImportPreviews
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSecurityImportPreviews <- function(searchConditionsList = NULL, CreatedTime = F, Identifier = F, ModifiedTime = F, Object = F, TempSecurityImportPreviewID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempSecurityImportPreview", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSecurityImportPreview
	#'
	#' This function returns a dataframe or json object of a TempSecurityImportPreview
	#' @param TempSecurityImportPreviewID The ID of the TempSecurityImportPreview to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSecurityImportPreview. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSecurityImportPreview.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSecurityImportPreview') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempSecurityImportPreview
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSecurityImportPreview <- function(TempSecurityImportPreviewID, CreatedTime = F, Identifier = F, ModifiedTime = F, Object = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSecurityImportPreviewID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempSecurityImportPreview", objectId = TempSecurityImportPreviewID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSecurityImportPreview
	#'
	#' This function deletes a TempSecurityImportPreview
	#' @param TempSecurityImportPreviewID The ID of the TempSecurityImportPreview to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempSecurityImportPreviewID of the deleted TempSecurityImportPreview.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSecurityImportPreview <- function(TempSecurityImportPreviewID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempSecurityImportPreview", objectId = TempSecurityImportPreviewID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSecurityImportPreview
	#'
	#' This function creates a TempSecurityImportPreview
	#' @param fieldNames The field values to give the created TempSecurityImportPreview. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempSecurityImportPreview
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSecurityImportPreview <- function(Identifier = NULL, Object = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempSecurityImportPreview", body = list(DataObject = body), searchFields = append("TempSecurityImportPreviewID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSecurityImportPreview
	#'
	#' This function modifies a TempSecurityImportPreview
	#' @param fieldNames The field values to give the modified TempSecurityImportPreview. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempSecurityImportPreview
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSecurityImportPreview <- function(TempSecurityImportPreviewID, Identifier = NULL, Object = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempSecurityImportPreview", objectId = TempSecurityImportPreviewID, body = list(DataObject = body), searchFields = append("TempSecurityImportPreviewID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSecurityImportGroupMemberships
	#'
	#' This function returns a dataframe or json object of TempSecurityImportGroupMemberships
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSecurityImportGroupMemberships. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSecurityImportGroupMemberships.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSecurityImportGroupMembership') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempSecurityImportGroupMemberships
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSecurityImportGroupMemberships <- function(searchConditionsList = NULL, CreatedTime = F, EntityName = F, ExistingUserID = F, ExternalUniqueIdentifier = F, GroupName = F, ImportExternalUniqueIdentifier = F, ImportUserNameBirthDate = F, ImportUserNameFullNameLegalLFM = F, ImportUserNameFullNameLFM = F, ImportUserNamePrimaryEmailAddress = F, Matches = F, ModifiedTime = F, TempSecurityImportGroupMembershipID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserNameBirthDate = F, UserNameFullNameLegalLFM = F, UserNameFullNameLFM = F, UserNamePrimaryEmailAddress = F, UserUsername = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempSecurityImportGroupMembership", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSecurityImportGroupMembership
	#'
	#' This function returns a dataframe or json object of a TempSecurityImportGroupMembership
	#' @param TempSecurityImportGroupMembershipID The ID of the TempSecurityImportGroupMembership to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSecurityImportGroupMembership. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSecurityImportGroupMembership.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSecurityImportGroupMembership') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempSecurityImportGroupMembership
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSecurityImportGroupMembership <- function(TempSecurityImportGroupMembershipID, CreatedTime = F, EntityName = F, ExistingUserID = F, ExternalUniqueIdentifier = F, GroupName = F, ImportExternalUniqueIdentifier = F, ImportUserNameBirthDate = F, ImportUserNameFullNameLegalLFM = F, ImportUserNameFullNameLFM = F, ImportUserNamePrimaryEmailAddress = F, Matches = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserNameBirthDate = F, UserNameFullNameLegalLFM = F, UserNameFullNameLFM = F, UserNamePrimaryEmailAddress = F, UserUsername = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSecurityImportGroupMembershipID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempSecurityImportGroupMembership", objectId = TempSecurityImportGroupMembershipID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSecurityImportGroupMembership
	#'
	#' This function deletes a TempSecurityImportGroupMembership
	#' @param TempSecurityImportGroupMembershipID The ID of the TempSecurityImportGroupMembership to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempSecurityImportGroupMembershipID of the deleted TempSecurityImportGroupMembership.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSecurityImportGroupMembership <- function(TempSecurityImportGroupMembershipID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempSecurityImportGroupMembership", objectId = TempSecurityImportGroupMembershipID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSecurityImportGroupMembership
	#'
	#' This function creates a TempSecurityImportGroupMembership
	#' @param fieldNames The field values to give the created TempSecurityImportGroupMembership. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempSecurityImportGroupMembership
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSecurityImportGroupMembership <- function(EntityName = NULL, ExistingUserID = NULL, ExternalUniqueIdentifier = NULL, GroupName = NULL, ImportExternalUniqueIdentifier = NULL, ImportUserNameBirthDate = NULL, ImportUserNameFullNameLegalLFM = NULL, ImportUserNameFullNameLFM = NULL, ImportUserNamePrimaryEmailAddress = NULL, Matches = NULL, UserNameBirthDate = NULL, UserNameFullNameLegalLFM = NULL, UserNameFullNameLFM = NULL, UserNamePrimaryEmailAddress = NULL, UserUsername = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempSecurityImportGroupMembership", body = list(DataObject = body), searchFields = append("TempSecurityImportGroupMembershipID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSecurityImportGroupMembership
	#'
	#' This function modifies a TempSecurityImportGroupMembership
	#' @param fieldNames The field values to give the modified TempSecurityImportGroupMembership. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempSecurityImportGroupMembership
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSecurityImportGroupMembership <- function(TempSecurityImportGroupMembershipID, EntityName = NULL, ExistingUserID = NULL, ExternalUniqueIdentifier = NULL, GroupName = NULL, ImportExternalUniqueIdentifier = NULL, ImportUserNameBirthDate = NULL, ImportUserNameFullNameLegalLFM = NULL, ImportUserNameFullNameLFM = NULL, ImportUserNamePrimaryEmailAddress = NULL, Matches = NULL, UserNameBirthDate = NULL, UserNameFullNameLegalLFM = NULL, UserNameFullNameLFM = NULL, UserNamePrimaryEmailAddress = NULL, UserUsername = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempSecurityImportGroupMembership", objectId = TempSecurityImportGroupMembershipID, body = list(DataObject = body), searchFields = append("TempSecurityImportGroupMembershipID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SkywardSupportAccesses
	#'
	#' This function returns a dataframe or json object of SkywardSupportAccesses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SkywardSupportAccesses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SkywardSupportAccesses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SkywardSupportAccess') to get more field paths.
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
	#' @concept Security
	#' @return A list of SkywardSupportAccesses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSkywardSupportAccesses <- function(searchConditionsList = NULL, CreatedTime = F, EndDate = F, IsActive = F, ModifiedTime = F, Notes = F, ServiceCallNumber = F, SkywardSupportAccessID = F, StartDate = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "SkywardSupportAccess", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SkywardSupportAccess
	#'
	#' This function returns a dataframe or json object of a SkywardSupportAccess
	#' @param SkywardSupportAccessID The ID of the SkywardSupportAccess to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SkywardSupportAccess. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SkywardSupportAccess.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SkywardSupportAccess') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of SkywardSupportAccess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSkywardSupportAccess <- function(SkywardSupportAccessID, CreatedTime = F, EndDate = F, IsActive = F, ModifiedTime = F, Notes = F, ServiceCallNumber = F, StartDate = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SkywardSupportAccessID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "SkywardSupportAccess", objectId = SkywardSupportAccessID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SkywardSupportAccess
	#'
	#' This function deletes a SkywardSupportAccess
	#' @param SkywardSupportAccessID The ID of the SkywardSupportAccess to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The SkywardSupportAccessID of the deleted SkywardSupportAccess.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSkywardSupportAccess <- function(SkywardSupportAccessID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "SkywardSupportAccess", objectId = SkywardSupportAccessID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SkywardSupportAccess
	#'
	#' This function creates a SkywardSupportAccess
	#' @param fieldNames The field values to give the created SkywardSupportAccess. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created SkywardSupportAccess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSkywardSupportAccess <- function(EndDate = NULL, IsActive = NULL, Notes = NULL, ServiceCallNumber = NULL, StartDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "SkywardSupportAccess", body = list(DataObject = body), searchFields = append("SkywardSupportAccessID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SkywardSupportAccess
	#'
	#' This function modifies a SkywardSupportAccess
	#' @param fieldNames The field values to give the modified SkywardSupportAccess. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified SkywardSupportAccess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySkywardSupportAccess <- function(SkywardSupportAccessID, EndDate = NULL, IsActive = NULL, Notes = NULL, ServiceCallNumber = NULL, StartDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "SkywardSupportAccess", objectId = SkywardSupportAccessID, body = list(DataObject = body), searchFields = append("SkywardSupportAccessID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SkywardSupportAccessLoginHistories
	#'
	#' This function returns a dataframe or json object of SkywardSupportAccessLoginHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SkywardSupportAccessLoginHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SkywardSupportAccessLoginHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SkywardSupportAccessLoginHistory') to get more field paths.
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
	#' @concept Security
	#' @return A list of SkywardSupportAccessLoginHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSkywardSupportAccessLoginHistories <- function(searchConditionsList = NULL, AccessedTime = F, CreatedTime = F, ModifiedTime = F, SessionID = F, SkywardEmployeeName = F, SkywardSupportAccessID = F, SkywardSupportAccessLoginHistoryID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "SkywardSupportAccessLoginHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SkywardSupportAccessLoginHistory
	#'
	#' This function returns a dataframe or json object of a SkywardSupportAccessLoginHistory
	#' @param SkywardSupportAccessLoginHistoryID The ID of the SkywardSupportAccessLoginHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SkywardSupportAccessLoginHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SkywardSupportAccessLoginHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SkywardSupportAccessLoginHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of SkywardSupportAccessLoginHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSkywardSupportAccessLoginHistory <- function(SkywardSupportAccessLoginHistoryID, AccessedTime = F, CreatedTime = F, ModifiedTime = F, SessionID = F, SkywardEmployeeName = F, SkywardSupportAccessID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SkywardSupportAccessLoginHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "SkywardSupportAccessLoginHistory", objectId = SkywardSupportAccessLoginHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SkywardSupportAccessLoginHistory
	#'
	#' This function deletes a SkywardSupportAccessLoginHistory
	#' @param SkywardSupportAccessLoginHistoryID The ID of the SkywardSupportAccessLoginHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The SkywardSupportAccessLoginHistoryID of the deleted SkywardSupportAccessLoginHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSkywardSupportAccessLoginHistory <- function(SkywardSupportAccessLoginHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "SkywardSupportAccessLoginHistory", objectId = SkywardSupportAccessLoginHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SkywardSupportAccessLoginHistory
	#'
	#' This function creates a SkywardSupportAccessLoginHistory
	#' @param fieldNames The field values to give the created SkywardSupportAccessLoginHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created SkywardSupportAccessLoginHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSkywardSupportAccessLoginHistory <- function(AccessedTime = NULL, SessionID = NULL, SkywardEmployeeName = NULL, SkywardSupportAccessID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "SkywardSupportAccessLoginHistory", body = list(DataObject = body), searchFields = append("SkywardSupportAccessLoginHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SkywardSupportAccessLoginHistory
	#'
	#' This function modifies a SkywardSupportAccessLoginHistory
	#' @param fieldNames The field values to give the modified SkywardSupportAccessLoginHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified SkywardSupportAccessLoginHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySkywardSupportAccessLoginHistory <- function(SkywardSupportAccessLoginHistoryID, AccessedTime = NULL, SessionID = NULL, SkywardEmployeeName = NULL, SkywardSupportAccessID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "SkywardSupportAccessLoginHistory", objectId = SkywardSupportAccessLoginHistoryID, body = list(DataObject = body), searchFields = append("SkywardSupportAccessLoginHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AuthenticationRoles
	#'
	#' This function returns a dataframe or json object of AuthenticationRoles
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AuthenticationRoles. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AuthenticationRoles.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AuthenticationRole') to get more field paths.
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
	#' @concept Security
	#' @return A list of AuthenticationRoles
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAuthenticationRoles <- function(searchConditionsList = NULL, AllowSkywardCredentials = F, AuthenticationRoleID = F, CreatedTime = F, DisplayText = F, HasAuthenticationMethod = F, HasLDAPProvider = F, MediaID = F, ModifiedTime = F, Name = F, Priority = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "AuthenticationRole", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AuthenticationRole
	#'
	#' This function returns a dataframe or json object of an AuthenticationRole
	#' @param AuthenticationRoleID The ID of the AuthenticationRole to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AuthenticationRole. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AuthenticationRole.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AuthenticationRole') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of AuthenticationRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAuthenticationRole <- function(AuthenticationRoleID, AllowSkywardCredentials = F, CreatedTime = F, DisplayText = F, HasAuthenticationMethod = F, HasLDAPProvider = F, MediaID = F, ModifiedTime = F, Name = F, Priority = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AuthenticationRoleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "AuthenticationRole", objectId = AuthenticationRoleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AuthenticationRole
	#'
	#' This function deletes an AuthenticationRole
	#' @param AuthenticationRoleID The ID of the AuthenticationRole to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The AuthenticationRoleID of the deleted AuthenticationRole.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAuthenticationRole <- function(AuthenticationRoleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "AuthenticationRole", objectId = AuthenticationRoleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AuthenticationRole
	#'
	#' This function creates an AuthenticationRole
	#' @param fieldNames The field values to give the created AuthenticationRole. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created AuthenticationRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAuthenticationRole <- function(AllowSkywardCredentials = NULL, DisplayText = NULL, MediaID = NULL, Name = NULL, Priority = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "AuthenticationRole", body = list(DataObject = body), searchFields = append("AuthenticationRoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AuthenticationRole
	#'
	#' This function modifies an AuthenticationRole
	#' @param fieldNames The field values to give the modified AuthenticationRole. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified AuthenticationRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAuthenticationRole <- function(AuthenticationRoleID, AllowSkywardCredentials = NULL, DisplayText = NULL, MediaID = NULL, Name = NULL, Priority = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "AuthenticationRole", objectId = AuthenticationRoleID, body = list(DataObject = body), searchFields = append("AuthenticationRoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AuthenticationRoleMethods
	#'
	#' This function returns a dataframe or json object of AuthenticationRoleMethods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AuthenticationRoleMethods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AuthenticationRoleMethods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AuthenticationRoleMethod') to get more field paths.
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
	#' @concept Security
	#' @return A list of AuthenticationRoleMethods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAuthenticationRoleMethods <- function(searchConditionsList = NULL, AuthenticationMethodID = F, AuthenticationRoleID = F, AuthenticationRoleMethodID = F, CreatedTime = F, DisplayOrder = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "AuthenticationRoleMethod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AuthenticationRoleMethod
	#'
	#' This function returns a dataframe or json object of an AuthenticationRoleMethod
	#' @param AuthenticationRoleMethodID The ID of the AuthenticationRoleMethod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AuthenticationRoleMethod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AuthenticationRoleMethod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AuthenticationRoleMethod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of AuthenticationRoleMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAuthenticationRoleMethod <- function(AuthenticationRoleMethodID, AuthenticationMethodID = F, AuthenticationRoleID = F, CreatedTime = F, DisplayOrder = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AuthenticationRoleMethodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "AuthenticationRoleMethod", objectId = AuthenticationRoleMethodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AuthenticationRoleMethod
	#'
	#' This function deletes an AuthenticationRoleMethod
	#' @param AuthenticationRoleMethodID The ID of the AuthenticationRoleMethod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The AuthenticationRoleMethodID of the deleted AuthenticationRoleMethod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAuthenticationRoleMethod <- function(AuthenticationRoleMethodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "AuthenticationRoleMethod", objectId = AuthenticationRoleMethodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AuthenticationRoleMethod
	#'
	#' This function creates an AuthenticationRoleMethod
	#' @param fieldNames The field values to give the created AuthenticationRoleMethod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created AuthenticationRoleMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAuthenticationRoleMethod <- function(AuthenticationMethodID = NULL, AuthenticationRoleID = NULL, DisplayOrder = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "AuthenticationRoleMethod", body = list(DataObject = body), searchFields = append("AuthenticationRoleMethodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AuthenticationRoleMethod
	#'
	#' This function modifies an AuthenticationRoleMethod
	#' @param fieldNames The field values to give the modified AuthenticationRoleMethod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified AuthenticationRoleMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAuthenticationRoleMethod <- function(AuthenticationRoleMethodID, AuthenticationMethodID = NULL, AuthenticationRoleID = NULL, DisplayOrder = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "AuthenticationRoleMethod", objectId = AuthenticationRoleMethodID, body = list(DataObject = body), searchFields = append("AuthenticationRoleMethodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AuthenticationAssertions
	#'
	#' This function returns a dataframe or json object of AuthenticationAssertions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AuthenticationAssertions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AuthenticationAssertions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AuthenticationAssertion') to get more field paths.
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
	#' @concept Security
	#' @return A list of AuthenticationAssertions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAuthenticationAssertions <- function(searchConditionsList = NULL, AssertionGuid = F, AuthenticationAssertionID = F, AuthenticationMethodID = F, CreatedTime = F, MobileDevice = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "AuthenticationAssertion", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AuthenticationAssertion
	#'
	#' This function returns a dataframe or json object of an AuthenticationAssertion
	#' @param AuthenticationAssertionID The ID of the AuthenticationAssertion to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AuthenticationAssertion. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AuthenticationAssertion.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AuthenticationAssertion') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of AuthenticationAssertion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAuthenticationAssertion <- function(AuthenticationAssertionID, AssertionGuid = F, AuthenticationMethodID = F, CreatedTime = F, MobileDevice = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AuthenticationAssertionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "AuthenticationAssertion", objectId = AuthenticationAssertionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AuthenticationAssertion
	#'
	#' This function deletes an AuthenticationAssertion
	#' @param AuthenticationAssertionID The ID of the AuthenticationAssertion to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The AuthenticationAssertionID of the deleted AuthenticationAssertion.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAuthenticationAssertion <- function(AuthenticationAssertionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "AuthenticationAssertion", objectId = AuthenticationAssertionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AuthenticationAssertion
	#'
	#' This function creates an AuthenticationAssertion
	#' @param fieldNames The field values to give the created AuthenticationAssertion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created AuthenticationAssertion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAuthenticationAssertion <- function(AssertionGuid = NULL, AuthenticationMethodID = NULL, MobileDevice = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "AuthenticationAssertion", body = list(DataObject = body), searchFields = append("AuthenticationAssertionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AuthenticationAssertion
	#'
	#' This function modifies an AuthenticationAssertion
	#' @param fieldNames The field values to give the modified AuthenticationAssertion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified AuthenticationAssertion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAuthenticationAssertion <- function(AuthenticationAssertionID, AssertionGuid = NULL, AuthenticationMethodID = NULL, MobileDevice = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "AuthenticationAssertion", objectId = AuthenticationAssertionID, body = list(DataObject = body), searchFields = append("AuthenticationAssertionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSpecialtyAccessGroups
	#'
	#' This function returns a dataframe or json object of TempSpecialtyAccessGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSpecialtyAccessGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSpecialtyAccessGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSpecialtyAccessGroup') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempSpecialtyAccessGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSpecialtyAccessGroups <- function(searchConditionsList = NULL, CreatedTime = F, GroupName = F, Identifier = F, ModifiedTime = F, Selected = F, SpecialtyAccessGroupPrimaryKey = F, TempSpecialtyAccessGroupID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempSpecialtyAccessGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSpecialtyAccessGroup
	#'
	#' This function returns a dataframe or json object of a TempSpecialtyAccessGroup
	#' @param TempSpecialtyAccessGroupID The ID of the TempSpecialtyAccessGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSpecialtyAccessGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSpecialtyAccessGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSpecialtyAccessGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempSpecialtyAccessGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSpecialtyAccessGroup <- function(TempSpecialtyAccessGroupID, CreatedTime = F, GroupName = F, Identifier = F, ModifiedTime = F, Selected = F, SpecialtyAccessGroupPrimaryKey = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSpecialtyAccessGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempSpecialtyAccessGroup", objectId = TempSpecialtyAccessGroupID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSpecialtyAccessGroup
	#'
	#' This function deletes a TempSpecialtyAccessGroup
	#' @param TempSpecialtyAccessGroupID The ID of the TempSpecialtyAccessGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempSpecialtyAccessGroupID of the deleted TempSpecialtyAccessGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSpecialtyAccessGroup <- function(TempSpecialtyAccessGroupID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempSpecialtyAccessGroup", objectId = TempSpecialtyAccessGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSpecialtyAccessGroup
	#'
	#' This function creates a TempSpecialtyAccessGroup
	#' @param fieldNames The field values to give the created TempSpecialtyAccessGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempSpecialtyAccessGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSpecialtyAccessGroup <- function(GroupName = NULL, Identifier = NULL, Selected = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempSpecialtyAccessGroup", body = list(DataObject = body), searchFields = append("TempSpecialtyAccessGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSpecialtyAccessGroup
	#'
	#' This function modifies a TempSpecialtyAccessGroup
	#' @param fieldNames The field values to give the modified TempSpecialtyAccessGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempSpecialtyAccessGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSpecialtyAccessGroup <- function(TempSpecialtyAccessGroupID, GroupName = NULL, Identifier = NULL, Selected = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempSpecialtyAccessGroup", objectId = TempSpecialtyAccessGroupID, body = list(DataObject = body), searchFields = append("TempSpecialtyAccessGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GroupImpersonationRoles
	#'
	#' This function returns a dataframe or json object of GroupImpersonationRoles
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GroupImpersonationRoles. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GroupImpersonationRoles.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GroupImpersonationRole') to get more field paths.
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
	#' @concept Security
	#' @return A list of GroupImpersonationRoles
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGroupImpersonationRoles <- function(searchConditionsList = NULL, CreatedTime = F, GroupID = F, GroupImpersonationRoleID = F, IsReadOnly = F, ModifiedTime = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "GroupImpersonationRole", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GroupImpersonationRole
	#'
	#' This function returns a dataframe or json object of a GroupImpersonationRole
	#' @param GroupImpersonationRoleID The ID of the GroupImpersonationRole to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GroupImpersonationRole. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GroupImpersonationRole.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GroupImpersonationRole') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of GroupImpersonationRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGroupImpersonationRole <- function(GroupImpersonationRoleID, CreatedTime = F, GroupID = F, IsReadOnly = F, ModifiedTime = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GroupImpersonationRoleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "GroupImpersonationRole", objectId = GroupImpersonationRoleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GroupImpersonationRole
	#'
	#' This function deletes a GroupImpersonationRole
	#' @param GroupImpersonationRoleID The ID of the GroupImpersonationRole to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The GroupImpersonationRoleID of the deleted GroupImpersonationRole.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGroupImpersonationRole <- function(GroupImpersonationRoleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "GroupImpersonationRole", objectId = GroupImpersonationRoleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GroupImpersonationRole
	#'
	#' This function creates a GroupImpersonationRole
	#' @param fieldNames The field values to give the created GroupImpersonationRole. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created GroupImpersonationRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGroupImpersonationRole <- function(GroupID = NULL, IsReadOnly = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "GroupImpersonationRole", body = list(DataObject = body), searchFields = append("GroupImpersonationRoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GroupImpersonationRole
	#'
	#' This function modifies a GroupImpersonationRole
	#' @param fieldNames The field values to give the modified GroupImpersonationRole. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified GroupImpersonationRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGroupImpersonationRole <- function(GroupImpersonationRoleID, GroupID = NULL, IsReadOnly = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "GroupImpersonationRole", objectId = GroupImpersonationRoleID, body = list(DataObject = body), searchFields = append("GroupImpersonationRoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MobileSSOS
	#'
	#' This function returns a dataframe or json object of MobileSSOS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MobileSSOS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MobileSSOS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MobileSSO') to get more field paths.
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
	#' @concept Security
	#' @return A list of MobileSSOS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMobileSSOS <- function(searchConditionsList = NULL, CreatedTime = F, MobileDevice = F, MobileSSOID = F, ModifiedTime = F, SSOToken = F, SSOTokenExpirationDate = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "MobileSSO", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MobileSSO
	#'
	#' This function returns a dataframe or json object of a MobileSSO
	#' @param MobileSSOID The ID of the MobileSSO to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MobileSSO. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MobileSSO.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MobileSSO') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of MobileSSO
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMobileSSO <- function(MobileSSOID, CreatedTime = F, MobileDevice = F, ModifiedTime = F, SSOToken = F, SSOTokenExpirationDate = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MobileSSOID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "MobileSSO", objectId = MobileSSOID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MobileSSO
	#'
	#' This function deletes a MobileSSO
	#' @param MobileSSOID The ID of the MobileSSO to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The MobileSSOID of the deleted MobileSSO.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMobileSSO <- function(MobileSSOID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "MobileSSO", objectId = MobileSSOID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MobileSSO
	#'
	#' This function creates a MobileSSO
	#' @param fieldNames The field values to give the created MobileSSO. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created MobileSSO
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMobileSSO <- function(MobileDevice = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "MobileSSO", body = list(DataObject = body), searchFields = append("MobileSSOID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MobileSSO
	#'
	#' This function modifies a MobileSSO
	#' @param fieldNames The field values to give the modified MobileSSO. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified MobileSSO
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMobileSSO <- function(MobileSSOID, MobileDevice = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "MobileSSO", objectId = MobileSSOID, body = list(DataObject = body), searchFields = append("MobileSSOID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MultifactorAuthentications
	#'
	#' This function returns a dataframe or json object of MultifactorAuthentications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MultifactorAuthentications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MultifactorAuthentications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MultifactorAuthentication') to get more field paths.
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
	#' @concept Security
	#' @return A list of MultifactorAuthentications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMultifactorAuthentications <- function(searchConditionsList = NULL, Code = F, CreatedTime = F, DaysToExpiration = F, Description = F, IsRequired = F, ModifiedTime = F, MultifactorAuthenticationID = F, Priority = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UsesEmail = F, UsesSMS = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "MultifactorAuthentication", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MultifactorAuthentication
	#'
	#' This function returns a dataframe or json object of a MultifactorAuthentication
	#' @param MultifactorAuthenticationID The ID of the MultifactorAuthentication to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MultifactorAuthentication. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MultifactorAuthentication.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MultifactorAuthentication') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of MultifactorAuthentication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMultifactorAuthentication <- function(MultifactorAuthenticationID, Code = F, CreatedTime = F, DaysToExpiration = F, Description = F, IsRequired = F, ModifiedTime = F, Priority = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UsesEmail = F, UsesSMS = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MultifactorAuthenticationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "MultifactorAuthentication", objectId = MultifactorAuthenticationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MultifactorAuthentication
	#'
	#' This function deletes a MultifactorAuthentication
	#' @param MultifactorAuthenticationID The ID of the MultifactorAuthentication to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The MultifactorAuthenticationID of the deleted MultifactorAuthentication.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMultifactorAuthentication <- function(MultifactorAuthenticationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "MultifactorAuthentication", objectId = MultifactorAuthenticationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MultifactorAuthentication
	#'
	#' This function creates a MultifactorAuthentication
	#' @param fieldNames The field values to give the created MultifactorAuthentication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created MultifactorAuthentication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMultifactorAuthentication <- function(Code = NULL, DaysToExpiration = NULL, Description = NULL, IsRequired = NULL, Priority = NULL, UsesEmail = NULL, UsesSMS = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "MultifactorAuthentication", body = list(DataObject = body), searchFields = append("MultifactorAuthenticationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MultifactorAuthentication
	#'
	#' This function modifies a MultifactorAuthentication
	#' @param fieldNames The field values to give the modified MultifactorAuthentication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified MultifactorAuthentication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMultifactorAuthentication <- function(MultifactorAuthenticationID, Code = NULL, DaysToExpiration = NULL, Description = NULL, IsRequired = NULL, Priority = NULL, UsesEmail = NULL, UsesSMS = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "MultifactorAuthentication", objectId = MultifactorAuthenticationID, body = list(DataObject = body), searchFields = append("MultifactorAuthenticationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MultifactorAuthenticationAssertions
	#'
	#' This function returns a dataframe or json object of MultifactorAuthenticationAssertions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MultifactorAuthenticationAssertions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MultifactorAuthenticationAssertions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MultifactorAuthenticationAssertion') to get more field paths.
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
	#' @concept Security
	#' @return A list of MultifactorAuthenticationAssertions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMultifactorAuthenticationAssertions <- function(searchConditionsList = NULL, AssertionCode = F, AssertionIdentifier = F, CreatedTime = F, ExpirationTime = F, ModifiedTime = F, MultifactorAuthenticationAssertionID = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "MultifactorAuthenticationAssertion", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MultifactorAuthenticationAssertion
	#'
	#' This function returns a dataframe or json object of a MultifactorAuthenticationAssertion
	#' @param MultifactorAuthenticationAssertionID The ID of the MultifactorAuthenticationAssertion to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MultifactorAuthenticationAssertion. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MultifactorAuthenticationAssertion.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MultifactorAuthenticationAssertion') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of MultifactorAuthenticationAssertion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMultifactorAuthenticationAssertion <- function(MultifactorAuthenticationAssertionID, AssertionCode = F, AssertionIdentifier = F, CreatedTime = F, ExpirationTime = F, ModifiedTime = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MultifactorAuthenticationAssertionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "MultifactorAuthenticationAssertion", objectId = MultifactorAuthenticationAssertionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MultifactorAuthenticationAssertion
	#'
	#' This function deletes a MultifactorAuthenticationAssertion
	#' @param MultifactorAuthenticationAssertionID The ID of the MultifactorAuthenticationAssertion to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The MultifactorAuthenticationAssertionID of the deleted MultifactorAuthenticationAssertion.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMultifactorAuthenticationAssertion <- function(MultifactorAuthenticationAssertionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "MultifactorAuthenticationAssertion", objectId = MultifactorAuthenticationAssertionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MultifactorAuthenticationAssertion
	#'
	#' This function creates a MultifactorAuthenticationAssertion
	#' @param fieldNames The field values to give the created MultifactorAuthenticationAssertion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created MultifactorAuthenticationAssertion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMultifactorAuthenticationAssertion <- function(AssertionCode = NULL, AssertionIdentifier = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "MultifactorAuthenticationAssertion", body = list(DataObject = body), searchFields = append("MultifactorAuthenticationAssertionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MultifactorAuthenticationAssertion
	#'
	#' This function modifies a MultifactorAuthenticationAssertion
	#' @param fieldNames The field values to give the modified MultifactorAuthenticationAssertion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified MultifactorAuthenticationAssertion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMultifactorAuthenticationAssertion <- function(MultifactorAuthenticationAssertionID, AssertionCode = NULL, AssertionIdentifier = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "MultifactorAuthenticationAssertion", objectId = MultifactorAuthenticationAssertionID, body = list(DataObject = body), searchFields = append("MultifactorAuthenticationAssertionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TrustedDevices
	#'
	#' This function returns a dataframe or json object of TrustedDevices
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TrustedDevices. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TrustedDevices.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TrustedDevice') to get more field paths.
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
	#' @concept Security
	#' @return A list of TrustedDevices
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTrustedDevices <- function(searchConditionsList = NULL, BrowserType = F, BrowserVersion = F, CreatedTime = F, DeviceType = F, HostAddress = F, Identifier = F, ModifiedTime = F, OperatingSystemType = F, TrustedDeviceID = F, UserAgent = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TrustedDevice", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TrustedDevice
	#'
	#' This function returns a dataframe or json object of a TrustedDevice
	#' @param TrustedDeviceID The ID of the TrustedDevice to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TrustedDevice. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TrustedDevice.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TrustedDevice') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TrustedDevice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTrustedDevice <- function(TrustedDeviceID, BrowserType = F, BrowserVersion = F, CreatedTime = F, DeviceType = F, HostAddress = F, Identifier = F, ModifiedTime = F, OperatingSystemType = F, UserAgent = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TrustedDeviceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TrustedDevice", objectId = TrustedDeviceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TrustedDevice
	#'
	#' This function deletes a TrustedDevice
	#' @param TrustedDeviceID The ID of the TrustedDevice to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TrustedDeviceID of the deleted TrustedDevice.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTrustedDevice <- function(TrustedDeviceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TrustedDevice", objectId = TrustedDeviceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TrustedDevice
	#'
	#' This function creates a TrustedDevice
	#' @param fieldNames The field values to give the created TrustedDevice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TrustedDevice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTrustedDevice <- function(BrowserType = NULL, BrowserVersion = NULL, DeviceType = NULL, HostAddress = NULL, OperatingSystemType = NULL, UserAgent = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TrustedDevice", body = list(DataObject = body), searchFields = append("TrustedDeviceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TrustedDevice
	#'
	#' This function modifies a TrustedDevice
	#' @param fieldNames The field values to give the modified TrustedDevice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TrustedDevice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTrustedDevice <- function(TrustedDeviceID, BrowserType = NULL, BrowserVersion = NULL, DeviceType = NULL, HostAddress = NULL, OperatingSystemType = NULL, UserAgent = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TrustedDevice", objectId = TrustedDeviceID, body = list(DataObject = body), searchFields = append("TrustedDeviceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEntityForClones
	#'
	#' This function returns a dataframe or json object of TempEntityForClones
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEntityForClones. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEntityForClones.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEntityForClone') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempEntityForClones
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEntityForClones <- function(searchConditionsList = NULL, CreatedTime = F, EntityCode = F, EntityName = F, EntityPrimaryKey = F, ModifiedTime = F, Selected = F, TempEntityForCloneID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempEntityForClone", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEntityForClone
	#'
	#' This function returns a dataframe or json object of a TempEntityForClone
	#' @param TempEntityForCloneID The ID of the TempEntityForClone to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEntityForClone. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEntityForClone.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEntityForClone') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempEntityForClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEntityForClone <- function(TempEntityForCloneID, CreatedTime = F, EntityCode = F, EntityName = F, EntityPrimaryKey = F, ModifiedTime = F, Selected = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEntityForCloneID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempEntityForClone", objectId = TempEntityForCloneID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEntityForClone
	#'
	#' This function deletes a TempEntityForClone
	#' @param TempEntityForCloneID The ID of the TempEntityForClone to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempEntityForCloneID of the deleted TempEntityForClone.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEntityForClone <- function(TempEntityForCloneID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempEntityForClone", objectId = TempEntityForCloneID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEntityForClone
	#'
	#' This function creates a TempEntityForClone
	#' @param fieldNames The field values to give the created TempEntityForClone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempEntityForClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEntityForClone <- function(EntityCode = NULL, EntityName = NULL, Selected = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempEntityForClone", body = list(DataObject = body), searchFields = append("TempEntityForCloneID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEntityForClone
	#'
	#' This function modifies a TempEntityForClone
	#' @param fieldNames The field values to give the modified TempEntityForClone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempEntityForClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEntityForClone <- function(TempEntityForCloneID, EntityCode = NULL, EntityName = NULL, Selected = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempEntityForClone", objectId = TempEntityForCloneID, body = list(DataObject = body), searchFields = append("TempEntityForCloneID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempRoleForClones
	#'
	#' This function returns a dataframe or json object of TempRoleForClones
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempRoleForClones. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempRoleForClones.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempRoleForClone') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempRoleForClones
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempRoleForClones <- function(searchConditionsList = NULL, CreatedTime = F, Description = F, ModifiedTime = F, RoleName = F, RolePrimaryKey = F, Selected = F, TempRoleForCloneID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempRoleForClone", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempRoleForClone
	#'
	#' This function returns a dataframe or json object of a TempRoleForClone
	#' @param TempRoleForCloneID The ID of the TempRoleForClone to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempRoleForClone. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempRoleForClone.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempRoleForClone') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempRoleForClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempRoleForClone <- function(TempRoleForCloneID, CreatedTime = F, Description = F, ModifiedTime = F, RoleName = F, RolePrimaryKey = F, Selected = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempRoleForCloneID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempRoleForClone", objectId = TempRoleForCloneID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempRoleForClone
	#'
	#' This function deletes a TempRoleForClone
	#' @param TempRoleForCloneID The ID of the TempRoleForClone to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempRoleForCloneID of the deleted TempRoleForClone.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempRoleForClone <- function(TempRoleForCloneID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempRoleForClone", objectId = TempRoleForCloneID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempRoleForClone
	#'
	#' This function creates a TempRoleForClone
	#' @param fieldNames The field values to give the created TempRoleForClone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempRoleForClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempRoleForClone <- function(Description = NULL, RoleName = NULL, Selected = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempRoleForClone", body = list(DataObject = body), searchFields = append("TempRoleForCloneID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempRoleForClone
	#'
	#' This function modifies a TempRoleForClone
	#' @param fieldNames The field values to give the modified TempRoleForClone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempRoleForClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempRoleForClone <- function(TempRoleForCloneID, Description = NULL, RoleName = NULL, Selected = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempRoleForClone", objectId = TempRoleForCloneID, body = list(DataObject = body), searchFields = append("TempRoleForCloneID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempImpersonationRoleForClones
	#'
	#' This function returns a dataframe or json object of TempImpersonationRoleForClones
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempImpersonationRoleForClones. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempImpersonationRoleForClones.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempImpersonationRoleForClone') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempImpersonationRoleForClones
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempImpersonationRoleForClones <- function(searchConditionsList = NULL, CreatedTime = F, Description = F, IsReadOnly = F, ModifiedTime = F, RoleName = F, RolePrimaryKey = F, Selected = F, TempImpersonationRoleForCloneID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempImpersonationRoleForClone", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempImpersonationRoleForClone
	#'
	#' This function returns a dataframe or json object of a TempImpersonationRoleForClone
	#' @param TempImpersonationRoleForCloneID The ID of the TempImpersonationRoleForClone to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempImpersonationRoleForClone. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempImpersonationRoleForClone.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempImpersonationRoleForClone') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempImpersonationRoleForClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempImpersonationRoleForClone <- function(TempImpersonationRoleForCloneID, CreatedTime = F, Description = F, IsReadOnly = F, ModifiedTime = F, RoleName = F, RolePrimaryKey = F, Selected = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempImpersonationRoleForCloneID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempImpersonationRoleForClone", objectId = TempImpersonationRoleForCloneID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempImpersonationRoleForClone
	#'
	#' This function deletes a TempImpersonationRoleForClone
	#' @param TempImpersonationRoleForCloneID The ID of the TempImpersonationRoleForClone to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempImpersonationRoleForCloneID of the deleted TempImpersonationRoleForClone.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempImpersonationRoleForClone <- function(TempImpersonationRoleForCloneID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempImpersonationRoleForClone", objectId = TempImpersonationRoleForCloneID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempImpersonationRoleForClone
	#'
	#' This function creates a TempImpersonationRoleForClone
	#' @param fieldNames The field values to give the created TempImpersonationRoleForClone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempImpersonationRoleForClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempImpersonationRoleForClone <- function(Description = NULL, IsReadOnly = NULL, RoleName = NULL, Selected = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempImpersonationRoleForClone", body = list(DataObject = body), searchFields = append("TempImpersonationRoleForCloneID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempImpersonationRoleForClone
	#'
	#' This function modifies a TempImpersonationRoleForClone
	#' @param fieldNames The field values to give the modified TempImpersonationRoleForClone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempImpersonationRoleForClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempImpersonationRoleForClone <- function(TempImpersonationRoleForCloneID, Description = NULL, IsReadOnly = NULL, RoleName = NULL, Selected = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempImpersonationRoleForClone", objectId = TempImpersonationRoleForCloneID, body = list(DataObject = body), searchFields = append("TempImpersonationRoleForCloneID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AuthenticationRoleLDAPProviders
	#'
	#' This function returns a dataframe or json object of AuthenticationRoleLDAPProviders
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AuthenticationRoleLDAPProviders. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AuthenticationRoleLDAPProviders.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AuthenticationRoleLDAPProvider') to get more field paths.
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
	#' @concept Security
	#' @return A list of AuthenticationRoleLDAPProviders
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAuthenticationRoleLDAPProviders <- function(searchConditionsList = NULL, AuthenticationRoleID = F, AuthenticationRoleLDAPProviderID = F, CreatedTime = F, LDAPProviderID = F, ModifiedTime = F, Priority = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "AuthenticationRoleLDAPProvider", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AuthenticationRoleLDAPProvider
	#'
	#' This function returns a dataframe or json object of an AuthenticationRoleLDAPProvider
	#' @param AuthenticationRoleLDAPProviderID The ID of the AuthenticationRoleLDAPProvider to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AuthenticationRoleLDAPProvider. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AuthenticationRoleLDAPProvider.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AuthenticationRoleLDAPProvider') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of AuthenticationRoleLDAPProvider
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAuthenticationRoleLDAPProvider <- function(AuthenticationRoleLDAPProviderID, AuthenticationRoleID = F, CreatedTime = F, LDAPProviderID = F, ModifiedTime = F, Priority = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AuthenticationRoleLDAPProviderID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "AuthenticationRoleLDAPProvider", objectId = AuthenticationRoleLDAPProviderID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AuthenticationRoleLDAPProvider
	#'
	#' This function deletes an AuthenticationRoleLDAPProvider
	#' @param AuthenticationRoleLDAPProviderID The ID of the AuthenticationRoleLDAPProvider to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The AuthenticationRoleLDAPProviderID of the deleted AuthenticationRoleLDAPProvider.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAuthenticationRoleLDAPProvider <- function(AuthenticationRoleLDAPProviderID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "AuthenticationRoleLDAPProvider", objectId = AuthenticationRoleLDAPProviderID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AuthenticationRoleLDAPProvider
	#'
	#' This function creates an AuthenticationRoleLDAPProvider
	#' @param fieldNames The field values to give the created AuthenticationRoleLDAPProvider. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created AuthenticationRoleLDAPProvider
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAuthenticationRoleLDAPProvider <- function(AuthenticationRoleID = NULL, LDAPProviderID = NULL, Priority = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "AuthenticationRoleLDAPProvider", body = list(DataObject = body), searchFields = append("AuthenticationRoleLDAPProviderID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AuthenticationRoleLDAPProvider
	#'
	#' This function modifies an AuthenticationRoleLDAPProvider
	#' @param fieldNames The field values to give the modified AuthenticationRoleLDAPProvider. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified AuthenticationRoleLDAPProvider
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAuthenticationRoleLDAPProvider <- function(AuthenticationRoleLDAPProviderID, AuthenticationRoleID = NULL, LDAPProviderID = NULL, Priority = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "AuthenticationRoleLDAPProvider", objectId = AuthenticationRoleLDAPProviderID, body = list(DataObject = body), searchFields = append("AuthenticationRoleLDAPProviderID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FieldRestrictions
	#'
	#' This function returns a dataframe or json object of FieldRestrictions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FieldRestrictions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FieldRestrictions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FieldRestriction') to get more field paths.
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
	#' @concept Security
	#' @return A list of FieldRestrictions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFieldRestrictions <- function(searchConditionsList = NULL, CreatedTime = F, FieldID = F, FieldRestrictionID = F, ModifiedTime = F, Name = F, RestrictionType = F, RoleSetType = F, ScreenSetType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "FieldRestriction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FieldRestriction
	#'
	#' This function returns a dataframe or json object of a FieldRestriction
	#' @param FieldRestrictionID The ID of the FieldRestriction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FieldRestriction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FieldRestriction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FieldRestriction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of FieldRestriction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFieldRestriction <- function(FieldRestrictionID, CreatedTime = F, FieldID = F, ModifiedTime = F, Name = F, RestrictionType = F, RoleSetType = F, ScreenSetType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FieldRestrictionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "FieldRestriction", objectId = FieldRestrictionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FieldRestriction
	#'
	#' This function deletes a FieldRestriction
	#' @param FieldRestrictionID The ID of the FieldRestriction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The FieldRestrictionID of the deleted FieldRestriction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFieldRestriction <- function(FieldRestrictionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "FieldRestriction", objectId = FieldRestrictionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FieldRestriction
	#'
	#' This function creates a FieldRestriction
	#' @param fieldNames The field values to give the created FieldRestriction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created FieldRestriction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFieldRestriction <- function(FieldID = NULL, Name = NULL, RestrictionType = NULL, RoleSetType = NULL, ScreenSetType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "FieldRestriction", body = list(DataObject = body), searchFields = append("FieldRestrictionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FieldRestriction
	#'
	#' This function modifies a FieldRestriction
	#' @param fieldNames The field values to give the modified FieldRestriction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified FieldRestriction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFieldRestriction <- function(FieldRestrictionID, FieldID = NULL, Name = NULL, RestrictionType = NULL, RoleSetType = NULL, ScreenSetType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "FieldRestriction", objectId = FieldRestrictionID, body = list(DataObject = body), searchFields = append("FieldRestrictionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FieldRestrictionRoles
	#'
	#' This function returns a dataframe or json object of FieldRestrictionRoles
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FieldRestrictionRoles. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FieldRestrictionRoles.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FieldRestrictionRole') to get more field paths.
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
	#' @concept Security
	#' @return A list of FieldRestrictionRoles
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFieldRestrictionRoles <- function(searchConditionsList = NULL, CreatedTime = F, FieldRestrictionID = F, FieldRestrictionRoleID = F, ModifiedTime = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "FieldRestrictionRole", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FieldRestrictionRole
	#'
	#' This function returns a dataframe or json object of a FieldRestrictionRole
	#' @param FieldRestrictionRoleID The ID of the FieldRestrictionRole to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FieldRestrictionRole. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FieldRestrictionRole.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FieldRestrictionRole') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of FieldRestrictionRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFieldRestrictionRole <- function(FieldRestrictionRoleID, CreatedTime = F, FieldRestrictionID = F, ModifiedTime = F, RoleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FieldRestrictionRoleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "FieldRestrictionRole", objectId = FieldRestrictionRoleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FieldRestrictionRole
	#'
	#' This function deletes a FieldRestrictionRole
	#' @param FieldRestrictionRoleID The ID of the FieldRestrictionRole to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The FieldRestrictionRoleID of the deleted FieldRestrictionRole.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFieldRestrictionRole <- function(FieldRestrictionRoleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "FieldRestrictionRole", objectId = FieldRestrictionRoleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FieldRestrictionRole
	#'
	#' This function creates a FieldRestrictionRole
	#' @param fieldNames The field values to give the created FieldRestrictionRole. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created FieldRestrictionRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFieldRestrictionRole <- function(FieldRestrictionID = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "FieldRestrictionRole", body = list(DataObject = body), searchFields = append("FieldRestrictionRoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FieldRestrictionRole
	#'
	#' This function modifies a FieldRestrictionRole
	#' @param fieldNames The field values to give the modified FieldRestrictionRole. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified FieldRestrictionRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFieldRestrictionRole <- function(FieldRestrictionRoleID, FieldRestrictionID = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "FieldRestrictionRole", objectId = FieldRestrictionRoleID, body = list(DataObject = body), searchFields = append("FieldRestrictionRoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FieldRestrictionScreens
	#'
	#' This function returns a dataframe or json object of FieldRestrictionScreens
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FieldRestrictionScreens. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FieldRestrictionScreens.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FieldRestrictionScreen') to get more field paths.
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
	#' @concept Security
	#' @return A list of FieldRestrictionScreens
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFieldRestrictionScreens <- function(searchConditionsList = NULL, CreatedTime = F, FieldRestrictionID = F, FieldRestrictionScreenID = F, ModifiedTime = F, SecurityLocationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "FieldRestrictionScreen", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FieldRestrictionScreen
	#'
	#' This function returns a dataframe or json object of a FieldRestrictionScreen
	#' @param FieldRestrictionScreenID The ID of the FieldRestrictionScreen to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FieldRestrictionScreen. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FieldRestrictionScreen.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FieldRestrictionScreen') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of FieldRestrictionScreen
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFieldRestrictionScreen <- function(FieldRestrictionScreenID, CreatedTime = F, FieldRestrictionID = F, ModifiedTime = F, SecurityLocationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FieldRestrictionScreenID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "FieldRestrictionScreen", objectId = FieldRestrictionScreenID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FieldRestrictionScreen
	#'
	#' This function deletes a FieldRestrictionScreen
	#' @param FieldRestrictionScreenID The ID of the FieldRestrictionScreen to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The FieldRestrictionScreenID of the deleted FieldRestrictionScreen.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFieldRestrictionScreen <- function(FieldRestrictionScreenID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "FieldRestrictionScreen", objectId = FieldRestrictionScreenID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FieldRestrictionScreen
	#'
	#' This function creates a FieldRestrictionScreen
	#' @param fieldNames The field values to give the created FieldRestrictionScreen. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created FieldRestrictionScreen
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFieldRestrictionScreen <- function(FieldRestrictionID = NULL, SecurityLocationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "FieldRestrictionScreen", body = list(DataObject = body), searchFields = append("FieldRestrictionScreenID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FieldRestrictionScreen
	#'
	#' This function modifies a FieldRestrictionScreen
	#' @param fieldNames The field values to give the modified FieldRestrictionScreen. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified FieldRestrictionScreen
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFieldRestrictionScreen <- function(FieldRestrictionScreenID, FieldRestrictionID = NULL, SecurityLocationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "FieldRestrictionScreen", objectId = FieldRestrictionScreenID, body = list(DataObject = body), searchFields = append("FieldRestrictionScreenID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LDAPGroups
	#'
	#' This function returns a dataframe or json object of LDAPGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LDAPGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LDAPGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LDAPGroup') to get more field paths.
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
	#' @concept Security
	#' @return A list of LDAPGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLDAPGroups <- function(searchConditionsList = NULL, CommonName = F, CreatedTime = F, DistinguishedName = F, LDAPGroupID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "LDAPGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LDAPGroup
	#'
	#' This function returns a dataframe or json object of a LDAPGroup
	#' @param LDAPGroupID The ID of the LDAPGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LDAPGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LDAPGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LDAPGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of LDAPGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLDAPGroup <- function(LDAPGroupID, CommonName = F, CreatedTime = F, DistinguishedName = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LDAPGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "LDAPGroup", objectId = LDAPGroupID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LDAPGroup
	#'
	#' This function deletes a LDAPGroup
	#' @param LDAPGroupID The ID of the LDAPGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The LDAPGroupID of the deleted LDAPGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLDAPGroup <- function(LDAPGroupID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "LDAPGroup", objectId = LDAPGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LDAPGroup
	#'
	#' This function creates a LDAPGroup
	#' @param fieldNames The field values to give the created LDAPGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created LDAPGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLDAPGroup <- function(CommonName = NULL, DistinguishedName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "LDAPGroup", body = list(DataObject = body), searchFields = append("LDAPGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LDAPGroup
	#'
	#' This function modifies a LDAPGroup
	#' @param fieldNames The field values to give the modified LDAPGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified LDAPGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLDAPGroup <- function(LDAPGroupID, CommonName = NULL, DistinguishedName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "LDAPGroup", objectId = LDAPGroupID, body = list(DataObject = body), searchFields = append("LDAPGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GroupLDAPSynchronizations
	#'
	#' This function returns a dataframe or json object of GroupLDAPSynchronizations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GroupLDAPSynchronizations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GroupLDAPSynchronizations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GroupLDAPSynchronization') to get more field paths.
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
	#' @concept Security
	#' @return A list of GroupLDAPSynchronizations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGroupLDAPSynchronizations <- function(searchConditionsList = NULL, CommonName = F, CreatedTime = F, DistinguishedName = F, EntityID = F, GroupID = F, GroupLDAPSynchronizationID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "GroupLDAPSynchronization", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GroupLDAPSynchronization
	#'
	#' This function returns a dataframe or json object of a GroupLDAPSynchronization
	#' @param GroupLDAPSynchronizationID The ID of the GroupLDAPSynchronization to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GroupLDAPSynchronization. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GroupLDAPSynchronization.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GroupLDAPSynchronization') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of GroupLDAPSynchronization
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGroupLDAPSynchronization <- function(GroupLDAPSynchronizationID, CommonName = F, CreatedTime = F, DistinguishedName = F, EntityID = F, GroupID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GroupLDAPSynchronizationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "GroupLDAPSynchronization", objectId = GroupLDAPSynchronizationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GroupLDAPSynchronization
	#'
	#' This function deletes a GroupLDAPSynchronization
	#' @param GroupLDAPSynchronizationID The ID of the GroupLDAPSynchronization to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The GroupLDAPSynchronizationID of the deleted GroupLDAPSynchronization.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGroupLDAPSynchronization <- function(GroupLDAPSynchronizationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "GroupLDAPSynchronization", objectId = GroupLDAPSynchronizationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GroupLDAPSynchronization
	#'
	#' This function creates a GroupLDAPSynchronization
	#' @param fieldNames The field values to give the created GroupLDAPSynchronization. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created GroupLDAPSynchronization
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGroupLDAPSynchronization <- function(CommonName = NULL, DistinguishedName = NULL, EntityID = NULL, GroupID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "GroupLDAPSynchronization", body = list(DataObject = body), searchFields = append("GroupLDAPSynchronizationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GroupLDAPSynchronization
	#'
	#' This function modifies a GroupLDAPSynchronization
	#' @param fieldNames The field values to give the modified GroupLDAPSynchronization. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified GroupLDAPSynchronization
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGroupLDAPSynchronization <- function(GroupLDAPSynchronizationID, CommonName = NULL, DistinguishedName = NULL, EntityID = NULL, GroupID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "GroupLDAPSynchronization", objectId = GroupLDAPSynchronizationID, body = list(DataObject = body), searchFields = append("GroupLDAPSynchronizationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFieldRestrictionScreens
	#'
	#' This function returns a dataframe or json object of TempFieldRestrictionScreens
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFieldRestrictionScreens. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFieldRestrictionScreens.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFieldRestrictionScreen') to get more field paths.
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
	#' @concept Security
	#' @return A list of TempFieldRestrictionScreens
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFieldRestrictionScreens <- function(searchConditionsList = NULL, CreatedTime = F, DisplayText = F, ModifiedTime = F, SecurityLocationID = F, TempFieldRestrictionScreenID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "TempFieldRestrictionScreen", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempFieldRestrictionScreen
	#'
	#' This function returns a dataframe or json object of a TempFieldRestrictionScreen
	#' @param TempFieldRestrictionScreenID The ID of the TempFieldRestrictionScreen to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFieldRestrictionScreen. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFieldRestrictionScreen.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFieldRestrictionScreen') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of TempFieldRestrictionScreen
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFieldRestrictionScreen <- function(TempFieldRestrictionScreenID, CreatedTime = F, DisplayText = F, ModifiedTime = F, SecurityLocationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFieldRestrictionScreenID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "TempFieldRestrictionScreen", objectId = TempFieldRestrictionScreenID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempFieldRestrictionScreen
	#'
	#' This function deletes a TempFieldRestrictionScreen
	#' @param TempFieldRestrictionScreenID The ID of the TempFieldRestrictionScreen to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The TempFieldRestrictionScreenID of the deleted TempFieldRestrictionScreen.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempFieldRestrictionScreen <- function(TempFieldRestrictionScreenID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "TempFieldRestrictionScreen", objectId = TempFieldRestrictionScreenID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempFieldRestrictionScreen
	#'
	#' This function creates a TempFieldRestrictionScreen
	#' @param fieldNames The field values to give the created TempFieldRestrictionScreen. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created TempFieldRestrictionScreen
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempFieldRestrictionScreen <- function(DisplayText = NULL, SecurityLocationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "TempFieldRestrictionScreen", body = list(DataObject = body), searchFields = append("TempFieldRestrictionScreenID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempFieldRestrictionScreen
	#'
	#' This function modifies a TempFieldRestrictionScreen
	#' @param fieldNames The field values to give the modified TempFieldRestrictionScreen. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified TempFieldRestrictionScreen
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempFieldRestrictionScreen <- function(TempFieldRestrictionScreenID, DisplayText = NULL, SecurityLocationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "TempFieldRestrictionScreen", objectId = TempFieldRestrictionScreenID, body = list(DataObject = body), searchFields = append("TempFieldRestrictionScreenID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RoleAttachmentTypes
	#'
	#' This function returns a dataframe or json object of RoleAttachmentTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoleAttachmentTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoleAttachmentTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoleAttachmentType') to get more field paths.
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
	#' @concept Security
	#' @return A list of RoleAttachmentTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoleAttachmentTypes <- function(searchConditionsList = NULL, AllowCreate = F, AllowDelete = F, AllowRead = F, AllowUpdate = F, AttachmentTypeID = F, Portal = F, RoleID = F, RoleMenuSecurityItemID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Security", objectName = "RoleAttachmentType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RoleAttachmentType
	#'
	#' This function returns a dataframe or json object of a RoleAttachmentType
	#' @param RoleAttachmentTypeID The ID of the RoleAttachmentType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoleAttachmentType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoleAttachmentType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoleAttachmentType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A dataframe or of RoleAttachmentType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoleAttachmentType <- function(RoleAttachmentTypeID, AllowCreate = F, AllowDelete = F, AllowRead = F, AllowUpdate = F, AttachmentTypeID = F, Portal = F, RoleID = F, RoleMenuSecurityItemID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoleAttachmentTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Security", objectName = "RoleAttachmentType", objectId = RoleAttachmentTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RoleAttachmentType
	#'
	#' This function deletes a RoleAttachmentType
	#' @param RoleAttachmentTypeID The ID of the RoleAttachmentType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The RoleAttachmentTypeID of the deleted RoleAttachmentType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoleAttachmentType <- function(RoleAttachmentTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Security", objectName = "RoleAttachmentType", objectId = RoleAttachmentTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RoleAttachmentType
	#'
	#' This function creates a RoleAttachmentType
	#' @param fieldNames The field values to give the created RoleAttachmentType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return A newly created RoleAttachmentType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoleAttachmentType <- function(AllowCreate = NULL, AllowDelete = NULL, AllowRead = NULL, AllowUpdate = NULL, AttachmentTypeID = NULL, Portal = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Security", objectName = "RoleAttachmentType", body = list(DataObject = body), searchFields = append("RoleAttachmentTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RoleAttachmentType
	#'
	#' This function modifies a RoleAttachmentType
	#' @param fieldNames The field values to give the modified RoleAttachmentType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Security
	#' @return The modified RoleAttachmentType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoleAttachmentType <- function(RoleAttachmentTypeID, AllowCreate = NULL, AllowDelete = NULL, AllowRead = NULL, AllowUpdate = NULL, AttachmentTypeID = NULL, Portal = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Security", objectName = "RoleAttachmentType", objectId = RoleAttachmentTypeID, body = list(DataObject = body), searchFields = append("RoleAttachmentTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
