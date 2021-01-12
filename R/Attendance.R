
	#' List CrossEntityAttendanceReasons
	#'
	#' This function returns a dataframe or json object of CrossEntityAttendanceReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CrossEntityAttendanceReasons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CrossEntityAttendanceReasons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CrossEntityAttendanceReason') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of CrossEntityAttendanceReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCrossEntityAttendanceReasons <- function(searchConditionsList = NULL, AttendanceReasonIDFrom = F, AttendanceReasonIDTo = F, CreatedTime = F, CrossEntityAttendanceReasonID = F, EntityIDTo = F, ModifiedTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CrossEntityAttendanceReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CrossEntityAttendanceReason
	#'
	#' This function returns a dataframe or json object of a CrossEntityAttendanceReason
	#' @param CrossEntityAttendanceReasonID The ID of the CrossEntityAttendanceReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CrossEntityAttendanceReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CrossEntityAttendanceReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CrossEntityAttendanceReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CrossEntityAttendanceReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCrossEntityAttendanceReason <- function(CrossEntityAttendanceReasonID, AttendanceReasonIDFrom = F, AttendanceReasonIDTo = F, CreatedTime = F, EntityIDTo = F, ModifiedTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CrossEntityAttendanceReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CrossEntityAttendanceReason", objectId = CrossEntityAttendanceReasonID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CrossEntityAttendanceReason
	#'
	#' This function deletes a CrossEntityAttendanceReason
	#' @param CrossEntityAttendanceReasonID The ID of the CrossEntityAttendanceReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CrossEntityAttendanceReasonID of the deleted CrossEntityAttendanceReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCrossEntityAttendanceReason <- function(CrossEntityAttendanceReasonID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CrossEntityAttendanceReason", objectId = CrossEntityAttendanceReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CrossEntityAttendanceReason
	#'
	#' This function creates a CrossEntityAttendanceReason
	#' @param fieldNames The field values to give the created CrossEntityAttendanceReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CrossEntityAttendanceReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCrossEntityAttendanceReason <- function(AttendanceReasonIDFrom = NULL, AttendanceReasonIDTo = NULL, EntityIDTo = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "CrossEntityAttendanceReason", body = list(DataObject = body), searchFields = append("CrossEntityAttendanceReasonID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CrossEntityAttendanceReason
	#'
	#' This function modifies a CrossEntityAttendanceReason
	#' @param fieldNames The field values to give the modified CrossEntityAttendanceReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CrossEntityAttendanceReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCrossEntityAttendanceReason <- function(CrossEntityAttendanceReasonID, AttendanceReasonIDFrom = NULL, AttendanceReasonIDTo = NULL, EntityIDTo = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "CrossEntityAttendanceReason", objectId = CrossEntityAttendanceReasonID, body = list(DataObject = body), searchFields = append("CrossEntityAttendanceReasonID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CrossEntityAttendanceTypes
	#'
	#' This function returns a dataframe or json object of CrossEntityAttendanceTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CrossEntityAttendanceTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CrossEntityAttendanceTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CrossEntityAttendanceType') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of CrossEntityAttendanceTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCrossEntityAttendanceTypes <- function(searchConditionsList = NULL, AttendanceTypeIDFrom = F, AttendanceTypeIDTo = F, CreatedTime = F, CrossEntityAttendanceTypeID = F, EntityIDTo = F, ModifiedTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CrossEntityAttendanceType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CrossEntityAttendanceType
	#'
	#' This function returns a dataframe or json object of a CrossEntityAttendanceType
	#' @param CrossEntityAttendanceTypeID The ID of the CrossEntityAttendanceType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CrossEntityAttendanceType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CrossEntityAttendanceType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CrossEntityAttendanceType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CrossEntityAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCrossEntityAttendanceType <- function(CrossEntityAttendanceTypeID, AttendanceTypeIDFrom = F, AttendanceTypeIDTo = F, CreatedTime = F, EntityIDTo = F, ModifiedTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CrossEntityAttendanceTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CrossEntityAttendanceType", objectId = CrossEntityAttendanceTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CrossEntityAttendanceType
	#'
	#' This function deletes a CrossEntityAttendanceType
	#' @param CrossEntityAttendanceTypeID The ID of the CrossEntityAttendanceType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CrossEntityAttendanceTypeID of the deleted CrossEntityAttendanceType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCrossEntityAttendanceType <- function(CrossEntityAttendanceTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CrossEntityAttendanceType", objectId = CrossEntityAttendanceTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CrossEntityAttendanceType
	#'
	#' This function creates a CrossEntityAttendanceType
	#' @param fieldNames The field values to give the created CrossEntityAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CrossEntityAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCrossEntityAttendanceType <- function(AttendanceTypeIDFrom = NULL, AttendanceTypeIDTo = NULL, EntityIDTo = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "CrossEntityAttendanceType", body = list(DataObject = body), searchFields = append("CrossEntityAttendanceTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CrossEntityAttendanceType
	#'
	#' This function modifies a CrossEntityAttendanceType
	#' @param fieldNames The field values to give the modified CrossEntityAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CrossEntityAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCrossEntityAttendanceType <- function(CrossEntityAttendanceTypeID, AttendanceTypeIDFrom = NULL, AttendanceTypeIDTo = NULL, EntityIDTo = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "CrossEntityAttendanceType", objectId = CrossEntityAttendanceTypeID, body = list(DataObject = body), searchFields = append("CrossEntityAttendanceTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CrossEntityCalendarDisplayPeriods
	#'
	#' This function returns a dataframe or json object of CrossEntityCalendarDisplayPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CrossEntityCalendarDisplayPeriods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CrossEntityCalendarDisplayPeriods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CrossEntityCalendarDisplayPeriod') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of CrossEntityCalendarDisplayPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCrossEntityCalendarDisplayPeriods <- function(searchConditionsList = NULL, CalendarDisplayPeriodIDFrom = F, CalendarDisplayPeriodIDTo = F, CreatedTime = F, CrossEntityCalendarDisplayPeriodID = F, CrossEntityCalendarDisplayPeriodIDClonedFrom = F, IsAutoCreated = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CrossEntityCalendarDisplayPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CrossEntityCalendarDisplayPeriod
	#'
	#' This function returns a dataframe or json object of a CrossEntityCalendarDisplayPeriod
	#' @param CrossEntityCalendarDisplayPeriodID The ID of the CrossEntityCalendarDisplayPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CrossEntityCalendarDisplayPeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CrossEntityCalendarDisplayPeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CrossEntityCalendarDisplayPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CrossEntityCalendarDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCrossEntityCalendarDisplayPeriod <- function(CrossEntityCalendarDisplayPeriodID, CalendarDisplayPeriodIDFrom = F, CalendarDisplayPeriodIDTo = F, CreatedTime = F, CrossEntityCalendarDisplayPeriodIDClonedFrom = F, IsAutoCreated = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CrossEntityCalendarDisplayPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CrossEntityCalendarDisplayPeriod", objectId = CrossEntityCalendarDisplayPeriodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CrossEntityCalendarDisplayPeriod
	#'
	#' This function deletes a CrossEntityCalendarDisplayPeriod
	#' @param CrossEntityCalendarDisplayPeriodID The ID of the CrossEntityCalendarDisplayPeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CrossEntityCalendarDisplayPeriodID of the deleted CrossEntityCalendarDisplayPeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCrossEntityCalendarDisplayPeriod <- function(CrossEntityCalendarDisplayPeriodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CrossEntityCalendarDisplayPeriod", objectId = CrossEntityCalendarDisplayPeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CrossEntityCalendarDisplayPeriod
	#'
	#' This function creates a CrossEntityCalendarDisplayPeriod
	#' @param fieldNames The field values to give the created CrossEntityCalendarDisplayPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CrossEntityCalendarDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCrossEntityCalendarDisplayPeriod <- function(CalendarDisplayPeriodIDFrom = NULL, CalendarDisplayPeriodIDTo = NULL, CrossEntityCalendarDisplayPeriodIDClonedFrom = NULL, IsAutoCreated = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "CrossEntityCalendarDisplayPeriod", body = list(DataObject = body), searchFields = append("CrossEntityCalendarDisplayPeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CrossEntityCalendarDisplayPeriod
	#'
	#' This function modifies a CrossEntityCalendarDisplayPeriod
	#' @param fieldNames The field values to give the modified CrossEntityCalendarDisplayPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CrossEntityCalendarDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCrossEntityCalendarDisplayPeriod <- function(CrossEntityCalendarDisplayPeriodID, CalendarDisplayPeriodIDFrom = NULL, CalendarDisplayPeriodIDTo = NULL, CrossEntityCalendarDisplayPeriodIDClonedFrom = NULL, IsAutoCreated = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "CrossEntityCalendarDisplayPeriod", objectId = CrossEntityCalendarDisplayPeriodID, body = list(DataObject = body), searchFields = append("CrossEntityCalendarDisplayPeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Calendars
	#'
	#' This function returns a dataframe or json object of Calendars
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Calendars. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Calendars.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Calendar') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of Calendars
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendars <- function(searchConditionsList = NULL, AttendanceCalculationMethod = F, CalendarID = F, CalendarIDClonedFrom = F, CalendarIDClonedTo = F, CalendarMNID = F, Code = F, CodeDescription = F, CreatedTime = F, DefaultDayLengthMinutes = F, Description = F, EdFiCalendarTypeDescriptorID = F, EndDate = F, EntityID = F, HalfDayHighPeriodCount = F, IsDefault = F, MCCCAcademicYearImportID = F, MCCCCalendarImportID = F, ModifiedTime = F, SchoolYearID = F, StartDate = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZeroDayHighPeriodCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "Calendar", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Calendar
	#'
	#' This function returns a dataframe or json object of a Calendar
	#' @param CalendarID The ID of the Calendar to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Calendar. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Calendar.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Calendar') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of Calendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendar <- function(CalendarID, AttendanceCalculationMethod = F, CalendarIDClonedFrom = F, CalendarIDClonedTo = F, CalendarMNID = F, Code = F, CodeDescription = F, CreatedTime = F, DefaultDayLengthMinutes = F, Description = F, EdFiCalendarTypeDescriptorID = F, EndDate = F, EntityID = F, HalfDayHighPeriodCount = F, IsDefault = F, MCCCAcademicYearImportID = F, MCCCCalendarImportID = F, ModifiedTime = F, SchoolYearID = F, StartDate = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZeroDayHighPeriodCount = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "Calendar", objectId = CalendarID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Calendar
	#'
	#' This function deletes a Calendar
	#' @param CalendarID The ID of the Calendar to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarID of the deleted Calendar.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendar <- function(CalendarID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "Calendar", objectId = CalendarID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Calendar
	#'
	#' This function creates a Calendar
	#' @param fieldNames The field values to give the created Calendar. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created Calendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendar <- function(AttendanceCalculationMethod = NULL, CalendarIDClonedFrom = NULL, Code = NULL, DefaultDayLengthMinutes = NULL, Description = NULL, EdFiCalendarTypeDescriptorID = NULL, EndDate = NULL, EntityID = NULL, HalfDayHighPeriodCount = NULL, IsDefault = NULL, MCCCAcademicYearImportID = NULL, MCCCCalendarImportID = NULL, SchoolYearID = NULL, StartDate = NULL, ZeroDayHighPeriodCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "Calendar", body = list(DataObject = body), searchFields = append("CalendarID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Calendar
	#'
	#' This function modifies a Calendar
	#' @param fieldNames The field values to give the modified Calendar. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified Calendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendar <- function(CalendarID, AttendanceCalculationMethod = NULL, CalendarIDClonedFrom = NULL, Code = NULL, DefaultDayLengthMinutes = NULL, Description = NULL, EdFiCalendarTypeDescriptorID = NULL, EndDate = NULL, EntityID = NULL, HalfDayHighPeriodCount = NULL, IsDefault = NULL, MCCCAcademicYearImportID = NULL, MCCCCalendarImportID = NULL, SchoolYearID = NULL, StartDate = NULL, ZeroDayHighPeriodCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "Calendar", objectId = CalendarID, body = list(DataObject = body), searchFields = append("CalendarID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalendarEvents
	#'
	#' This function returns a dataframe or json object of CalendarEvents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarEvents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarEvents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarEvent') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of CalendarEvents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarEvents <- function(searchConditionsList = NULL, CalendarEventID = F, CalendarEventIDClonedFrom = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EdFiCalendarEventDescriptorID = F, EdFiCalendarEventID = F, EntityID = F, ModifiedTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarEvent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarEvent
	#'
	#' This function returns a dataframe or json object of a CalendarEvent
	#' @param CalendarEventID The ID of the CalendarEvent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarEvent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarEvent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarEvent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarEvent <- function(CalendarEventID, CalendarEventIDClonedFrom = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EdFiCalendarEventDescriptorID = F, EdFiCalendarEventID = F, EntityID = F, ModifiedTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarEventID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarEvent", objectId = CalendarEventID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarEvent
	#'
	#' This function deletes a CalendarEvent
	#' @param CalendarEventID The ID of the CalendarEvent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarEventID of the deleted CalendarEvent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarEvent <- function(CalendarEventID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarEvent", objectId = CalendarEventID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarEvent
	#'
	#' This function creates a CalendarEvent
	#' @param fieldNames The field values to give the created CalendarEvent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarEvent <- function(CalendarEventIDClonedFrom = NULL, Code = NULL, Description = NULL, EdFiCalendarEventDescriptorID = NULL, EdFiCalendarEventID = NULL, EntityID = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "CalendarEvent", body = list(DataObject = body), searchFields = append("CalendarEventID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarEvent
	#'
	#' This function modifies a CalendarEvent
	#' @param fieldNames The field values to give the modified CalendarEvent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarEvent <- function(CalendarEventID, CalendarEventIDClonedFrom = NULL, Code = NULL, Description = NULL, EdFiCalendarEventDescriptorID = NULL, EdFiCalendarEventID = NULL, EntityID = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarEvent", objectId = CalendarEventID, body = list(DataObject = body), searchFields = append("CalendarEventID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalendarDayCalendarEvents
	#'
	#' This function returns a dataframe or json object of CalendarDayCalendarEvents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDayCalendarEvents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDayCalendarEvents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDayCalendarEvent') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of CalendarDayCalendarEvents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarDayCalendarEvents <- function(searchConditionsList = NULL, CalendarDayCalendarEventID = F, CalendarDayID = F, CalendarEventID = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarDayCalendarEvent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarDayCalendarEvent
	#'
	#' This function returns a dataframe or json object of a CalendarDayCalendarEvent
	#' @param CalendarDayCalendarEventID The ID of the CalendarDayCalendarEvent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDayCalendarEvent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDayCalendarEvent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDayCalendarEvent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CalendarDayCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarDayCalendarEvent <- function(CalendarDayCalendarEventID, CalendarDayID = F, CalendarEventID = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarDayCalendarEventID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarDayCalendarEvent", objectId = CalendarDayCalendarEventID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarDayCalendarEvent
	#'
	#' This function deletes a CalendarDayCalendarEvent
	#' @param CalendarDayCalendarEventID The ID of the CalendarDayCalendarEvent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarDayCalendarEventID of the deleted CalendarDayCalendarEvent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarDayCalendarEvent <- function(CalendarDayCalendarEventID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarDayCalendarEvent", objectId = CalendarDayCalendarEventID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarDayCalendarEvent
	#'
	#' This function creates a CalendarDayCalendarEvent
	#' @param fieldNames The field values to give the created CalendarDayCalendarEvent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CalendarDayCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarDayCalendarEvent <- function(CalendarDayID = NULL, CalendarEventID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "CalendarDayCalendarEvent", body = list(DataObject = body), searchFields = append("CalendarDayCalendarEventID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarDayCalendarEvent
	#'
	#' This function modifies a CalendarDayCalendarEvent
	#' @param fieldNames The field values to give the modified CalendarDayCalendarEvent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CalendarDayCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarDayCalendarEvent <- function(CalendarDayCalendarEventID, CalendarDayID = NULL, CalendarEventID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarDayCalendarEvent", objectId = CalendarDayCalendarEventID, body = list(DataObject = body), searchFields = append("CalendarDayCalendarEventID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineThresholds
	#'
	#' This function returns a dataframe or json object of DisciplineThresholds
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineThresholds. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineThresholds.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineThreshold') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of DisciplineThresholds
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineThresholds <- function(searchConditionsList = NULL, ActionID = F, AllowDisciplineOnCurrentDay = F, AttachmentDisplayNameOverride = F, AttendanceLettersRan = F, AttendanceSlipComment = F, BuildingIDServing = F, CreateDisciplineRecord = F, CreatedTime = F, DisciplineSlipComment = F, DisciplineThresholdID = F, DurationToServe = F, DurationToServePerDay = F, FooterComment = F, GenerateActionDetail = F, Greeting = F, IncidentDefaultComment = F, IncidentDescription = F, IsRepeatable = F, LocationIDServing = F, ModifiedTime = F, OffenseID = F, RangeDisplay = F, RoomIDServing = F, ServeOnFriday = F, ServeOnMonday = F, ServeOnSaturday = F, ServeOnSunday = F, ServeOnThursday = F, ServeOnTuesday = F, ServeOnWednesday = F, ServingTime = F, StaffIDAuthorizedBy = F, ThresholdRangeHigh = F, ThresholdRangeLow = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "DisciplineThreshold", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineThreshold
	#'
	#' This function returns a dataframe or json object of a DisciplineThreshold
	#' @param DisciplineThresholdID The ID of the DisciplineThreshold to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineThreshold. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineThreshold.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineThreshold') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of DisciplineThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineThreshold <- function(DisciplineThresholdID, ActionID = F, AllowDisciplineOnCurrentDay = F, AttachmentDisplayNameOverride = F, AttendanceLettersRan = F, AttendanceSlipComment = F, BuildingIDServing = F, CreateDisciplineRecord = F, CreatedTime = F, DisciplineSlipComment = F, DurationToServe = F, DurationToServePerDay = F, FooterComment = F, GenerateActionDetail = F, Greeting = F, IncidentDefaultComment = F, IncidentDescription = F, IsRepeatable = F, LocationIDServing = F, ModifiedTime = F, OffenseID = F, RangeDisplay = F, RoomIDServing = F, ServeOnFriday = F, ServeOnMonday = F, ServeOnSaturday = F, ServeOnSunday = F, ServeOnThursday = F, ServeOnTuesday = F, ServeOnWednesday = F, ServingTime = F, StaffIDAuthorizedBy = F, ThresholdRangeHigh = F, ThresholdRangeLow = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineThresholdID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "DisciplineThreshold", objectId = DisciplineThresholdID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineThreshold
	#'
	#' This function deletes a DisciplineThreshold
	#' @param DisciplineThresholdID The ID of the DisciplineThreshold to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The DisciplineThresholdID of the deleted DisciplineThreshold.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineThreshold <- function(DisciplineThresholdID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "DisciplineThreshold", objectId = DisciplineThresholdID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineThreshold
	#'
	#' This function creates a DisciplineThreshold
	#' @param fieldNames The field values to give the created DisciplineThreshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created DisciplineThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineThreshold <- function(ActionID = NULL, AllowDisciplineOnCurrentDay = NULL, AttachmentDisplayNameOverride = NULL, AttendanceSlipComment = NULL, BuildingIDServing = NULL, CreateDisciplineRecord = NULL, DisciplineSlipComment = NULL, DurationToServe = NULL, DurationToServePerDay = NULL, FooterComment = NULL, GenerateActionDetail = NULL, Greeting = NULL, IncidentDefaultComment = NULL, IncidentDescription = NULL, IsRepeatable = NULL, LocationIDServing = NULL, OffenseID = NULL, RoomIDServing = NULL, ServeOnFriday = NULL, ServeOnMonday = NULL, ServeOnSaturday = NULL, ServeOnSunday = NULL, ServeOnThursday = NULL, ServeOnTuesday = NULL, ServeOnWednesday = NULL, ServingTime = NULL, StaffIDAuthorizedBy = NULL, ThresholdRangeHigh = NULL, ThresholdRangeLow = NULL, ThresholdResetRangeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "DisciplineThreshold", body = list(DataObject = body), searchFields = append("DisciplineThresholdID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineThreshold
	#'
	#' This function modifies a DisciplineThreshold
	#' @param fieldNames The field values to give the modified DisciplineThreshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified DisciplineThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineThreshold <- function(DisciplineThresholdID, ActionID = NULL, AllowDisciplineOnCurrentDay = NULL, AttachmentDisplayNameOverride = NULL, AttendanceSlipComment = NULL, BuildingIDServing = NULL, CreateDisciplineRecord = NULL, DisciplineSlipComment = NULL, DurationToServe = NULL, DurationToServePerDay = NULL, FooterComment = NULL, GenerateActionDetail = NULL, Greeting = NULL, IncidentDefaultComment = NULL, IncidentDescription = NULL, IsRepeatable = NULL, LocationIDServing = NULL, OffenseID = NULL, RoomIDServing = NULL, ServeOnFriday = NULL, ServeOnMonday = NULL, ServeOnSaturday = NULL, ServeOnSunday = NULL, ServeOnThursday = NULL, ServeOnTuesday = NULL, ServeOnWednesday = NULL, ServingTime = NULL, StaffIDAuthorizedBy = NULL, ThresholdRangeHigh = NULL, ThresholdRangeLow = NULL, ThresholdResetRangeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "DisciplineThreshold", objectId = DisciplineThresholdID, body = list(DataObject = body), searchFields = append("DisciplineThresholdID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DroppedStudentAttendancePeriods
	#'
	#' This function returns a dataframe or json object of DroppedStudentAttendancePeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DroppedStudentAttendancePeriods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DroppedStudentAttendancePeriods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DroppedStudentAttendancePeriod') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of DroppedStudentAttendancePeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDroppedStudentAttendancePeriods <- function(searchConditionsList = NULL, AttendancePeriodID = F, AttendanceReasonID = F, AttendanceTypeID = F, CalendarDayID = F, Comment = F, CourseID = F, CreatedTime = F, DroppedStudentAttendancePeriodID = F, IncidentOffenseNameActionDetailID = F, IsGuardianNotified = F, ModifiedTime = F, SectionID = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "DroppedStudentAttendancePeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DroppedStudentAttendancePeriod
	#'
	#' This function returns a dataframe or json object of a DroppedStudentAttendancePeriod
	#' @param DroppedStudentAttendancePeriodID The ID of the DroppedStudentAttendancePeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DroppedStudentAttendancePeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DroppedStudentAttendancePeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DroppedStudentAttendancePeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of DroppedStudentAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDroppedStudentAttendancePeriod <- function(DroppedStudentAttendancePeriodID, AttendancePeriodID = F, AttendanceReasonID = F, AttendanceTypeID = F, CalendarDayID = F, Comment = F, CourseID = F, CreatedTime = F, IncidentOffenseNameActionDetailID = F, IsGuardianNotified = F, ModifiedTime = F, SectionID = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DroppedStudentAttendancePeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "DroppedStudentAttendancePeriod", objectId = DroppedStudentAttendancePeriodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DroppedStudentAttendancePeriod
	#'
	#' This function deletes a DroppedStudentAttendancePeriod
	#' @param DroppedStudentAttendancePeriodID The ID of the DroppedStudentAttendancePeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The DroppedStudentAttendancePeriodID of the deleted DroppedStudentAttendancePeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDroppedStudentAttendancePeriod <- function(DroppedStudentAttendancePeriodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "DroppedStudentAttendancePeriod", objectId = DroppedStudentAttendancePeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DroppedStudentAttendancePeriod
	#'
	#' This function creates a DroppedStudentAttendancePeriod
	#' @param fieldNames The field values to give the created DroppedStudentAttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created DroppedStudentAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDroppedStudentAttendancePeriod <- function(AttendancePeriodID = NULL, AttendanceReasonID = NULL, AttendanceTypeID = NULL, CalendarDayID = NULL, Comment = NULL, CourseID = NULL, IncidentOffenseNameActionDetailID = NULL, IsGuardianNotified = NULL, SectionID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "DroppedStudentAttendancePeriod", body = list(DataObject = body), searchFields = append("DroppedStudentAttendancePeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DroppedStudentAttendancePeriod
	#'
	#' This function modifies a DroppedStudentAttendancePeriod
	#' @param fieldNames The field values to give the modified DroppedStudentAttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified DroppedStudentAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDroppedStudentAttendancePeriod <- function(DroppedStudentAttendancePeriodID, AttendancePeriodID = NULL, AttendanceReasonID = NULL, AttendanceTypeID = NULL, CalendarDayID = NULL, Comment = NULL, CourseID = NULL, IncidentOffenseNameActionDetailID = NULL, IsGuardianNotified = NULL, SectionID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "DroppedStudentAttendancePeriod", objectId = DroppedStudentAttendancePeriodID, body = list(DataObject = body), searchFields = append("DroppedStudentAttendancePeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RoomLayouts
	#'
	#' This function returns a dataframe or json object of RoomLayouts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoomLayouts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoomLayouts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoomLayout') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of RoomLayouts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoomLayouts <- function(searchConditionsList = NULL, CreatedTime = F, Description = F, ModifiedTime = F, RoomID = F, RoomLayoutID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "RoomLayout", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RoomLayout
	#'
	#' This function returns a dataframe or json object of a RoomLayout
	#' @param RoomLayoutID The ID of the RoomLayout to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoomLayout. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoomLayout.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoomLayout') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of RoomLayout
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoomLayout <- function(RoomLayoutID, CreatedTime = F, Description = F, ModifiedTime = F, RoomID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoomLayoutID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "RoomLayout", objectId = RoomLayoutID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RoomLayout
	#'
	#' This function deletes a RoomLayout
	#' @param RoomLayoutID The ID of the RoomLayout to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The RoomLayoutID of the deleted RoomLayout.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoomLayout <- function(RoomLayoutID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "RoomLayout", objectId = RoomLayoutID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RoomLayout
	#'
	#' This function creates a RoomLayout
	#' @param fieldNames The field values to give the created RoomLayout. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created RoomLayout
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoomLayout <- function(Description = NULL, RoomID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "RoomLayout", body = list(DataObject = body), searchFields = append("RoomLayoutID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RoomLayout
	#'
	#' This function modifies a RoomLayout
	#' @param fieldNames The field values to give the modified RoomLayout. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified RoomLayout
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoomLayout <- function(RoomLayoutID, Description = NULL, RoomID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "RoomLayout", objectId = RoomLayoutID, body = list(DataObject = body), searchFields = append("RoomLayoutID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RoomLayoutObjects
	#'
	#' This function returns a dataframe or json object of RoomLayoutObjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoomLayoutObjects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoomLayoutObjects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoomLayoutObject') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of RoomLayoutObjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoomLayoutObjects <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, RoomLayoutID = F, RoomLayoutObjectID = F, RoomObjectID = F, Rotation = F, UserIDCreatedBy = F, UserIDModifiedBy = F, XLocation = F, YLocation = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "RoomLayoutObject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RoomLayoutObject
	#'
	#' This function returns a dataframe or json object of a RoomLayoutObject
	#' @param RoomLayoutObjectID The ID of the RoomLayoutObject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoomLayoutObject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoomLayoutObject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoomLayoutObject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of RoomLayoutObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoomLayoutObject <- function(RoomLayoutObjectID, CreatedTime = F, ModifiedTime = F, RoomLayoutID = F, RoomObjectID = F, Rotation = F, UserIDCreatedBy = F, UserIDModifiedBy = F, XLocation = F, YLocation = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoomLayoutObjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "RoomLayoutObject", objectId = RoomLayoutObjectID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RoomLayoutObject
	#'
	#' This function deletes a RoomLayoutObject
	#' @param RoomLayoutObjectID The ID of the RoomLayoutObject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The RoomLayoutObjectID of the deleted RoomLayoutObject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoomLayoutObject <- function(RoomLayoutObjectID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "RoomLayoutObject", objectId = RoomLayoutObjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RoomLayoutObject
	#'
	#' This function creates a RoomLayoutObject
	#' @param fieldNames The field values to give the created RoomLayoutObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created RoomLayoutObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoomLayoutObject <- function(RoomLayoutID = NULL, RoomObjectID = NULL, Rotation = NULL, XLocation = NULL, YLocation = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "RoomLayoutObject", body = list(DataObject = body), searchFields = append("RoomLayoutObjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RoomLayoutObject
	#'
	#' This function modifies a RoomLayoutObject
	#' @param fieldNames The field values to give the modified RoomLayoutObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified RoomLayoutObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoomLayoutObject <- function(RoomLayoutObjectID, RoomLayoutID = NULL, RoomObjectID = NULL, Rotation = NULL, XLocation = NULL, YLocation = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "RoomLayoutObject", objectId = RoomLayoutObjectID, body = list(DataObject = body), searchFields = append("RoomLayoutObjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RoomObjects
	#'
	#' This function returns a dataframe or json object of RoomObjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoomObjects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoomObjects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoomObject') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of RoomObjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoomObjects <- function(searchConditionsList = NULL, CreatedTime = F, IsStudentSeat = F, Label = F, ModifiedTime = F, Parameters = F, RoomObjectID = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "RoomObject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RoomObject
	#'
	#' This function returns a dataframe or json object of a RoomObject
	#' @param RoomObjectID The ID of the RoomObject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoomObject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoomObject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoomObject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of RoomObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoomObject <- function(RoomObjectID, CreatedTime = F, IsStudentSeat = F, Label = F, ModifiedTime = F, Parameters = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoomObjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "RoomObject", objectId = RoomObjectID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RoomObject
	#'
	#' This function deletes a RoomObject
	#' @param RoomObjectID The ID of the RoomObject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The RoomObjectID of the deleted RoomObject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoomObject <- function(RoomObjectID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "RoomObject", objectId = RoomObjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RoomObject
	#'
	#' This function creates a RoomObject
	#' @param fieldNames The field values to give the created RoomObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created RoomObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoomObject <- function(IsStudentSeat = NULL, Label = NULL, Parameters = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "RoomObject", body = list(DataObject = body), searchFields = append("RoomObjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RoomObject
	#'
	#' This function modifies a RoomObject
	#' @param fieldNames The field values to give the modified RoomObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified RoomObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoomObject <- function(RoomObjectID, IsStudentSeat = NULL, Label = NULL, Parameters = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "RoomObject", objectId = RoomObjectID, body = list(DataObject = body), searchFields = append("RoomObjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SeatingCharts
	#'
	#' This function returns a dataframe or json object of SeatingCharts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SeatingCharts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SeatingCharts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SeatingChart') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of SeatingCharts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSeatingCharts <- function(searchConditionsList = NULL, CreatedTime = F, Description = F, ModifiedTime = F, RoomLayoutID = F, SeatingChartID = F, SeatingChartType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "SeatingChart", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SeatingChart
	#'
	#' This function returns a dataframe or json object of a SeatingChart
	#' @param SeatingChartID The ID of the SeatingChart to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SeatingChart. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SeatingChart.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SeatingChart') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of SeatingChart
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSeatingChart <- function(SeatingChartID, CreatedTime = F, Description = F, ModifiedTime = F, RoomLayoutID = F, SeatingChartType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SeatingChartID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "SeatingChart", objectId = SeatingChartID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SeatingChart
	#'
	#' This function deletes a SeatingChart
	#' @param SeatingChartID The ID of the SeatingChart to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The SeatingChartID of the deleted SeatingChart.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSeatingChart <- function(SeatingChartID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "SeatingChart", objectId = SeatingChartID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SeatingChart
	#'
	#' This function creates a SeatingChart
	#' @param fieldNames The field values to give the created SeatingChart. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created SeatingChart
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSeatingChart <- function(Description = NULL, RoomLayoutID = NULL, SeatingChartType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "SeatingChart", body = list(DataObject = body), searchFields = append("SeatingChartID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SeatingChart
	#'
	#' This function modifies a SeatingChart
	#' @param fieldNames The field values to give the modified SeatingChart. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified SeatingChart
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySeatingChart <- function(SeatingChartID, Description = NULL, RoomLayoutID = NULL, SeatingChartType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "SeatingChart", objectId = SeatingChartID, body = list(DataObject = body), searchFields = append("SeatingChartID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SeatingChartMeets
	#'
	#' This function returns a dataframe or json object of SeatingChartMeets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SeatingChartMeets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SeatingChartMeets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SeatingChartMeet') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of SeatingChartMeets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSeatingChartMeets <- function(searchConditionsList = NULL, CreatedTime = F, IsCurrent = F, MeetID = F, ModifiedTime = F, SeatingChartID = F, SeatingChartMeetID = F, SectionList = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "SeatingChartMeet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SeatingChartMeet
	#'
	#' This function returns a dataframe or json object of a SeatingChartMeet
	#' @param SeatingChartMeetID The ID of the SeatingChartMeet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SeatingChartMeet. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SeatingChartMeet.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SeatingChartMeet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of SeatingChartMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSeatingChartMeet <- function(SeatingChartMeetID, CreatedTime = F, IsCurrent = F, MeetID = F, ModifiedTime = F, SeatingChartID = F, SectionList = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SeatingChartMeetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "SeatingChartMeet", objectId = SeatingChartMeetID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SeatingChartMeet
	#'
	#' This function deletes a SeatingChartMeet
	#' @param SeatingChartMeetID The ID of the SeatingChartMeet to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The SeatingChartMeetID of the deleted SeatingChartMeet.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSeatingChartMeet <- function(SeatingChartMeetID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "SeatingChartMeet", objectId = SeatingChartMeetID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SeatingChartMeet
	#'
	#' This function creates a SeatingChartMeet
	#' @param fieldNames The field values to give the created SeatingChartMeet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created SeatingChartMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSeatingChartMeet <- function(IsCurrent = NULL, MeetID = NULL, SeatingChartID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "SeatingChartMeet", body = list(DataObject = body), searchFields = append("SeatingChartMeetID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SeatingChartMeet
	#'
	#' This function modifies a SeatingChartMeet
	#' @param fieldNames The field values to give the modified SeatingChartMeet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified SeatingChartMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySeatingChartMeet <- function(SeatingChartMeetID, IsCurrent = NULL, MeetID = NULL, SeatingChartID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "SeatingChartMeet", objectId = SeatingChartMeetID, body = list(DataObject = body), searchFields = append("SeatingChartMeetID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SeatingChartSeats
	#'
	#' This function returns a dataframe or json object of SeatingChartSeats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SeatingChartSeats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SeatingChartSeats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SeatingChartSeat') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of SeatingChartSeats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSeatingChartSeats <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, RoomLayoutObjectID = F, SeatingChartID = F, SeatingChartSeatID = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "SeatingChartSeat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SeatingChartSeat
	#'
	#' This function returns a dataframe or json object of a SeatingChartSeat
	#' @param SeatingChartSeatID The ID of the SeatingChartSeat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SeatingChartSeat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SeatingChartSeat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SeatingChartSeat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of SeatingChartSeat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSeatingChartSeat <- function(SeatingChartSeatID, CreatedTime = F, ModifiedTime = F, RoomLayoutObjectID = F, SeatingChartID = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SeatingChartSeatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "SeatingChartSeat", objectId = SeatingChartSeatID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SeatingChartSeat
	#'
	#' This function deletes a SeatingChartSeat
	#' @param SeatingChartSeatID The ID of the SeatingChartSeat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The SeatingChartSeatID of the deleted SeatingChartSeat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSeatingChartSeat <- function(SeatingChartSeatID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "SeatingChartSeat", objectId = SeatingChartSeatID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SeatingChartSeat
	#'
	#' This function creates a SeatingChartSeat
	#' @param fieldNames The field values to give the created SeatingChartSeat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created SeatingChartSeat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSeatingChartSeat <- function(RoomLayoutObjectID = NULL, SeatingChartID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "SeatingChartSeat", body = list(DataObject = body), searchFields = append("SeatingChartSeatID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SeatingChartSeat
	#'
	#' This function modifies a SeatingChartSeat
	#' @param fieldNames The field values to give the modified SeatingChartSeat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified SeatingChartSeat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySeatingChartSeat <- function(SeatingChartSeatID, RoomLayoutObjectID = NULL, SeatingChartID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "SeatingChartSeat", objectId = SeatingChartSeatID, body = list(DataObject = body), searchFields = append("SeatingChartSeatID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThresholdResetRangeAttendanceTypes
	#'
	#' This function returns a dataframe or json object of ThresholdResetRangeAttendanceTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdResetRangeAttendanceTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdResetRangeAttendanceTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdResetRangeAttendanceType') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of ThresholdResetRangeAttendanceTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThresholdResetRangeAttendanceTypes <- function(searchConditionsList = NULL, AttendanceTypeID = F, CreatedTime = F, ModifiedTime = F, ThresholdResetRangeAttendanceTypeID = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "ThresholdResetRangeAttendanceType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThresholdResetRangeAttendanceType
	#'
	#' This function returns a dataframe or json object of a ThresholdResetRangeAttendanceType
	#' @param ThresholdResetRangeAttendanceTypeID The ID of the ThresholdResetRangeAttendanceType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdResetRangeAttendanceType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdResetRangeAttendanceType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdResetRangeAttendanceType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of ThresholdResetRangeAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThresholdResetRangeAttendanceType <- function(ThresholdResetRangeAttendanceTypeID, AttendanceTypeID = F, CreatedTime = F, ModifiedTime = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThresholdResetRangeAttendanceTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendanceType", objectId = ThresholdResetRangeAttendanceTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThresholdResetRangeAttendanceType
	#'
	#' This function deletes a ThresholdResetRangeAttendanceType
	#' @param ThresholdResetRangeAttendanceTypeID The ID of the ThresholdResetRangeAttendanceType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The ThresholdResetRangeAttendanceTypeID of the deleted ThresholdResetRangeAttendanceType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThresholdResetRangeAttendanceType <- function(ThresholdResetRangeAttendanceTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendanceType", objectId = ThresholdResetRangeAttendanceTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThresholdResetRangeAttendanceType
	#'
	#' This function creates a ThresholdResetRangeAttendanceType
	#' @param fieldNames The field values to give the created ThresholdResetRangeAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created ThresholdResetRangeAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThresholdResetRangeAttendanceType <- function(AttendanceTypeID = NULL, ThresholdResetRangeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendanceType", body = list(DataObject = body), searchFields = append("ThresholdResetRangeAttendanceTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThresholdResetRangeAttendanceType
	#'
	#' This function modifies a ThresholdResetRangeAttendanceType
	#' @param fieldNames The field values to give the modified ThresholdResetRangeAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified ThresholdResetRangeAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThresholdResetRangeAttendanceType <- function(ThresholdResetRangeAttendanceTypeID, AttendanceTypeID = NULL, ThresholdResetRangeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendanceType", objectId = ThresholdResetRangeAttendanceTypeID, body = list(DataObject = body), searchFields = append("ThresholdResetRangeAttendanceTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThresholdResetRanges
	#'
	#' This function returns a dataframe or json object of ThresholdResetRanges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdResetRanges. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdResetRanges.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdResetRange') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of ThresholdResetRanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThresholdResetRanges <- function(searchConditionsList = NULL, AttendanceLettersRan = F, AttendanceTypeCodes = F, CountType = F, CreatedTime = F, DateDisplay = F, DateHigh = F, DateLow = F, DateType = F, DayCountType = F, Description = F, EntityID = F, IsForAttendanceLetters = F, IsForTardyKiosk = F, ModifiedTime = F, NumberOfDays = F, NumberPerDay = F, ResetRangeAttendanceTypes = F, SchoolYearID = F, ThresholdResetRangeID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "ThresholdResetRange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThresholdResetRange
	#'
	#' This function returns a dataframe or json object of a ThresholdResetRange
	#' @param ThresholdResetRangeID The ID of the ThresholdResetRange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdResetRange. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdResetRange.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdResetRange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of ThresholdResetRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThresholdResetRange <- function(ThresholdResetRangeID, AttendanceLettersRan = F, AttendanceTypeCodes = F, CountType = F, CreatedTime = F, DateDisplay = F, DateHigh = F, DateLow = F, DateType = F, DayCountType = F, Description = F, EntityID = F, IsForAttendanceLetters = F, IsForTardyKiosk = F, ModifiedTime = F, NumberOfDays = F, NumberPerDay = F, ResetRangeAttendanceTypes = F, SchoolYearID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThresholdResetRangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "ThresholdResetRange", objectId = ThresholdResetRangeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThresholdResetRange
	#'
	#' This function deletes a ThresholdResetRange
	#' @param ThresholdResetRangeID The ID of the ThresholdResetRange to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The ThresholdResetRangeID of the deleted ThresholdResetRange.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThresholdResetRange <- function(ThresholdResetRangeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "ThresholdResetRange", objectId = ThresholdResetRangeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThresholdResetRange
	#'
	#' This function creates a ThresholdResetRange
	#' @param fieldNames The field values to give the created ThresholdResetRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created ThresholdResetRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThresholdResetRange <- function(CountType = NULL, DateHigh = NULL, DateLow = NULL, DateType = NULL, DayCountType = NULL, Description = NULL, EntityID = NULL, NumberOfDays = NULL, NumberPerDay = NULL, SchoolYearID = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "ThresholdResetRange", body = list(DataObject = body), searchFields = append("ThresholdResetRangeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThresholdResetRange
	#'
	#' This function modifies a ThresholdResetRange
	#' @param fieldNames The field values to give the modified ThresholdResetRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified ThresholdResetRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThresholdResetRange <- function(ThresholdResetRangeID, CountType = NULL, DateHigh = NULL, DateLow = NULL, DateType = NULL, DayCountType = NULL, Description = NULL, EntityID = NULL, NumberOfDays = NULL, NumberPerDay = NULL, SchoolYearID = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "ThresholdResetRange", objectId = ThresholdResetRangeID, body = list(DataObject = body), searchFields = append("ThresholdResetRangeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCalendars
	#'
	#' This function returns a dataframe or json object of TempCalendars
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendars. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendars.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendar') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempCalendars
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalendars <- function(searchConditionsList = NULL, AffectedPrimaryKey = F, CalendarID = F, Code = F, CodeDescription = F, CreatedTime = F, EndDate = F, IsDefault = F, IsUpdated = F, ModifiedTime = F, NewEndDate = F, NewStartDate = F, OldEndDate = F, OldStartDate = F, OriginalEndDate = F, OriginalStartDate = F, ProcessAction = F, StartDate = F, TempCalendarID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCalendar", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCalendar
	#'
	#' This function returns a dataframe or json object of a TempCalendar
	#' @param TempCalendarID The ID of the TempCalendar to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendar. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendar.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendar') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalendar <- function(TempCalendarID, AffectedPrimaryKey = F, CalendarID = F, Code = F, CodeDescription = F, CreatedTime = F, EndDate = F, IsDefault = F, IsUpdated = F, ModifiedTime = F, NewEndDate = F, NewStartDate = F, OldEndDate = F, OldStartDate = F, OriginalEndDate = F, OriginalStartDate = F, ProcessAction = F, StartDate = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalendarID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCalendar", objectId = TempCalendarID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCalendar
	#'
	#' This function deletes a TempCalendar
	#' @param TempCalendarID The ID of the TempCalendar to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCalendarID of the deleted TempCalendar.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCalendar <- function(TempCalendarID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCalendar", objectId = TempCalendarID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCalendar
	#'
	#' This function creates a TempCalendar
	#' @param fieldNames The field values to give the created TempCalendar. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCalendar <- function(AffectedPrimaryKey = NULL, CalendarID = NULL, Code = NULL, CodeDescription = NULL, EndDate = NULL, IsDefault = NULL, NewEndDate = NULL, NewStartDate = NULL, OldEndDate = NULL, OldStartDate = NULL, OriginalEndDate = NULL, OriginalStartDate = NULL, ProcessAction = NULL, StartDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempCalendar", body = list(DataObject = body), searchFields = append("TempCalendarID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCalendar
	#'
	#' This function modifies a TempCalendar
	#' @param fieldNames The field values to give the modified TempCalendar. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCalendar <- function(TempCalendarID, AffectedPrimaryKey = NULL, CalendarID = NULL, Code = NULL, CodeDescription = NULL, EndDate = NULL, IsDefault = NULL, NewEndDate = NULL, NewStartDate = NULL, OldEndDate = NULL, OldStartDate = NULL, OriginalEndDate = NULL, OriginalStartDate = NULL, ProcessAction = NULL, StartDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempCalendar", objectId = TempCalendarID, body = list(DataObject = body), searchFields = append("TempCalendarID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentAttendanceRecords
	#'
	#' This function returns a dataframe or json object of TempStudentAttendanceRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentAttendanceRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentAttendanceRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentAttendanceRecord') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempStudentAttendanceRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentAttendanceRecords <- function(searchConditionsList = NULL, AffectedPrimaryKey = F, AttendanceTakenByPeriod = F, CreatedTime = F, Date = F, DayOfTheWeek = F, DayRotation = F, DayRotationID = F, GuardianNotified = F, ModifiedTime = F, StudentName = F, StudentNumber = F, TempStudentAttendanceRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempStudentAttendanceRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentAttendanceRecord
	#'
	#' This function returns a dataframe or json object of a TempStudentAttendanceRecord
	#' @param TempStudentAttendanceRecordID The ID of the TempStudentAttendanceRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentAttendanceRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentAttendanceRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentAttendanceRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempStudentAttendanceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentAttendanceRecord <- function(TempStudentAttendanceRecordID, AffectedPrimaryKey = F, AttendanceTakenByPeriod = F, CreatedTime = F, Date = F, DayOfTheWeek = F, DayRotation = F, DayRotationID = F, GuardianNotified = F, ModifiedTime = F, StudentName = F, StudentNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentAttendanceRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempStudentAttendanceRecord", objectId = TempStudentAttendanceRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentAttendanceRecord
	#'
	#' This function deletes a TempStudentAttendanceRecord
	#' @param TempStudentAttendanceRecordID The ID of the TempStudentAttendanceRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempStudentAttendanceRecordID of the deleted TempStudentAttendanceRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentAttendanceRecord <- function(TempStudentAttendanceRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempStudentAttendanceRecord", objectId = TempStudentAttendanceRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentAttendanceRecord
	#'
	#' This function creates a TempStudentAttendanceRecord
	#' @param fieldNames The field values to give the created TempStudentAttendanceRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempStudentAttendanceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentAttendanceRecord <- function(AffectedPrimaryKey = NULL, AttendanceTakenByPeriod = NULL, Date = NULL, DayOfTheWeek = NULL, DayRotation = NULL, DayRotationID = NULL, GuardianNotified = NULL, StudentName = NULL, StudentNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempStudentAttendanceRecord", body = list(DataObject = body), searchFields = append("TempStudentAttendanceRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentAttendanceRecord
	#'
	#' This function modifies a TempStudentAttendanceRecord
	#' @param fieldNames The field values to give the modified TempStudentAttendanceRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempStudentAttendanceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentAttendanceRecord <- function(TempStudentAttendanceRecordID, AffectedPrimaryKey = NULL, AttendanceTakenByPeriod = NULL, Date = NULL, DayOfTheWeek = NULL, DayRotation = NULL, DayRotationID = NULL, GuardianNotified = NULL, StudentName = NULL, StudentNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempStudentAttendanceRecord", objectId = TempStudentAttendanceRecordID, body = list(DataObject = body), searchFields = append("TempStudentAttendanceRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAffectedStudentAttendanceRecords
	#'
	#' This function returns a dataframe or json object of TempAffectedStudentAttendanceRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAffectedStudentAttendanceRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAffectedStudentAttendanceRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAffectedStudentAttendanceRecord') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempAffectedStudentAttendanceRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAffectedStudentAttendanceRecords <- function(searchConditionsList = NULL, AffectedPrimaryKey = F, CalendarDayID = F, Comment = F, CreatedTime = F, Date = F, DayRotationCode = F, FailedStudentAttendancePeriods = F, FailureReason = F, FullName = F, IsGuardianNotified = F, ModifiedTime = F, NewDaysAbsent = F, NewDaysExcused = F, NewDaysOther = F, NewDaysUnexcused = F, NewGuardianNotified = F, NewStudentAttendancePeriods = F, NewTardyCount = F, OldDaysAbsent = F, OldDaysExcused = F, OldDaysOther = F, OldDaysUnexcused = F, OldStudentAttendancePeriods = F, OldTardyCount = F, PreviousGuardianNotified = F, StudentID = F, StudentNumber = F, TempAffectedStudentAttendanceRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempAffectedStudentAttendanceRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAffectedStudentAttendanceRecord
	#'
	#' This function returns a dataframe or json object of a TempAffectedStudentAttendanceRecord
	#' @param TempAffectedStudentAttendanceRecordID The ID of the TempAffectedStudentAttendanceRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAffectedStudentAttendanceRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAffectedStudentAttendanceRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAffectedStudentAttendanceRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempAffectedStudentAttendanceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAffectedStudentAttendanceRecord <- function(TempAffectedStudentAttendanceRecordID, AffectedPrimaryKey = F, CalendarDayID = F, Comment = F, CreatedTime = F, Date = F, DayRotationCode = F, FailedStudentAttendancePeriods = F, FailureReason = F, FullName = F, IsGuardianNotified = F, ModifiedTime = F, NewDaysAbsent = F, NewDaysExcused = F, NewDaysOther = F, NewDaysUnexcused = F, NewGuardianNotified = F, NewStudentAttendancePeriods = F, NewTardyCount = F, OldDaysAbsent = F, OldDaysExcused = F, OldDaysOther = F, OldDaysUnexcused = F, OldStudentAttendancePeriods = F, OldTardyCount = F, PreviousGuardianNotified = F, StudentID = F, StudentNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAffectedStudentAttendanceRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendanceRecord", objectId = TempAffectedStudentAttendanceRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAffectedStudentAttendanceRecord
	#'
	#' This function deletes a TempAffectedStudentAttendanceRecord
	#' @param TempAffectedStudentAttendanceRecordID The ID of the TempAffectedStudentAttendanceRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempAffectedStudentAttendanceRecordID of the deleted TempAffectedStudentAttendanceRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAffectedStudentAttendanceRecord <- function(TempAffectedStudentAttendanceRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendanceRecord", objectId = TempAffectedStudentAttendanceRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAffectedStudentAttendanceRecord
	#'
	#' This function creates a TempAffectedStudentAttendanceRecord
	#' @param fieldNames The field values to give the created TempAffectedStudentAttendanceRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempAffectedStudentAttendanceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAffectedStudentAttendanceRecord <- function(AffectedPrimaryKey = NULL, CalendarDayID = NULL, Comment = NULL, Date = NULL, DayRotationCode = NULL, FailedStudentAttendancePeriods = NULL, FailureReason = NULL, FullName = NULL, IsGuardianNotified = NULL, NewDaysAbsent = NULL, NewDaysExcused = NULL, NewDaysOther = NULL, NewDaysUnexcused = NULL, NewGuardianNotified = NULL, NewStudentAttendancePeriods = NULL, NewTardyCount = NULL, OldDaysAbsent = NULL, OldDaysExcused = NULL, OldDaysOther = NULL, OldDaysUnexcused = NULL, OldStudentAttendancePeriods = NULL, OldTardyCount = NULL, PreviousGuardianNotified = NULL, StudentID = NULL, StudentNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendanceRecord", body = list(DataObject = body), searchFields = append("TempAffectedStudentAttendanceRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAffectedStudentAttendanceRecord
	#'
	#' This function modifies a TempAffectedStudentAttendanceRecord
	#' @param fieldNames The field values to give the modified TempAffectedStudentAttendanceRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempAffectedStudentAttendanceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAffectedStudentAttendanceRecord <- function(TempAffectedStudentAttendanceRecordID, AffectedPrimaryKey = NULL, CalendarDayID = NULL, Comment = NULL, Date = NULL, DayRotationCode = NULL, FailedStudentAttendancePeriods = NULL, FailureReason = NULL, FullName = NULL, IsGuardianNotified = NULL, NewDaysAbsent = NULL, NewDaysExcused = NULL, NewDaysOther = NULL, NewDaysUnexcused = NULL, NewGuardianNotified = NULL, NewStudentAttendancePeriods = NULL, NewTardyCount = NULL, OldDaysAbsent = NULL, OldDaysExcused = NULL, OldDaysOther = NULL, OldDaysUnexcused = NULL, OldStudentAttendancePeriods = NULL, OldTardyCount = NULL, PreviousGuardianNotified = NULL, StudentID = NULL, StudentNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendanceRecord", objectId = TempAffectedStudentAttendanceRecordID, body = list(DataObject = body), searchFields = append("TempAffectedStudentAttendanceRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAffectedStudentAttendancePeriodRecords
	#'
	#' This function returns a dataframe or json object of TempAffectedStudentAttendancePeriodRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAffectedStudentAttendancePeriodRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAffectedStudentAttendancePeriodRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAffectedStudentAttendancePeriodRecord') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempAffectedStudentAttendancePeriodRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAffectedStudentAttendancePeriodRecords <- function(searchConditionsList = NULL, Action = F, AffectedPrimaryKey = F, AttendanceCategory = F, AttendancePeriodID = F, AttendanceReasonCodeDescription = F, AttendanceReasonID = F, AttendanceTypeCodeDescription = F, AttendanceTypeID = F, CalendarDayID = F, CalendarID = F, CECEAttendancePeriodID = F, CECEAttendanceReasonID = F, CECEAttendanceTypeID = F, Comment = F, CreatedTime = F, Date = F, DayRotationCode = F, Entity = F, FailureReason = F, FullName = F, IsForCECEAttendancePeriod = F, IsGuardianNotified = F, ModifiedTime = F, NewStudentSectionCode = F, NewStudentSectionID = F, OldStudentSectionCode = F, OldStudentSectionID = F, PeriodCode = F, ProcessFromCECEEntity = F, StudentAttendanceID = F, StudentID = F, StudentNumber = F, TempAffectedStudentAttendancePeriodRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempAffectedStudentAttendancePeriodRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAffectedStudentAttendancePeriodRecord
	#'
	#' This function returns a dataframe or json object of a TempAffectedStudentAttendancePeriodRecord
	#' @param TempAffectedStudentAttendancePeriodRecordID The ID of the TempAffectedStudentAttendancePeriodRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAffectedStudentAttendancePeriodRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAffectedStudentAttendancePeriodRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAffectedStudentAttendancePeriodRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempAffectedStudentAttendancePeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAffectedStudentAttendancePeriodRecord <- function(TempAffectedStudentAttendancePeriodRecordID, Action = F, AffectedPrimaryKey = F, AttendanceCategory = F, AttendancePeriodID = F, AttendanceReasonCodeDescription = F, AttendanceReasonID = F, AttendanceTypeCodeDescription = F, AttendanceTypeID = F, CalendarDayID = F, CalendarID = F, CECEAttendancePeriodID = F, CECEAttendanceReasonID = F, CECEAttendanceTypeID = F, Comment = F, CreatedTime = F, Date = F, DayRotationCode = F, Entity = F, FailureReason = F, FullName = F, IsForCECEAttendancePeriod = F, IsGuardianNotified = F, ModifiedTime = F, NewStudentSectionCode = F, NewStudentSectionID = F, OldStudentSectionCode = F, OldStudentSectionID = F, PeriodCode = F, ProcessFromCECEEntity = F, StudentAttendanceID = F, StudentID = F, StudentNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAffectedStudentAttendancePeriodRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendancePeriodRecord", objectId = TempAffectedStudentAttendancePeriodRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAffectedStudentAttendancePeriodRecord
	#'
	#' This function deletes a TempAffectedStudentAttendancePeriodRecord
	#' @param TempAffectedStudentAttendancePeriodRecordID The ID of the TempAffectedStudentAttendancePeriodRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempAffectedStudentAttendancePeriodRecordID of the deleted TempAffectedStudentAttendancePeriodRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAffectedStudentAttendancePeriodRecord <- function(TempAffectedStudentAttendancePeriodRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendancePeriodRecord", objectId = TempAffectedStudentAttendancePeriodRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAffectedStudentAttendancePeriodRecord
	#'
	#' This function creates a TempAffectedStudentAttendancePeriodRecord
	#' @param fieldNames The field values to give the created TempAffectedStudentAttendancePeriodRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempAffectedStudentAttendancePeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAffectedStudentAttendancePeriodRecord <- function(Action = NULL, AffectedPrimaryKey = NULL, AttendanceCategory = NULL, AttendancePeriodID = NULL, AttendanceReasonCodeDescription = NULL, AttendanceReasonID = NULL, AttendanceTypeCodeDescription = NULL, AttendanceTypeID = NULL, CalendarDayID = NULL, CalendarID = NULL, CECEAttendancePeriodID = NULL, CECEAttendanceReasonID = NULL, CECEAttendanceTypeID = NULL, Comment = NULL, Date = NULL, DayRotationCode = NULL, Entity = NULL, FailureReason = NULL, FullName = NULL, IsForCECEAttendancePeriod = NULL, IsGuardianNotified = NULL, NewStudentSectionCode = NULL, NewStudentSectionID = NULL, OldStudentSectionCode = NULL, OldStudentSectionID = NULL, PeriodCode = NULL, ProcessFromCECEEntity = NULL, StudentAttendanceID = NULL, StudentID = NULL, StudentNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendancePeriodRecord", body = list(DataObject = body), searchFields = append("TempAffectedStudentAttendancePeriodRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAffectedStudentAttendancePeriodRecord
	#'
	#' This function modifies a TempAffectedStudentAttendancePeriodRecord
	#' @param fieldNames The field values to give the modified TempAffectedStudentAttendancePeriodRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempAffectedStudentAttendancePeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAffectedStudentAttendancePeriodRecord <- function(TempAffectedStudentAttendancePeriodRecordID, Action = NULL, AffectedPrimaryKey = NULL, AttendanceCategory = NULL, AttendancePeriodID = NULL, AttendanceReasonCodeDescription = NULL, AttendanceReasonID = NULL, AttendanceTypeCodeDescription = NULL, AttendanceTypeID = NULL, CalendarDayID = NULL, CalendarID = NULL, CECEAttendancePeriodID = NULL, CECEAttendanceReasonID = NULL, CECEAttendanceTypeID = NULL, Comment = NULL, Date = NULL, DayRotationCode = NULL, Entity = NULL, FailureReason = NULL, FullName = NULL, IsForCECEAttendancePeriod = NULL, IsGuardianNotified = NULL, NewStudentSectionCode = NULL, NewStudentSectionID = NULL, OldStudentSectionCode = NULL, OldStudentSectionID = NULL, PeriodCode = NULL, ProcessFromCECEEntity = NULL, StudentAttendanceID = NULL, StudentID = NULL, StudentNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendancePeriodRecord", objectId = TempAffectedStudentAttendancePeriodRecordID, body = list(DataObject = body), searchFields = append("TempAffectedStudentAttendancePeriodRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StaffMeetSettings
	#'
	#' This function returns a dataframe or json object of StaffMeetSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffMeetSettings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffMeetSettings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffMeetSetting') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of StaffMeetSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffMeetSettings <- function(searchConditionsList = NULL, BrowseViewID = F, CreatedTime = F, DisplayAttendanceTotalsOnDesktop = F, DisplayAttendanceTotalsOnMobile = F, DisplayCourseDescription = F, DisplayHistoricalAttendanceOnDesktop = F, DisplayHistoricalAttendanceOnMobile = F, DisplayMethodOfInstruction = F, DisplayStudentGradeLevel = F, DisplayStudentNumber = F, HideLockedColumns = F, ModifiedTime = F, StaffMeetID = F, StaffMeetSettingID = F, StudentNameDisplayType = F, UseCustomClassRosterSort = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StaffMeetSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StaffMeetSetting
	#'
	#' This function returns a dataframe or json object of a StaffMeetSetting
	#' @param StaffMeetSettingID The ID of the StaffMeetSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffMeetSetting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffMeetSetting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffMeetSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StaffMeetSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffMeetSetting <- function(StaffMeetSettingID, BrowseViewID = F, CreatedTime = F, DisplayAttendanceTotalsOnDesktop = F, DisplayAttendanceTotalsOnMobile = F, DisplayCourseDescription = F, DisplayHistoricalAttendanceOnDesktop = F, DisplayHistoricalAttendanceOnMobile = F, DisplayMethodOfInstruction = F, DisplayStudentGradeLevel = F, DisplayStudentNumber = F, HideLockedColumns = F, ModifiedTime = F, StaffMeetID = F, StudentNameDisplayType = F, UseCustomClassRosterSort = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffMeetSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StaffMeetSetting", objectId = StaffMeetSettingID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StaffMeetSetting
	#'
	#' This function deletes a StaffMeetSetting
	#' @param StaffMeetSettingID The ID of the StaffMeetSetting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StaffMeetSettingID of the deleted StaffMeetSetting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStaffMeetSetting <- function(StaffMeetSettingID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StaffMeetSetting", objectId = StaffMeetSettingID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StaffMeetSetting
	#'
	#' This function creates a StaffMeetSetting
	#' @param fieldNames The field values to give the created StaffMeetSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StaffMeetSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStaffMeetSetting <- function(BrowseViewID = NULL, DisplayAttendanceTotalsOnDesktop = NULL, DisplayAttendanceTotalsOnMobile = NULL, DisplayCourseDescription = NULL, DisplayHistoricalAttendanceOnDesktop = NULL, DisplayHistoricalAttendanceOnMobile = NULL, DisplayMethodOfInstruction = NULL, DisplayStudentGradeLevel = NULL, DisplayStudentNumber = NULL, HideLockedColumns = NULL, StaffMeetID = NULL, StudentNameDisplayType = NULL, UseCustomClassRosterSort = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "StaffMeetSetting", body = list(DataObject = body), searchFields = append("StaffMeetSettingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StaffMeetSetting
	#'
	#' This function modifies a StaffMeetSetting
	#' @param fieldNames The field values to give the modified StaffMeetSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StaffMeetSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStaffMeetSetting <- function(StaffMeetSettingID, BrowseViewID = NULL, DisplayAttendanceTotalsOnDesktop = NULL, DisplayAttendanceTotalsOnMobile = NULL, DisplayCourseDescription = NULL, DisplayHistoricalAttendanceOnDesktop = NULL, DisplayHistoricalAttendanceOnMobile = NULL, DisplayMethodOfInstruction = NULL, DisplayStudentGradeLevel = NULL, DisplayStudentNumber = NULL, HideLockedColumns = NULL, StaffMeetID = NULL, StudentNameDisplayType = NULL, UseCustomClassRosterSort = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "StaffMeetSetting", objectId = StaffMeetSettingID, body = list(DataObject = body), searchFields = append("StaffMeetSettingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RecordedUnrecordedAttendances
	#'
	#' This function returns a dataframe or json object of RecordedUnrecordedAttendances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RecordedUnrecordedAttendances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RecordedUnrecordedAttendances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RecordedUnrecordedAttendance') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of RecordedUnrecordedAttendances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRecordedUnrecordedAttendances <- function(searchConditionsList = NULL, AllStudentsHaveAttendance = F, AttendanceTaken = F, CountAs = F, CreatedTime = F, DailySectionAttendanceID = F, Date = F, DayOfTheWeek = F, DisplayPeriodCode = F, MeetDisplayPeriodID = F, MeetID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "RecordedUnrecordedAttendance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RecordedUnrecordedAttendance
	#'
	#' This function returns a dataframe or json object of a RecordedUnrecordedAttendance
	#' @param RecordedUnrecordedAttendanceID The ID of the RecordedUnrecordedAttendance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RecordedUnrecordedAttendance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RecordedUnrecordedAttendance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RecordedUnrecordedAttendance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of RecordedUnrecordedAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRecordedUnrecordedAttendance <- function(RecordedUnrecordedAttendanceID, AllStudentsHaveAttendance = F, AttendanceTaken = F, CountAs = F, CreatedTime = F, DailySectionAttendanceID = F, Date = F, DayOfTheWeek = F, DisplayPeriodCode = F, MeetDisplayPeriodID = F, MeetID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RecordedUnrecordedAttendanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "RecordedUnrecordedAttendance", objectId = RecordedUnrecordedAttendanceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RecordedUnrecordedAttendance
	#'
	#' This function deletes a RecordedUnrecordedAttendance
	#' @param RecordedUnrecordedAttendanceID The ID of the RecordedUnrecordedAttendance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The RecordedUnrecordedAttendanceID of the deleted RecordedUnrecordedAttendance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRecordedUnrecordedAttendance <- function(RecordedUnrecordedAttendanceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "RecordedUnrecordedAttendance", objectId = RecordedUnrecordedAttendanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DailySectionAttendances
	#'
	#' This function returns a dataframe or json object of DailySectionAttendances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DailySectionAttendances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DailySectionAttendances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DailySectionAttendance') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of DailySectionAttendances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDailySectionAttendances <- function(searchConditionsList = NULL, AttendancePeriodID = F, CalendarDayID = F, CreatedTime = F, DailySectionAttendanceID = F, MeetID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "DailySectionAttendance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DailySectionAttendance
	#'
	#' This function returns a dataframe or json object of a DailySectionAttendance
	#' @param DailySectionAttendanceID The ID of the DailySectionAttendance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DailySectionAttendance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DailySectionAttendance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DailySectionAttendance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of DailySectionAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDailySectionAttendance <- function(DailySectionAttendanceID, AttendancePeriodID = F, CalendarDayID = F, CreatedTime = F, MeetID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DailySectionAttendanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "DailySectionAttendance", objectId = DailySectionAttendanceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DailySectionAttendance
	#'
	#' This function deletes a DailySectionAttendance
	#' @param DailySectionAttendanceID The ID of the DailySectionAttendance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The DailySectionAttendanceID of the deleted DailySectionAttendance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDailySectionAttendance <- function(DailySectionAttendanceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "DailySectionAttendance", objectId = DailySectionAttendanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DailySectionAttendance
	#'
	#' This function creates a DailySectionAttendance
	#' @param fieldNames The field values to give the created DailySectionAttendance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created DailySectionAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDailySectionAttendance <- function(AttendancePeriodID = NULL, CalendarDayID = NULL, MeetID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "DailySectionAttendance", body = list(DataObject = body), searchFields = append("DailySectionAttendanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DailySectionAttendance
	#'
	#' This function modifies a DailySectionAttendance
	#' @param fieldNames The field values to give the modified DailySectionAttendance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified DailySectionAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDailySectionAttendance <- function(DailySectionAttendanceID, AttendancePeriodID = NULL, CalendarDayID = NULL, MeetID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "DailySectionAttendance", objectId = DailySectionAttendanceID, body = list(DataObject = body), searchFields = append("DailySectionAttendanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAttendanceTerms
	#'
	#' This function returns a dataframe or json object of StudentAttendanceTerms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendanceTerms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendanceTerms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendanceTerm') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of StudentAttendanceTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendanceTerms <- function(searchConditionsList = NULL, AttendanceTermCode = F, AttendanceTermID = F, EndDate = F, EntityID = F, IsDefault = F, SchoolYearID = F, StartDate = F, StudentID = F, TotalDaysAbsent = F, TotalDaysExcused = F, TotalDaysOther = F, TotalDaysPossible = F, TotalDaysPresent = F, TotalDaysUnexcused = F, TotalTardyCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendanceTerm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendanceTerm
	#'
	#' This function returns a dataframe or json object of a StudentAttendanceTerm
	#' @param StudentAttendanceTermID The ID of the StudentAttendanceTerm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendanceTerm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendanceTerm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendanceTerm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendanceTerm <- function(StudentAttendanceTermID, AttendanceTermCode = F, AttendanceTermID = F, EndDate = F, EntityID = F, IsDefault = F, SchoolYearID = F, StartDate = F, StudentID = F, TotalDaysAbsent = F, TotalDaysExcused = F, TotalDaysOther = F, TotalDaysPossible = F, TotalDaysPresent = F, TotalDaysUnexcused = F, TotalTardyCount = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendanceTermID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendanceTerm", objectId = StudentAttendanceTermID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendanceTerm
	#'
	#' This function deletes a StudentAttendanceTerm
	#' @param StudentAttendanceTermID The ID of the StudentAttendanceTerm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendanceTermID of the deleted StudentAttendanceTerm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendanceTerm <- function(StudentAttendanceTermID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendanceTerm", objectId = StudentAttendanceTermID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAffectedCalendarDayRecords
	#'
	#' This function returns a dataframe or json object of TempAffectedCalendarDayRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAffectedCalendarDayRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAffectedCalendarDayRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAffectedCalendarDayRecord') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempAffectedCalendarDayRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAffectedCalendarDayRecords <- function(searchConditionsList = NULL, Action = F, AffectedPrimaryKey = F, Calendar = F, CalendarID = F, Comment = F, CountAs = F, CreatedTime = F, Date = F, DayOfTheWeek = F, EdFiCalendarEventDescriptorINID = F, Entity = F, FailureReason = F, ModifiedTime = F, NewBellSchedule = F, NewCountAs = F, NewDayRotation = F, NewDayRotationID = F, NewFundingPeriod = F, NewFundingPeriodID = F, NewInstructionalMinutesOverride = F, NewOperationalMinutesOverride = F, NewStateCalendarWaiverEventTypeCodeTX = F, NewStateCalendarWaiverEventTypeCodeTXID = F, NewStateSchoolDayEventCodeTX = F, NewStateSchoolDayEventCodeTXID = F, NewUseInstructionalMinutesOverride = F, NewUseOperationalMinutesOverride = F, NewWaiverMinutes = F, OldBellSchedule = F, OldDayRotation = F, OldDayRotationID = F, OldFundingPeriod = F, OldFundingPeriodID = F, OldInstructionalMinutesOverride = F, OldOperationalMinutesOverride = F, OldStateCalendarWaiverEventTypeCodeTX = F, OldStateCalendarWaiverEventTypeCodeTXID = F, OldStateSchoolDayEventCodeTX = F, OldStateSchoolDayEventCodeTXID = F, OldUseInstructionalMinutesOverride = F, OldUseOperationalMinutesOverride = F, OldWaiverMinutes = F, ShowCommentOnCalendar = F, TempAffectedCalendarDayRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempAffectedCalendarDayRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAffectedCalendarDayRecord
	#'
	#' This function returns a dataframe or json object of a TempAffectedCalendarDayRecord
	#' @param TempAffectedCalendarDayRecordID The ID of the TempAffectedCalendarDayRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAffectedCalendarDayRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAffectedCalendarDayRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAffectedCalendarDayRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempAffectedCalendarDayRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAffectedCalendarDayRecord <- function(TempAffectedCalendarDayRecordID, Action = F, AffectedPrimaryKey = F, Calendar = F, CalendarID = F, Comment = F, CountAs = F, CreatedTime = F, Date = F, DayOfTheWeek = F, EdFiCalendarEventDescriptorINID = F, Entity = F, FailureReason = F, ModifiedTime = F, NewBellSchedule = F, NewCountAs = F, NewDayRotation = F, NewDayRotationID = F, NewFundingPeriod = F, NewFundingPeriodID = F, NewInstructionalMinutesOverride = F, NewOperationalMinutesOverride = F, NewStateCalendarWaiverEventTypeCodeTX = F, NewStateCalendarWaiverEventTypeCodeTXID = F, NewStateSchoolDayEventCodeTX = F, NewStateSchoolDayEventCodeTXID = F, NewUseInstructionalMinutesOverride = F, NewUseOperationalMinutesOverride = F, NewWaiverMinutes = F, OldBellSchedule = F, OldDayRotation = F, OldDayRotationID = F, OldFundingPeriod = F, OldFundingPeriodID = F, OldInstructionalMinutesOverride = F, OldOperationalMinutesOverride = F, OldStateCalendarWaiverEventTypeCodeTX = F, OldStateCalendarWaiverEventTypeCodeTXID = F, OldStateSchoolDayEventCodeTX = F, OldStateSchoolDayEventCodeTXID = F, OldUseInstructionalMinutesOverride = F, OldUseOperationalMinutesOverride = F, OldWaiverMinutes = F, ShowCommentOnCalendar = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAffectedCalendarDayRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempAffectedCalendarDayRecord", objectId = TempAffectedCalendarDayRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAffectedCalendarDayRecord
	#'
	#' This function deletes a TempAffectedCalendarDayRecord
	#' @param TempAffectedCalendarDayRecordID The ID of the TempAffectedCalendarDayRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempAffectedCalendarDayRecordID of the deleted TempAffectedCalendarDayRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAffectedCalendarDayRecord <- function(TempAffectedCalendarDayRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempAffectedCalendarDayRecord", objectId = TempAffectedCalendarDayRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAffectedCalendarDayRecord
	#'
	#' This function creates a TempAffectedCalendarDayRecord
	#' @param fieldNames The field values to give the created TempAffectedCalendarDayRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempAffectedCalendarDayRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAffectedCalendarDayRecord <- function(Action = NULL, AffectedPrimaryKey = NULL, Calendar = NULL, CalendarID = NULL, Comment = NULL, CountAs = NULL, Date = NULL, DayOfTheWeek = NULL, EdFiCalendarEventDescriptorINID = NULL, Entity = NULL, FailureReason = NULL, NewBellSchedule = NULL, NewCountAs = NULL, NewDayRotation = NULL, NewDayRotationID = NULL, NewFundingPeriod = NULL, NewFundingPeriodID = NULL, NewInstructionalMinutesOverride = NULL, NewOperationalMinutesOverride = NULL, NewStateCalendarWaiverEventTypeCodeTX = NULL, NewStateCalendarWaiverEventTypeCodeTXID = NULL, NewStateSchoolDayEventCodeTX = NULL, NewStateSchoolDayEventCodeTXID = NULL, NewUseInstructionalMinutesOverride = NULL, NewUseOperationalMinutesOverride = NULL, NewWaiverMinutes = NULL, OldBellSchedule = NULL, OldDayRotation = NULL, OldDayRotationID = NULL, OldFundingPeriod = NULL, OldFundingPeriodID = NULL, OldInstructionalMinutesOverride = NULL, OldOperationalMinutesOverride = NULL, OldStateCalendarWaiverEventTypeCodeTX = NULL, OldStateCalendarWaiverEventTypeCodeTXID = NULL, OldStateSchoolDayEventCodeTX = NULL, OldStateSchoolDayEventCodeTXID = NULL, OldUseInstructionalMinutesOverride = NULL, OldUseOperationalMinutesOverride = NULL, OldWaiverMinutes = NULL, ShowCommentOnCalendar = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempAffectedCalendarDayRecord", body = list(DataObject = body), searchFields = append("TempAffectedCalendarDayRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAffectedCalendarDayRecord
	#'
	#' This function modifies a TempAffectedCalendarDayRecord
	#' @param fieldNames The field values to give the modified TempAffectedCalendarDayRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempAffectedCalendarDayRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAffectedCalendarDayRecord <- function(TempAffectedCalendarDayRecordID, Action = NULL, AffectedPrimaryKey = NULL, Calendar = NULL, CalendarID = NULL, Comment = NULL, CountAs = NULL, Date = NULL, DayOfTheWeek = NULL, EdFiCalendarEventDescriptorINID = NULL, Entity = NULL, FailureReason = NULL, NewBellSchedule = NULL, NewCountAs = NULL, NewDayRotation = NULL, NewDayRotationID = NULL, NewFundingPeriod = NULL, NewFundingPeriodID = NULL, NewInstructionalMinutesOverride = NULL, NewOperationalMinutesOverride = NULL, NewStateCalendarWaiverEventTypeCodeTX = NULL, NewStateCalendarWaiverEventTypeCodeTXID = NULL, NewStateSchoolDayEventCodeTX = NULL, NewStateSchoolDayEventCodeTXID = NULL, NewUseInstructionalMinutesOverride = NULL, NewUseOperationalMinutesOverride = NULL, NewWaiverMinutes = NULL, OldBellSchedule = NULL, OldDayRotation = NULL, OldDayRotationID = NULL, OldFundingPeriod = NULL, OldFundingPeriodID = NULL, OldInstructionalMinutesOverride = NULL, OldOperationalMinutesOverride = NULL, OldStateCalendarWaiverEventTypeCodeTX = NULL, OldStateCalendarWaiverEventTypeCodeTXID = NULL, OldStateSchoolDayEventCodeTX = NULL, OldStateSchoolDayEventCodeTXID = NULL, OldUseInstructionalMinutesOverride = NULL, OldUseOperationalMinutesOverride = NULL, OldWaiverMinutes = NULL, ShowCommentOnCalendar = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempAffectedCalendarDayRecord", objectId = TempAffectedCalendarDayRecordID, body = list(DataObject = body), searchFields = append("TempAffectedCalendarDayRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendanceConfigEntityGroupYears
	#'
	#' This function returns a dataframe or json object of AttendanceConfigEntityGroupYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceConfigEntityGroupYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceConfigEntityGroupYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceConfigEntityGroupYear') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of AttendanceConfigEntityGroupYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceConfigEntityGroupYears <- function(searchConditionsList = NULL, AllowTeachersToModifyPreviousAttendance = F, AttendanceReasonIDTardyDefault = F, AttendanceTypeIDTardyDefault = F, AttendanceTypeIDTeacherDefault = F, ConfigEntityGroupYearID = F, ConfigEntityGroupYearIDClonedFrom = F, CreatedTime = F, DisplayStudentCountOnTiles = F, EnableInOutTime = F, EntityGroupKey = F, EntityID = F, HasAttendanceTypeTeacherDefault = F, IncludeGradeLevelOnLetter = F, IncludeParentNameAndOrPhoneNumberOnLetter = F, IncludeSchoolOrCampusOnLetter = F, IncludeSignatureLineForOfficeOnLetter = F, IncludeSignatureLineForParentOnLetter = F, IncludeSignatureLineForStudentOnLetter = F, IncludeStudentNameAndOrNumberOnLetter = F, IncludeTardyCountOnLetter = F, ModifiedTime = F, MultiPeriodClassCountMethod = F, NumberOfDaysToAllowTeachersToModifyPreviousAttendance = F, PresentBackgroundColor = F, PrintAttendanceLetterForWindowedEnvelope = F, RestrictTeacherAttendanceUpdates = F, SchoolYearID = F, SpecialClassCountsLabel = F, TardyDefaultComment = F, TardyKioskTardySlipTitle = F, TeacherEntryCutoffDuration = F, TeacherEntryCutOffNumberOfMinutesAfter = F, TeacherEntryCutOffTime = F, TeacherEntryCutoffWindowEndTime = F, TeacherEntryCutoffWindowStartTime = F, TeacherEntrySpecificCutOffTime = F, UseInOutTimeForCalculations = F, UseMarkAllStudentsPresentOnTile = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UseSpecialClassCounts = F, UseTardyCalculator = F, UseTardyKiosk = F, UseTeacherPerfectAttendanceConfirmation = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "ConfigEntityGroupYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceConfigEntityGroupYear
	#'
	#' This function returns a dataframe or json object of an AttendanceConfigEntityGroupYear
	#' @param AttendanceConfigEntityGroupYearID The ID of the AttendanceConfigEntityGroupYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceConfigEntityGroupYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceConfigEntityGroupYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceConfigEntityGroupYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceConfigEntityGroupYear <- function(AttendanceConfigEntityGroupYearID, AllowTeachersToModifyPreviousAttendance = F, AttendanceReasonIDTardyDefault = F, AttendanceTypeIDTardyDefault = F, AttendanceTypeIDTeacherDefault = F, ConfigEntityGroupYearID = F, ConfigEntityGroupYearIDClonedFrom = F, CreatedTime = F, DisplayStudentCountOnTiles = F, EnableInOutTime = F, EntityGroupKey = F, EntityID = F, HasAttendanceTypeTeacherDefault = F, IncludeGradeLevelOnLetter = F, IncludeParentNameAndOrPhoneNumberOnLetter = F, IncludeSchoolOrCampusOnLetter = F, IncludeSignatureLineForOfficeOnLetter = F, IncludeSignatureLineForParentOnLetter = F, IncludeSignatureLineForStudentOnLetter = F, IncludeStudentNameAndOrNumberOnLetter = F, IncludeTardyCountOnLetter = F, ModifiedTime = F, MultiPeriodClassCountMethod = F, NumberOfDaysToAllowTeachersToModifyPreviousAttendance = F, PresentBackgroundColor = F, PrintAttendanceLetterForWindowedEnvelope = F, RestrictTeacherAttendanceUpdates = F, SchoolYearID = F, SpecialClassCountsLabel = F, TardyDefaultComment = F, TardyKioskTardySlipTitle = F, TeacherEntryCutoffDuration = F, TeacherEntryCutOffNumberOfMinutesAfter = F, TeacherEntryCutOffTime = F, TeacherEntryCutoffWindowEndTime = F, TeacherEntryCutoffWindowStartTime = F, TeacherEntrySpecificCutOffTime = F, UseInOutTimeForCalculations = F, UseMarkAllStudentsPresentOnTile = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UseSpecialClassCounts = F, UseTardyCalculator = F, UseTardyKiosk = F, UseTeacherPerfectAttendanceConfirmation = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceConfigEntityGroupYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "ConfigEntityGroupYear", objectId = AttendanceConfigEntityGroupYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceConfigEntityGroupYear
	#'
	#' This function deletes an AttendanceConfigEntityGroupYear
	#' @param AttendanceConfigEntityGroupYearID The ID of the AttendanceConfigEntityGroupYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceConfigEntityGroupYearID of the deleted AttendanceConfigEntityGroupYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceConfigEntityGroupYear <- function(AttendanceConfigEntityGroupYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "ConfigEntityGroupYear", objectId = AttendanceConfigEntityGroupYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceConfigEntityGroupYear
	#'
	#' This function creates an AttendanceConfigEntityGroupYear
	#' @param fieldNames The field values to give the created AttendanceConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceConfigEntityGroupYear <- function(AllowTeachersToModifyPreviousAttendance = NULL, AttendanceReasonIDTardyDefault = NULL, AttendanceTypeIDTardyDefault = NULL, AttendanceTypeIDTeacherDefault = NULL, ConfigEntityGroupYearIDClonedFrom = NULL, DisplayStudentCountOnTiles = NULL, EnableInOutTime = NULL, EntityGroupKey = NULL, EntityID = NULL, IncludeGradeLevelOnLetter = NULL, IncludeParentNameAndOrPhoneNumberOnLetter = NULL, IncludeSchoolOrCampusOnLetter = NULL, IncludeSignatureLineForOfficeOnLetter = NULL, IncludeSignatureLineForParentOnLetter = NULL, IncludeSignatureLineForStudentOnLetter = NULL, IncludeStudentNameAndOrNumberOnLetter = NULL, IncludeTardyCountOnLetter = NULL, MultiPeriodClassCountMethod = NULL, NumberOfDaysToAllowTeachersToModifyPreviousAttendance = NULL, PresentBackgroundColor = NULL, PrintAttendanceLetterForWindowedEnvelope = NULL, RestrictTeacherAttendanceUpdates = NULL, SchoolYearID = NULL, SpecialClassCountsLabel = NULL, TardyDefaultComment = NULL, TardyKioskTardySlipTitle = NULL, TeacherEntryCutoffDuration = NULL, TeacherEntryCutOffNumberOfMinutesAfter = NULL, TeacherEntryCutOffTime = NULL, TeacherEntryCutoffWindowEndTime = NULL, TeacherEntryCutoffWindowStartTime = NULL, TeacherEntrySpecificCutOffTime = NULL, UseInOutTimeForCalculations = NULL, UseMarkAllStudentsPresentOnTile = NULL, UseSpecialClassCounts = NULL, UseTardyCalculator = NULL, UseTardyKiosk = NULL, UseTeacherPerfectAttendanceConfirmation = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "ConfigEntityGroupYear", body = list(DataObject = body), searchFields = append("ConfigEntityGroupYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceConfigEntityGroupYear
	#'
	#' This function modifies an AttendanceConfigEntityGroupYear
	#' @param fieldNames The field values to give the modified AttendanceConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceConfigEntityGroupYear <- function(ConfigEntityGroupYearID, AllowTeachersToModifyPreviousAttendance = NULL, AttendanceReasonIDTardyDefault = NULL, AttendanceTypeIDTardyDefault = NULL, AttendanceTypeIDTeacherDefault = NULL, ConfigEntityGroupYearIDClonedFrom = NULL, DisplayStudentCountOnTiles = NULL, EnableInOutTime = NULL, EntityGroupKey = NULL, EntityID = NULL, IncludeGradeLevelOnLetter = NULL, IncludeParentNameAndOrPhoneNumberOnLetter = NULL, IncludeSchoolOrCampusOnLetter = NULL, IncludeSignatureLineForOfficeOnLetter = NULL, IncludeSignatureLineForParentOnLetter = NULL, IncludeSignatureLineForStudentOnLetter = NULL, IncludeStudentNameAndOrNumberOnLetter = NULL, IncludeTardyCountOnLetter = NULL, MultiPeriodClassCountMethod = NULL, NumberOfDaysToAllowTeachersToModifyPreviousAttendance = NULL, PresentBackgroundColor = NULL, PrintAttendanceLetterForWindowedEnvelope = NULL, RestrictTeacherAttendanceUpdates = NULL, SchoolYearID = NULL, SpecialClassCountsLabel = NULL, TardyDefaultComment = NULL, TardyKioskTardySlipTitle = NULL, TeacherEntryCutoffDuration = NULL, TeacherEntryCutOffNumberOfMinutesAfter = NULL, TeacherEntryCutOffTime = NULL, TeacherEntryCutoffWindowEndTime = NULL, TeacherEntryCutoffWindowStartTime = NULL, TeacherEntrySpecificCutOffTime = NULL, UseInOutTimeForCalculations = NULL, UseMarkAllStudentsPresentOnTile = NULL, UseSpecialClassCounts = NULL, UseTardyCalculator = NULL, UseTardyKiosk = NULL, UseTeacherPerfectAttendanceConfirmation = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "ConfigEntityGroupYear", objectId = ConfigEntityGroupYearID, body = list(DataObject = body), searchFields = append("ConfigEntityGroupYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendancePeriods
	#'
	#' This function returns a dataframe or json object of AttendancePeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendancePeriods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendancePeriods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendancePeriod') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of AttendancePeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendancePeriods <- function(searchConditionsList = NULL, AttendancePeriodID = F, AttendancePeriodIDClonedFrom = F, AttendancePeriodIDClonedTo = F, Code = F, CreatedTime = F, DisplayOrder = F, DynamicRelationshipID = F, EntityGroupKey = F, EntityID = F, ModifiedTime = F, SchoolYearID = F, UseForSchoolTrakPositiveAttendance = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UseTeacherEntryCutOffTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendancePeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendancePeriod
	#'
	#' This function returns a dataframe or json object of an AttendancePeriod
	#' @param AttendancePeriodID The ID of the AttendancePeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendancePeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendancePeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendancePeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendancePeriod <- function(AttendancePeriodID, AttendancePeriodIDClonedFrom = F, AttendancePeriodIDClonedTo = F, Code = F, CreatedTime = F, DisplayOrder = F, DynamicRelationshipID = F, EntityGroupKey = F, EntityID = F, ModifiedTime = F, SchoolYearID = F, UseForSchoolTrakPositiveAttendance = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UseTeacherEntryCutOffTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendancePeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendancePeriod", objectId = AttendancePeriodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendancePeriod
	#'
	#' This function deletes an AttendancePeriod
	#' @param AttendancePeriodID The ID of the AttendancePeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendancePeriodID of the deleted AttendancePeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendancePeriod <- function(AttendancePeriodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendancePeriod", objectId = AttendancePeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendancePeriod
	#'
	#' This function creates an AttendancePeriod
	#' @param fieldNames The field values to give the created AttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendancePeriod <- function(AttendancePeriodIDClonedFrom = NULL, Code = NULL, DisplayOrder = NULL, EntityGroupKey = NULL, EntityID = NULL, SchoolYearID = NULL, UseForSchoolTrakPositiveAttendance = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "AttendancePeriod", body = list(DataObject = body), searchFields = append("AttendancePeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendancePeriod
	#'
	#' This function modifies an AttendancePeriod
	#' @param fieldNames The field values to give the modified AttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendancePeriod <- function(AttendancePeriodID, AttendancePeriodIDClonedFrom = NULL, Code = NULL, DisplayOrder = NULL, EntityGroupKey = NULL, EntityID = NULL, SchoolYearID = NULL, UseForSchoolTrakPositiveAttendance = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "AttendancePeriod", objectId = AttendancePeriodID, body = list(DataObject = body), searchFields = append("AttendancePeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendanceTypes
	#'
	#' This function returns a dataframe or json object of AttendanceTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceType') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of AttendanceTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceTypes <- function(searchConditionsList = NULL, AttendanceTypeID = F, AttendanceTypeIDClonedFrom = F, AttendanceTypeMNID = F, Category = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EdFiAttendanceEventCategoryDescriptorID = F, EntityGroupKey = F, EntityID = F, IncludeInClassCounts = F, IncludeInSpecialClassCounts = F, IncludeInTotals = F, IsTruant = F, ModifiedTime = F, SchoolYearID = F, ShowOnGradesheetAssignments = F, TeacherEntryID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendanceType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceType
	#'
	#' This function returns a dataframe or json object of an AttendanceType
	#' @param AttendanceTypeID The ID of the AttendanceType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceType <- function(AttendanceTypeID, AttendanceTypeIDClonedFrom = F, AttendanceTypeMNID = F, Category = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EdFiAttendanceEventCategoryDescriptorID = F, EntityGroupKey = F, EntityID = F, IncludeInClassCounts = F, IncludeInSpecialClassCounts = F, IncludeInTotals = F, IsTruant = F, ModifiedTime = F, SchoolYearID = F, ShowOnGradesheetAssignments = F, TeacherEntryID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendanceType", objectId = AttendanceTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceType
	#'
	#' This function deletes an AttendanceType
	#' @param AttendanceTypeID The ID of the AttendanceType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceTypeID of the deleted AttendanceType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceType <- function(AttendanceTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendanceType", objectId = AttendanceTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceType
	#'
	#' This function creates an AttendanceType
	#' @param fieldNames The field values to give the created AttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceType <- function(AttendanceTypeIDClonedFrom = NULL, Category = NULL, Code = NULL, Description = NULL, EdFiAttendanceEventCategoryDescriptorID = NULL, EntityGroupKey = NULL, EntityID = NULL, IncludeInClassCounts = NULL, IncludeInSpecialClassCounts = NULL, IncludeInTotals = NULL, IsTruant = NULL, SchoolYearID = NULL, ShowOnGradesheetAssignments = NULL, TeacherEntryID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "AttendanceType", body = list(DataObject = body), searchFields = append("AttendanceTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceType
	#'
	#' This function modifies an AttendanceType
	#' @param fieldNames The field values to give the modified AttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceType <- function(AttendanceTypeID, AttendanceTypeIDClonedFrom = NULL, Category = NULL, Code = NULL, Description = NULL, EdFiAttendanceEventCategoryDescriptorID = NULL, EntityGroupKey = NULL, EntityID = NULL, IncludeInClassCounts = NULL, IncludeInSpecialClassCounts = NULL, IncludeInTotals = NULL, IsTruant = NULL, SchoolYearID = NULL, ShowOnGradesheetAssignments = NULL, TeacherEntryID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "AttendanceType", objectId = AttendanceTypeID, body = list(DataObject = body), searchFields = append("AttendanceTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendanceReasons
	#'
	#' This function returns a dataframe or json object of AttendanceReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceReasons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceReasons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceReason') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of AttendanceReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceReasons <- function(searchConditionsList = NULL, AttendanceReasonID = F, AttendanceReasonIDClonedFrom = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EntityGroupKey = F, EntityID = F, ModifiedTime = F, SchoolYearID = F, TeacherEntryID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendanceReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceReason
	#'
	#' This function returns a dataframe or json object of an AttendanceReason
	#' @param AttendanceReasonID The ID of the AttendanceReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceReason <- function(AttendanceReasonID, AttendanceReasonIDClonedFrom = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EntityGroupKey = F, EntityID = F, ModifiedTime = F, SchoolYearID = F, TeacherEntryID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendanceReason", objectId = AttendanceReasonID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceReason
	#'
	#' This function deletes an AttendanceReason
	#' @param AttendanceReasonID The ID of the AttendanceReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceReasonID of the deleted AttendanceReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceReason <- function(AttendanceReasonID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendanceReason", objectId = AttendanceReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceReason
	#'
	#' This function creates an AttendanceReason
	#' @param fieldNames The field values to give the created AttendanceReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceReason <- function(AttendanceReasonIDClonedFrom = NULL, Code = NULL, Description = NULL, EntityGroupKey = NULL, EntityID = NULL, SchoolYearID = NULL, TeacherEntryID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "AttendanceReason", body = list(DataObject = body), searchFields = append("AttendanceReasonID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceReason
	#'
	#' This function modifies an AttendanceReason
	#' @param fieldNames The field values to give the modified AttendanceReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceReason <- function(AttendanceReasonID, AttendanceReasonIDClonedFrom = NULL, Code = NULL, Description = NULL, EntityGroupKey = NULL, EntityID = NULL, SchoolYearID = NULL, TeacherEntryID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "AttendanceReason", objectId = AttendanceReasonID, body = list(DataObject = body), searchFields = append("AttendanceReasonID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendanceTerms
	#'
	#' This function returns a dataframe or json object of AttendanceTerms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceTerms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceTerms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceTerm') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of AttendanceTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceTerms <- function(searchConditionsList = NULL, AttendanceTermID = F, AttendanceTermIDClonedFrom = F, CalendarID = F, Code = F, CreatedTime = F, DaysInTerm = F, EndDate = F, ModifiedTime = F, StartDate = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendanceTerm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceTerm
	#'
	#' This function returns a dataframe or json object of an AttendanceTerm
	#' @param AttendanceTermID The ID of the AttendanceTerm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceTerm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceTerm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceTerm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceTerm <- function(AttendanceTermID, AttendanceTermIDClonedFrom = F, CalendarID = F, Code = F, CreatedTime = F, DaysInTerm = F, EndDate = F, ModifiedTime = F, StartDate = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceTermID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendanceTerm", objectId = AttendanceTermID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceTerm
	#'
	#' This function deletes an AttendanceTerm
	#' @param AttendanceTermID The ID of the AttendanceTerm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceTermID of the deleted AttendanceTerm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceTerm <- function(AttendanceTermID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendanceTerm", objectId = AttendanceTermID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceTerm
	#'
	#' This function creates an AttendanceTerm
	#' @param fieldNames The field values to give the created AttendanceTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceTerm <- function(AttendanceTermIDClonedFrom = NULL, CalendarID = NULL, Code = NULL, EndDate = NULL, StartDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "AttendanceTerm", body = list(DataObject = body), searchFields = append("AttendanceTermID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceTerm
	#'
	#' This function modifies an AttendanceTerm
	#' @param fieldNames The field values to give the modified AttendanceTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceTerm <- function(AttendanceTermID, AttendanceTermIDClonedFrom = NULL, CalendarID = NULL, Code = NULL, EndDate = NULL, StartDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "AttendanceTerm", objectId = AttendanceTermID, body = list(DataObject = body), searchFields = append("AttendanceTermID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalendarDaySchedulingPeriodTimesOverrides
	#'
	#' This function returns a dataframe or json object of CalendarDaySchedulingPeriodTimesOverrides
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDaySchedulingPeriodTimesOverrides. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDaySchedulingPeriodTimesOverrides.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDaySchedulingPeriodTimesOverride') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of CalendarDaySchedulingPeriodTimesOverrides
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarDaySchedulingPeriodTimesOverrides <- function(searchConditionsList = NULL, CalendarDayID = F, CalendarDaySchedulingPeriodTimesOverrideID = F, CreatedTime = F, DurationInMinutes = F, EndTime = F, ModifiedTime = F, SchedulingPeriodID = F, StartTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarDaySchedulingPeriodTimesOverride", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarDaySchedulingPeriodTimesOverride
	#'
	#' This function returns a dataframe or json object of a CalendarDaySchedulingPeriodTimesOverride
	#' @param CalendarDaySchedulingPeriodTimesOverrideID The ID of the CalendarDaySchedulingPeriodTimesOverride to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDaySchedulingPeriodTimesOverride. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDaySchedulingPeriodTimesOverride.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDaySchedulingPeriodTimesOverride') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CalendarDaySchedulingPeriodTimesOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarDaySchedulingPeriodTimesOverride <- function(CalendarDaySchedulingPeriodTimesOverrideID, CalendarDayID = F, CreatedTime = F, DurationInMinutes = F, EndTime = F, ModifiedTime = F, SchedulingPeriodID = F, StartTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarDaySchedulingPeriodTimesOverrideID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarDaySchedulingPeriodTimesOverride", objectId = CalendarDaySchedulingPeriodTimesOverrideID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarDaySchedulingPeriodTimesOverride
	#'
	#' This function deletes a CalendarDaySchedulingPeriodTimesOverride
	#' @param CalendarDaySchedulingPeriodTimesOverrideID The ID of the CalendarDaySchedulingPeriodTimesOverride to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarDaySchedulingPeriodTimesOverrideID of the deleted CalendarDaySchedulingPeriodTimesOverride.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarDaySchedulingPeriodTimesOverride <- function(CalendarDaySchedulingPeriodTimesOverrideID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarDaySchedulingPeriodTimesOverride", objectId = CalendarDaySchedulingPeriodTimesOverrideID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarDaySchedulingPeriodTimesOverride
	#'
	#' This function creates a CalendarDaySchedulingPeriodTimesOverride
	#' @param fieldNames The field values to give the created CalendarDaySchedulingPeriodTimesOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CalendarDaySchedulingPeriodTimesOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarDaySchedulingPeriodTimesOverride <- function(CalendarDayID = NULL, EndTime = NULL, SchedulingPeriodID = NULL, StartTime = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "CalendarDaySchedulingPeriodTimesOverride", body = list(DataObject = body), searchFields = append("CalendarDaySchedulingPeriodTimesOverrideID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarDaySchedulingPeriodTimesOverride
	#'
	#' This function modifies a CalendarDaySchedulingPeriodTimesOverride
	#' @param fieldNames The field values to give the modified CalendarDaySchedulingPeriodTimesOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CalendarDaySchedulingPeriodTimesOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarDaySchedulingPeriodTimesOverride <- function(CalendarDaySchedulingPeriodTimesOverrideID, CalendarDayID = NULL, EndTime = NULL, SchedulingPeriodID = NULL, StartTime = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarDaySchedulingPeriodTimesOverride", objectId = CalendarDaySchedulingPeriodTimesOverrideID, body = list(DataObject = body), searchFields = append("CalendarDaySchedulingPeriodTimesOverrideID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalendarDayDisplayPeriodOverrides
	#'
	#' This function returns a dataframe or json object of CalendarDayDisplayPeriodOverrides
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDayDisplayPeriodOverrides. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDayDisplayPeriodOverrides.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDayDisplayPeriodOverride') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of CalendarDayDisplayPeriodOverrides
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarDayDisplayPeriodOverrides <- function(searchConditionsList = NULL, CalendarDayDisplayPeriodOverrideID = F, CalendarDayID = F, CreatedTime = F, DisplayPeriodID = F, LengthMinutes = F, ModifiedTime = F, RemovePeriod = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarDayDisplayPeriodOverride", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarDayDisplayPeriodOverride
	#'
	#' This function returns a dataframe or json object of a CalendarDayDisplayPeriodOverride
	#' @param CalendarDayDisplayPeriodOverrideID The ID of the CalendarDayDisplayPeriodOverride to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDayDisplayPeriodOverride. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDayDisplayPeriodOverride.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDayDisplayPeriodOverride') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CalendarDayDisplayPeriodOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarDayDisplayPeriodOverride <- function(CalendarDayDisplayPeriodOverrideID, CalendarDayID = F, CreatedTime = F, DisplayPeriodID = F, LengthMinutes = F, ModifiedTime = F, RemovePeriod = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarDayDisplayPeriodOverrideID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarDayDisplayPeriodOverride", objectId = CalendarDayDisplayPeriodOverrideID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarDayDisplayPeriodOverride
	#'
	#' This function deletes a CalendarDayDisplayPeriodOverride
	#' @param CalendarDayDisplayPeriodOverrideID The ID of the CalendarDayDisplayPeriodOverride to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarDayDisplayPeriodOverrideID of the deleted CalendarDayDisplayPeriodOverride.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarDayDisplayPeriodOverride <- function(CalendarDayDisplayPeriodOverrideID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarDayDisplayPeriodOverride", objectId = CalendarDayDisplayPeriodOverrideID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarDayDisplayPeriodOverride
	#'
	#' This function creates a CalendarDayDisplayPeriodOverride
	#' @param fieldNames The field values to give the created CalendarDayDisplayPeriodOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CalendarDayDisplayPeriodOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarDayDisplayPeriodOverride <- function(CalendarDayID = NULL, DisplayPeriodID = NULL, LengthMinutes = NULL, RemovePeriod = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "CalendarDayDisplayPeriodOverride", body = list(DataObject = body), searchFields = append("CalendarDayDisplayPeriodOverrideID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarDayDisplayPeriodOverride
	#'
	#' This function modifies a CalendarDayDisplayPeriodOverride
	#' @param fieldNames The field values to give the modified CalendarDayDisplayPeriodOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CalendarDayDisplayPeriodOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarDayDisplayPeriodOverride <- function(CalendarDayDisplayPeriodOverrideID, CalendarDayID = NULL, DisplayPeriodID = NULL, LengthMinutes = NULL, RemovePeriod = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarDayDisplayPeriodOverride", objectId = CalendarDayDisplayPeriodOverrideID, body = list(DataObject = body), searchFields = append("CalendarDayDisplayPeriodOverrideID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalendarDisplayPeriods
	#'
	#' This function returns a dataframe or json object of CalendarDisplayPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDisplayPeriods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDisplayPeriods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDisplayPeriod') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of CalendarDisplayPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarDisplayPeriods <- function(searchConditionsList = NULL, CalendarDisplayPeriodID = F, CalendarDisplayPeriodIDClonedFrom = F, CalendarDisplayPeriodIDClonedTo = F, CalendarID = F, CreatedTime = F, DisplayPeriodID = F, IncludeInClassCounts = F, IncludeInTotalCounts = F, ModifiedTime = F, TakeAttendance = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarDisplayPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarDisplayPeriod
	#'
	#' This function returns a dataframe or json object of a CalendarDisplayPeriod
	#' @param CalendarDisplayPeriodID The ID of the CalendarDisplayPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDisplayPeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDisplayPeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDisplayPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CalendarDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarDisplayPeriod <- function(CalendarDisplayPeriodID, CalendarDisplayPeriodIDClonedFrom = F, CalendarDisplayPeriodIDClonedTo = F, CalendarID = F, CreatedTime = F, DisplayPeriodID = F, IncludeInClassCounts = F, IncludeInTotalCounts = F, ModifiedTime = F, TakeAttendance = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarDisplayPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarDisplayPeriod", objectId = CalendarDisplayPeriodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarDisplayPeriod
	#'
	#' This function deletes a CalendarDisplayPeriod
	#' @param CalendarDisplayPeriodID The ID of the CalendarDisplayPeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarDisplayPeriodID of the deleted CalendarDisplayPeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarDisplayPeriod <- function(CalendarDisplayPeriodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarDisplayPeriod", objectId = CalendarDisplayPeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarDisplayPeriod
	#'
	#' This function creates a CalendarDisplayPeriod
	#' @param fieldNames The field values to give the created CalendarDisplayPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CalendarDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarDisplayPeriod <- function(CalendarDisplayPeriodIDClonedFrom = NULL, CalendarID = NULL, DisplayPeriodID = NULL, IncludeInClassCounts = NULL, IncludeInTotalCounts = NULL, TakeAttendance = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "CalendarDisplayPeriod", body = list(DataObject = body), searchFields = append("CalendarDisplayPeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarDisplayPeriod
	#'
	#' This function modifies a CalendarDisplayPeriod
	#' @param fieldNames The field values to give the modified CalendarDisplayPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CalendarDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarDisplayPeriod <- function(CalendarDisplayPeriodID, CalendarDisplayPeriodIDClonedFrom = NULL, CalendarID = NULL, DisplayPeriodID = NULL, IncludeInClassCounts = NULL, IncludeInTotalCounts = NULL, TakeAttendance = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarDisplayPeriod", objectId = CalendarDisplayPeriodID, body = list(DataObject = body), searchFields = append("CalendarDisplayPeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalendarDays
	#'
	#' This function returns a dataframe or json object of CalendarDays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDay') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of CalendarDays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarDays <- function(searchConditionsList = NULL, AttendanceTerm = F, BellScheduleGroupSummary = F, BellScheduleID = F, CalendarDayID = F, CalendarID = F, Comment = F, CountAs = F, CreatedTime = F, Date = F, DateWithDayOfWeekAbbreviated = F, DayOfTheWeek = F, DayOfTheWeekNumber = F, DayRotationID = F, DoNotSendAttendanceToEdFi = F, DynamicRelationshipID = F, FoodServicePurchaseExists = F, ModifiedTime = F, NumberOfCalendarDayEvents = F, ShowCommentOnCalendar = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarDay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarDay
	#'
	#' This function returns a dataframe or json object of a CalendarDay
	#' @param CalendarDayID The ID of the CalendarDay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarDay <- function(CalendarDayID, AttendanceTerm = F, BellScheduleGroupSummary = F, BellScheduleID = F, CalendarID = F, Comment = F, CountAs = F, CreatedTime = F, Date = F, DateWithDayOfWeekAbbreviated = F, DayOfTheWeek = F, DayOfTheWeekNumber = F, DayRotationID = F, DoNotSendAttendanceToEdFi = F, DynamicRelationshipID = F, FoodServicePurchaseExists = F, ModifiedTime = F, NumberOfCalendarDayEvents = F, ShowCommentOnCalendar = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarDayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarDay", objectId = CalendarDayID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarDay
	#'
	#' This function deletes a CalendarDay
	#' @param CalendarDayID The ID of the CalendarDay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarDayID of the deleted CalendarDay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarDay <- function(CalendarDayID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarDay", objectId = CalendarDayID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarDay
	#'
	#' This function creates a CalendarDay
	#' @param fieldNames The field values to give the created CalendarDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarDay <- function(BellScheduleID = NULL, CalendarID = NULL, Comment = NULL, CountAs = NULL, Date = NULL, DayRotationID = NULL, DoNotSendAttendanceToEdFi = NULL, ShowCommentOnCalendar = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "CalendarDay", body = list(DataObject = body), searchFields = append("CalendarDayID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarDay
	#'
	#' This function modifies a CalendarDay
	#' @param fieldNames The field values to give the modified CalendarDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarDay <- function(CalendarDayID, BellScheduleID = NULL, CalendarID = NULL, Comment = NULL, CountAs = NULL, Date = NULL, DayRotationID = NULL, DoNotSendAttendanceToEdFi = NULL, ShowCommentOnCalendar = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarDay", objectId = CalendarDayID, body = list(DataObject = body), searchFields = append("CalendarDayID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DayRotationPatterns
	#'
	#' This function returns a dataframe or json object of DayRotationPatterns
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DayRotationPatterns. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DayRotationPatterns.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DayRotationPattern') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of DayRotationPatterns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDayRotationPatterns <- function(searchConditionsList = NULL, CreatedTime = F, DayNumber = F, DayRotationID = F, DayRotationPatternID = F, DayRotationPatternIDClonedFrom = F, EntityGroupKey = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "DayRotationPattern", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DayRotationPattern
	#'
	#' This function returns a dataframe or json object of a DayRotationPattern
	#' @param DayRotationPatternID The ID of the DayRotationPattern to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DayRotationPattern. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DayRotationPattern.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DayRotationPattern') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of DayRotationPattern
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDayRotationPattern <- function(DayRotationPatternID, CreatedTime = F, DayNumber = F, DayRotationID = F, DayRotationPatternIDClonedFrom = F, EntityGroupKey = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DayRotationPatternID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "DayRotationPattern", objectId = DayRotationPatternID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DayRotationPattern
	#'
	#' This function deletes a DayRotationPattern
	#' @param DayRotationPatternID The ID of the DayRotationPattern to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The DayRotationPatternID of the deleted DayRotationPattern.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDayRotationPattern <- function(DayRotationPatternID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "DayRotationPattern", objectId = DayRotationPatternID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DayRotationPattern
	#'
	#' This function creates a DayRotationPattern
	#' @param fieldNames The field values to give the created DayRotationPattern. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created DayRotationPattern
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDayRotationPattern <- function(DayNumber = NULL, DayRotationID = NULL, DayRotationPatternIDClonedFrom = NULL, EntityGroupKey = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "DayRotationPattern", body = list(DataObject = body), searchFields = append("DayRotationPatternID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DayRotationPattern
	#'
	#' This function modifies a DayRotationPattern
	#' @param fieldNames The field values to give the modified DayRotationPattern. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified DayRotationPattern
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDayRotationPattern <- function(DayRotationPatternID, DayNumber = NULL, DayRotationID = NULL, DayRotationPatternIDClonedFrom = NULL, EntityGroupKey = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "DayRotationPattern", objectId = DayRotationPatternID, body = list(DataObject = body), searchFields = append("DayRotationPatternID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAttendances
	#'
	#' This function returns a dataframe or json object of StudentAttendances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendance') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of StudentAttendances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendances <- function(searchConditionsList = NULL, CalendarDayID = F, Comment = F, CommentsExistForStudentAttendance = F, CreatedTime = F, DaysAbsent = F, DaysExcused = F, DaysOther = F, DaysUnexcused = F, EntityID = F, HideRecordMA = F, IsGuardianNotified = F, ModifiedTime = F, SchoolYearID = F, StudentAttendanceID = F, StudentID = F, TardyCount = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendance
	#'
	#' This function returns a dataframe or json object of a StudentAttendance
	#' @param StudentAttendanceID The ID of the StudentAttendance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendance <- function(StudentAttendanceID, CalendarDayID = F, Comment = F, CommentsExistForStudentAttendance = F, CreatedTime = F, DaysAbsent = F, DaysExcused = F, DaysOther = F, DaysUnexcused = F, EntityID = F, HideRecordMA = F, IsGuardianNotified = F, ModifiedTime = F, SchoolYearID = F, StudentID = F, TardyCount = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendance", objectId = StudentAttendanceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendance
	#'
	#' This function deletes a StudentAttendance
	#' @param StudentAttendanceID The ID of the StudentAttendance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendanceID of the deleted StudentAttendance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendance <- function(StudentAttendanceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendance", objectId = StudentAttendanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentAttendance
	#'
	#' This function creates a StudentAttendance
	#' @param fieldNames The field values to give the created StudentAttendance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentAttendance <- function(CalendarDayID = NULL, Comment = NULL, DaysAbsent = NULL, DaysExcused = NULL, DaysOther = NULL, DaysUnexcused = NULL, EntityID = NULL, HideRecordMA = NULL, IsGuardianNotified = NULL, SchoolYearID = NULL, StudentID = NULL, TardyCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "StudentAttendance", body = list(DataObject = body), searchFields = append("StudentAttendanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentAttendance
	#'
	#' This function modifies a StudentAttendance
	#' @param fieldNames The field values to give the modified StudentAttendance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentAttendance <- function(StudentAttendanceID, CalendarDayID = NULL, Comment = NULL, DaysAbsent = NULL, DaysExcused = NULL, DaysOther = NULL, DaysUnexcused = NULL, EntityID = NULL, HideRecordMA = NULL, IsGuardianNotified = NULL, SchoolYearID = NULL, StudentID = NULL, TardyCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "StudentAttendance", objectId = StudentAttendanceID, body = list(DataObject = body), searchFields = append("StudentAttendanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAttendancePeriods
	#'
	#' This function returns a dataframe or json object of StudentAttendancePeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendancePeriods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendancePeriods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendancePeriod') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of StudentAttendancePeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendancePeriods <- function(searchConditionsList = NULL, AttendancePeriodID = F, AttendanceReasonID = F, AttendanceTypeID = F, AttendanceTypeWithReason = F, Comment = F, CreatedTime = F, CrossWalkedAttendanceTypeWithReason = F, EntityIDAttendancePeriod = F, EntityIDCourse = F, IncidentOffenseNameActionDetailID = F, ModifiedTime = F, StudentAttendanceID = F, StudentAttendancePeriodID = F, StudentSectionID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ViewingFromAttendanceEntity = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendancePeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendancePeriod
	#'
	#' This function returns a dataframe or json object of a StudentAttendancePeriod
	#' @param StudentAttendancePeriodID The ID of the StudentAttendancePeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendancePeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendancePeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendancePeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendancePeriod <- function(StudentAttendancePeriodID, AttendancePeriodID = F, AttendanceReasonID = F, AttendanceTypeID = F, AttendanceTypeWithReason = F, Comment = F, CreatedTime = F, CrossWalkedAttendanceTypeWithReason = F, EntityIDAttendancePeriod = F, EntityIDCourse = F, IncidentOffenseNameActionDetailID = F, ModifiedTime = F, StudentAttendanceID = F, StudentSectionID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ViewingFromAttendanceEntity = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendancePeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendancePeriod", objectId = StudentAttendancePeriodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendancePeriod
	#'
	#' This function deletes a StudentAttendancePeriod
	#' @param StudentAttendancePeriodID The ID of the StudentAttendancePeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendancePeriodID of the deleted StudentAttendancePeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendancePeriod <- function(StudentAttendancePeriodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendancePeriod", objectId = StudentAttendancePeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentAttendancePeriod
	#'
	#' This function creates a StudentAttendancePeriod
	#' @param fieldNames The field values to give the created StudentAttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentAttendancePeriod <- function(AttendancePeriodID = NULL, AttendanceReasonID = NULL, AttendanceTypeID = NULL, Comment = NULL, EntityIDAttendancePeriod = NULL, EntityIDCourse = NULL, IncidentOffenseNameActionDetailID = NULL, StudentAttendanceID = NULL, StudentSectionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "StudentAttendancePeriod", body = list(DataObject = body), searchFields = append("StudentAttendancePeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentAttendancePeriod
	#'
	#' This function modifies a StudentAttendancePeriod
	#' @param fieldNames The field values to give the modified StudentAttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentAttendancePeriod <- function(StudentAttendancePeriodID, AttendancePeriodID = NULL, AttendanceReasonID = NULL, AttendanceTypeID = NULL, Comment = NULL, EntityIDAttendancePeriod = NULL, EntityIDCourse = NULL, IncidentOffenseNameActionDetailID = NULL, StudentAttendanceID = NULL, StudentSectionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "StudentAttendancePeriod", objectId = StudentAttendancePeriodID, body = list(DataObject = body), searchFields = append("StudentAttendancePeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TeacherEntries
	#'
	#' This function returns a dataframe or json object of TeacherEntries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TeacherEntries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TeacherEntries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TeacherEntry') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TeacherEntries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherEntries <- function(searchConditionsList = NULL, BackgroundColor = F, CreatedTime = F, DisplayOrder = F, EntityGroupKey = F, EntityID = F, Label = F, ModifiedTime = F, SchoolYearID = F, TeacherEntryID = F, TeacherEntryIDClonedFrom = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TeacherEntry", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TeacherEntry
	#'
	#' This function returns a dataframe or json object of a TeacherEntry
	#' @param TeacherEntryID The ID of the TeacherEntry to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TeacherEntry. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TeacherEntry.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TeacherEntry') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TeacherEntry
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherEntry <- function(TeacherEntryID, BackgroundColor = F, CreatedTime = F, DisplayOrder = F, EntityGroupKey = F, EntityID = F, Label = F, ModifiedTime = F, SchoolYearID = F, TeacherEntryIDClonedFrom = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherEntryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TeacherEntry", objectId = TeacherEntryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TeacherEntry
	#'
	#' This function deletes a TeacherEntry
	#' @param TeacherEntryID The ID of the TeacherEntry to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TeacherEntryID of the deleted TeacherEntry.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTeacherEntry <- function(TeacherEntryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TeacherEntry", objectId = TeacherEntryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TeacherEntry
	#'
	#' This function creates a TeacherEntry
	#' @param fieldNames The field values to give the created TeacherEntry. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TeacherEntry
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTeacherEntry <- function(BackgroundColor = NULL, DisplayOrder = NULL, EntityGroupKey = NULL, EntityID = NULL, Label = NULL, SchoolYearID = NULL, TeacherEntryIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TeacherEntry", body = list(DataObject = body), searchFields = append("TeacherEntryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TeacherEntry
	#'
	#' This function modifies a TeacherEntry
	#' @param fieldNames The field values to give the modified TeacherEntry. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TeacherEntry
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTeacherEntry <- function(TeacherEntryID, BackgroundColor = NULL, DisplayOrder = NULL, EntityGroupKey = NULL, EntityID = NULL, Label = NULL, SchoolYearID = NULL, TeacherEntryIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TeacherEntry", objectId = TeacherEntryID, body = list(DataObject = body), searchFields = append("TeacherEntryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BellSchedulingPeriods
	#'
	#' This function returns a dataframe or json object of BellSchedulingPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BellSchedulingPeriods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BellSchedulingPeriods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BellSchedulingPeriod') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of BellSchedulingPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBellSchedulingPeriods <- function(searchConditionsList = NULL, BellScheduleID = F, BellSchedulingPeriodID = F, BellSchedulingPeriodIDClonedFrom = F, CreatedTime = F, EndTime = F, EndTimeWithOverride = F, LengthInMinutes = F, ModifiedTime = F, SchedulingPeriodID = F, StartTime = F, StartTimeWithOverride = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "BellSchedulingPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BellSchedulingPeriod
	#'
	#' This function returns a dataframe or json object of a BellSchedulingPeriod
	#' @param BellSchedulingPeriodID The ID of the BellSchedulingPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BellSchedulingPeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BellSchedulingPeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BellSchedulingPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of BellSchedulingPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBellSchedulingPeriod <- function(BellSchedulingPeriodID, BellScheduleID = F, BellSchedulingPeriodIDClonedFrom = F, CreatedTime = F, EndTime = F, EndTimeWithOverride = F, LengthInMinutes = F, ModifiedTime = F, SchedulingPeriodID = F, StartTime = F, StartTimeWithOverride = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BellSchedulingPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "BellSchedulingPeriod", objectId = BellSchedulingPeriodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BellSchedulingPeriod
	#'
	#' This function deletes a BellSchedulingPeriod
	#' @param BellSchedulingPeriodID The ID of the BellSchedulingPeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The BellSchedulingPeriodID of the deleted BellSchedulingPeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBellSchedulingPeriod <- function(BellSchedulingPeriodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "BellSchedulingPeriod", objectId = BellSchedulingPeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BellSchedulingPeriod
	#'
	#' This function creates a BellSchedulingPeriod
	#' @param fieldNames The field values to give the created BellSchedulingPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created BellSchedulingPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBellSchedulingPeriod <- function(BellScheduleID = NULL, BellSchedulingPeriodIDClonedFrom = NULL, EndTime = NULL, SchedulingPeriodID = NULL, StartTime = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "BellSchedulingPeriod", body = list(DataObject = body), searchFields = append("BellSchedulingPeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BellSchedulingPeriod
	#'
	#' This function modifies a BellSchedulingPeriod
	#' @param fieldNames The field values to give the modified BellSchedulingPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified BellSchedulingPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBellSchedulingPeriod <- function(BellSchedulingPeriodID, BellScheduleID = NULL, BellSchedulingPeriodIDClonedFrom = NULL, EndTime = NULL, SchedulingPeriodID = NULL, StartTime = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "BellSchedulingPeriod", objectId = BellSchedulingPeriodID, body = list(DataObject = body), searchFields = append("BellSchedulingPeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BellSchedules
	#'
	#' This function returns a dataframe or json object of BellSchedules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BellSchedules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BellSchedules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BellSchedule') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of BellSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBellSchedules <- function(searchConditionsList = NULL, BellScheduleID = F, BellScheduleIDClonedFrom = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EntityID = F, IsDefault = F, ModifiedTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "BellSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BellSchedule
	#'
	#' This function returns a dataframe or json object of a BellSchedule
	#' @param BellScheduleID The ID of the BellSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BellSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BellSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BellSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of BellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBellSchedule <- function(BellScheduleID, BellScheduleIDClonedFrom = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EntityID = F, IsDefault = F, ModifiedTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BellScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "BellSchedule", objectId = BellScheduleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BellSchedule
	#'
	#' This function deletes a BellSchedule
	#' @param BellScheduleID The ID of the BellSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The BellScheduleID of the deleted BellSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBellSchedule <- function(BellScheduleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "BellSchedule", objectId = BellScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BellSchedule
	#'
	#' This function creates a BellSchedule
	#' @param fieldNames The field values to give the created BellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created BellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBellSchedule <- function(BellScheduleIDClonedFrom = NULL, Code = NULL, Description = NULL, EntityID = NULL, IsDefault = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "BellSchedule", body = list(DataObject = body), searchFields = append("BellScheduleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BellSchedule
	#'
	#' This function modifies a BellSchedule
	#' @param fieldNames The field values to give the modified BellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified BellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBellSchedule <- function(BellScheduleID, BellScheduleIDClonedFrom = NULL, Code = NULL, Description = NULL, EntityID = NULL, IsDefault = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "BellSchedule", objectId = BellScheduleID, body = list(DataObject = body), searchFields = append("BellScheduleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCalendarAttendanceTerms
	#'
	#' This function returns a dataframe or json object of TempCalendarAttendanceTerms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarAttendanceTerms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarAttendanceTerms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarAttendanceTerm') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempCalendarAttendanceTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalendarAttendanceTerms <- function(searchConditionsList = NULL, AttendanceTermID = F, CalendarID = F, Code = F, CodeDescription = F, CreatedTime = F, EndDate = F, ModifiedTime = F, OriginalEndDate = F, OriginalStartDate = F, StartDate = F, TableType = F, TableTypeString = F, TempCalendarAttendanceTermID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCalendarAttendanceTerm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCalendarAttendanceTerm
	#'
	#' This function returns a dataframe or json object of a TempCalendarAttendanceTerm
	#' @param TempCalendarAttendanceTermID The ID of the TempCalendarAttendanceTerm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarAttendanceTerm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarAttendanceTerm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarAttendanceTerm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCalendarAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalendarAttendanceTerm <- function(TempCalendarAttendanceTermID, AttendanceTermID = F, CalendarID = F, Code = F, CodeDescription = F, CreatedTime = F, EndDate = F, ModifiedTime = F, OriginalEndDate = F, OriginalStartDate = F, StartDate = F, TableType = F, TableTypeString = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalendarAttendanceTermID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCalendarAttendanceTerm", objectId = TempCalendarAttendanceTermID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCalendarAttendanceTerm
	#'
	#' This function deletes a TempCalendarAttendanceTerm
	#' @param TempCalendarAttendanceTermID The ID of the TempCalendarAttendanceTerm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCalendarAttendanceTermID of the deleted TempCalendarAttendanceTerm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCalendarAttendanceTerm <- function(TempCalendarAttendanceTermID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCalendarAttendanceTerm", objectId = TempCalendarAttendanceTermID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCalendarAttendanceTerm
	#'
	#' This function creates a TempCalendarAttendanceTerm
	#' @param fieldNames The field values to give the created TempCalendarAttendanceTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCalendarAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCalendarAttendanceTerm <- function(AttendanceTermID = NULL, CalendarID = NULL, Code = NULL, CodeDescription = NULL, EndDate = NULL, OriginalEndDate = NULL, OriginalStartDate = NULL, StartDate = NULL, TableType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempCalendarAttendanceTerm", body = list(DataObject = body), searchFields = append("TempCalendarAttendanceTermID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCalendarAttendanceTerm
	#'
	#' This function modifies a TempCalendarAttendanceTerm
	#' @param fieldNames The field values to give the modified TempCalendarAttendanceTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCalendarAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCalendarAttendanceTerm <- function(TempCalendarAttendanceTermID, AttendanceTermID = NULL, CalendarID = NULL, Code = NULL, CodeDescription = NULL, EndDate = NULL, OriginalEndDate = NULL, OriginalStartDate = NULL, StartDate = NULL, TableType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempCalendarAttendanceTerm", objectId = TempCalendarAttendanceTermID, body = list(DataObject = body), searchFields = append("TempCalendarAttendanceTermID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCalendarDayCalendarEventRecords
	#'
	#' This function returns a dataframe or json object of TempCalendarDayCalendarEventRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarDayCalendarEventRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarDayCalendarEventRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarDayCalendarEventRecord') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempCalendarDayCalendarEventRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalendarDayCalendarEventRecords <- function(searchConditionsList = NULL, Calendar = F, CalendarDayID = F, CalendarEvent = F, CalendarEventID = F, CreatedTime = F, Date = F, FailureReason = F, ModifiedTime = F, TempCalendarDayCalendarEventRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCalendarDayCalendarEventRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCalendarDayCalendarEventRecord
	#'
	#' This function returns a dataframe or json object of a TempCalendarDayCalendarEventRecord
	#' @param TempCalendarDayCalendarEventRecordID The ID of the TempCalendarDayCalendarEventRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarDayCalendarEventRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarDayCalendarEventRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarDayCalendarEventRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCalendarDayCalendarEventRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalendarDayCalendarEventRecord <- function(TempCalendarDayCalendarEventRecordID, Calendar = F, CalendarDayID = F, CalendarEvent = F, CalendarEventID = F, CreatedTime = F, Date = F, FailureReason = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalendarDayCalendarEventRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCalendarDayCalendarEventRecord", objectId = TempCalendarDayCalendarEventRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCalendarDayCalendarEventRecord
	#'
	#' This function deletes a TempCalendarDayCalendarEventRecord
	#' @param TempCalendarDayCalendarEventRecordID The ID of the TempCalendarDayCalendarEventRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCalendarDayCalendarEventRecordID of the deleted TempCalendarDayCalendarEventRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCalendarDayCalendarEventRecord <- function(TempCalendarDayCalendarEventRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCalendarDayCalendarEventRecord", objectId = TempCalendarDayCalendarEventRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCalendarDayCalendarEventRecord
	#'
	#' This function creates a TempCalendarDayCalendarEventRecord
	#' @param fieldNames The field values to give the created TempCalendarDayCalendarEventRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCalendarDayCalendarEventRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCalendarDayCalendarEventRecord <- function(Calendar = NULL, CalendarDayID = NULL, CalendarEvent = NULL, CalendarEventID = NULL, Date = NULL, FailureReason = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempCalendarDayCalendarEventRecord", body = list(DataObject = body), searchFields = append("TempCalendarDayCalendarEventRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCalendarDayCalendarEventRecord
	#'
	#' This function modifies a TempCalendarDayCalendarEventRecord
	#' @param fieldNames The field values to give the modified TempCalendarDayCalendarEventRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCalendarDayCalendarEventRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCalendarDayCalendarEventRecord <- function(TempCalendarDayCalendarEventRecordID, Calendar = NULL, CalendarDayID = NULL, CalendarEvent = NULL, CalendarEventID = NULL, Date = NULL, FailureReason = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempCalendarDayCalendarEventRecord", objectId = TempCalendarDayCalendarEventRecordID, body = list(DataObject = body), searchFields = append("TempCalendarDayCalendarEventRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentThresholdPeriods
	#'
	#' This function returns a dataframe or json object of StudentThresholdPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentThresholdPeriods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentThresholdPeriods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentThresholdPeriod') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of StudentThresholdPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentThresholdPeriods <- function(searchConditionsList = NULL, AttendancePeriodID = F, AttendanceTypeID = F, CalendarDayID = F, CountsTowardsThreshold = F, CreatedTime = F, Date = F, ModifiedTime = F, SectionID = F, StudentAttendancePeriodID = F, StudentDisciplineThresholdAttendanceReportRunHistoryID = F, StudentSectionID = F, StudentThresholdPeriodID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentThresholdPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentThresholdPeriod
	#'
	#' This function returns a dataframe or json object of a StudentThresholdPeriod
	#' @param StudentThresholdPeriodID The ID of the StudentThresholdPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentThresholdPeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentThresholdPeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentThresholdPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentThresholdPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentThresholdPeriod <- function(StudentThresholdPeriodID, AttendancePeriodID = F, AttendanceTypeID = F, CalendarDayID = F, CountsTowardsThreshold = F, CreatedTime = F, Date = F, ModifiedTime = F, SectionID = F, StudentAttendancePeriodID = F, StudentDisciplineThresholdAttendanceReportRunHistoryID = F, StudentSectionID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentThresholdPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentThresholdPeriod", objectId = StudentThresholdPeriodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentThresholdPeriod
	#'
	#' This function deletes a StudentThresholdPeriod
	#' @param StudentThresholdPeriodID The ID of the StudentThresholdPeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentThresholdPeriodID of the deleted StudentThresholdPeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentThresholdPeriod <- function(StudentThresholdPeriodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentThresholdPeriod", objectId = StudentThresholdPeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentThresholdPeriod
	#'
	#' This function creates a StudentThresholdPeriod
	#' @param fieldNames The field values to give the created StudentThresholdPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentThresholdPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentThresholdPeriod <- function(AttendancePeriodID = NULL, AttendanceTypeID = NULL, CalendarDayID = NULL, CountsTowardsThreshold = NULL, Date = NULL, SectionID = NULL, StudentAttendancePeriodID = NULL, StudentDisciplineThresholdAttendanceReportRunHistoryID = NULL, StudentSectionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "StudentThresholdPeriod", body = list(DataObject = body), searchFields = append("StudentThresholdPeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentThresholdPeriod
	#'
	#' This function modifies a StudentThresholdPeriod
	#' @param fieldNames The field values to give the modified StudentThresholdPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentThresholdPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentThresholdPeriod <- function(StudentThresholdPeriodID, AttendancePeriodID = NULL, AttendanceTypeID = NULL, CalendarDayID = NULL, CountsTowardsThreshold = NULL, Date = NULL, SectionID = NULL, StudentAttendancePeriodID = NULL, StudentDisciplineThresholdAttendanceReportRunHistoryID = NULL, StudentSectionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "StudentThresholdPeriod", objectId = StudentThresholdPeriodID, body = list(DataObject = body), searchFields = append("StudentThresholdPeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentDisciplineThresholdAttendanceReportRunHistories
	#'
	#' This function returns a dataframe or json object of StudentDisciplineThresholdAttendanceReportRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentDisciplineThresholdAttendanceReportRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentDisciplineThresholdAttendanceReportRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentDisciplineThresholdAttendanceReportRunHistory') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of StudentDisciplineThresholdAttendanceReportRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentDisciplineThresholdAttendanceReportRunHistories <- function(searchConditionsList = NULL, AttachmentDisplayName = F, AttachmentID = F, AttendanceReportRunHistoryID = F, Body = F, BodyForReport = F, CreatedTime = F, DisciplineThresholdID = F, Footer = F, FooterForReport = F, Header = F, HeaderForReport = F, IsActive = F, ModifiedTime = F, StudentDisciplineThresholdAttendanceReportRunHistoryID = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentDisciplineThresholdAttendanceReportRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentDisciplineThresholdAttendanceReportRunHistory
	#'
	#' This function returns a dataframe or json object of a StudentDisciplineThresholdAttendanceReportRunHistory
	#' @param StudentDisciplineThresholdAttendanceReportRunHistoryID The ID of the StudentDisciplineThresholdAttendanceReportRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentDisciplineThresholdAttendanceReportRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentDisciplineThresholdAttendanceReportRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentDisciplineThresholdAttendanceReportRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentDisciplineThresholdAttendanceReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentDisciplineThresholdAttendanceReportRunHistory <- function(StudentDisciplineThresholdAttendanceReportRunHistoryID, AttachmentDisplayName = F, AttachmentID = F, AttendanceReportRunHistoryID = F, Body = F, BodyForReport = F, CreatedTime = F, DisciplineThresholdID = F, Footer = F, FooterForReport = F, Header = F, HeaderForReport = F, IsActive = F, ModifiedTime = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentDisciplineThresholdAttendanceReportRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentDisciplineThresholdAttendanceReportRunHistory", objectId = StudentDisciplineThresholdAttendanceReportRunHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentDisciplineThresholdAttendanceReportRunHistory
	#'
	#' This function deletes a StudentDisciplineThresholdAttendanceReportRunHistory
	#' @param StudentDisciplineThresholdAttendanceReportRunHistoryID The ID of the StudentDisciplineThresholdAttendanceReportRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentDisciplineThresholdAttendanceReportRunHistoryID of the deleted StudentDisciplineThresholdAttendanceReportRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentDisciplineThresholdAttendanceReportRunHistory <- function(StudentDisciplineThresholdAttendanceReportRunHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentDisciplineThresholdAttendanceReportRunHistory", objectId = StudentDisciplineThresholdAttendanceReportRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentDisciplineThresholdAttendanceReportRunHistory
	#'
	#' This function creates a StudentDisciplineThresholdAttendanceReportRunHistory
	#' @param fieldNames The field values to give the created StudentDisciplineThresholdAttendanceReportRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentDisciplineThresholdAttendanceReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentDisciplineThresholdAttendanceReportRunHistory <- function(AttachmentID = NULL, AttendanceReportRunHistoryID = NULL, Body = NULL, DisciplineThresholdID = NULL, Footer = NULL, Header = NULL, IsActive = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "StudentDisciplineThresholdAttendanceReportRunHistory", body = list(DataObject = body), searchFields = append("StudentDisciplineThresholdAttendanceReportRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentDisciplineThresholdAttendanceReportRunHistory
	#'
	#' This function modifies a StudentDisciplineThresholdAttendanceReportRunHistory
	#' @param fieldNames The field values to give the modified StudentDisciplineThresholdAttendanceReportRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentDisciplineThresholdAttendanceReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentDisciplineThresholdAttendanceReportRunHistory <- function(StudentDisciplineThresholdAttendanceReportRunHistoryID, AttachmentID = NULL, AttendanceReportRunHistoryID = NULL, Body = NULL, DisciplineThresholdID = NULL, Footer = NULL, Header = NULL, IsActive = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "StudentDisciplineThresholdAttendanceReportRunHistory", objectId = StudentDisciplineThresholdAttendanceReportRunHistoryID, body = list(DataObject = body), searchFields = append("StudentDisciplineThresholdAttendanceReportRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendanceReportRunHistoryThresholdResetRanges
	#'
	#' This function returns a dataframe or json object of AttendanceReportRunHistoryThresholdResetRanges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceReportRunHistoryThresholdResetRanges. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceReportRunHistoryThresholdResetRanges.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceReportRunHistoryThresholdResetRange') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of AttendanceReportRunHistoryThresholdResetRanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceReportRunHistoryThresholdResetRanges <- function(searchConditionsList = NULL, AttendanceReportRunHistoryID = F, AttendanceReportRunHistoryThresholdResetRangeID = F, CreatedTime = F, ModifiedTime = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendanceReportRunHistoryThresholdResetRange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceReportRunHistoryThresholdResetRange
	#'
	#' This function returns a dataframe or json object of an AttendanceReportRunHistoryThresholdResetRange
	#' @param AttendanceReportRunHistoryThresholdResetRangeID The ID of the AttendanceReportRunHistoryThresholdResetRange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceReportRunHistoryThresholdResetRange. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceReportRunHistoryThresholdResetRange.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceReportRunHistoryThresholdResetRange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceReportRunHistoryThresholdResetRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceReportRunHistoryThresholdResetRange <- function(AttendanceReportRunHistoryThresholdResetRangeID, AttendanceReportRunHistoryID = F, CreatedTime = F, ModifiedTime = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceReportRunHistoryThresholdResetRangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendanceReportRunHistoryThresholdResetRange", objectId = AttendanceReportRunHistoryThresholdResetRangeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceReportRunHistoryThresholdResetRange
	#'
	#' This function deletes an AttendanceReportRunHistoryThresholdResetRange
	#' @param AttendanceReportRunHistoryThresholdResetRangeID The ID of the AttendanceReportRunHistoryThresholdResetRange to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceReportRunHistoryThresholdResetRangeID of the deleted AttendanceReportRunHistoryThresholdResetRange.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceReportRunHistoryThresholdResetRange <- function(AttendanceReportRunHistoryThresholdResetRangeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendanceReportRunHistoryThresholdResetRange", objectId = AttendanceReportRunHistoryThresholdResetRangeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceReportRunHistoryThresholdResetRange
	#'
	#' This function creates an AttendanceReportRunHistoryThresholdResetRange
	#' @param fieldNames The field values to give the created AttendanceReportRunHistoryThresholdResetRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceReportRunHistoryThresholdResetRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceReportRunHistoryThresholdResetRange <- function(AttendanceReportRunHistoryID = NULL, ThresholdResetRangeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "AttendanceReportRunHistoryThresholdResetRange", body = list(DataObject = body), searchFields = append("AttendanceReportRunHistoryThresholdResetRangeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceReportRunHistoryThresholdResetRange
	#'
	#' This function modifies an AttendanceReportRunHistoryThresholdResetRange
	#' @param fieldNames The field values to give the modified AttendanceReportRunHistoryThresholdResetRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceReportRunHistoryThresholdResetRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceReportRunHistoryThresholdResetRange <- function(AttendanceReportRunHistoryThresholdResetRangeID, AttendanceReportRunHistoryID = NULL, ThresholdResetRangeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "AttendanceReportRunHistoryThresholdResetRange", objectId = AttendanceReportRunHistoryThresholdResetRangeID, body = list(DataObject = body), searchFields = append("AttendanceReportRunHistoryThresholdResetRangeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThresholdResetRangeAttendancePeriods
	#'
	#' This function returns a dataframe or json object of ThresholdResetRangeAttendancePeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdResetRangeAttendancePeriods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdResetRangeAttendancePeriods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdResetRangeAttendancePeriod') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of ThresholdResetRangeAttendancePeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThresholdResetRangeAttendancePeriods <- function(searchConditionsList = NULL, AttendancePeriodID = F, CreatedTime = F, ModifiedTime = F, ThresholdResetRangeAttendancePeriodID = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "ThresholdResetRangeAttendancePeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThresholdResetRangeAttendancePeriod
	#'
	#' This function returns a dataframe or json object of a ThresholdResetRangeAttendancePeriod
	#' @param ThresholdResetRangeAttendancePeriodID The ID of the ThresholdResetRangeAttendancePeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdResetRangeAttendancePeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdResetRangeAttendancePeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdResetRangeAttendancePeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of ThresholdResetRangeAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThresholdResetRangeAttendancePeriod <- function(ThresholdResetRangeAttendancePeriodID, AttendancePeriodID = F, CreatedTime = F, ModifiedTime = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThresholdResetRangeAttendancePeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendancePeriod", objectId = ThresholdResetRangeAttendancePeriodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThresholdResetRangeAttendancePeriod
	#'
	#' This function deletes a ThresholdResetRangeAttendancePeriod
	#' @param ThresholdResetRangeAttendancePeriodID The ID of the ThresholdResetRangeAttendancePeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The ThresholdResetRangeAttendancePeriodID of the deleted ThresholdResetRangeAttendancePeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThresholdResetRangeAttendancePeriod <- function(ThresholdResetRangeAttendancePeriodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendancePeriod", objectId = ThresholdResetRangeAttendancePeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThresholdResetRangeAttendancePeriod
	#'
	#' This function creates a ThresholdResetRangeAttendancePeriod
	#' @param fieldNames The field values to give the created ThresholdResetRangeAttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created ThresholdResetRangeAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThresholdResetRangeAttendancePeriod <- function(AttendancePeriodID = NULL, ThresholdResetRangeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendancePeriod", body = list(DataObject = body), searchFields = append("ThresholdResetRangeAttendancePeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThresholdResetRangeAttendancePeriod
	#'
	#' This function modifies a ThresholdResetRangeAttendancePeriod
	#' @param fieldNames The field values to give the modified ThresholdResetRangeAttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified ThresholdResetRangeAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThresholdResetRangeAttendancePeriod <- function(ThresholdResetRangeAttendancePeriodID, AttendancePeriodID = NULL, ThresholdResetRangeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendancePeriod", objectId = ThresholdResetRangeAttendancePeriodID, body = list(DataObject = body), searchFields = append("ThresholdResetRangeAttendancePeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentThresholdPeriodRecords
	#'
	#' This function returns a dataframe or json object of TempStudentThresholdPeriodRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentThresholdPeriodRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentThresholdPeriodRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentThresholdPeriodRecord') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempStudentThresholdPeriodRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentThresholdPeriodRecords <- function(searchConditionsList = NULL, AttendancePeriodID = F, AttendanceTypeID = F, CalendarDayID = F, CreatedTime = F, Date = F, ModifiedTime = F, SectionID = F, StudentAttendancePeriodID = F, StudentSectionID = F, TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID = F, TempStudentThresholdPeriodRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempStudentThresholdPeriodRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentThresholdPeriodRecord
	#'
	#' This function returns a dataframe or json object of a TempStudentThresholdPeriodRecord
	#' @param TempStudentThresholdPeriodRecordID The ID of the TempStudentThresholdPeriodRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentThresholdPeriodRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentThresholdPeriodRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentThresholdPeriodRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempStudentThresholdPeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentThresholdPeriodRecord <- function(TempStudentThresholdPeriodRecordID, AttendancePeriodID = F, AttendanceTypeID = F, CalendarDayID = F, CreatedTime = F, Date = F, ModifiedTime = F, SectionID = F, StudentAttendancePeriodID = F, StudentSectionID = F, TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentThresholdPeriodRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempStudentThresholdPeriodRecord", objectId = TempStudentThresholdPeriodRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentThresholdPeriodRecord
	#'
	#' This function deletes a TempStudentThresholdPeriodRecord
	#' @param TempStudentThresholdPeriodRecordID The ID of the TempStudentThresholdPeriodRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempStudentThresholdPeriodRecordID of the deleted TempStudentThresholdPeriodRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentThresholdPeriodRecord <- function(TempStudentThresholdPeriodRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempStudentThresholdPeriodRecord", objectId = TempStudentThresholdPeriodRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentThresholdPeriodRecord
	#'
	#' This function creates a TempStudentThresholdPeriodRecord
	#' @param fieldNames The field values to give the created TempStudentThresholdPeriodRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempStudentThresholdPeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentThresholdPeriodRecord <- function(AttendancePeriodID = NULL, AttendanceTypeID = NULL, CalendarDayID = NULL, Date = NULL, SectionID = NULL, StudentAttendancePeriodID = NULL, StudentSectionID = NULL, TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempStudentThresholdPeriodRecord", body = list(DataObject = body), searchFields = append("TempStudentThresholdPeriodRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentThresholdPeriodRecord
	#'
	#' This function modifies a TempStudentThresholdPeriodRecord
	#' @param fieldNames The field values to give the modified TempStudentThresholdPeriodRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempStudentThresholdPeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentThresholdPeriodRecord <- function(TempStudentThresholdPeriodRecordID, AttendancePeriodID = NULL, AttendanceTypeID = NULL, CalendarDayID = NULL, Date = NULL, SectionID = NULL, StudentAttendancePeriodID = NULL, StudentSectionID = NULL, TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempStudentThresholdPeriodRecord", objectId = TempStudentThresholdPeriodRecordID, body = list(DataObject = body), searchFields = append("TempStudentThresholdPeriodRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentDisciplineThresholdAttendanceReportRunHistoryRecords
	#'
	#' This function returns a dataframe or json object of TempStudentDisciplineThresholdAttendanceReportRunHistoryRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentDisciplineThresholdAttendanceReportRunHistoryRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentDisciplineThresholdAttendanceReportRunHistoryRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempStudentDisciplineThresholdAttendanceReportRunHistoryRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentDisciplineThresholdAttendanceReportRunHistoryRecords <- function(searchConditionsList = NULL, AttachmentDisplayName = F, CountType = F, CreatedTime = F, DateHigh = F, DateLow = F, DateType = F, DayCountType = F, DisciplineThresholdID = F, ModifiedTime = F, NumberOfDays = F, ResetRangeAttendanceTypes = F, StudentID = F, StudentName = F, TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID = F, ThresholdValue = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#'
	#' This function returns a dataframe or json object of a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' @param TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID The ID of the TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentDisciplineThresholdAttendanceReportRunHistoryRecord <- function(TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID, AttachmentDisplayName = F, CountType = F, CreatedTime = F, DateHigh = F, DateLow = F, DateType = F, DayCountType = F, DisciplineThresholdID = F, ModifiedTime = F, NumberOfDays = F, ResetRangeAttendanceTypes = F, StudentID = F, StudentName = F, ThresholdValue = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord", objectId = TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#'
	#' This function deletes a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' @param TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID The ID of the TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID of the deleted TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentDisciplineThresholdAttendanceReportRunHistoryRecord <- function(TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord", objectId = TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#'
	#' This function creates a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' @param fieldNames The field values to give the created TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentDisciplineThresholdAttendanceReportRunHistoryRecord <- function(AttachmentDisplayName = NULL, CountType = NULL, DateHigh = NULL, DateLow = NULL, DateType = NULL, DayCountType = NULL, DisciplineThresholdID = NULL, NumberOfDays = NULL, ResetRangeAttendanceTypes = NULL, StudentID = NULL, StudentName = NULL, ThresholdValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord", body = list(DataObject = body), searchFields = append("TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#'
	#' This function modifies a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' @param fieldNames The field values to give the modified TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentDisciplineThresholdAttendanceReportRunHistoryRecord <- function(TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID, AttachmentDisplayName = NULL, CountType = NULL, DateHigh = NULL, DateLow = NULL, DateType = NULL, DayCountType = NULL, DisciplineThresholdID = NULL, NumberOfDays = NULL, ResetRangeAttendanceTypes = NULL, StudentID = NULL, StudentName = NULL, ThresholdValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord", objectId = TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID, body = list(DataObject = body), searchFields = append("TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendanceReportRunHistories
	#'
	#' This function returns a dataframe or json object of AttendanceReportRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceReportRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceReportRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceReportRunHistory') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of AttendanceReportRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceReportRunHistories <- function(searchConditionsList = NULL, AttachmentDisplayName = F, AttendanceReportRunHistoryID = F, CachedEntity = F, CachedFiscalYear = F, CachedSchoolYear = F, CountType = F, CreatedTime = F, Date = F, EntityID = F, EntityIDList = F, FilterType = F, FiscalYearID = F, GracePeriod = F, IsActive = F, ModifiedTime = F, ParameterData = F, ParameterDescription = F, PostToFASA = F, PrintAttendanceLetterForWindowedEnvelope = F, ReportRunInfoID = F, RunDescription = F, SchoolYearID = F, SectionID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendanceReportRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceReportRunHistory
	#'
	#' This function returns a dataframe or json object of an AttendanceReportRunHistory
	#' @param AttendanceReportRunHistoryID The ID of the AttendanceReportRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceReportRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceReportRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceReportRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceReportRunHistory <- function(AttendanceReportRunHistoryID, AttachmentDisplayName = F, CachedEntity = F, CachedFiscalYear = F, CachedSchoolYear = F, CountType = F, CreatedTime = F, Date = F, EntityID = F, EntityIDList = F, FilterType = F, FiscalYearID = F, GracePeriod = F, IsActive = F, ModifiedTime = F, ParameterData = F, ParameterDescription = F, PostToFASA = F, PrintAttendanceLetterForWindowedEnvelope = F, ReportRunInfoID = F, RunDescription = F, SchoolYearID = F, SectionID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceReportRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendanceReportRunHistory", objectId = AttendanceReportRunHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceReportRunHistory
	#'
	#' This function deletes an AttendanceReportRunHistory
	#' @param AttendanceReportRunHistoryID The ID of the AttendanceReportRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceReportRunHistoryID of the deleted AttendanceReportRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceReportRunHistory <- function(AttendanceReportRunHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendanceReportRunHistory", objectId = AttendanceReportRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceReportRunHistory
	#'
	#' This function creates an AttendanceReportRunHistory
	#' @param fieldNames The field values to give the created AttendanceReportRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceReportRunHistory <- function(AttachmentDisplayName = NULL, Date = NULL, EntityID = NULL, FilterType = NULL, GracePeriod = NULL, IsActive = NULL, PostToFASA = NULL, ReportRunInfoID = NULL, RunDescription = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "AttendanceReportRunHistory", body = list(DataObject = body), searchFields = append("AttendanceReportRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceReportRunHistory
	#'
	#' This function modifies an AttendanceReportRunHistory
	#' @param fieldNames The field values to give the modified AttendanceReportRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceReportRunHistory <- function(AttendanceReportRunHistoryID, AttachmentDisplayName = NULL, Date = NULL, EntityID = NULL, FilterType = NULL, GracePeriod = NULL, IsActive = NULL, PostToFASA = NULL, ReportRunInfoID = NULL, RunDescription = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "AttendanceReportRunHistory", objectId = AttendanceReportRunHistoryID, body = list(DataObject = body), searchFields = append("AttendanceReportRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentInOutTimes
	#'
	#' This function returns a dataframe or json object of StudentInOutTimes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentInOutTimes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentInOutTimes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentInOutTime') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of StudentInOutTimes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentInOutTimes <- function(searchConditionsList = NULL, CreatedTime = F, MinutesPresent = F, ModifiedTime = F, PeriodTimes = F, StudentAttendanceID = F, StudentInOutTimeID = F, TimeIn = F, TimeOut = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentInOutTime", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentInOutTime
	#'
	#' This function returns a dataframe or json object of a StudentInOutTime
	#' @param StudentInOutTimeID The ID of the StudentInOutTime to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentInOutTime. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentInOutTime.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentInOutTime') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentInOutTime
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentInOutTime <- function(StudentInOutTimeID, CreatedTime = F, MinutesPresent = F, ModifiedTime = F, PeriodTimes = F, StudentAttendanceID = F, TimeIn = F, TimeOut = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentInOutTimeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentInOutTime", objectId = StudentInOutTimeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentInOutTime
	#'
	#' This function deletes a StudentInOutTime
	#' @param StudentInOutTimeID The ID of the StudentInOutTime to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentInOutTimeID of the deleted StudentInOutTime.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentInOutTime <- function(StudentInOutTimeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentInOutTime", objectId = StudentInOutTimeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentInOutTime
	#'
	#' This function creates a StudentInOutTime
	#' @param fieldNames The field values to give the created StudentInOutTime. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentInOutTime
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentInOutTime <- function(StudentAttendanceID = NULL, TimeIn = NULL, TimeOut = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "StudentInOutTime", body = list(DataObject = body), searchFields = append("StudentInOutTimeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentInOutTime
	#'
	#' This function modifies a StudentInOutTime
	#' @param fieldNames The field values to give the modified StudentInOutTime. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentInOutTime
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentInOutTime <- function(StudentInOutTimeID, StudentAttendanceID = NULL, TimeIn = NULL, TimeOut = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "StudentInOutTime", objectId = StudentInOutTimeID, body = list(DataObject = body), searchFields = append("StudentInOutTimeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAttendancePeriodGroups
	#'
	#' This function returns a dataframe or json object of StudentAttendancePeriodGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendancePeriodGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendancePeriodGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendancePeriodGroup') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of StudentAttendancePeriodGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendancePeriodGroups <- function(searchConditionsList = NULL, AttendancePeriodID = F, EntityID = F, SchoolYearID = F, StudentAttendanceID = F, StudentAttendancePeriodID = F, StudentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendancePeriodGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendancePeriodGroup
	#'
	#' This function returns a dataframe or json object of a StudentAttendancePeriodGroup
	#' @param StudentAttendancePeriodGroupID The ID of the StudentAttendancePeriodGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendancePeriodGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendancePeriodGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendancePeriodGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendancePeriodGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendancePeriodGroup <- function(StudentAttendancePeriodGroupID, AttendancePeriodID = F, EntityID = F, SchoolYearID = F, StudentAttendanceID = F, StudentAttendancePeriodID = F, StudentID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendancePeriodGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendancePeriodGroup", objectId = StudentAttendancePeriodGroupID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendancePeriodGroup
	#'
	#' This function deletes a StudentAttendancePeriodGroup
	#' @param StudentAttendancePeriodGroupID The ID of the StudentAttendancePeriodGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendancePeriodGroupID of the deleted StudentAttendancePeriodGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendancePeriodGroup <- function(StudentAttendancePeriodGroupID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendancePeriodGroup", objectId = StudentAttendancePeriodGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SeatingChartUsedLasts
	#'
	#' This function returns a dataframe or json object of SeatingChartUsedLasts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SeatingChartUsedLasts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SeatingChartUsedLasts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SeatingChartUsedLast') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of SeatingChartUsedLasts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSeatingChartUsedLasts <- function(searchConditionsList = NULL, CreatedTime = F, DisplayPeriodID = F, ModifiedTime = F, RoomID = F, SeatingChartID = F, SeatingChartUsedLastID = F, StaffID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "SeatingChartUsedLast", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SeatingChartUsedLast
	#'
	#' This function returns a dataframe or json object of a SeatingChartUsedLast
	#' @param SeatingChartUsedLastID The ID of the SeatingChartUsedLast to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SeatingChartUsedLast. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SeatingChartUsedLast.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SeatingChartUsedLast') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of SeatingChartUsedLast
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSeatingChartUsedLast <- function(SeatingChartUsedLastID, CreatedTime = F, DisplayPeriodID = F, ModifiedTime = F, RoomID = F, SeatingChartID = F, StaffID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SeatingChartUsedLastID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "SeatingChartUsedLast", objectId = SeatingChartUsedLastID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SeatingChartUsedLast
	#'
	#' This function deletes a SeatingChartUsedLast
	#' @param SeatingChartUsedLastID The ID of the SeatingChartUsedLast to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The SeatingChartUsedLastID of the deleted SeatingChartUsedLast.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSeatingChartUsedLast <- function(SeatingChartUsedLastID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "SeatingChartUsedLast", objectId = SeatingChartUsedLastID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SeatingChartUsedLast
	#'
	#' This function creates a SeatingChartUsedLast
	#' @param fieldNames The field values to give the created SeatingChartUsedLast. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created SeatingChartUsedLast
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSeatingChartUsedLast <- function(DisplayPeriodID = NULL, RoomID = NULL, SeatingChartID = NULL, StaffID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "SeatingChartUsedLast", body = list(DataObject = body), searchFields = append("SeatingChartUsedLastID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SeatingChartUsedLast
	#'
	#' This function modifies a SeatingChartUsedLast
	#' @param fieldNames The field values to give the modified SeatingChartUsedLast. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified SeatingChartUsedLast
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySeatingChartUsedLast <- function(SeatingChartUsedLastID, DisplayPeriodID = NULL, RoomID = NULL, SeatingChartID = NULL, StaffID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "SeatingChartUsedLast", objectId = SeatingChartUsedLastID, body = list(DataObject = body), searchFields = append("SeatingChartUsedLastID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCloneCalendarRecords
	#'
	#' This function returns a dataframe or json object of TempCloneCalendarRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCloneCalendarRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCloneCalendarRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCloneCalendarRecord') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempCloneCalendarRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCloneCalendarRecords <- function(searchConditionsList = NULL, AffectedPrimaryKey = F, AttendanceCalculationMethod = F, Code = F, CreatedTime = F, DefaultDayLengthMinutes = F, Description = F, EndDate = F, Entity = F, EntityID = F, HalfDayHighPeriodCount = F, IsDefault = F, ModifiedTime = F, SchoolYearID = F, StartDate = F, TempCloneCalendarRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZeroDayHighPeriodCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCloneCalendarRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCloneCalendarRecord
	#'
	#' This function returns a dataframe or json object of a TempCloneCalendarRecord
	#' @param TempCloneCalendarRecordID The ID of the TempCloneCalendarRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCloneCalendarRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCloneCalendarRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCloneCalendarRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCloneCalendarRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCloneCalendarRecord <- function(TempCloneCalendarRecordID, AffectedPrimaryKey = F, AttendanceCalculationMethod = F, Code = F, CreatedTime = F, DefaultDayLengthMinutes = F, Description = F, EndDate = F, Entity = F, EntityID = F, HalfDayHighPeriodCount = F, IsDefault = F, ModifiedTime = F, SchoolYearID = F, StartDate = F, UserIDCreatedBy = F, UserIDModifiedBy = F, ZeroDayHighPeriodCount = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCloneCalendarRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCloneCalendarRecord", objectId = TempCloneCalendarRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCloneCalendarRecord
	#'
	#' This function deletes a TempCloneCalendarRecord
	#' @param TempCloneCalendarRecordID The ID of the TempCloneCalendarRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCloneCalendarRecordID of the deleted TempCloneCalendarRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCloneCalendarRecord <- function(TempCloneCalendarRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCloneCalendarRecord", objectId = TempCloneCalendarRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCloneCalendarRecord
	#'
	#' This function creates a TempCloneCalendarRecord
	#' @param fieldNames The field values to give the created TempCloneCalendarRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCloneCalendarRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCloneCalendarRecord <- function(AffectedPrimaryKey = NULL, AttendanceCalculationMethod = NULL, Code = NULL, DefaultDayLengthMinutes = NULL, Description = NULL, EndDate = NULL, Entity = NULL, EntityID = NULL, HalfDayHighPeriodCount = NULL, IsDefault = NULL, SchoolYearID = NULL, StartDate = NULL, ZeroDayHighPeriodCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempCloneCalendarRecord", body = list(DataObject = body), searchFields = append("TempCloneCalendarRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCloneCalendarRecord
	#'
	#' This function modifies a TempCloneCalendarRecord
	#' @param fieldNames The field values to give the modified TempCloneCalendarRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCloneCalendarRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCloneCalendarRecord <- function(TempCloneCalendarRecordID, AffectedPrimaryKey = NULL, AttendanceCalculationMethod = NULL, Code = NULL, DefaultDayLengthMinutes = NULL, Description = NULL, EndDate = NULL, Entity = NULL, EntityID = NULL, HalfDayHighPeriodCount = NULL, IsDefault = NULL, SchoolYearID = NULL, StartDate = NULL, ZeroDayHighPeriodCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempCloneCalendarRecord", objectId = TempCloneCalendarRecordID, body = list(DataObject = body), searchFields = append("TempCloneCalendarRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCloneCalendarErrors
	#'
	#' This function returns a dataframe or json object of TempCloneCalendarErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCloneCalendarErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCloneCalendarErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCloneCalendarError') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempCloneCalendarErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCloneCalendarErrors <- function(searchConditionsList = NULL, CreatedTime = F, Description = F, EntityName = F, FailureReason = F, ModifiedTime = F, RecordType = F, TempCloneCalendarErrorID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCloneCalendarError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCloneCalendarError
	#'
	#' This function returns a dataframe or json object of a TempCloneCalendarError
	#' @param TempCloneCalendarErrorID The ID of the TempCloneCalendarError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCloneCalendarError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCloneCalendarError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCloneCalendarError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCloneCalendarError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCloneCalendarError <- function(TempCloneCalendarErrorID, CreatedTime = F, Description = F, EntityName = F, FailureReason = F, ModifiedTime = F, RecordType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCloneCalendarErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCloneCalendarError", objectId = TempCloneCalendarErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCloneCalendarError
	#'
	#' This function deletes a TempCloneCalendarError
	#' @param TempCloneCalendarErrorID The ID of the TempCloneCalendarError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCloneCalendarErrorID of the deleted TempCloneCalendarError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCloneCalendarError <- function(TempCloneCalendarErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCloneCalendarError", objectId = TempCloneCalendarErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCloneCalendarError
	#'
	#' This function creates a TempCloneCalendarError
	#' @param fieldNames The field values to give the created TempCloneCalendarError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCloneCalendarError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCloneCalendarError <- function(Description = NULL, EntityName = NULL, FailureReason = NULL, RecordType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempCloneCalendarError", body = list(DataObject = body), searchFields = append("TempCloneCalendarErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCloneCalendarError
	#'
	#' This function modifies a TempCloneCalendarError
	#' @param fieldNames The field values to give the modified TempCloneCalendarError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCloneCalendarError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCloneCalendarError <- function(TempCloneCalendarErrorID, Description = NULL, EntityName = NULL, FailureReason = NULL, RecordType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempCloneCalendarError", objectId = TempCloneCalendarErrorID, body = list(DataObject = body), searchFields = append("TempCloneCalendarErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAttendanceTerms
	#'
	#' This function returns a dataframe or json object of TempAttendanceTerms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAttendanceTerms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAttendanceTerms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAttendanceTerm') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempAttendanceTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAttendanceTerms <- function(searchConditionsList = NULL, AttendanceTermCode = F, AttendanceTermID = F, CalendarCode = F, CalendarEndDate = F, CalendarID = F, CalendarStartDate = F, CreatedTime = F, EndDate = F, IsUpdated = F, ModifiedTime = F, OriginalEndDate = F, OriginalStartDate = F, ProcessAction = F, StartDate = F, TempAttendanceTermID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempAttendanceTerm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAttendanceTerm
	#'
	#' This function returns a dataframe or json object of a TempAttendanceTerm
	#' @param TempAttendanceTermID The ID of the TempAttendanceTerm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAttendanceTerm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAttendanceTerm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAttendanceTerm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAttendanceTerm <- function(TempAttendanceTermID, AttendanceTermCode = F, AttendanceTermID = F, CalendarCode = F, CalendarEndDate = F, CalendarID = F, CalendarStartDate = F, CreatedTime = F, EndDate = F, IsUpdated = F, ModifiedTime = F, OriginalEndDate = F, OriginalStartDate = F, ProcessAction = F, StartDate = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAttendanceTermID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempAttendanceTerm", objectId = TempAttendanceTermID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAttendanceTerm
	#'
	#' This function deletes a TempAttendanceTerm
	#' @param TempAttendanceTermID The ID of the TempAttendanceTerm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempAttendanceTermID of the deleted TempAttendanceTerm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAttendanceTerm <- function(TempAttendanceTermID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempAttendanceTerm", objectId = TempAttendanceTermID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAttendanceTerm
	#'
	#' This function creates a TempAttendanceTerm
	#' @param fieldNames The field values to give the created TempAttendanceTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAttendanceTerm <- function(AttendanceTermCode = NULL, AttendanceTermID = NULL, CalendarCode = NULL, CalendarEndDate = NULL, CalendarID = NULL, CalendarStartDate = NULL, EndDate = NULL, IsUpdated = NULL, OriginalEndDate = NULL, OriginalStartDate = NULL, ProcessAction = NULL, StartDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempAttendanceTerm", body = list(DataObject = body), searchFields = append("TempAttendanceTermID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAttendanceTerm
	#'
	#' This function modifies a TempAttendanceTerm
	#' @param fieldNames The field values to give the modified TempAttendanceTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAttendanceTerm <- function(TempAttendanceTermID, AttendanceTermCode = NULL, AttendanceTermID = NULL, CalendarCode = NULL, CalendarEndDate = NULL, CalendarID = NULL, CalendarStartDate = NULL, EndDate = NULL, IsUpdated = NULL, OriginalEndDate = NULL, OriginalStartDate = NULL, ProcessAction = NULL, StartDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempAttendanceTerm", objectId = TempAttendanceTermID, body = list(DataObject = body), searchFields = append("TempAttendanceTermID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendancePeriodConfigEntityGroupYears
	#'
	#' This function returns a dataframe or json object of AttendancePeriodConfigEntityGroupYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendancePeriodConfigEntityGroupYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendancePeriodConfigEntityGroupYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendancePeriodConfigEntityGroupYear') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of AttendancePeriodConfigEntityGroupYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendancePeriodConfigEntityGroupYears <- function(searchConditionsList = NULL, AllowPreviousDayTeacherEntry = F, AttendancePeriodConfigEntityGroupYearID = F, AttendancePeriodConfigEntityGroupYearIDClonedFrom = F, AttendancePeriodID = F, ConfigEntityGroupYearID = F, CreatedTime = F, EntityGroupKey = F, ModifiedTime = F, TeacherEntryCutoffDuration = F, TeacherEntryCutoffNumberOfMinutesAfter = F, TeacherEntryCutoffTime = F, TeacherEntryCutoffWindowEndTime = F, TeacherEntryCutoffWindowStartTime = F, TeacherEntrySpecificCutoffTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendancePeriodConfigEntityGroupYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendancePeriodConfigEntityGroupYear
	#'
	#' This function returns a dataframe or json object of an AttendancePeriodConfigEntityGroupYear
	#' @param AttendancePeriodConfigEntityGroupYearID The ID of the AttendancePeriodConfigEntityGroupYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendancePeriodConfigEntityGroupYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendancePeriodConfigEntityGroupYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendancePeriodConfigEntityGroupYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendancePeriodConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendancePeriodConfigEntityGroupYear <- function(AttendancePeriodConfigEntityGroupYearID, AllowPreviousDayTeacherEntry = F, AttendancePeriodConfigEntityGroupYearIDClonedFrom = F, AttendancePeriodID = F, ConfigEntityGroupYearID = F, CreatedTime = F, EntityGroupKey = F, ModifiedTime = F, TeacherEntryCutoffDuration = F, TeacherEntryCutoffNumberOfMinutesAfter = F, TeacherEntryCutoffTime = F, TeacherEntryCutoffWindowEndTime = F, TeacherEntryCutoffWindowStartTime = F, TeacherEntrySpecificCutoffTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendancePeriodConfigEntityGroupYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendancePeriodConfigEntityGroupYear", objectId = AttendancePeriodConfigEntityGroupYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendancePeriodConfigEntityGroupYear
	#'
	#' This function deletes an AttendancePeriodConfigEntityGroupYear
	#' @param AttendancePeriodConfigEntityGroupYearID The ID of the AttendancePeriodConfigEntityGroupYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendancePeriodConfigEntityGroupYearID of the deleted AttendancePeriodConfigEntityGroupYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendancePeriodConfigEntityGroupYear <- function(AttendancePeriodConfigEntityGroupYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendancePeriodConfigEntityGroupYear", objectId = AttendancePeriodConfigEntityGroupYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendancePeriodConfigEntityGroupYear
	#'
	#' This function creates an AttendancePeriodConfigEntityGroupYear
	#' @param fieldNames The field values to give the created AttendancePeriodConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendancePeriodConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendancePeriodConfigEntityGroupYear <- function(AllowPreviousDayTeacherEntry = NULL, AttendancePeriodConfigEntityGroupYearIDClonedFrom = NULL, AttendancePeriodID = NULL, ConfigEntityGroupYearID = NULL, EntityGroupKey = NULL, TeacherEntryCutoffDuration = NULL, TeacherEntryCutoffNumberOfMinutesAfter = NULL, TeacherEntryCutoffTime = NULL, TeacherEntryCutoffWindowEndTime = NULL, TeacherEntryCutoffWindowStartTime = NULL, TeacherEntrySpecificCutoffTime = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "AttendancePeriodConfigEntityGroupYear", body = list(DataObject = body), searchFields = append("AttendancePeriodConfigEntityGroupYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendancePeriodConfigEntityGroupYear
	#'
	#' This function modifies an AttendancePeriodConfigEntityGroupYear
	#' @param fieldNames The field values to give the modified AttendancePeriodConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendancePeriodConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendancePeriodConfigEntityGroupYear <- function(AttendancePeriodConfigEntityGroupYearID, AllowPreviousDayTeacherEntry = NULL, AttendancePeriodConfigEntityGroupYearIDClonedFrom = NULL, AttendancePeriodID = NULL, ConfigEntityGroupYearID = NULL, EntityGroupKey = NULL, TeacherEntryCutoffDuration = NULL, TeacherEntryCutoffNumberOfMinutesAfter = NULL, TeacherEntryCutoffTime = NULL, TeacherEntryCutoffWindowEndTime = NULL, TeacherEntryCutoffWindowStartTime = NULL, TeacherEntrySpecificCutoffTime = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "AttendancePeriodConfigEntityGroupYear", objectId = AttendancePeriodConfigEntityGroupYearID, body = list(DataObject = body), searchFields = append("AttendancePeriodConfigEntityGroupYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BellScheduleGroupBellSchedules
	#'
	#' This function returns a dataframe or json object of BellScheduleGroupBellSchedules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BellScheduleGroupBellSchedules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BellScheduleGroupBellSchedules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BellScheduleGroupBellSchedule') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of BellScheduleGroupBellSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBellScheduleGroupBellSchedules <- function(searchConditionsList = NULL, BellScheduleGroupBellScheduleID = F, BellScheduleGroupID = F, BellScheduleID = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "BellScheduleGroupBellSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BellScheduleGroupBellSchedule
	#'
	#' This function returns a dataframe or json object of a BellScheduleGroupBellSchedule
	#' @param BellScheduleGroupBellScheduleID The ID of the BellScheduleGroupBellSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BellScheduleGroupBellSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BellScheduleGroupBellSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BellScheduleGroupBellSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of BellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBellScheduleGroupBellSchedule <- function(BellScheduleGroupBellScheduleID, BellScheduleGroupID = F, BellScheduleID = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BellScheduleGroupBellScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "BellScheduleGroupBellSchedule", objectId = BellScheduleGroupBellScheduleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BellScheduleGroupBellSchedule
	#'
	#' This function deletes a BellScheduleGroupBellSchedule
	#' @param BellScheduleGroupBellScheduleID The ID of the BellScheduleGroupBellSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The BellScheduleGroupBellScheduleID of the deleted BellScheduleGroupBellSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBellScheduleGroupBellSchedule <- function(BellScheduleGroupBellScheduleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "BellScheduleGroupBellSchedule", objectId = BellScheduleGroupBellScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BellScheduleGroupBellSchedule
	#'
	#' This function creates a BellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the created BellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created BellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBellScheduleGroupBellSchedule <- function(BellScheduleGroupID = NULL, BellScheduleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "BellScheduleGroupBellSchedule", body = list(DataObject = body), searchFields = append("BellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BellScheduleGroupBellSchedule
	#'
	#' This function modifies a BellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the modified BellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified BellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBellScheduleGroupBellSchedule <- function(BellScheduleGroupBellScheduleID, BellScheduleGroupID = NULL, BellScheduleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "BellScheduleGroupBellSchedule", objectId = BellScheduleGroupBellScheduleID, body = list(DataObject = body), searchFields = append("BellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalendarDayBellScheduleGroupBellSchedules
	#'
	#' This function returns a dataframe or json object of CalendarDayBellScheduleGroupBellSchedules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDayBellScheduleGroupBellSchedules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDayBellScheduleGroupBellSchedules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDayBellScheduleGroupBellSchedule') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of CalendarDayBellScheduleGroupBellSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarDayBellScheduleGroupBellSchedules <- function(searchConditionsList = NULL, BellScheduleGroupBellScheduleID = F, BellScheduleGroupID = F, CalendarDayBellScheduleGroupBellScheduleID = F, CalendarDayID = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarDayBellScheduleGroupBellSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function returns a dataframe or json object of a CalendarDayBellScheduleGroupBellSchedule
	#' @param CalendarDayBellScheduleGroupBellScheduleID The ID of the CalendarDayBellScheduleGroupBellSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDayBellScheduleGroupBellSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDayBellScheduleGroupBellSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDayBellScheduleGroupBellSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CalendarDayBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarDayBellScheduleGroupBellSchedule <- function(CalendarDayBellScheduleGroupBellScheduleID, BellScheduleGroupBellScheduleID = F, BellScheduleGroupID = F, CalendarDayID = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarDayBellScheduleGroupBellScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarDayBellScheduleGroupBellSchedule", objectId = CalendarDayBellScheduleGroupBellScheduleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function deletes a CalendarDayBellScheduleGroupBellSchedule
	#' @param CalendarDayBellScheduleGroupBellScheduleID The ID of the CalendarDayBellScheduleGroupBellSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarDayBellScheduleGroupBellScheduleID of the deleted CalendarDayBellScheduleGroupBellSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarDayBellScheduleGroupBellSchedule <- function(CalendarDayBellScheduleGroupBellScheduleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarDayBellScheduleGroupBellSchedule", objectId = CalendarDayBellScheduleGroupBellScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function creates a CalendarDayBellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the created CalendarDayBellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CalendarDayBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarDayBellScheduleGroupBellSchedule <- function(BellScheduleGroupBellScheduleID = NULL, BellScheduleGroupID = NULL, CalendarDayID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "CalendarDayBellScheduleGroupBellSchedule", body = list(DataObject = body), searchFields = append("CalendarDayBellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function modifies a CalendarDayBellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the modified CalendarDayBellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CalendarDayBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarDayBellScheduleGroupBellSchedule <- function(CalendarDayBellScheduleGroupBellScheduleID, BellScheduleGroupBellScheduleID = NULL, BellScheduleGroupID = NULL, CalendarDayID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarDayBellScheduleGroupBellSchedule", objectId = CalendarDayBellScheduleGroupBellScheduleID, body = list(DataObject = body), searchFields = append("CalendarDayBellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BellScheduleGroups
	#'
	#' This function returns a dataframe or json object of BellScheduleGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BellScheduleGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BellScheduleGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BellScheduleGroup') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of BellScheduleGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBellScheduleGroups <- function(searchConditionsList = NULL, AttendancePeriodIDAsOfDate = F, BellScheduleGroupID = F, BellScheduleGroupIDClonedFrom = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EntityID = F, IsDefault = F, ModifiedTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "BellScheduleGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BellScheduleGroup
	#'
	#' This function returns a dataframe or json object of a BellScheduleGroup
	#' @param BellScheduleGroupID The ID of the BellScheduleGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BellScheduleGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BellScheduleGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BellScheduleGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of BellScheduleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBellScheduleGroup <- function(BellScheduleGroupID, AttendancePeriodIDAsOfDate = F, BellScheduleGroupIDClonedFrom = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, EntityID = F, IsDefault = F, ModifiedTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BellScheduleGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "BellScheduleGroup", objectId = BellScheduleGroupID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BellScheduleGroup
	#'
	#' This function deletes a BellScheduleGroup
	#' @param BellScheduleGroupID The ID of the BellScheduleGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The BellScheduleGroupID of the deleted BellScheduleGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBellScheduleGroup <- function(BellScheduleGroupID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "BellScheduleGroup", objectId = BellScheduleGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BellScheduleGroup
	#'
	#' This function creates a BellScheduleGroup
	#' @param fieldNames The field values to give the created BellScheduleGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created BellScheduleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBellScheduleGroup <- function(BellScheduleGroupIDClonedFrom = NULL, Code = NULL, Description = NULL, EntityID = NULL, IsDefault = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "BellScheduleGroup", body = list(DataObject = body), searchFields = append("BellScheduleGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BellScheduleGroup
	#'
	#' This function modifies a BellScheduleGroup
	#' @param fieldNames The field values to give the modified BellScheduleGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified BellScheduleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBellScheduleGroup <- function(BellScheduleGroupID, BellScheduleGroupIDClonedFrom = NULL, Code = NULL, Description = NULL, EntityID = NULL, IsDefault = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "BellScheduleGroup", objectId = BellScheduleGroupID, body = list(DataObject = body), searchFields = append("BellScheduleGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MassCreateAttendanceByClassActivityRangeRuns
	#'
	#' This function returns a dataframe or json object of MassCreateAttendanceByClassActivityRangeRuns
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassCreateAttendanceByClassActivityRangeRuns. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassCreateAttendanceByClassActivityRangeRuns.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassCreateAttendanceByClassActivityRangeRun') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of MassCreateAttendanceByClassActivityRangeRuns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMassCreateAttendanceByClassActivityRangeRuns <- function(searchConditionsList = NULL, AffectedStudentAttendanceCount = F, CreatedTime = F, EntityID = F, IsActive = F, MassCreateAttendanceByClassActivityRangeRunID = F, ModifiedTime = F, RunTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDRunBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "MassCreateAttendanceByClassActivityRangeRun", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MassCreateAttendanceByClassActivityRangeRun
	#'
	#' This function returns a dataframe or json object of a MassCreateAttendanceByClassActivityRangeRun
	#' @param MassCreateAttendanceByClassActivityRangeRunID The ID of the MassCreateAttendanceByClassActivityRangeRun to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassCreateAttendanceByClassActivityRangeRun. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassCreateAttendanceByClassActivityRangeRun.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassCreateAttendanceByClassActivityRangeRun') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of MassCreateAttendanceByClassActivityRangeRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMassCreateAttendanceByClassActivityRangeRun <- function(MassCreateAttendanceByClassActivityRangeRunID, AffectedStudentAttendanceCount = F, CreatedTime = F, EntityID = F, IsActive = F, ModifiedTime = F, RunTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDRunBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MassCreateAttendanceByClassActivityRangeRunID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "MassCreateAttendanceByClassActivityRangeRun", objectId = MassCreateAttendanceByClassActivityRangeRunID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MassCreateAttendanceByClassActivityRangeRun
	#'
	#' This function deletes a MassCreateAttendanceByClassActivityRangeRun
	#' @param MassCreateAttendanceByClassActivityRangeRunID The ID of the MassCreateAttendanceByClassActivityRangeRun to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The MassCreateAttendanceByClassActivityRangeRunID of the deleted MassCreateAttendanceByClassActivityRangeRun.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMassCreateAttendanceByClassActivityRangeRun <- function(MassCreateAttendanceByClassActivityRangeRunID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "MassCreateAttendanceByClassActivityRangeRun", objectId = MassCreateAttendanceByClassActivityRangeRunID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MassCreateAttendanceByClassActivityRangeRun
	#'
	#' This function creates a MassCreateAttendanceByClassActivityRangeRun
	#' @param fieldNames The field values to give the created MassCreateAttendanceByClassActivityRangeRun. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created MassCreateAttendanceByClassActivityRangeRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMassCreateAttendanceByClassActivityRangeRun <- function(EntityID = NULL, IsActive = NULL, RunTime = NULL, SchoolYearID = NULL, UserIDRunBy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "MassCreateAttendanceByClassActivityRangeRun", body = list(DataObject = body), searchFields = append("MassCreateAttendanceByClassActivityRangeRunID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MassCreateAttendanceByClassActivityRangeRun
	#'
	#' This function modifies a MassCreateAttendanceByClassActivityRangeRun
	#' @param fieldNames The field values to give the modified MassCreateAttendanceByClassActivityRangeRun. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified MassCreateAttendanceByClassActivityRangeRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMassCreateAttendanceByClassActivityRangeRun <- function(MassCreateAttendanceByClassActivityRangeRunID, EntityID = NULL, IsActive = NULL, RunTime = NULL, SchoolYearID = NULL, UserIDRunBy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "MassCreateAttendanceByClassActivityRangeRun", objectId = MassCreateAttendanceByClassActivityRangeRunID, body = list(DataObject = body), searchFields = append("MassCreateAttendanceByClassActivityRangeRunID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAttendanceRunHistories
	#'
	#' This function returns a dataframe or json object of StudentAttendanceRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendanceRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendanceRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendanceRunHistory') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of StudentAttendanceRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendanceRunHistories <- function(searchConditionsList = NULL, CalendarDayID = F, CreatedTime = F, IsActive = F, IsInsert = F, MassCreateAttendanceByClassActivityRangeRunID = F, ModifiedTime = F, NewComment = F, NewIsGuardianNotified = F, OriginalComment = F, OriginalIsGuardianNotified = F, Procedure = F, Status = F, StudentAttendanceID = F, StudentAttendanceRunHistoryID = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendanceRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendanceRunHistory
	#'
	#' This function returns a dataframe or json object of a StudentAttendanceRunHistory
	#' @param StudentAttendanceRunHistoryID The ID of the StudentAttendanceRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendanceRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendanceRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendanceRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendanceRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendanceRunHistory <- function(StudentAttendanceRunHistoryID, CalendarDayID = F, CreatedTime = F, IsActive = F, IsInsert = F, MassCreateAttendanceByClassActivityRangeRunID = F, ModifiedTime = F, NewComment = F, NewIsGuardianNotified = F, OriginalComment = F, OriginalIsGuardianNotified = F, Procedure = F, Status = F, StudentAttendanceID = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendanceRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendanceRunHistory", objectId = StudentAttendanceRunHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendanceRunHistory
	#'
	#' This function deletes a StudentAttendanceRunHistory
	#' @param StudentAttendanceRunHistoryID The ID of the StudentAttendanceRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendanceRunHistoryID of the deleted StudentAttendanceRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendanceRunHistory <- function(StudentAttendanceRunHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendanceRunHistory", objectId = StudentAttendanceRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentAttendanceRunHistory
	#'
	#' This function creates a StudentAttendanceRunHistory
	#' @param fieldNames The field values to give the created StudentAttendanceRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentAttendanceRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentAttendanceRunHistory <- function(CalendarDayID = NULL, IsActive = NULL, IsInsert = NULL, MassCreateAttendanceByClassActivityRangeRunID = NULL, NewComment = NULL, NewIsGuardianNotified = NULL, OriginalComment = NULL, OriginalIsGuardianNotified = NULL, StudentAttendanceID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "StudentAttendanceRunHistory", body = list(DataObject = body), searchFields = append("StudentAttendanceRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentAttendanceRunHistory
	#'
	#' This function modifies a StudentAttendanceRunHistory
	#' @param fieldNames The field values to give the modified StudentAttendanceRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentAttendanceRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentAttendanceRunHistory <- function(StudentAttendanceRunHistoryID, CalendarDayID = NULL, IsActive = NULL, IsInsert = NULL, MassCreateAttendanceByClassActivityRangeRunID = NULL, NewComment = NULL, NewIsGuardianNotified = NULL, OriginalComment = NULL, OriginalIsGuardianNotified = NULL, StudentAttendanceID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "StudentAttendanceRunHistory", objectId = StudentAttendanceRunHistoryID, body = list(DataObject = body), searchFields = append("StudentAttendanceRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAttendancePeriodRunHistories
	#'
	#' This function returns a dataframe or json object of StudentAttendancePeriodRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendancePeriodRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendancePeriodRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendancePeriodRunHistory') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of StudentAttendancePeriodRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendancePeriodRunHistories <- function(searchConditionsList = NULL, AttendancePeriodID = F, CreatedTime = F, IsActive = F, IsInsert = F, ModifiedTime = F, NewAttendanceReasonID = F, NewAttendanceTypeID = F, NewComment = F, OriginalAttendanceReasonID = F, OriginalAttendanceTypeID = F, OriginalComment = F, Procedure = F, Status = F, StudentAttendancePeriodID = F, StudentAttendancePeriodRunHistoryID = F, StudentAttendanceRunHistoryID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendancePeriodRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendancePeriodRunHistory
	#'
	#' This function returns a dataframe or json object of a StudentAttendancePeriodRunHistory
	#' @param StudentAttendancePeriodRunHistoryID The ID of the StudentAttendancePeriodRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendancePeriodRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendancePeriodRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendancePeriodRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendancePeriodRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendancePeriodRunHistory <- function(StudentAttendancePeriodRunHistoryID, AttendancePeriodID = F, CreatedTime = F, IsActive = F, IsInsert = F, ModifiedTime = F, NewAttendanceReasonID = F, NewAttendanceTypeID = F, NewComment = F, OriginalAttendanceReasonID = F, OriginalAttendanceTypeID = F, OriginalComment = F, Procedure = F, Status = F, StudentAttendancePeriodID = F, StudentAttendanceRunHistoryID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendancePeriodRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendancePeriodRunHistory", objectId = StudentAttendancePeriodRunHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendancePeriodRunHistory
	#'
	#' This function deletes a StudentAttendancePeriodRunHistory
	#' @param StudentAttendancePeriodRunHistoryID The ID of the StudentAttendancePeriodRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendancePeriodRunHistoryID of the deleted StudentAttendancePeriodRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendancePeriodRunHistory <- function(StudentAttendancePeriodRunHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendancePeriodRunHistory", objectId = StudentAttendancePeriodRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentAttendancePeriodRunHistory
	#'
	#' This function creates a StudentAttendancePeriodRunHistory
	#' @param fieldNames The field values to give the created StudentAttendancePeriodRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentAttendancePeriodRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentAttendancePeriodRunHistory <- function(AttendancePeriodID = NULL, IsActive = NULL, IsInsert = NULL, NewAttendanceReasonID = NULL, NewAttendanceTypeID = NULL, NewComment = NULL, OriginalAttendanceReasonID = NULL, OriginalAttendanceTypeID = NULL, OriginalComment = NULL, StudentAttendancePeriodID = NULL, StudentAttendanceRunHistoryID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "StudentAttendancePeriodRunHistory", body = list(DataObject = body), searchFields = append("StudentAttendancePeriodRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentAttendancePeriodRunHistory
	#'
	#' This function modifies a StudentAttendancePeriodRunHistory
	#' @param fieldNames The field values to give the modified StudentAttendancePeriodRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentAttendancePeriodRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentAttendancePeriodRunHistory <- function(StudentAttendancePeriodRunHistoryID, AttendancePeriodID = NULL, IsActive = NULL, IsInsert = NULL, NewAttendanceReasonID = NULL, NewAttendanceTypeID = NULL, NewComment = NULL, OriginalAttendanceReasonID = NULL, OriginalAttendanceTypeID = NULL, OriginalComment = NULL, StudentAttendancePeriodID = NULL, StudentAttendanceRunHistoryID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "StudentAttendancePeriodRunHistory", objectId = StudentAttendancePeriodRunHistoryID, body = list(DataObject = body), searchFields = append("StudentAttendancePeriodRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAttendanceEntities
	#'
	#' This function returns a dataframe or json object of StudentAttendanceEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendanceEntities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendanceEntities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendanceEntity') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of StudentAttendanceEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendanceEntities <- function(searchConditionsList = NULL, EntityID = F, StudentAttendanceID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendanceEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendanceEntity
	#'
	#' This function returns a dataframe or json object of a StudentAttendanceEntity
	#' @param StudentAttendanceEntityID The ID of the StudentAttendanceEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendanceEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendanceEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendanceEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendanceEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendanceEntity <- function(StudentAttendanceEntityID, EntityID = F, StudentAttendanceID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendanceEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendanceEntity", objectId = StudentAttendanceEntityID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendanceEntity
	#'
	#' This function deletes a StudentAttendanceEntity
	#' @param StudentAttendanceEntityID The ID of the StudentAttendanceEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendanceEntityID of the deleted StudentAttendanceEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendanceEntity <- function(StudentAttendanceEntityID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendanceEntity", objectId = StudentAttendanceEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempBellScheduleGroupBellSchedules
	#'
	#' This function returns a dataframe or json object of TempBellScheduleGroupBellSchedules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBellScheduleGroupBellSchedules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBellScheduleGroupBellSchedules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBellScheduleGroupBellSchedule') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempBellScheduleGroupBellSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempBellScheduleGroupBellSchedules <- function(searchConditionsList = NULL, BellScheduleGroupBellScheduleID = F, BellScheduleGroupCodeDescription = F, BellScheduleGroupID = F, BellScheduleID = F, CreatedTime = F, IsDefault = F, ModifiedTime = F, ShouldUpdate = F, TempBellScheduleGroupBellScheduleID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempBellScheduleGroupBellSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempBellScheduleGroupBellSchedule
	#'
	#' This function returns a dataframe or json object of a TempBellScheduleGroupBellSchedule
	#' @param TempBellScheduleGroupBellScheduleID The ID of the TempBellScheduleGroupBellSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBellScheduleGroupBellSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBellScheduleGroupBellSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBellScheduleGroupBellSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempBellScheduleGroupBellSchedule <- function(TempBellScheduleGroupBellScheduleID, BellScheduleGroupBellScheduleID = F, BellScheduleGroupCodeDescription = F, BellScheduleGroupID = F, BellScheduleID = F, CreatedTime = F, IsDefault = F, ModifiedTime = F, ShouldUpdate = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempBellScheduleGroupBellScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempBellScheduleGroupBellSchedule", objectId = TempBellScheduleGroupBellScheduleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempBellScheduleGroupBellSchedule
	#'
	#' This function deletes a TempBellScheduleGroupBellSchedule
	#' @param TempBellScheduleGroupBellScheduleID The ID of the TempBellScheduleGroupBellSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempBellScheduleGroupBellScheduleID of the deleted TempBellScheduleGroupBellSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempBellScheduleGroupBellSchedule <- function(TempBellScheduleGroupBellScheduleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempBellScheduleGroupBellSchedule", objectId = TempBellScheduleGroupBellScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempBellScheduleGroupBellSchedule
	#'
	#' This function creates a TempBellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the created TempBellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempBellScheduleGroupBellSchedule <- function(BellScheduleGroupBellScheduleID = NULL, BellScheduleGroupCodeDescription = NULL, BellScheduleGroupID = NULL, BellScheduleID = NULL, IsDefault = NULL, ShouldUpdate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempBellScheduleGroupBellSchedule", body = list(DataObject = body), searchFields = append("TempBellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempBellScheduleGroupBellSchedule
	#'
	#' This function modifies a TempBellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the modified TempBellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempBellScheduleGroupBellSchedule <- function(TempBellScheduleGroupBellScheduleID, BellScheduleGroupBellScheduleID = NULL, BellScheduleGroupCodeDescription = NULL, BellScheduleGroupID = NULL, BellScheduleID = NULL, IsDefault = NULL, ShouldUpdate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempBellScheduleGroupBellSchedule", objectId = TempBellScheduleGroupBellScheduleID, body = list(DataObject = body), searchFields = append("TempBellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCalendarDayBellScheduleGroupBellSchedules
	#'
	#' This function returns a dataframe or json object of TempCalendarDayBellScheduleGroupBellSchedules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarDayBellScheduleGroupBellSchedules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarDayBellScheduleGroupBellSchedules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarDayBellScheduleGroupBellSchedule') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempCalendarDayBellScheduleGroupBellSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalendarDayBellScheduleGroupBellSchedules <- function(searchConditionsList = NULL, BellScheduleDescription = F, BellScheduleGroupBellScheduleID = F, BellScheduleGroupCodeDescription = F, BellScheduleGroupID = F, BellScheduleID = F, Calendar = F, CalendarDayID = F, CountAs = F, CreatedTime = F, Date = F, DayRotationCode = F, ExistingBellScheduleCode = F, ExistingBellScheduleGroupBellScheduleID = F, ExistingCalendarDayBellScheduleGroupBellScheduleID = F, IsDefault = F, ModifiedTime = F, TempCalendarDayBellScheduleGroupBellScheduleID = F, UpdateBellSchedule = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCalendarDayBellScheduleGroupBellSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function returns a dataframe or json object of a TempCalendarDayBellScheduleGroupBellSchedule
	#' @param TempCalendarDayBellScheduleGroupBellScheduleID The ID of the TempCalendarDayBellScheduleGroupBellSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarDayBellScheduleGroupBellSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarDayBellScheduleGroupBellSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarDayBellScheduleGroupBellSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCalendarDayBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalendarDayBellScheduleGroupBellSchedule <- function(TempCalendarDayBellScheduleGroupBellScheduleID, BellScheduleDescription = F, BellScheduleGroupBellScheduleID = F, BellScheduleGroupCodeDescription = F, BellScheduleGroupID = F, BellScheduleID = F, Calendar = F, CalendarDayID = F, CountAs = F, CreatedTime = F, Date = F, DayRotationCode = F, ExistingBellScheduleCode = F, ExistingBellScheduleGroupBellScheduleID = F, ExistingCalendarDayBellScheduleGroupBellScheduleID = F, IsDefault = F, ModifiedTime = F, UpdateBellSchedule = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalendarDayBellScheduleGroupBellScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCalendarDayBellScheduleGroupBellSchedule", objectId = TempCalendarDayBellScheduleGroupBellScheduleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function deletes a TempCalendarDayBellScheduleGroupBellSchedule
	#' @param TempCalendarDayBellScheduleGroupBellScheduleID The ID of the TempCalendarDayBellScheduleGroupBellSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCalendarDayBellScheduleGroupBellScheduleID of the deleted TempCalendarDayBellScheduleGroupBellSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCalendarDayBellScheduleGroupBellSchedule <- function(TempCalendarDayBellScheduleGroupBellScheduleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCalendarDayBellScheduleGroupBellSchedule", objectId = TempCalendarDayBellScheduleGroupBellScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function creates a TempCalendarDayBellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the created TempCalendarDayBellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCalendarDayBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCalendarDayBellScheduleGroupBellSchedule <- function(BellScheduleDescription = NULL, BellScheduleGroupBellScheduleID = NULL, BellScheduleGroupCodeDescription = NULL, BellScheduleGroupID = NULL, BellScheduleID = NULL, Calendar = NULL, CalendarDayID = NULL, CountAs = NULL, Date = NULL, DayRotationCode = NULL, ExistingBellScheduleCode = NULL, ExistingBellScheduleGroupBellScheduleID = NULL, ExistingCalendarDayBellScheduleGroupBellScheduleID = NULL, IsDefault = NULL, UpdateBellSchedule = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempCalendarDayBellScheduleGroupBellSchedule", body = list(DataObject = body), searchFields = append("TempCalendarDayBellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function modifies a TempCalendarDayBellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the modified TempCalendarDayBellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCalendarDayBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCalendarDayBellScheduleGroupBellSchedule <- function(TempCalendarDayBellScheduleGroupBellScheduleID, BellScheduleDescription = NULL, BellScheduleGroupBellScheduleID = NULL, BellScheduleGroupCodeDescription = NULL, BellScheduleGroupID = NULL, BellScheduleID = NULL, Calendar = NULL, CalendarDayID = NULL, CountAs = NULL, Date = NULL, DayRotationCode = NULL, ExistingBellScheduleCode = NULL, ExistingBellScheduleGroupBellScheduleID = NULL, ExistingCalendarDayBellScheduleGroupBellScheduleID = NULL, IsDefault = NULL, UpdateBellSchedule = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempCalendarDayBellScheduleGroupBellSchedule", objectId = TempCalendarDayBellScheduleGroupBellScheduleID, body = list(DataObject = body), searchFields = append("TempCalendarDayBellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAttendanceCounts
	#'
	#' This function returns a dataframe or json object of StudentAttendanceCounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendanceCounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendanceCounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendanceCounts') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of StudentAttendanceCounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendanceCounts <- function(searchConditionsList = NULL, CreatedTime = F, DaysAbsent = F, DaysEnrolled = F, DaysExcused = F, DaysOther = F, DaysUnexcused = F, ModifiedTime = F, StudentEntityYearID = F, TardyCount = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendanceCounts", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendanceCounts
	#'
	#' This function returns a dataframe or json object of a StudentAttendanceCounts
	#' @param StudentAttendanceCountsID The ID of the StudentAttendanceCounts to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendanceCounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendanceCounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendanceCounts') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendanceCounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendanceCounts <- function(StudentAttendanceCountsID, CreatedTime = F, DaysAbsent = F, DaysEnrolled = F, DaysExcused = F, DaysOther = F, DaysUnexcused = F, ModifiedTime = F, StudentEntityYearID = F, TardyCount = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendanceCountsID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendanceCounts", objectId = StudentAttendanceCountsID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendanceCounts
	#'
	#' This function deletes a StudentAttendanceCounts
	#' @param StudentAttendanceCountsID The ID of the StudentAttendanceCounts to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendanceCountsID of the deleted StudentAttendanceCounts.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendanceCounts <- function(StudentAttendanceCountsID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendanceCounts", objectId = StudentAttendanceCountsID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentAttendanceCounts
	#'
	#' This function creates a StudentAttendanceCounts
	#' @param fieldNames The field values to give the created StudentAttendanceCounts. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentAttendanceCounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentAttendanceCounts <- function(DaysAbsent = NULL, DaysEnrolled = NULL, DaysExcused = NULL, DaysOther = NULL, DaysUnexcused = NULL, TardyCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "StudentAttendanceCounts", body = list(DataObject = body), searchFields = append("StudentAttendanceCountsID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentAttendanceCounts
	#'
	#' This function modifies a StudentAttendanceCounts
	#' @param fieldNames The field values to give the modified StudentAttendanceCounts. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentAttendanceCounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentAttendanceCounts <- function(StudentAttendanceCountsID, DaysAbsent = NULL, DaysEnrolled = NULL, DaysExcused = NULL, DaysOther = NULL, DaysUnexcused = NULL, TardyCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "StudentAttendanceCounts", objectId = StudentAttendanceCountsID, body = list(DataObject = body), searchFields = append("StudentAttendanceCountsID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDroppedStudentAttendancePeriodErrors
	#'
	#' This function returns a dataframe or json object of TempDroppedStudentAttendancePeriodErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDroppedStudentAttendancePeriodErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDroppedStudentAttendancePeriodErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDroppedStudentAttendancePeriodError') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempDroppedStudentAttendancePeriodErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDroppedStudentAttendancePeriodErrors <- function(searchConditionsList = NULL, CreatedTime = F, ErrorDescription = F, ErrorNumber = F, ModifiedTime = F, TempDroppedStudentAttendancePeriodErrorID = F, TempDroppedStudentAttendancePeriodRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDroppedStudentAttendancePeriodError
	#'
	#' This function returns a dataframe or json object of a TempDroppedStudentAttendancePeriodError
	#' @param TempDroppedStudentAttendancePeriodErrorID The ID of the TempDroppedStudentAttendancePeriodError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDroppedStudentAttendancePeriodError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDroppedStudentAttendancePeriodError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDroppedStudentAttendancePeriodError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempDroppedStudentAttendancePeriodError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDroppedStudentAttendancePeriodError <- function(TempDroppedStudentAttendancePeriodErrorID, CreatedTime = F, ErrorDescription = F, ErrorNumber = F, ModifiedTime = F, TempDroppedStudentAttendancePeriodRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDroppedStudentAttendancePeriodErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodError", objectId = TempDroppedStudentAttendancePeriodErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDroppedStudentAttendancePeriodError
	#'
	#' This function deletes a TempDroppedStudentAttendancePeriodError
	#' @param TempDroppedStudentAttendancePeriodErrorID The ID of the TempDroppedStudentAttendancePeriodError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempDroppedStudentAttendancePeriodErrorID of the deleted TempDroppedStudentAttendancePeriodError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDroppedStudentAttendancePeriodError <- function(TempDroppedStudentAttendancePeriodErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodError", objectId = TempDroppedStudentAttendancePeriodErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDroppedStudentAttendancePeriodError
	#'
	#' This function creates a TempDroppedStudentAttendancePeriodError
	#' @param fieldNames The field values to give the created TempDroppedStudentAttendancePeriodError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempDroppedStudentAttendancePeriodError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDroppedStudentAttendancePeriodError <- function(ErrorDescription = NULL, ErrorNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodError", body = list(DataObject = body), searchFields = append("TempDroppedStudentAttendancePeriodErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDroppedStudentAttendancePeriodError
	#'
	#' This function modifies a TempDroppedStudentAttendancePeriodError
	#' @param fieldNames The field values to give the modified TempDroppedStudentAttendancePeriodError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempDroppedStudentAttendancePeriodError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDroppedStudentAttendancePeriodError <- function(TempDroppedStudentAttendancePeriodErrorID, ErrorDescription = NULL, ErrorNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodError", objectId = TempDroppedStudentAttendancePeriodErrorID, body = list(DataObject = body), searchFields = append("TempDroppedStudentAttendancePeriodErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDroppedStudentAttendancePeriodRecords
	#'
	#' This function returns a dataframe or json object of TempDroppedStudentAttendancePeriodRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDroppedStudentAttendancePeriodRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDroppedStudentAttendancePeriodRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDroppedStudentAttendancePeriodRecord') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempDroppedStudentAttendancePeriodRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDroppedStudentAttendancePeriodRecords <- function(searchConditionsList = NULL, AffectedPrimaryKey = F, AttendancePeriodCode = F, AttendanceTypeCode = F, CourseDescription = F, CreatedTime = F, Date = F, ErrorCount = F, ModifiedTime = F, StudentName = F, TempDroppedStudentAttendancePeriodRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDroppedStudentAttendancePeriodRecord
	#'
	#' This function returns a dataframe or json object of a TempDroppedStudentAttendancePeriodRecord
	#' @param TempDroppedStudentAttendancePeriodRecordID The ID of the TempDroppedStudentAttendancePeriodRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDroppedStudentAttendancePeriodRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDroppedStudentAttendancePeriodRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDroppedStudentAttendancePeriodRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempDroppedStudentAttendancePeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDroppedStudentAttendancePeriodRecord <- function(TempDroppedStudentAttendancePeriodRecordID, AffectedPrimaryKey = F, AttendancePeriodCode = F, AttendanceTypeCode = F, CourseDescription = F, CreatedTime = F, Date = F, ErrorCount = F, ModifiedTime = F, StudentName = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDroppedStudentAttendancePeriodRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodRecord", objectId = TempDroppedStudentAttendancePeriodRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDroppedStudentAttendancePeriodRecord
	#'
	#' This function deletes a TempDroppedStudentAttendancePeriodRecord
	#' @param TempDroppedStudentAttendancePeriodRecordID The ID of the TempDroppedStudentAttendancePeriodRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempDroppedStudentAttendancePeriodRecordID of the deleted TempDroppedStudentAttendancePeriodRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDroppedStudentAttendancePeriodRecord <- function(TempDroppedStudentAttendancePeriodRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodRecord", objectId = TempDroppedStudentAttendancePeriodRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDroppedStudentAttendancePeriodRecord
	#'
	#' This function creates a TempDroppedStudentAttendancePeriodRecord
	#' @param fieldNames The field values to give the created TempDroppedStudentAttendancePeriodRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempDroppedStudentAttendancePeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDroppedStudentAttendancePeriodRecord <- function(AffectedPrimaryKey = NULL, AttendancePeriodCode = NULL, AttendanceTypeCode = NULL, CourseDescription = NULL, Date = NULL, ErrorCount = NULL, StudentName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodRecord", body = list(DataObject = body), searchFields = append("TempDroppedStudentAttendancePeriodRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDroppedStudentAttendancePeriodRecord
	#'
	#' This function modifies a TempDroppedStudentAttendancePeriodRecord
	#' @param fieldNames The field values to give the modified TempDroppedStudentAttendancePeriodRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempDroppedStudentAttendancePeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDroppedStudentAttendancePeriodRecord <- function(TempDroppedStudentAttendancePeriodRecordID, AffectedPrimaryKey = NULL, AttendancePeriodCode = NULL, AttendanceTypeCode = NULL, CourseDescription = NULL, Date = NULL, ErrorCount = NULL, StudentName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodRecord", objectId = TempDroppedStudentAttendancePeriodRecordID, body = list(DataObject = body), searchFields = append("TempDroppedStudentAttendancePeriodRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCalendarDayFieldRecords
	#'
	#' This function returns a dataframe or json object of TempCalendarDayFieldRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarDayFieldRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarDayFieldRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarDayFieldRecord') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempCalendarDayFieldRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalendarDayFieldRecords <- function(searchConditionsList = NULL, CreatedTime = F, FieldName = F, ModifiedTime = F, TempCalendarDayFieldRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCalendarDayFieldRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCalendarDayFieldRecord
	#'
	#' This function returns a dataframe or json object of a TempCalendarDayFieldRecord
	#' @param TempCalendarDayFieldRecordID The ID of the TempCalendarDayFieldRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarDayFieldRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarDayFieldRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarDayFieldRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCalendarDayFieldRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalendarDayFieldRecord <- function(TempCalendarDayFieldRecordID, CreatedTime = F, FieldName = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalendarDayFieldRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCalendarDayFieldRecord", objectId = TempCalendarDayFieldRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCalendarDayFieldRecord
	#'
	#' This function deletes a TempCalendarDayFieldRecord
	#' @param TempCalendarDayFieldRecordID The ID of the TempCalendarDayFieldRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCalendarDayFieldRecordID of the deleted TempCalendarDayFieldRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCalendarDayFieldRecord <- function(TempCalendarDayFieldRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCalendarDayFieldRecord", objectId = TempCalendarDayFieldRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCalendarDayFieldRecord
	#'
	#' This function creates a TempCalendarDayFieldRecord
	#' @param fieldNames The field values to give the created TempCalendarDayFieldRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCalendarDayFieldRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCalendarDayFieldRecord <- function(FieldName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempCalendarDayFieldRecord", body = list(DataObject = body), searchFields = append("TempCalendarDayFieldRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCalendarDayFieldRecord
	#'
	#' This function modifies a TempCalendarDayFieldRecord
	#' @param fieldNames The field values to give the modified TempCalendarDayFieldRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCalendarDayFieldRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCalendarDayFieldRecord <- function(TempCalendarDayFieldRecordID, FieldName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempCalendarDayFieldRecord", objectId = TempCalendarDayFieldRecordID, body = list(DataObject = body), searchFields = append("TempCalendarDayFieldRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassUpdateCalendarDayEntryRecords
	#'
	#' This function returns a dataframe or json object of TempMassUpdateCalendarDayEntryRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassUpdateCalendarDayEntryRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassUpdateCalendarDayEntryRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassUpdateCalendarDayEntryRecord') to get more field paths.
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
	#' @concept Attendance
	#' @return A list of TempMassUpdateCalendarDayEntryRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassUpdateCalendarDayEntryRecords <- function(searchConditionsList = NULL, AddUpdateDate = F, Comment = F, CountAs = F, CreatedTime = F, Date = F, HasFailureReasons = F, ModifiedTime = F, ShowCommentOnCalendar = F, TempMassUpdateCalendarDayEntryRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempMassUpdateCalendarDayEntryRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassUpdateCalendarDayEntryRecord
	#'
	#' This function returns a dataframe or json object of a TempMassUpdateCalendarDayEntryRecord
	#' @param TempMassUpdateCalendarDayEntryRecordID The ID of the TempMassUpdateCalendarDayEntryRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassUpdateCalendarDayEntryRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassUpdateCalendarDayEntryRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassUpdateCalendarDayEntryRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempMassUpdateCalendarDayEntryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassUpdateCalendarDayEntryRecord <- function(TempMassUpdateCalendarDayEntryRecordID, AddUpdateDate = F, Comment = F, CountAs = F, CreatedTime = F, Date = F, HasFailureReasons = F, ModifiedTime = F, ShowCommentOnCalendar = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassUpdateCalendarDayEntryRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempMassUpdateCalendarDayEntryRecord", objectId = TempMassUpdateCalendarDayEntryRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassUpdateCalendarDayEntryRecord
	#'
	#' This function deletes a TempMassUpdateCalendarDayEntryRecord
	#' @param TempMassUpdateCalendarDayEntryRecordID The ID of the TempMassUpdateCalendarDayEntryRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempMassUpdateCalendarDayEntryRecordID of the deleted TempMassUpdateCalendarDayEntryRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassUpdateCalendarDayEntryRecord <- function(TempMassUpdateCalendarDayEntryRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempMassUpdateCalendarDayEntryRecord", objectId = TempMassUpdateCalendarDayEntryRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassUpdateCalendarDayEntryRecord
	#'
	#' This function creates a TempMassUpdateCalendarDayEntryRecord
	#' @param fieldNames The field values to give the created TempMassUpdateCalendarDayEntryRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempMassUpdateCalendarDayEntryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassUpdateCalendarDayEntryRecord <- function(AddUpdateDate = NULL, Comment = NULL, CountAs = NULL, Date = NULL, HasFailureReasons = NULL, ShowCommentOnCalendar = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Attendance", objectName = "TempMassUpdateCalendarDayEntryRecord", body = list(DataObject = body), searchFields = append("TempMassUpdateCalendarDayEntryRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassUpdateCalendarDayEntryRecord
	#'
	#' This function modifies a TempMassUpdateCalendarDayEntryRecord
	#' @param fieldNames The field values to give the modified TempMassUpdateCalendarDayEntryRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempMassUpdateCalendarDayEntryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassUpdateCalendarDayEntryRecord <- function(TempMassUpdateCalendarDayEntryRecordID, AddUpdateDate = NULL, Comment = NULL, CountAs = NULL, Date = NULL, HasFailureReasons = NULL, ShowCommentOnCalendar = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Attendance", objectName = "TempMassUpdateCalendarDayEntryRecord", objectId = TempMassUpdateCalendarDayEntryRecordID, body = list(DataObject = body), searchFields = append("TempMassUpdateCalendarDayEntryRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
