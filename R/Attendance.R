
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of CrossEntityAttendanceReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCrossEntityAttendanceReasons <- function(searchConditionsList = NULL, CrossEntityAttendanceReasonID = F, AttendanceReasonIDFrom = F, AttendanceReasonIDTo = F, EntityIDTo = F, SchoolYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CrossEntityAttendanceReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CrossEntityAttendanceReason
	#'
	#' This function returns a dataframe or json object of a CrossEntityAttendanceReason
	#' @param CrossEntityAttendanceReasonID The ID of the CrossEntityAttendanceReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CrossEntityAttendanceReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CrossEntityAttendanceReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CrossEntityAttendanceReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CrossEntityAttendanceReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCrossEntityAttendanceReason <- function(CrossEntityAttendanceReasonID, AttendanceReasonIDFrom = F, AttendanceReasonIDTo = F, EntityIDTo = F, SchoolYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CrossEntityAttendanceReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CrossEntityAttendanceReason", objectId = CrossEntityAttendanceReasonID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CrossEntityAttendanceReason
	#'
	#' This function deletes a CrossEntityAttendanceReason
	#' @param CrossEntityAttendanceReasonID The ID of the CrossEntityAttendanceReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CrossEntityAttendanceReasonID of the deleted CrossEntityAttendanceReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCrossEntityAttendanceReason <- function(CrossEntityAttendanceReasonID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CrossEntityAttendanceReason", objectId = CrossEntityAttendanceReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CrossEntityAttendanceReason
	#'
	#' This function creates a CrossEntityAttendanceReason
	#' @param fieldNames The field values to give the created CrossEntityAttendanceReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CrossEntityAttendanceReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCrossEntityAttendanceReason <- function(AttendanceReasonIDFrom = NULL, AttendanceReasonIDTo = NULL, EntityIDTo = NULL, SchoolYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "CrossEntityAttendanceReason", body = list(DataObject = body), searchFields = append("CrossEntityAttendanceReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CrossEntityAttendanceReason
	#'
	#' This function modifies a CrossEntityAttendanceReason
	#' @param fieldNames The field values to give the modified CrossEntityAttendanceReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CrossEntityAttendanceReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCrossEntityAttendanceReason <- function(CrossEntityAttendanceReasonID, AttendanceReasonIDFrom = NULL, AttendanceReasonIDTo = NULL, EntityIDTo = NULL, SchoolYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "CrossEntityAttendanceReason", objectId = CrossEntityAttendanceReasonID, body = list(DataObject = body), searchFields = append("CrossEntityAttendanceReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of CrossEntityAttendanceTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCrossEntityAttendanceTypes <- function(searchConditionsList = NULL, CrossEntityAttendanceTypeID = F, AttendanceTypeIDFrom = F, AttendanceTypeIDTo = F, EntityIDTo = F, SchoolYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CrossEntityAttendanceType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CrossEntityAttendanceType
	#'
	#' This function returns a dataframe or json object of a CrossEntityAttendanceType
	#' @param CrossEntityAttendanceTypeID The ID of the CrossEntityAttendanceType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CrossEntityAttendanceType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CrossEntityAttendanceType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CrossEntityAttendanceType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CrossEntityAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCrossEntityAttendanceType <- function(CrossEntityAttendanceTypeID, AttendanceTypeIDFrom = F, AttendanceTypeIDTo = F, EntityIDTo = F, SchoolYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CrossEntityAttendanceTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CrossEntityAttendanceType", objectId = CrossEntityAttendanceTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CrossEntityAttendanceType
	#'
	#' This function deletes a CrossEntityAttendanceType
	#' @param CrossEntityAttendanceTypeID The ID of the CrossEntityAttendanceType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CrossEntityAttendanceTypeID of the deleted CrossEntityAttendanceType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCrossEntityAttendanceType <- function(CrossEntityAttendanceTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CrossEntityAttendanceType", objectId = CrossEntityAttendanceTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CrossEntityAttendanceType
	#'
	#' This function creates a CrossEntityAttendanceType
	#' @param fieldNames The field values to give the created CrossEntityAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CrossEntityAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCrossEntityAttendanceType <- function(AttendanceTypeIDFrom = NULL, AttendanceTypeIDTo = NULL, EntityIDTo = NULL, SchoolYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "CrossEntityAttendanceType", body = list(DataObject = body), searchFields = append("CrossEntityAttendanceTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CrossEntityAttendanceType
	#'
	#' This function modifies a CrossEntityAttendanceType
	#' @param fieldNames The field values to give the modified CrossEntityAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CrossEntityAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCrossEntityAttendanceType <- function(CrossEntityAttendanceTypeID, AttendanceTypeIDFrom = NULL, AttendanceTypeIDTo = NULL, EntityIDTo = NULL, SchoolYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "CrossEntityAttendanceType", objectId = CrossEntityAttendanceTypeID, body = list(DataObject = body), searchFields = append("CrossEntityAttendanceTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of CrossEntityCalendarDisplayPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCrossEntityCalendarDisplayPeriods <- function(searchConditionsList = NULL, CrossEntityCalendarDisplayPeriodID = F, CalendarDisplayPeriodIDFrom = F, CalendarDisplayPeriodIDTo = F, IsAutoCreated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CrossEntityCalendarDisplayPeriodIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CrossEntityCalendarDisplayPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CrossEntityCalendarDisplayPeriod
	#'
	#' This function returns a dataframe or json object of a CrossEntityCalendarDisplayPeriod
	#' @param CrossEntityCalendarDisplayPeriodID The ID of the CrossEntityCalendarDisplayPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CrossEntityCalendarDisplayPeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CrossEntityCalendarDisplayPeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CrossEntityCalendarDisplayPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CrossEntityCalendarDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCrossEntityCalendarDisplayPeriod <- function(CrossEntityCalendarDisplayPeriodID, CalendarDisplayPeriodIDFrom = F, CalendarDisplayPeriodIDTo = F, IsAutoCreated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CrossEntityCalendarDisplayPeriodIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CrossEntityCalendarDisplayPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CrossEntityCalendarDisplayPeriod", objectId = CrossEntityCalendarDisplayPeriodID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CrossEntityCalendarDisplayPeriod
	#'
	#' This function deletes a CrossEntityCalendarDisplayPeriod
	#' @param CrossEntityCalendarDisplayPeriodID The ID of the CrossEntityCalendarDisplayPeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CrossEntityCalendarDisplayPeriodID of the deleted CrossEntityCalendarDisplayPeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCrossEntityCalendarDisplayPeriod <- function(CrossEntityCalendarDisplayPeriodID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CrossEntityCalendarDisplayPeriod", objectId = CrossEntityCalendarDisplayPeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CrossEntityCalendarDisplayPeriod
	#'
	#' This function creates a CrossEntityCalendarDisplayPeriod
	#' @param fieldNames The field values to give the created CrossEntityCalendarDisplayPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CrossEntityCalendarDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCrossEntityCalendarDisplayPeriod <- function(CalendarDisplayPeriodIDFrom = NULL, CalendarDisplayPeriodIDTo = NULL, IsAutoCreated = NULL, CrossEntityCalendarDisplayPeriodIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "CrossEntityCalendarDisplayPeriod", body = list(DataObject = body), searchFields = append("CrossEntityCalendarDisplayPeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CrossEntityCalendarDisplayPeriod
	#'
	#' This function modifies a CrossEntityCalendarDisplayPeriod
	#' @param fieldNames The field values to give the modified CrossEntityCalendarDisplayPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CrossEntityCalendarDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCrossEntityCalendarDisplayPeriod <- function(CrossEntityCalendarDisplayPeriodID, CalendarDisplayPeriodIDFrom = NULL, CalendarDisplayPeriodIDTo = NULL, IsAutoCreated = NULL, CrossEntityCalendarDisplayPeriodIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "CrossEntityCalendarDisplayPeriod", objectId = CrossEntityCalendarDisplayPeriodID, body = list(DataObject = body), searchFields = append("CrossEntityCalendarDisplayPeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendanceCalendars
	#'
	#' This function returns a dataframe or json object of AttendanceCalendars
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceCalendars. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceCalendars.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceCalendar') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of AttendanceCalendars
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceCalendars <- function(searchConditionsList = NULL, CalendarMNID = F, MCCCAcademicYearImportID = F, MCCCCalendarImportID = F, CalendarID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, IsDefault = F, DefaultDayLengthMinutes = F, CodeDescription = F, AttendanceCalculationMethod = F, StartDate = F, EndDate = F, HalfDayHighPeriodCount = F, ZeroDayHighPeriodCount = F, CalendarIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalendarIDClonedTo = F, EdFiCalendarTypeDescriptorID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "Calendar", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceCalendar
	#'
	#' This function returns a dataframe or json object of an AttendanceCalendar
	#' @param AttendanceCalendarID The ID of the AttendanceCalendar to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceCalendar. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceCalendar.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceCalendar') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceCalendar <- function(AttendanceCalendarID, CalendarMNID = F, MCCCAcademicYearImportID = F, MCCCCalendarImportID = F, CalendarID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, IsDefault = F, DefaultDayLengthMinutes = F, CodeDescription = F, AttendanceCalculationMethod = F, StartDate = F, EndDate = F, HalfDayHighPeriodCount = F, ZeroDayHighPeriodCount = F, CalendarIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalendarIDClonedTo = F, EdFiCalendarTypeDescriptorID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceCalendarID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "Calendar", objectId = AttendanceCalendarID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceCalendar
	#'
	#' This function deletes an AttendanceCalendar
	#' @param AttendanceCalendarID The ID of the AttendanceCalendar to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceCalendarID of the deleted AttendanceCalendar.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceCalendar <- function(AttendanceCalendarID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "Calendar", objectId = AttendanceCalendarID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceCalendar
	#'
	#' This function creates an AttendanceCalendar
	#' @param fieldNames The field values to give the created AttendanceCalendar. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceCalendar <- function(MCCCAcademicYearImportID = NULL, MCCCCalendarImportID = NULL, EntityID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, IsDefault = NULL, DefaultDayLengthMinutes = NULL, AttendanceCalculationMethod = NULL, StartDate = NULL, EndDate = NULL, HalfDayHighPeriodCount = NULL, ZeroDayHighPeriodCount = NULL, CalendarIDClonedFrom = NULL, EdFiCalendarTypeDescriptorID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "Calendar", body = list(DataObject = body), searchFields = append("CalendarID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceCalendar
	#'
	#' This function modifies an AttendanceCalendar
	#' @param fieldNames The field values to give the modified AttendanceCalendar. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceCalendar <- function(CalendarID, MCCCAcademicYearImportID = NULL, MCCCCalendarImportID = NULL, EntityID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, IsDefault = NULL, DefaultDayLengthMinutes = NULL, AttendanceCalculationMethod = NULL, StartDate = NULL, EndDate = NULL, HalfDayHighPeriodCount = NULL, ZeroDayHighPeriodCount = NULL, CalendarIDClonedFrom = NULL, EdFiCalendarTypeDescriptorID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "Calendar", objectId = CalendarID, body = list(DataObject = body), searchFields = append("CalendarID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendanceCalendarEvents
	#'
	#' This function returns a dataframe or json object of AttendanceCalendarEvents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceCalendarEvents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceCalendarEvents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceCalendarEvent') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of AttendanceCalendarEvents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceCalendarEvents <- function(searchConditionsList = NULL, CalendarEventID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, EdFiCalendarEventID = F, CalendarEventIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiCalendarEventDescriptorID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarEvent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceCalendarEvent
	#'
	#' This function returns a dataframe or json object of an AttendanceCalendarEvent
	#' @param AttendanceCalendarEventID The ID of the AttendanceCalendarEvent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceCalendarEvent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceCalendarEvent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceCalendarEvent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceCalendarEvent <- function(AttendanceCalendarEventID, CalendarEventID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, EdFiCalendarEventID = F, CalendarEventIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiCalendarEventDescriptorID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceCalendarEventID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarEvent", objectId = AttendanceCalendarEventID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceCalendarEvent
	#'
	#' This function deletes an AttendanceCalendarEvent
	#' @param AttendanceCalendarEventID The ID of the AttendanceCalendarEvent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceCalendarEventID of the deleted AttendanceCalendarEvent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceCalendarEvent <- function(AttendanceCalendarEventID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarEvent", objectId = AttendanceCalendarEventID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceCalendarEvent
	#'
	#' This function creates an AttendanceCalendarEvent
	#' @param fieldNames The field values to give the created AttendanceCalendarEvent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceCalendarEvent <- function(EntityID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, EdFiCalendarEventID = NULL, CalendarEventIDClonedFrom = NULL, EdFiCalendarEventDescriptorID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "CalendarEvent", body = list(DataObject = body), searchFields = append("CalendarEventID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceCalendarEvent
	#'
	#' This function modifies an AttendanceCalendarEvent
	#' @param fieldNames The field values to give the modified AttendanceCalendarEvent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceCalendarEvent <- function(CalendarEventID, EntityID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, EdFiCalendarEventID = NULL, CalendarEventIDClonedFrom = NULL, EdFiCalendarEventDescriptorID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarEvent", objectId = CalendarEventID, body = list(DataObject = body), searchFields = append("CalendarEventID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of CalendarDayCalendarEvents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarDayCalendarEvents <- function(searchConditionsList = NULL, CalendarDayCalendarEventID = F, CalendarEventID = F, CalendarDayID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarDayCalendarEvent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarDayCalendarEvent
	#'
	#' This function returns a dataframe or json object of a CalendarDayCalendarEvent
	#' @param CalendarDayCalendarEventID The ID of the CalendarDayCalendarEvent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDayCalendarEvent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDayCalendarEvent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDayCalendarEvent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CalendarDayCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarDayCalendarEvent <- function(CalendarDayCalendarEventID, CalendarEventID = F, CalendarDayID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarDayCalendarEventID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarDayCalendarEvent", objectId = CalendarDayCalendarEventID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarDayCalendarEvent
	#'
	#' This function deletes a CalendarDayCalendarEvent
	#' @param CalendarDayCalendarEventID The ID of the CalendarDayCalendarEvent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarDayCalendarEventID of the deleted CalendarDayCalendarEvent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarDayCalendarEvent <- function(CalendarDayCalendarEventID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarDayCalendarEvent", objectId = CalendarDayCalendarEventID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarDayCalendarEvent
	#'
	#' This function creates a CalendarDayCalendarEvent
	#' @param fieldNames The field values to give the created CalendarDayCalendarEvent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CalendarDayCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarDayCalendarEvent <- function(CalendarEventID = NULL, CalendarDayID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "CalendarDayCalendarEvent", body = list(DataObject = body), searchFields = append("CalendarDayCalendarEventID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarDayCalendarEvent
	#'
	#' This function modifies a CalendarDayCalendarEvent
	#' @param fieldNames The field values to give the modified CalendarDayCalendarEvent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CalendarDayCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarDayCalendarEvent <- function(CalendarDayCalendarEventID, CalendarEventID = NULL, CalendarDayID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarDayCalendarEvent", objectId = CalendarDayCalendarEventID, body = list(DataObject = body), searchFields = append("CalendarDayCalendarEventID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of DisciplineThresholds
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineThresholds <- function(searchConditionsList = NULL, DisciplineThresholdID = F, ThresholdResetRangeID = F, ActionID = F, OffenseID = F, StaffIDAuthorizedBy = F, ThresholdRangeLow = F, ThresholdRangeHigh = F, IncidentDescription = F, IncidentDefaultComment = F, ServingTime = F, DurationToServe = F, DurationToServePerDay = F, CreateDisciplineRecord = F, GenerateActionDetail = F, AllowDisciplineOnCurrentDay = F, ServeOnMonday = F, ServeOnTuesday = F, ServeOnWednesday = F, ServeOnThursday = F, ServeOnFriday = F, ServeOnSaturday = F, ServeOnSunday = F, LocationIDServing = F, RoomIDServing = F, BuildingIDServing = F, AttendanceSlipComment = F, DisciplineSlipComment = F, RangeDisplay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsRepeatable = F, FooterComment = F, AttendanceLettersRan = F, Greeting = F, AttachmentDisplayNameOverride = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "DisciplineThreshold", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineThreshold
	#'
	#' This function returns a dataframe or json object of a DisciplineThreshold
	#' @param DisciplineThresholdID The ID of the DisciplineThreshold to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineThreshold. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineThreshold.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineThreshold') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of DisciplineThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineThreshold <- function(DisciplineThresholdID, ThresholdResetRangeID = F, ActionID = F, OffenseID = F, StaffIDAuthorizedBy = F, ThresholdRangeLow = F, ThresholdRangeHigh = F, IncidentDescription = F, IncidentDefaultComment = F, ServingTime = F, DurationToServe = F, DurationToServePerDay = F, CreateDisciplineRecord = F, GenerateActionDetail = F, AllowDisciplineOnCurrentDay = F, ServeOnMonday = F, ServeOnTuesday = F, ServeOnWednesday = F, ServeOnThursday = F, ServeOnFriday = F, ServeOnSaturday = F, ServeOnSunday = F, LocationIDServing = F, RoomIDServing = F, BuildingIDServing = F, AttendanceSlipComment = F, DisciplineSlipComment = F, RangeDisplay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsRepeatable = F, FooterComment = F, AttendanceLettersRan = F, Greeting = F, AttachmentDisplayNameOverride = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineThresholdID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "DisciplineThreshold", objectId = DisciplineThresholdID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineThreshold
	#'
	#' This function deletes a DisciplineThreshold
	#' @param DisciplineThresholdID The ID of the DisciplineThreshold to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The DisciplineThresholdID of the deleted DisciplineThreshold.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineThreshold <- function(DisciplineThresholdID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "DisciplineThreshold", objectId = DisciplineThresholdID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineThreshold
	#'
	#' This function creates a DisciplineThreshold
	#' @param fieldNames The field values to give the created DisciplineThreshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created DisciplineThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineThreshold <- function(ThresholdResetRangeID = NULL, ActionID = NULL, OffenseID = NULL, StaffIDAuthorizedBy = NULL, ThresholdRangeLow = NULL, ThresholdRangeHigh = NULL, IncidentDescription = NULL, IncidentDefaultComment = NULL, ServingTime = NULL, DurationToServe = NULL, DurationToServePerDay = NULL, CreateDisciplineRecord = NULL, GenerateActionDetail = NULL, AllowDisciplineOnCurrentDay = NULL, ServeOnMonday = NULL, ServeOnTuesday = NULL, ServeOnWednesday = NULL, ServeOnThursday = NULL, ServeOnFriday = NULL, ServeOnSaturday = NULL, ServeOnSunday = NULL, LocationIDServing = NULL, RoomIDServing = NULL, BuildingIDServing = NULL, AttendanceSlipComment = NULL, DisciplineSlipComment = NULL, IsRepeatable = NULL, FooterComment = NULL, Greeting = NULL, AttachmentDisplayNameOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "DisciplineThreshold", body = list(DataObject = body), searchFields = append("DisciplineThresholdID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineThreshold
	#'
	#' This function modifies a DisciplineThreshold
	#' @param fieldNames The field values to give the modified DisciplineThreshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified DisciplineThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineThreshold <- function(DisciplineThresholdID, ThresholdResetRangeID = NULL, ActionID = NULL, OffenseID = NULL, StaffIDAuthorizedBy = NULL, ThresholdRangeLow = NULL, ThresholdRangeHigh = NULL, IncidentDescription = NULL, IncidentDefaultComment = NULL, ServingTime = NULL, DurationToServe = NULL, DurationToServePerDay = NULL, CreateDisciplineRecord = NULL, GenerateActionDetail = NULL, AllowDisciplineOnCurrentDay = NULL, ServeOnMonday = NULL, ServeOnTuesday = NULL, ServeOnWednesday = NULL, ServeOnThursday = NULL, ServeOnFriday = NULL, ServeOnSaturday = NULL, ServeOnSunday = NULL, LocationIDServing = NULL, RoomIDServing = NULL, BuildingIDServing = NULL, AttendanceSlipComment = NULL, DisciplineSlipComment = NULL, IsRepeatable = NULL, FooterComment = NULL, Greeting = NULL, AttachmentDisplayNameOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "DisciplineThreshold", objectId = DisciplineThresholdID, body = list(DataObject = body), searchFields = append("DisciplineThresholdID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of DroppedStudentAttendancePeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDroppedStudentAttendancePeriods <- function(searchConditionsList = NULL, DroppedStudentAttendancePeriodID = F, AttendanceTypeID = F, AttendanceReasonID = F, AttendancePeriodID = F, Comment = F, IncidentOffenseNameActionDetailID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentID = F, CalendarDayID = F, IsGuardianNotified = F, CourseID = F, SectionID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "DroppedStudentAttendancePeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DroppedStudentAttendancePeriod
	#'
	#' This function returns a dataframe or json object of a DroppedStudentAttendancePeriod
	#' @param DroppedStudentAttendancePeriodID The ID of the DroppedStudentAttendancePeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DroppedStudentAttendancePeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DroppedStudentAttendancePeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DroppedStudentAttendancePeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of DroppedStudentAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDroppedStudentAttendancePeriod <- function(DroppedStudentAttendancePeriodID, AttendanceTypeID = F, AttendanceReasonID = F, AttendancePeriodID = F, Comment = F, IncidentOffenseNameActionDetailID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentID = F, CalendarDayID = F, IsGuardianNotified = F, CourseID = F, SectionID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DroppedStudentAttendancePeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "DroppedStudentAttendancePeriod", objectId = DroppedStudentAttendancePeriodID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DroppedStudentAttendancePeriod
	#'
	#' This function deletes a DroppedStudentAttendancePeriod
	#' @param DroppedStudentAttendancePeriodID The ID of the DroppedStudentAttendancePeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The DroppedStudentAttendancePeriodID of the deleted DroppedStudentAttendancePeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDroppedStudentAttendancePeriod <- function(DroppedStudentAttendancePeriodID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "DroppedStudentAttendancePeriod", objectId = DroppedStudentAttendancePeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DroppedStudentAttendancePeriod
	#'
	#' This function creates a DroppedStudentAttendancePeriod
	#' @param fieldNames The field values to give the created DroppedStudentAttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created DroppedStudentAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDroppedStudentAttendancePeriod <- function(AttendanceTypeID = NULL, AttendanceReasonID = NULL, AttendancePeriodID = NULL, Comment = NULL, IncidentOffenseNameActionDetailID = NULL, StudentID = NULL, CalendarDayID = NULL, IsGuardianNotified = NULL, CourseID = NULL, SectionID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "DroppedStudentAttendancePeriod", body = list(DataObject = body), searchFields = append("DroppedStudentAttendancePeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DroppedStudentAttendancePeriod
	#'
	#' This function modifies a DroppedStudentAttendancePeriod
	#' @param fieldNames The field values to give the modified DroppedStudentAttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified DroppedStudentAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDroppedStudentAttendancePeriod <- function(DroppedStudentAttendancePeriodID, AttendanceTypeID = NULL, AttendanceReasonID = NULL, AttendancePeriodID = NULL, Comment = NULL, IncidentOffenseNameActionDetailID = NULL, StudentID = NULL, CalendarDayID = NULL, IsGuardianNotified = NULL, CourseID = NULL, SectionID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "DroppedStudentAttendancePeriod", objectId = DroppedStudentAttendancePeriodID, body = list(DataObject = body), searchFields = append("DroppedStudentAttendancePeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of RoomLayouts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoomLayouts <- function(searchConditionsList = NULL, RoomLayoutID = F, RoomID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "RoomLayout", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RoomLayout
	#'
	#' This function returns a dataframe or json object of a RoomLayout
	#' @param RoomLayoutID The ID of the RoomLayout to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoomLayout. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoomLayout.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoomLayout') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of RoomLayout
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoomLayout <- function(RoomLayoutID, RoomID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoomLayoutID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "RoomLayout", objectId = RoomLayoutID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RoomLayout
	#'
	#' This function deletes a RoomLayout
	#' @param RoomLayoutID The ID of the RoomLayout to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The RoomLayoutID of the deleted RoomLayout.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoomLayout <- function(RoomLayoutID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "RoomLayout", objectId = RoomLayoutID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RoomLayout
	#'
	#' This function creates a RoomLayout
	#' @param fieldNames The field values to give the created RoomLayout. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created RoomLayout
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoomLayout <- function(RoomID = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "RoomLayout", body = list(DataObject = body), searchFields = append("RoomLayoutID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RoomLayout
	#'
	#' This function modifies a RoomLayout
	#' @param fieldNames The field values to give the modified RoomLayout. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified RoomLayout
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoomLayout <- function(RoomLayoutID, RoomID = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "RoomLayout", objectId = RoomLayoutID, body = list(DataObject = body), searchFields = append("RoomLayoutID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of RoomLayoutObjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoomLayoutObjects <- function(searchConditionsList = NULL, RoomLayoutObjectID = F, RoomLayoutID = F, RoomObjectID = F, XLocation = F, YLocation = F, Rotation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "RoomLayoutObject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RoomLayoutObject
	#'
	#' This function returns a dataframe or json object of a RoomLayoutObject
	#' @param RoomLayoutObjectID The ID of the RoomLayoutObject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoomLayoutObject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoomLayoutObject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoomLayoutObject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of RoomLayoutObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoomLayoutObject <- function(RoomLayoutObjectID, RoomLayoutID = F, RoomObjectID = F, XLocation = F, YLocation = F, Rotation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoomLayoutObjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "RoomLayoutObject", objectId = RoomLayoutObjectID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RoomLayoutObject
	#'
	#' This function deletes a RoomLayoutObject
	#' @param RoomLayoutObjectID The ID of the RoomLayoutObject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The RoomLayoutObjectID of the deleted RoomLayoutObject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoomLayoutObject <- function(RoomLayoutObjectID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "RoomLayoutObject", objectId = RoomLayoutObjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RoomLayoutObject
	#'
	#' This function creates a RoomLayoutObject
	#' @param fieldNames The field values to give the created RoomLayoutObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created RoomLayoutObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoomLayoutObject <- function(RoomLayoutID = NULL, RoomObjectID = NULL, XLocation = NULL, YLocation = NULL, Rotation = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "RoomLayoutObject", body = list(DataObject = body), searchFields = append("RoomLayoutObjectID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RoomLayoutObject
	#'
	#' This function modifies a RoomLayoutObject
	#' @param fieldNames The field values to give the modified RoomLayoutObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified RoomLayoutObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoomLayoutObject <- function(RoomLayoutObjectID, RoomLayoutID = NULL, RoomObjectID = NULL, XLocation = NULL, YLocation = NULL, Rotation = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "RoomLayoutObject", objectId = RoomLayoutObjectID, body = list(DataObject = body), searchFields = append("RoomLayoutObjectID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of RoomObjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRoomObjects <- function(searchConditionsList = NULL, RoomObjectID = F, Label = F, Parameters = F, SkywardID = F, IsStudentSeat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "RoomObject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RoomObject
	#'
	#' This function returns a dataframe or json object of a RoomObject
	#' @param RoomObjectID The ID of the RoomObject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RoomObject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RoomObject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RoomObject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of RoomObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRoomObject <- function(RoomObjectID, Label = F, Parameters = F, SkywardID = F, IsStudentSeat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RoomObjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "RoomObject", objectId = RoomObjectID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RoomObject
	#'
	#' This function deletes a RoomObject
	#' @param RoomObjectID The ID of the RoomObject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The RoomObjectID of the deleted RoomObject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRoomObject <- function(RoomObjectID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "RoomObject", objectId = RoomObjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RoomObject
	#'
	#' This function creates a RoomObject
	#' @param fieldNames The field values to give the created RoomObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created RoomObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRoomObject <- function(Label = NULL, Parameters = NULL, IsStudentSeat = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "RoomObject", body = list(DataObject = body), searchFields = append("RoomObjectID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RoomObject
	#'
	#' This function modifies a RoomObject
	#' @param fieldNames The field values to give the modified RoomObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified RoomObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRoomObject <- function(RoomObjectID, Label = NULL, Parameters = NULL, IsStudentSeat = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "RoomObject", objectId = RoomObjectID, body = list(DataObject = body), searchFields = append("RoomObjectID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of SeatingCharts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSeatingCharts <- function(searchConditionsList = NULL, SeatingChartID = F, RoomLayoutID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SeatingChartType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "SeatingChart", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SeatingChart
	#'
	#' This function returns a dataframe or json object of a SeatingChart
	#' @param SeatingChartID The ID of the SeatingChart to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SeatingChart. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SeatingChart.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SeatingChart') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of SeatingChart
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSeatingChart <- function(SeatingChartID, RoomLayoutID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SeatingChartType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SeatingChartID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "SeatingChart", objectId = SeatingChartID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SeatingChart
	#'
	#' This function deletes a SeatingChart
	#' @param SeatingChartID The ID of the SeatingChart to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The SeatingChartID of the deleted SeatingChart.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSeatingChart <- function(SeatingChartID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "SeatingChart", objectId = SeatingChartID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SeatingChart
	#'
	#' This function creates a SeatingChart
	#' @param fieldNames The field values to give the created SeatingChart. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created SeatingChart
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSeatingChart <- function(RoomLayoutID = NULL, Description = NULL, SeatingChartType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "SeatingChart", body = list(DataObject = body), searchFields = append("SeatingChartID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SeatingChart
	#'
	#' This function modifies a SeatingChart
	#' @param fieldNames The field values to give the modified SeatingChart. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified SeatingChart
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySeatingChart <- function(SeatingChartID, RoomLayoutID = NULL, Description = NULL, SeatingChartType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "SeatingChart", objectId = SeatingChartID, body = list(DataObject = body), searchFields = append("SeatingChartID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of SeatingChartMeets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSeatingChartMeets <- function(searchConditionsList = NULL, SeatingChartMeetID = F, SeatingChartID = F, MeetID = F, IsCurrent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionList = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "SeatingChartMeet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SeatingChartMeet
	#'
	#' This function returns a dataframe or json object of a SeatingChartMeet
	#' @param SeatingChartMeetID The ID of the SeatingChartMeet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SeatingChartMeet. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SeatingChartMeet.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SeatingChartMeet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of SeatingChartMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSeatingChartMeet <- function(SeatingChartMeetID, SeatingChartID = F, MeetID = F, IsCurrent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionList = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SeatingChartMeetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "SeatingChartMeet", objectId = SeatingChartMeetID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SeatingChartMeet
	#'
	#' This function deletes a SeatingChartMeet
	#' @param SeatingChartMeetID The ID of the SeatingChartMeet to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The SeatingChartMeetID of the deleted SeatingChartMeet.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSeatingChartMeet <- function(SeatingChartMeetID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "SeatingChartMeet", objectId = SeatingChartMeetID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SeatingChartMeet
	#'
	#' This function creates a SeatingChartMeet
	#' @param fieldNames The field values to give the created SeatingChartMeet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created SeatingChartMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSeatingChartMeet <- function(SeatingChartID = NULL, MeetID = NULL, IsCurrent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "SeatingChartMeet", body = list(DataObject = body), searchFields = append("SeatingChartMeetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SeatingChartMeet
	#'
	#' This function modifies a SeatingChartMeet
	#' @param fieldNames The field values to give the modified SeatingChartMeet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified SeatingChartMeet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySeatingChartMeet <- function(SeatingChartMeetID, SeatingChartID = NULL, MeetID = NULL, IsCurrent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "SeatingChartMeet", objectId = SeatingChartMeetID, body = list(DataObject = body), searchFields = append("SeatingChartMeetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of SeatingChartSeats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSeatingChartSeats <- function(searchConditionsList = NULL, SeatingChartSeatID = F, SeatingChartID = F, RoomLayoutObjectID = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "SeatingChartSeat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SeatingChartSeat
	#'
	#' This function returns a dataframe or json object of a SeatingChartSeat
	#' @param SeatingChartSeatID The ID of the SeatingChartSeat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SeatingChartSeat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SeatingChartSeat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SeatingChartSeat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of SeatingChartSeat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSeatingChartSeat <- function(SeatingChartSeatID, SeatingChartID = F, RoomLayoutObjectID = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SeatingChartSeatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "SeatingChartSeat", objectId = SeatingChartSeatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SeatingChartSeat
	#'
	#' This function deletes a SeatingChartSeat
	#' @param SeatingChartSeatID The ID of the SeatingChartSeat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The SeatingChartSeatID of the deleted SeatingChartSeat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSeatingChartSeat <- function(SeatingChartSeatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "SeatingChartSeat", objectId = SeatingChartSeatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SeatingChartSeat
	#'
	#' This function creates a SeatingChartSeat
	#' @param fieldNames The field values to give the created SeatingChartSeat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created SeatingChartSeat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSeatingChartSeat <- function(SeatingChartID = NULL, RoomLayoutObjectID = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "SeatingChartSeat", body = list(DataObject = body), searchFields = append("SeatingChartSeatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SeatingChartSeat
	#'
	#' This function modifies a SeatingChartSeat
	#' @param fieldNames The field values to give the modified SeatingChartSeat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified SeatingChartSeat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySeatingChartSeat <- function(SeatingChartSeatID, SeatingChartID = NULL, RoomLayoutObjectID = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "SeatingChartSeat", objectId = SeatingChartSeatID, body = list(DataObject = body), searchFields = append("SeatingChartSeatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of ThresholdResetRangeAttendanceTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThresholdResetRangeAttendanceTypes <- function(searchConditionsList = NULL, ThresholdResetRangeAttendanceTypeID = F, AttendanceTypeID = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "ThresholdResetRangeAttendanceType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThresholdResetRangeAttendanceType
	#'
	#' This function returns a dataframe or json object of a ThresholdResetRangeAttendanceType
	#' @param ThresholdResetRangeAttendanceTypeID The ID of the ThresholdResetRangeAttendanceType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdResetRangeAttendanceType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdResetRangeAttendanceType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdResetRangeAttendanceType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of ThresholdResetRangeAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThresholdResetRangeAttendanceType <- function(ThresholdResetRangeAttendanceTypeID, AttendanceTypeID = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThresholdResetRangeAttendanceTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendanceType", objectId = ThresholdResetRangeAttendanceTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThresholdResetRangeAttendanceType
	#'
	#' This function deletes a ThresholdResetRangeAttendanceType
	#' @param ThresholdResetRangeAttendanceTypeID The ID of the ThresholdResetRangeAttendanceType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The ThresholdResetRangeAttendanceTypeID of the deleted ThresholdResetRangeAttendanceType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThresholdResetRangeAttendanceType <- function(ThresholdResetRangeAttendanceTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendanceType", objectId = ThresholdResetRangeAttendanceTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThresholdResetRangeAttendanceType
	#'
	#' This function creates a ThresholdResetRangeAttendanceType
	#' @param fieldNames The field values to give the created ThresholdResetRangeAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created ThresholdResetRangeAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThresholdResetRangeAttendanceType <- function(AttendanceTypeID = NULL, ThresholdResetRangeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendanceType", body = list(DataObject = body), searchFields = append("ThresholdResetRangeAttendanceTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThresholdResetRangeAttendanceType
	#'
	#' This function modifies a ThresholdResetRangeAttendanceType
	#' @param fieldNames The field values to give the modified ThresholdResetRangeAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified ThresholdResetRangeAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThresholdResetRangeAttendanceType <- function(ThresholdResetRangeAttendanceTypeID, AttendanceTypeID = NULL, ThresholdResetRangeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendanceType", objectId = ThresholdResetRangeAttendanceTypeID, body = list(DataObject = body), searchFields = append("ThresholdResetRangeAttendanceTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of ThresholdResetRanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThresholdResetRanges <- function(searchConditionsList = NULL, ThresholdResetRangeID = F, EntityID = F, SchoolYearID = F, DateLow = F, DateHigh = F, ResetRangeAttendanceTypes = F, DateDisplay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Type = F, CountType = F, NumberPerDay = F, IsForAttendanceLetters = F, AttendanceLettersRan = F, DateType = F, DayCountType = F, NumberOfDays = F, AttendanceTypeCodes = F, IsForTardyKiosk = F, Description = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "ThresholdResetRange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThresholdResetRange
	#'
	#' This function returns a dataframe or json object of a ThresholdResetRange
	#' @param ThresholdResetRangeID The ID of the ThresholdResetRange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdResetRange. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdResetRange.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdResetRange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of ThresholdResetRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThresholdResetRange <- function(ThresholdResetRangeID, EntityID = F, SchoolYearID = F, DateLow = F, DateHigh = F, ResetRangeAttendanceTypes = F, DateDisplay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Type = F, CountType = F, NumberPerDay = F, IsForAttendanceLetters = F, AttendanceLettersRan = F, DateType = F, DayCountType = F, NumberOfDays = F, AttendanceTypeCodes = F, IsForTardyKiosk = F, Description = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThresholdResetRangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "ThresholdResetRange", objectId = ThresholdResetRangeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThresholdResetRange
	#'
	#' This function deletes a ThresholdResetRange
	#' @param ThresholdResetRangeID The ID of the ThresholdResetRange to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The ThresholdResetRangeID of the deleted ThresholdResetRange.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThresholdResetRange <- function(ThresholdResetRangeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "ThresholdResetRange", objectId = ThresholdResetRangeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThresholdResetRange
	#'
	#' This function creates a ThresholdResetRange
	#' @param fieldNames The field values to give the created ThresholdResetRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created ThresholdResetRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThresholdResetRange <- function(EntityID = NULL, SchoolYearID = NULL, DateLow = NULL, DateHigh = NULL, Type = NULL, CountType = NULL, NumberPerDay = NULL, DateType = NULL, DayCountType = NULL, NumberOfDays = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "ThresholdResetRange", body = list(DataObject = body), searchFields = append("ThresholdResetRangeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThresholdResetRange
	#'
	#' This function modifies a ThresholdResetRange
	#' @param fieldNames The field values to give the modified ThresholdResetRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified ThresholdResetRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThresholdResetRange <- function(ThresholdResetRangeID, EntityID = NULL, SchoolYearID = NULL, DateLow = NULL, DateHigh = NULL, Type = NULL, CountType = NULL, NumberPerDay = NULL, DateType = NULL, DayCountType = NULL, NumberOfDays = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "ThresholdResetRange", objectId = ThresholdResetRangeID, body = list(DataObject = body), searchFields = append("ThresholdResetRangeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendanceTempCalendars
	#'
	#' This function returns a dataframe or json object of AttendanceTempCalendars
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceTempCalendars. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceTempCalendars.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceTempCalendar') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of AttendanceTempCalendars
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceTempCalendars <- function(searchConditionsList = NULL, TempCalendarID = F, AffectedPrimaryKey = F, CodeDescription = F, OldStartDate = F, OldEndDate = F, NewStartDate = F, NewEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalendarID = F, Code = F, IsDefault = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, IsUpdated = F, ProcessAction = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCalendar", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceTempCalendar
	#'
	#' This function returns a dataframe or json object of an AttendanceTempCalendar
	#' @param AttendanceTempCalendarID The ID of the AttendanceTempCalendar to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceTempCalendar. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceTempCalendar.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceTempCalendar') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceTempCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceTempCalendar <- function(AttendanceTempCalendarID, TempCalendarID = F, AffectedPrimaryKey = F, CodeDescription = F, OldStartDate = F, OldEndDate = F, NewStartDate = F, NewEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalendarID = F, Code = F, IsDefault = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, IsUpdated = F, ProcessAction = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceTempCalendarID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCalendar", objectId = AttendanceTempCalendarID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceTempCalendar
	#'
	#' This function deletes an AttendanceTempCalendar
	#' @param AttendanceTempCalendarID The ID of the AttendanceTempCalendar to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceTempCalendarID of the deleted AttendanceTempCalendar.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceTempCalendar <- function(AttendanceTempCalendarID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCalendar", objectId = AttendanceTempCalendarID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceTempCalendar
	#'
	#' This function creates an AttendanceTempCalendar
	#' @param fieldNames The field values to give the created AttendanceTempCalendar. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceTempCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceTempCalendar <- function(AffectedPrimaryKey = NULL, CodeDescription = NULL, OldStartDate = NULL, OldEndDate = NULL, NewStartDate = NULL, NewEndDate = NULL, CalendarID = NULL, Code = NULL, IsDefault = NULL, StartDate = NULL, EndDate = NULL, OriginalStartDate = NULL, OriginalEndDate = NULL, ProcessAction = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempCalendar", body = list(DataObject = body), searchFields = append("TempCalendarID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceTempCalendar
	#'
	#' This function modifies an AttendanceTempCalendar
	#' @param fieldNames The field values to give the modified AttendanceTempCalendar. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceTempCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceTempCalendar <- function(TempCalendarID, AffectedPrimaryKey = NULL, CodeDescription = NULL, OldStartDate = NULL, OldEndDate = NULL, NewStartDate = NULL, NewEndDate = NULL, CalendarID = NULL, Code = NULL, IsDefault = NULL, StartDate = NULL, EndDate = NULL, OriginalStartDate = NULL, OriginalEndDate = NULL, ProcessAction = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempCalendar", objectId = TempCalendarID, body = list(DataObject = body), searchFields = append("TempCalendarID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempStudentAttendanceRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentAttendanceRecords <- function(searchConditionsList = NULL, TempStudentAttendanceRecordID = F, AffectedPrimaryKey = F, StudentName = F, StudentNumber = F, Date = F, DayOfTheWeek = F, DayRotationID = F, DayRotation = F, GuardianNotified = F, AttendanceTakenByPeriod = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempStudentAttendanceRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentAttendanceRecord
	#'
	#' This function returns a dataframe or json object of a TempStudentAttendanceRecord
	#' @param TempStudentAttendanceRecordID The ID of the TempStudentAttendanceRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentAttendanceRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentAttendanceRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentAttendanceRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempStudentAttendanceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentAttendanceRecord <- function(TempStudentAttendanceRecordID, AffectedPrimaryKey = F, StudentName = F, StudentNumber = F, Date = F, DayOfTheWeek = F, DayRotationID = F, DayRotation = F, GuardianNotified = F, AttendanceTakenByPeriod = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentAttendanceRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempStudentAttendanceRecord", objectId = TempStudentAttendanceRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentAttendanceRecord
	#'
	#' This function deletes a TempStudentAttendanceRecord
	#' @param TempStudentAttendanceRecordID The ID of the TempStudentAttendanceRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempStudentAttendanceRecordID of the deleted TempStudentAttendanceRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentAttendanceRecord <- function(TempStudentAttendanceRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempStudentAttendanceRecord", objectId = TempStudentAttendanceRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentAttendanceRecord
	#'
	#' This function creates a TempStudentAttendanceRecord
	#' @param fieldNames The field values to give the created TempStudentAttendanceRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempStudentAttendanceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentAttendanceRecord <- function(AffectedPrimaryKey = NULL, StudentName = NULL, StudentNumber = NULL, Date = NULL, DayOfTheWeek = NULL, DayRotationID = NULL, DayRotation = NULL, GuardianNotified = NULL, AttendanceTakenByPeriod = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempStudentAttendanceRecord", body = list(DataObject = body), searchFields = append("TempStudentAttendanceRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentAttendanceRecord
	#'
	#' This function modifies a TempStudentAttendanceRecord
	#' @param fieldNames The field values to give the modified TempStudentAttendanceRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempStudentAttendanceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentAttendanceRecord <- function(TempStudentAttendanceRecordID, AffectedPrimaryKey = NULL, StudentName = NULL, StudentNumber = NULL, Date = NULL, DayOfTheWeek = NULL, DayRotationID = NULL, DayRotation = NULL, GuardianNotified = NULL, AttendanceTakenByPeriod = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempStudentAttendanceRecord", objectId = TempStudentAttendanceRecordID, body = list(DataObject = body), searchFields = append("TempStudentAttendanceRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempAffectedStudentAttendanceRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAffectedStudentAttendanceRecords <- function(searchConditionsList = NULL, TempAffectedStudentAttendanceRecordID = F, AffectedPrimaryKey = F, FullName = F, StudentNumber = F, Date = F, OldDaysAbsent = F, NewDaysAbsent = F, OldDaysExcused = F, NewDaysExcused = F, OldDaysUnexcused = F, NewDaysUnexcused = F, OldDaysOther = F, NewDaysOther = F, OldTardyCount = F, NewTardyCount = F, StudentID = F, CalendarDayID = F, Comment = F, PreviousGuardianNotified = F, NewGuardianNotified = F, OldStudentAttendancePeriods = F, NewStudentAttendancePeriods = F, DayRotationCode = F, FailureReason = F, FailedStudentAttendancePeriods = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempAffectedStudentAttendanceRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAffectedStudentAttendanceRecord
	#'
	#' This function returns a dataframe or json object of a TempAffectedStudentAttendanceRecord
	#' @param TempAffectedStudentAttendanceRecordID The ID of the TempAffectedStudentAttendanceRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAffectedStudentAttendanceRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAffectedStudentAttendanceRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAffectedStudentAttendanceRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempAffectedStudentAttendanceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAffectedStudentAttendanceRecord <- function(TempAffectedStudentAttendanceRecordID, AffectedPrimaryKey = F, FullName = F, StudentNumber = F, Date = F, OldDaysAbsent = F, NewDaysAbsent = F, OldDaysExcused = F, NewDaysExcused = F, OldDaysUnexcused = F, NewDaysUnexcused = F, OldDaysOther = F, NewDaysOther = F, OldTardyCount = F, NewTardyCount = F, StudentID = F, CalendarDayID = F, Comment = F, PreviousGuardianNotified = F, NewGuardianNotified = F, OldStudentAttendancePeriods = F, NewStudentAttendancePeriods = F, DayRotationCode = F, FailureReason = F, FailedStudentAttendancePeriods = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAffectedStudentAttendanceRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendanceRecord", objectId = TempAffectedStudentAttendanceRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAffectedStudentAttendanceRecord
	#'
	#' This function deletes a TempAffectedStudentAttendanceRecord
	#' @param TempAffectedStudentAttendanceRecordID The ID of the TempAffectedStudentAttendanceRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempAffectedStudentAttendanceRecordID of the deleted TempAffectedStudentAttendanceRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAffectedStudentAttendanceRecord <- function(TempAffectedStudentAttendanceRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendanceRecord", objectId = TempAffectedStudentAttendanceRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAffectedStudentAttendanceRecord
	#'
	#' This function creates a TempAffectedStudentAttendanceRecord
	#' @param fieldNames The field values to give the created TempAffectedStudentAttendanceRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempAffectedStudentAttendanceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAffectedStudentAttendanceRecord <- function(AffectedPrimaryKey = NULL, FullName = NULL, StudentNumber = NULL, Date = NULL, OldDaysAbsent = NULL, NewDaysAbsent = NULL, OldDaysExcused = NULL, NewDaysExcused = NULL, OldDaysUnexcused = NULL, NewDaysUnexcused = NULL, OldDaysOther = NULL, NewDaysOther = NULL, OldTardyCount = NULL, NewTardyCount = NULL, StudentID = NULL, CalendarDayID = NULL, Comment = NULL, PreviousGuardianNotified = NULL, NewGuardianNotified = NULL, OldStudentAttendancePeriods = NULL, NewStudentAttendancePeriods = NULL, DayRotationCode = NULL, FailureReason = NULL, FailedStudentAttendancePeriods = NULL, IsGuardianNotified = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendanceRecord", body = list(DataObject = body), searchFields = append("TempAffectedStudentAttendanceRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAffectedStudentAttendanceRecord
	#'
	#' This function modifies a TempAffectedStudentAttendanceRecord
	#' @param fieldNames The field values to give the modified TempAffectedStudentAttendanceRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempAffectedStudentAttendanceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAffectedStudentAttendanceRecord <- function(TempAffectedStudentAttendanceRecordID, AffectedPrimaryKey = NULL, FullName = NULL, StudentNumber = NULL, Date = NULL, OldDaysAbsent = NULL, NewDaysAbsent = NULL, OldDaysExcused = NULL, NewDaysExcused = NULL, OldDaysUnexcused = NULL, NewDaysUnexcused = NULL, OldDaysOther = NULL, NewDaysOther = NULL, OldTardyCount = NULL, NewTardyCount = NULL, StudentID = NULL, CalendarDayID = NULL, Comment = NULL, PreviousGuardianNotified = NULL, NewGuardianNotified = NULL, OldStudentAttendancePeriods = NULL, NewStudentAttendancePeriods = NULL, DayRotationCode = NULL, FailureReason = NULL, FailedStudentAttendancePeriods = NULL, IsGuardianNotified = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendanceRecord", objectId = TempAffectedStudentAttendanceRecordID, body = list(DataObject = body), searchFields = append("TempAffectedStudentAttendanceRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempAffectedStudentAttendancePeriodRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAffectedStudentAttendancePeriodRecords <- function(searchConditionsList = NULL, TempAffectedStudentAttendancePeriodRecordID = F, AffectedPrimaryKey = F, StudentAttendanceID = F, AttendancePeriodID = F, AttendanceTypeID = F, AttendanceReasonID = F, Comment = F, IsGuardianNotified = F, FullName = F, StudentNumber = F, CalendarDayID = F, StudentID = F, Date = F, DayRotationCode = F, PeriodCode = F, Action = F, NewStudentSectionID = F, OldStudentSectionID = F, NewStudentSectionCode = F, OldStudentSectionCode = F, Entity = F, AttendanceCategory = F, FailureReason = F, AttendanceTypeCodeDescription = F, AttendanceReasonCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsForCECEAttendancePeriod = F, CECEAttendanceTypeID = F, CECEAttendanceReasonID = F, CECEAttendancePeriodID = F, ProcessFromCECEEntity = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempAffectedStudentAttendancePeriodRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAffectedStudentAttendancePeriodRecord
	#'
	#' This function returns a dataframe or json object of a TempAffectedStudentAttendancePeriodRecord
	#' @param TempAffectedStudentAttendancePeriodRecordID The ID of the TempAffectedStudentAttendancePeriodRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAffectedStudentAttendancePeriodRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAffectedStudentAttendancePeriodRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAffectedStudentAttendancePeriodRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempAffectedStudentAttendancePeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAffectedStudentAttendancePeriodRecord <- function(TempAffectedStudentAttendancePeriodRecordID, AffectedPrimaryKey = F, StudentAttendanceID = F, AttendancePeriodID = F, AttendanceTypeID = F, AttendanceReasonID = F, Comment = F, IsGuardianNotified = F, FullName = F, StudentNumber = F, CalendarDayID = F, StudentID = F, Date = F, DayRotationCode = F, PeriodCode = F, Action = F, NewStudentSectionID = F, OldStudentSectionID = F, NewStudentSectionCode = F, OldStudentSectionCode = F, Entity = F, AttendanceCategory = F, FailureReason = F, AttendanceTypeCodeDescription = F, AttendanceReasonCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsForCECEAttendancePeriod = F, CECEAttendanceTypeID = F, CECEAttendanceReasonID = F, CECEAttendancePeriodID = F, ProcessFromCECEEntity = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAffectedStudentAttendancePeriodRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendancePeriodRecord", objectId = TempAffectedStudentAttendancePeriodRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAffectedStudentAttendancePeriodRecord
	#'
	#' This function deletes a TempAffectedStudentAttendancePeriodRecord
	#' @param TempAffectedStudentAttendancePeriodRecordID The ID of the TempAffectedStudentAttendancePeriodRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempAffectedStudentAttendancePeriodRecordID of the deleted TempAffectedStudentAttendancePeriodRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAffectedStudentAttendancePeriodRecord <- function(TempAffectedStudentAttendancePeriodRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendancePeriodRecord", objectId = TempAffectedStudentAttendancePeriodRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAffectedStudentAttendancePeriodRecord
	#'
	#' This function creates a TempAffectedStudentAttendancePeriodRecord
	#' @param fieldNames The field values to give the created TempAffectedStudentAttendancePeriodRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempAffectedStudentAttendancePeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAffectedStudentAttendancePeriodRecord <- function(AffectedPrimaryKey = NULL, StudentAttendanceID = NULL, AttendancePeriodID = NULL, AttendanceTypeID = NULL, AttendanceReasonID = NULL, Comment = NULL, IsGuardianNotified = NULL, FullName = NULL, StudentNumber = NULL, CalendarDayID = NULL, StudentID = NULL, Date = NULL, DayRotationCode = NULL, PeriodCode = NULL, Action = NULL, NewStudentSectionID = NULL, OldStudentSectionID = NULL, NewStudentSectionCode = NULL, OldStudentSectionCode = NULL, Entity = NULL, AttendanceCategory = NULL, FailureReason = NULL, AttendanceTypeCodeDescription = NULL, AttendanceReasonCodeDescription = NULL, IsForCECEAttendancePeriod = NULL, CECEAttendanceTypeID = NULL, CECEAttendanceReasonID = NULL, CECEAttendancePeriodID = NULL, ProcessFromCECEEntity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendancePeriodRecord", body = list(DataObject = body), searchFields = append("TempAffectedStudentAttendancePeriodRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAffectedStudentAttendancePeriodRecord
	#'
	#' This function modifies a TempAffectedStudentAttendancePeriodRecord
	#' @param fieldNames The field values to give the modified TempAffectedStudentAttendancePeriodRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempAffectedStudentAttendancePeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAffectedStudentAttendancePeriodRecord <- function(TempAffectedStudentAttendancePeriodRecordID, AffectedPrimaryKey = NULL, StudentAttendanceID = NULL, AttendancePeriodID = NULL, AttendanceTypeID = NULL, AttendanceReasonID = NULL, Comment = NULL, IsGuardianNotified = NULL, FullName = NULL, StudentNumber = NULL, CalendarDayID = NULL, StudentID = NULL, Date = NULL, DayRotationCode = NULL, PeriodCode = NULL, Action = NULL, NewStudentSectionID = NULL, OldStudentSectionID = NULL, NewStudentSectionCode = NULL, OldStudentSectionCode = NULL, Entity = NULL, AttendanceCategory = NULL, FailureReason = NULL, AttendanceTypeCodeDescription = NULL, AttendanceReasonCodeDescription = NULL, IsForCECEAttendancePeriod = NULL, CECEAttendanceTypeID = NULL, CECEAttendanceReasonID = NULL, CECEAttendancePeriodID = NULL, ProcessFromCECEEntity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempAffectedStudentAttendancePeriodRecord", objectId = TempAffectedStudentAttendancePeriodRecordID, body = list(DataObject = body), searchFields = append("TempAffectedStudentAttendancePeriodRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of StaffMeetSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffMeetSettings <- function(searchConditionsList = NULL, StaffMeetSettingID = F, StaffMeetID = F, DisplayHistoricalAttendanceOnDesktop = F, DisplayHistoricalAttendanceOnMobile = F, DisplayAttendanceTotalsOnDesktop = F, DisplayAttendanceTotalsOnMobile = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BrowseViewID = F, HideLockedColumns = F, UseCustomClassRosterSort = F, StudentNameDisplayType = F, DisplayStudentGradeLevel = F, DisplayStudentNumber = F, DisplayCourseDescription = F, DisplayMethodOfInstruction = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StaffMeetSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StaffMeetSetting
	#'
	#' This function returns a dataframe or json object of a StaffMeetSetting
	#' @param StaffMeetSettingID The ID of the StaffMeetSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffMeetSetting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffMeetSetting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffMeetSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StaffMeetSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffMeetSetting <- function(StaffMeetSettingID, StaffMeetID = F, DisplayHistoricalAttendanceOnDesktop = F, DisplayHistoricalAttendanceOnMobile = F, DisplayAttendanceTotalsOnDesktop = F, DisplayAttendanceTotalsOnMobile = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BrowseViewID = F, HideLockedColumns = F, UseCustomClassRosterSort = F, StudentNameDisplayType = F, DisplayStudentGradeLevel = F, DisplayStudentNumber = F, DisplayCourseDescription = F, DisplayMethodOfInstruction = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffMeetSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StaffMeetSetting", objectId = StaffMeetSettingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StaffMeetSetting
	#'
	#' This function deletes a StaffMeetSetting
	#' @param StaffMeetSettingID The ID of the StaffMeetSetting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StaffMeetSettingID of the deleted StaffMeetSetting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStaffMeetSetting <- function(StaffMeetSettingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StaffMeetSetting", objectId = StaffMeetSettingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StaffMeetSetting
	#'
	#' This function creates a StaffMeetSetting
	#' @param fieldNames The field values to give the created StaffMeetSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StaffMeetSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStaffMeetSetting <- function(StaffMeetID = NULL, DisplayHistoricalAttendanceOnDesktop = NULL, DisplayHistoricalAttendanceOnMobile = NULL, DisplayAttendanceTotalsOnDesktop = NULL, DisplayAttendanceTotalsOnMobile = NULL, BrowseViewID = NULL, HideLockedColumns = NULL, UseCustomClassRosterSort = NULL, StudentNameDisplayType = NULL, DisplayStudentGradeLevel = NULL, DisplayStudentNumber = NULL, DisplayCourseDescription = NULL, DisplayMethodOfInstruction = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "StaffMeetSetting", body = list(DataObject = body), searchFields = append("StaffMeetSettingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StaffMeetSetting
	#'
	#' This function modifies a StaffMeetSetting
	#' @param fieldNames The field values to give the modified StaffMeetSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StaffMeetSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStaffMeetSetting <- function(StaffMeetSettingID, StaffMeetID = NULL, DisplayHistoricalAttendanceOnDesktop = NULL, DisplayHistoricalAttendanceOnMobile = NULL, DisplayAttendanceTotalsOnDesktop = NULL, DisplayAttendanceTotalsOnMobile = NULL, BrowseViewID = NULL, HideLockedColumns = NULL, UseCustomClassRosterSort = NULL, StudentNameDisplayType = NULL, DisplayStudentGradeLevel = NULL, DisplayStudentNumber = NULL, DisplayCourseDescription = NULL, DisplayMethodOfInstruction = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "StaffMeetSetting", objectId = StaffMeetSettingID, body = list(DataObject = body), searchFields = append("StaffMeetSettingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of RecordedUnrecordedAttendances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRecordedUnrecordedAttendances <- function(searchConditionsList = NULL, MeetDisplayPeriodID = F, DisplayPeriodCode = F, MeetID = F, DailySectionAttendanceID = F, AttendanceTaken = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Date = F, CountAs = F, DayOfTheWeek = F, AllStudentsHaveAttendance = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "RecordedUnrecordedAttendance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RecordedUnrecordedAttendance
	#'
	#' This function returns a dataframe or json object of a RecordedUnrecordedAttendance
	#' @param RecordedUnrecordedAttendanceID The ID of the RecordedUnrecordedAttendance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RecordedUnrecordedAttendance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RecordedUnrecordedAttendance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RecordedUnrecordedAttendance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of RecordedUnrecordedAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRecordedUnrecordedAttendance <- function(RecordedUnrecordedAttendanceID, MeetDisplayPeriodID = F, DisplayPeriodCode = F, MeetID = F, DailySectionAttendanceID = F, AttendanceTaken = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Date = F, CountAs = F, DayOfTheWeek = F, AllStudentsHaveAttendance = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RecordedUnrecordedAttendanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "RecordedUnrecordedAttendance", objectId = RecordedUnrecordedAttendanceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RecordedUnrecordedAttendance
	#'
	#' This function deletes a RecordedUnrecordedAttendance
	#' @param RecordedUnrecordedAttendanceID The ID of the RecordedUnrecordedAttendance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The RecordedUnrecordedAttendanceID of the deleted RecordedUnrecordedAttendance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRecordedUnrecordedAttendance <- function(RecordedUnrecordedAttendanceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "RecordedUnrecordedAttendance", objectId = RecordedUnrecordedAttendanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of DailySectionAttendances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDailySectionAttendances <- function(searchConditionsList = NULL, DailySectionAttendanceID = F, CalendarDayID = F, MeetID = F, AttendancePeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "DailySectionAttendance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DailySectionAttendance
	#'
	#' This function returns a dataframe or json object of a DailySectionAttendance
	#' @param DailySectionAttendanceID The ID of the DailySectionAttendance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DailySectionAttendance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DailySectionAttendance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DailySectionAttendance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of DailySectionAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDailySectionAttendance <- function(DailySectionAttendanceID, CalendarDayID = F, MeetID = F, AttendancePeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DailySectionAttendanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "DailySectionAttendance", objectId = DailySectionAttendanceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DailySectionAttendance
	#'
	#' This function deletes a DailySectionAttendance
	#' @param DailySectionAttendanceID The ID of the DailySectionAttendance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The DailySectionAttendanceID of the deleted DailySectionAttendance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDailySectionAttendance <- function(DailySectionAttendanceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "DailySectionAttendance", objectId = DailySectionAttendanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DailySectionAttendance
	#'
	#' This function creates a DailySectionAttendance
	#' @param fieldNames The field values to give the created DailySectionAttendance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created DailySectionAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDailySectionAttendance <- function(CalendarDayID = NULL, MeetID = NULL, AttendancePeriodID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "DailySectionAttendance", body = list(DataObject = body), searchFields = append("DailySectionAttendanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DailySectionAttendance
	#'
	#' This function modifies a DailySectionAttendance
	#' @param fieldNames The field values to give the modified DailySectionAttendance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified DailySectionAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDailySectionAttendance <- function(DailySectionAttendanceID, CalendarDayID = NULL, MeetID = NULL, AttendancePeriodID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "DailySectionAttendance", objectId = DailySectionAttendanceID, body = list(DataObject = body), searchFields = append("DailySectionAttendanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of StudentAttendanceTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendanceTerms <- function(searchConditionsList = NULL, StudentID = F, AttendanceTermCode = F, StartDate = F, EndDate = F, EntityID = F, SchoolYearID = F, IsDefault = F, TotalDaysAbsent = F, TotalDaysExcused = F, TotalDaysOther = F, TotalDaysUnexcused = F, TotalTardyCount = F, AttendanceTermID = F, TotalDaysPresent = F, TotalDaysPossible = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendanceTerm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendanceTerm
	#'
	#' This function returns a dataframe or json object of a StudentAttendanceTerm
	#' @param StudentAttendanceTermID The ID of the StudentAttendanceTerm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendanceTerm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendanceTerm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendanceTerm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendanceTerm <- function(StudentAttendanceTermID, StudentID = F, AttendanceTermCode = F, StartDate = F, EndDate = F, EntityID = F, SchoolYearID = F, IsDefault = F, TotalDaysAbsent = F, TotalDaysExcused = F, TotalDaysOther = F, TotalDaysUnexcused = F, TotalTardyCount = F, AttendanceTermID = F, TotalDaysPresent = F, TotalDaysPossible = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendanceTermID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendanceTerm", objectId = StudentAttendanceTermID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendanceTerm
	#'
	#' This function deletes a StudentAttendanceTerm
	#' @param StudentAttendanceTermID The ID of the StudentAttendanceTerm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendanceTermID of the deleted StudentAttendanceTerm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendanceTerm <- function(StudentAttendanceTermID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendanceTerm", objectId = StudentAttendanceTermID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempAffectedCalendarDayRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAffectedCalendarDayRecords <- function(searchConditionsList = NULL, TempAffectedCalendarDayRecordID = F, AffectedPrimaryKey = F, Date = F, DayOfTheWeek = F, NewDayRotationID = F, NewDayRotation = F, OldDayRotationID = F, OldDayRotation = F, CountAs = F, NewCountAs = F, NewFundingPeriodID = F, NewFundingPeriod = F, OldFundingPeriodID = F, OldFundingPeriod = F, Comment = F, FailureReason = F, Action = F, OldStateSchoolDayEventCodeTX = F, NewStateSchoolDayEventCodeTX = F, OldStateSchoolDayEventCodeTXID = F, NewStateSchoolDayEventCodeTXID = F, OldUseOperationalMinutesOverride = F, NewUseOperationalMinutesOverride = F, OldOperationalMinutesOverride = F, NewOperationalMinutesOverride = F, OldStateCalendarWaiverEventTypeCodeTXID = F, NewStateCalendarWaiverEventTypeCodeTXID = F, OldStateCalendarWaiverEventTypeCodeTX = F, NewStateCalendarWaiverEventTypeCodeTX = F, OldWaiverMinutes = F, NewWaiverMinutes = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NewBellSchedule = F, OldBellSchedule = F, CalendarID = F, Calendar = F, Entity = F, OldUseInstructionalMinutesOverride = F, NewUseInstructionalMinutesOverride = F, OldInstructionalMinutesOverride = F, NewInstructionalMinutesOverride = F, EdFiCalendarEventDescriptorINID = F, ShowCommentOnCalendar = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempAffectedCalendarDayRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAffectedCalendarDayRecord
	#'
	#' This function returns a dataframe or json object of a TempAffectedCalendarDayRecord
	#' @param TempAffectedCalendarDayRecordID The ID of the TempAffectedCalendarDayRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAffectedCalendarDayRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAffectedCalendarDayRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAffectedCalendarDayRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempAffectedCalendarDayRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAffectedCalendarDayRecord <- function(TempAffectedCalendarDayRecordID, AffectedPrimaryKey = F, Date = F, DayOfTheWeek = F, NewDayRotationID = F, NewDayRotation = F, OldDayRotationID = F, OldDayRotation = F, CountAs = F, NewCountAs = F, NewFundingPeriodID = F, NewFundingPeriod = F, OldFundingPeriodID = F, OldFundingPeriod = F, Comment = F, FailureReason = F, Action = F, OldStateSchoolDayEventCodeTX = F, NewStateSchoolDayEventCodeTX = F, OldStateSchoolDayEventCodeTXID = F, NewStateSchoolDayEventCodeTXID = F, OldUseOperationalMinutesOverride = F, NewUseOperationalMinutesOverride = F, OldOperationalMinutesOverride = F, NewOperationalMinutesOverride = F, OldStateCalendarWaiverEventTypeCodeTXID = F, NewStateCalendarWaiverEventTypeCodeTXID = F, OldStateCalendarWaiverEventTypeCodeTX = F, NewStateCalendarWaiverEventTypeCodeTX = F, OldWaiverMinutes = F, NewWaiverMinutes = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NewBellSchedule = F, OldBellSchedule = F, CalendarID = F, Calendar = F, Entity = F, OldUseInstructionalMinutesOverride = F, NewUseInstructionalMinutesOverride = F, OldInstructionalMinutesOverride = F, NewInstructionalMinutesOverride = F, EdFiCalendarEventDescriptorINID = F, ShowCommentOnCalendar = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAffectedCalendarDayRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempAffectedCalendarDayRecord", objectId = TempAffectedCalendarDayRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAffectedCalendarDayRecord
	#'
	#' This function deletes a TempAffectedCalendarDayRecord
	#' @param TempAffectedCalendarDayRecordID The ID of the TempAffectedCalendarDayRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempAffectedCalendarDayRecordID of the deleted TempAffectedCalendarDayRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAffectedCalendarDayRecord <- function(TempAffectedCalendarDayRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempAffectedCalendarDayRecord", objectId = TempAffectedCalendarDayRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAffectedCalendarDayRecord
	#'
	#' This function creates a TempAffectedCalendarDayRecord
	#' @param fieldNames The field values to give the created TempAffectedCalendarDayRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempAffectedCalendarDayRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAffectedCalendarDayRecord <- function(AffectedPrimaryKey = NULL, Date = NULL, DayOfTheWeek = NULL, NewDayRotationID = NULL, NewDayRotation = NULL, OldDayRotationID = NULL, OldDayRotation = NULL, CountAs = NULL, NewCountAs = NULL, NewFundingPeriodID = NULL, NewFundingPeriod = NULL, OldFundingPeriodID = NULL, OldFundingPeriod = NULL, Comment = NULL, FailureReason = NULL, Action = NULL, OldStateSchoolDayEventCodeTX = NULL, NewStateSchoolDayEventCodeTX = NULL, OldStateSchoolDayEventCodeTXID = NULL, NewStateSchoolDayEventCodeTXID = NULL, OldUseOperationalMinutesOverride = NULL, NewUseOperationalMinutesOverride = NULL, OldOperationalMinutesOverride = NULL, NewOperationalMinutesOverride = NULL, OldStateCalendarWaiverEventTypeCodeTXID = NULL, NewStateCalendarWaiverEventTypeCodeTXID = NULL, OldStateCalendarWaiverEventTypeCodeTX = NULL, NewStateCalendarWaiverEventTypeCodeTX = NULL, OldWaiverMinutes = NULL, NewWaiverMinutes = NULL, NewBellSchedule = NULL, OldBellSchedule = NULL, CalendarID = NULL, Calendar = NULL, Entity = NULL, OldUseInstructionalMinutesOverride = NULL, NewUseInstructionalMinutesOverride = NULL, OldInstructionalMinutesOverride = NULL, NewInstructionalMinutesOverride = NULL, EdFiCalendarEventDescriptorINID = NULL, ShowCommentOnCalendar = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempAffectedCalendarDayRecord", body = list(DataObject = body), searchFields = append("TempAffectedCalendarDayRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAffectedCalendarDayRecord
	#'
	#' This function modifies a TempAffectedCalendarDayRecord
	#' @param fieldNames The field values to give the modified TempAffectedCalendarDayRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempAffectedCalendarDayRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAffectedCalendarDayRecord <- function(TempAffectedCalendarDayRecordID, AffectedPrimaryKey = NULL, Date = NULL, DayOfTheWeek = NULL, NewDayRotationID = NULL, NewDayRotation = NULL, OldDayRotationID = NULL, OldDayRotation = NULL, CountAs = NULL, NewCountAs = NULL, NewFundingPeriodID = NULL, NewFundingPeriod = NULL, OldFundingPeriodID = NULL, OldFundingPeriod = NULL, Comment = NULL, FailureReason = NULL, Action = NULL, OldStateSchoolDayEventCodeTX = NULL, NewStateSchoolDayEventCodeTX = NULL, OldStateSchoolDayEventCodeTXID = NULL, NewStateSchoolDayEventCodeTXID = NULL, OldUseOperationalMinutesOverride = NULL, NewUseOperationalMinutesOverride = NULL, OldOperationalMinutesOverride = NULL, NewOperationalMinutesOverride = NULL, OldStateCalendarWaiverEventTypeCodeTXID = NULL, NewStateCalendarWaiverEventTypeCodeTXID = NULL, OldStateCalendarWaiverEventTypeCodeTX = NULL, NewStateCalendarWaiverEventTypeCodeTX = NULL, OldWaiverMinutes = NULL, NewWaiverMinutes = NULL, NewBellSchedule = NULL, OldBellSchedule = NULL, CalendarID = NULL, Calendar = NULL, Entity = NULL, OldUseInstructionalMinutesOverride = NULL, NewUseInstructionalMinutesOverride = NULL, OldInstructionalMinutesOverride = NULL, NewInstructionalMinutesOverride = NULL, EdFiCalendarEventDescriptorINID = NULL, ShowCommentOnCalendar = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempAffectedCalendarDayRecord", objectId = TempAffectedCalendarDayRecordID, body = list(DataObject = body), searchFields = append("TempAffectedCalendarDayRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of AttendanceConfigEntityGroupYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceConfigEntityGroupYears <- function(searchConditionsList = NULL, ConfigEntityGroupYearID = F, UseSpecialClassCounts = F, SpecialClassCountsLabel = F, SchoolYearID = F, EntityID = F, EntityGroupKey = F, ConfigEntityGroupYearIDClonedFrom = F, UseTardyKiosk = F, AttendanceTypeIDTardyDefault = F, AttendanceReasonIDTardyDefault = F, TardyDefaultComment = F, UseTardyCalculator = F, UseTeacherPerfectAttendanceConfirmation = F, UseMarkAllStudentsPresentOnTile = F, TardyKioskTardySlipTitle = F, IncludeStudentNameAndOrNumberOnLetter = F, IncludeGradeLevelOnLetter = F, IncludeSchoolOrCampusOnLetter = F, IncludeParentNameAndOrPhoneNumberOnLetter = F, IncludeTardyCountOnLetter = F, IncludeSignatureLineForParentOnLetter = F, IncludeSignatureLineForStudentOnLetter = F, IncludeSignatureLineForOfficeOnLetter = F, MultiPeriodClassCountMethod = F, TeacherEntryCutOffTime = F, TeacherEntrySpecificCutOffTime = F, TeacherEntryCutOffNumberOfMinutesAfter = F, RestrictTeacherAttendanceUpdates = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PresentBackgroundColor = F, EnableInOutTime = F, UseInOutTimeForCalculations = F, TeacherEntryCutoffWindowStartTime = F, TeacherEntryCutoffWindowEndTime = F, TeacherEntryCutoffDuration = F, PrintAttendanceLetterForWindowedEnvelope = F, DisplayStudentCountOnTiles = F, AllowTeachersToModifyPreviousAttendance = F, NumberOfDaysToAllowTeachersToModifyPreviousAttendance = F, AttendanceTypeIDTeacherDefault = F, HasAttendanceTypeTeacherDefault = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "ConfigEntityGroupYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceConfigEntityGroupYear
	#'
	#' This function returns a dataframe or json object of an AttendanceConfigEntityGroupYear
	#' @param AttendanceConfigEntityGroupYearID The ID of the AttendanceConfigEntityGroupYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceConfigEntityGroupYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceConfigEntityGroupYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceConfigEntityGroupYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceConfigEntityGroupYear <- function(AttendanceConfigEntityGroupYearID, ConfigEntityGroupYearID = F, UseSpecialClassCounts = F, SpecialClassCountsLabel = F, SchoolYearID = F, EntityID = F, EntityGroupKey = F, ConfigEntityGroupYearIDClonedFrom = F, UseTardyKiosk = F, AttendanceTypeIDTardyDefault = F, AttendanceReasonIDTardyDefault = F, TardyDefaultComment = F, UseTardyCalculator = F, UseTeacherPerfectAttendanceConfirmation = F, UseMarkAllStudentsPresentOnTile = F, TardyKioskTardySlipTitle = F, IncludeStudentNameAndOrNumberOnLetter = F, IncludeGradeLevelOnLetter = F, IncludeSchoolOrCampusOnLetter = F, IncludeParentNameAndOrPhoneNumberOnLetter = F, IncludeTardyCountOnLetter = F, IncludeSignatureLineForParentOnLetter = F, IncludeSignatureLineForStudentOnLetter = F, IncludeSignatureLineForOfficeOnLetter = F, MultiPeriodClassCountMethod = F, TeacherEntryCutOffTime = F, TeacherEntrySpecificCutOffTime = F, TeacherEntryCutOffNumberOfMinutesAfter = F, RestrictTeacherAttendanceUpdates = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PresentBackgroundColor = F, EnableInOutTime = F, UseInOutTimeForCalculations = F, TeacherEntryCutoffWindowStartTime = F, TeacherEntryCutoffWindowEndTime = F, TeacherEntryCutoffDuration = F, PrintAttendanceLetterForWindowedEnvelope = F, DisplayStudentCountOnTiles = F, AllowTeachersToModifyPreviousAttendance = F, NumberOfDaysToAllowTeachersToModifyPreviousAttendance = F, AttendanceTypeIDTeacherDefault = F, HasAttendanceTypeTeacherDefault = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceConfigEntityGroupYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "ConfigEntityGroupYear", objectId = AttendanceConfigEntityGroupYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceConfigEntityGroupYear
	#'
	#' This function deletes an AttendanceConfigEntityGroupYear
	#' @param AttendanceConfigEntityGroupYearID The ID of the AttendanceConfigEntityGroupYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceConfigEntityGroupYearID of the deleted AttendanceConfigEntityGroupYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceConfigEntityGroupYear <- function(AttendanceConfigEntityGroupYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "ConfigEntityGroupYear", objectId = AttendanceConfigEntityGroupYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceConfigEntityGroupYear
	#'
	#' This function creates an AttendanceConfigEntityGroupYear
	#' @param fieldNames The field values to give the created AttendanceConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceConfigEntityGroupYear <- function(UseSpecialClassCounts = NULL, SpecialClassCountsLabel = NULL, SchoolYearID = NULL, EntityID = NULL, EntityGroupKey = NULL, ConfigEntityGroupYearIDClonedFrom = NULL, UseTardyKiosk = NULL, AttendanceTypeIDTardyDefault = NULL, AttendanceReasonIDTardyDefault = NULL, TardyDefaultComment = NULL, UseTardyCalculator = NULL, UseTeacherPerfectAttendanceConfirmation = NULL, UseMarkAllStudentsPresentOnTile = NULL, TardyKioskTardySlipTitle = NULL, IncludeStudentNameAndOrNumberOnLetter = NULL, IncludeGradeLevelOnLetter = NULL, IncludeSchoolOrCampusOnLetter = NULL, IncludeParentNameAndOrPhoneNumberOnLetter = NULL, IncludeTardyCountOnLetter = NULL, IncludeSignatureLineForParentOnLetter = NULL, IncludeSignatureLineForStudentOnLetter = NULL, IncludeSignatureLineForOfficeOnLetter = NULL, MultiPeriodClassCountMethod = NULL, TeacherEntryCutOffTime = NULL, TeacherEntrySpecificCutOffTime = NULL, TeacherEntryCutOffNumberOfMinutesAfter = NULL, RestrictTeacherAttendanceUpdates = NULL, PresentBackgroundColor = NULL, EnableInOutTime = NULL, UseInOutTimeForCalculations = NULL, TeacherEntryCutoffWindowStartTime = NULL, TeacherEntryCutoffWindowEndTime = NULL, TeacherEntryCutoffDuration = NULL, PrintAttendanceLetterForWindowedEnvelope = NULL, DisplayStudentCountOnTiles = NULL, AllowTeachersToModifyPreviousAttendance = NULL, NumberOfDaysToAllowTeachersToModifyPreviousAttendance = NULL, AttendanceTypeIDTeacherDefault = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "ConfigEntityGroupYear", body = list(DataObject = body), searchFields = append("ConfigEntityGroupYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceConfigEntityGroupYear
	#'
	#' This function modifies an AttendanceConfigEntityGroupYear
	#' @param fieldNames The field values to give the modified AttendanceConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceConfigEntityGroupYear <- function(ConfigEntityGroupYearID, UseSpecialClassCounts = NULL, SpecialClassCountsLabel = NULL, SchoolYearID = NULL, EntityID = NULL, EntityGroupKey = NULL, ConfigEntityGroupYearIDClonedFrom = NULL, UseTardyKiosk = NULL, AttendanceTypeIDTardyDefault = NULL, AttendanceReasonIDTardyDefault = NULL, TardyDefaultComment = NULL, UseTardyCalculator = NULL, UseTeacherPerfectAttendanceConfirmation = NULL, UseMarkAllStudentsPresentOnTile = NULL, TardyKioskTardySlipTitle = NULL, IncludeStudentNameAndOrNumberOnLetter = NULL, IncludeGradeLevelOnLetter = NULL, IncludeSchoolOrCampusOnLetter = NULL, IncludeParentNameAndOrPhoneNumberOnLetter = NULL, IncludeTardyCountOnLetter = NULL, IncludeSignatureLineForParentOnLetter = NULL, IncludeSignatureLineForStudentOnLetter = NULL, IncludeSignatureLineForOfficeOnLetter = NULL, MultiPeriodClassCountMethod = NULL, TeacherEntryCutOffTime = NULL, TeacherEntrySpecificCutOffTime = NULL, TeacherEntryCutOffNumberOfMinutesAfter = NULL, RestrictTeacherAttendanceUpdates = NULL, PresentBackgroundColor = NULL, EnableInOutTime = NULL, UseInOutTimeForCalculations = NULL, TeacherEntryCutoffWindowStartTime = NULL, TeacherEntryCutoffWindowEndTime = NULL, TeacherEntryCutoffDuration = NULL, PrintAttendanceLetterForWindowedEnvelope = NULL, DisplayStudentCountOnTiles = NULL, AllowTeachersToModifyPreviousAttendance = NULL, NumberOfDaysToAllowTeachersToModifyPreviousAttendance = NULL, AttendanceTypeIDTeacherDefault = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "ConfigEntityGroupYear", objectId = ConfigEntityGroupYearID, body = list(DataObject = body), searchFields = append("ConfigEntityGroupYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of AttendancePeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendancePeriods <- function(searchConditionsList = NULL, AttendancePeriodID = F, EntityGroupKey = F, EntityID = F, SchoolYearID = F, Code = F, DisplayOrder = F, AttendancePeriodIDClonedFrom = F, UseTeacherEntryCutOffTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttendancePeriodIDClonedTo = F, UseForSchoolTrakPositiveAttendance = F, DynamicRelationshipID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendancePeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendancePeriod
	#'
	#' This function returns a dataframe or json object of an AttendancePeriod
	#' @param AttendancePeriodID The ID of the AttendancePeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendancePeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendancePeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendancePeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendancePeriod <- function(AttendancePeriodID, EntityGroupKey = F, EntityID = F, SchoolYearID = F, Code = F, DisplayOrder = F, AttendancePeriodIDClonedFrom = F, UseTeacherEntryCutOffTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttendancePeriodIDClonedTo = F, UseForSchoolTrakPositiveAttendance = F, DynamicRelationshipID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendancePeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendancePeriod", objectId = AttendancePeriodID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendancePeriod
	#'
	#' This function deletes an AttendancePeriod
	#' @param AttendancePeriodID The ID of the AttendancePeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendancePeriodID of the deleted AttendancePeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendancePeriod <- function(AttendancePeriodID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendancePeriod", objectId = AttendancePeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendancePeriod
	#'
	#' This function creates an AttendancePeriod
	#' @param fieldNames The field values to give the created AttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendancePeriod <- function(EntityGroupKey = NULL, EntityID = NULL, SchoolYearID = NULL, Code = NULL, DisplayOrder = NULL, AttendancePeriodIDClonedFrom = NULL, UseForSchoolTrakPositiveAttendance = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "AttendancePeriod", body = list(DataObject = body), searchFields = append("AttendancePeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendancePeriod
	#'
	#' This function modifies an AttendancePeriod
	#' @param fieldNames The field values to give the modified AttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendancePeriod <- function(AttendancePeriodID, EntityGroupKey = NULL, EntityID = NULL, SchoolYearID = NULL, Code = NULL, DisplayOrder = NULL, AttendancePeriodIDClonedFrom = NULL, UseForSchoolTrakPositiveAttendance = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "AttendancePeriod", objectId = AttendancePeriodID, body = list(DataObject = body), searchFields = append("AttendancePeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of AttendanceTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceTypes <- function(searchConditionsList = NULL, AttendanceTypeID = F, EntityGroupKey = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, IncludeInClassCounts = F, IncludeInTotals = F, IncludeInSpecialClassCounts = F, Category = F, ShowOnGradesheetAssignments = F, TeacherEntryID = F, CodeDescription = F, AttendanceTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttendanceTypeMNID = F, IsTruant = F, EdFiAttendanceEventCategoryDescriptorID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendanceType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceType
	#'
	#' This function returns a dataframe or json object of an AttendanceType
	#' @param AttendanceTypeID The ID of the AttendanceType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceType <- function(AttendanceTypeID, EntityGroupKey = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, IncludeInClassCounts = F, IncludeInTotals = F, IncludeInSpecialClassCounts = F, Category = F, ShowOnGradesheetAssignments = F, TeacherEntryID = F, CodeDescription = F, AttendanceTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttendanceTypeMNID = F, IsTruant = F, EdFiAttendanceEventCategoryDescriptorID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendanceType", objectId = AttendanceTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceType
	#'
	#' This function deletes an AttendanceType
	#' @param AttendanceTypeID The ID of the AttendanceType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceTypeID of the deleted AttendanceType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceType <- function(AttendanceTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendanceType", objectId = AttendanceTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceType
	#'
	#' This function creates an AttendanceType
	#' @param fieldNames The field values to give the created AttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceType <- function(EntityGroupKey = NULL, EntityID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, IncludeInClassCounts = NULL, IncludeInTotals = NULL, IncludeInSpecialClassCounts = NULL, Category = NULL, ShowOnGradesheetAssignments = NULL, TeacherEntryID = NULL, AttendanceTypeIDClonedFrom = NULL, IsTruant = NULL, EdFiAttendanceEventCategoryDescriptorID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "AttendanceType", body = list(DataObject = body), searchFields = append("AttendanceTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceType
	#'
	#' This function modifies an AttendanceType
	#' @param fieldNames The field values to give the modified AttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceType <- function(AttendanceTypeID, EntityGroupKey = NULL, EntityID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, IncludeInClassCounts = NULL, IncludeInTotals = NULL, IncludeInSpecialClassCounts = NULL, Category = NULL, ShowOnGradesheetAssignments = NULL, TeacherEntryID = NULL, AttendanceTypeIDClonedFrom = NULL, IsTruant = NULL, EdFiAttendanceEventCategoryDescriptorID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "AttendanceType", objectId = AttendanceTypeID, body = list(DataObject = body), searchFields = append("AttendanceTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of AttendanceReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceReasons <- function(searchConditionsList = NULL, AttendanceReasonID = F, Code = F, Description = F, SchoolYearID = F, EntityID = F, EntityGroupKey = F, TeacherEntryID = F, CodeDescription = F, AttendanceReasonIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendanceReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceReason
	#'
	#' This function returns a dataframe or json object of an AttendanceReason
	#' @param AttendanceReasonID The ID of the AttendanceReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceReason <- function(AttendanceReasonID, Code = F, Description = F, SchoolYearID = F, EntityID = F, EntityGroupKey = F, TeacherEntryID = F, CodeDescription = F, AttendanceReasonIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendanceReason", objectId = AttendanceReasonID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceReason
	#'
	#' This function deletes an AttendanceReason
	#' @param AttendanceReasonID The ID of the AttendanceReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceReasonID of the deleted AttendanceReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceReason <- function(AttendanceReasonID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendanceReason", objectId = AttendanceReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceReason
	#'
	#' This function creates an AttendanceReason
	#' @param fieldNames The field values to give the created AttendanceReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceReason <- function(Code = NULL, Description = NULL, SchoolYearID = NULL, EntityID = NULL, EntityGroupKey = NULL, TeacherEntryID = NULL, AttendanceReasonIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "AttendanceReason", body = list(DataObject = body), searchFields = append("AttendanceReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceReason
	#'
	#' This function modifies an AttendanceReason
	#' @param fieldNames The field values to give the modified AttendanceReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceReason <- function(AttendanceReasonID, Code = NULL, Description = NULL, SchoolYearID = NULL, EntityID = NULL, EntityGroupKey = NULL, TeacherEntryID = NULL, AttendanceReasonIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "AttendanceReason", objectId = AttendanceReasonID, body = list(DataObject = body), searchFields = append("AttendanceReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of AttendanceTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceTerms <- function(searchConditionsList = NULL, AttendanceTermID = F, Code = F, StartDate = F, EndDate = F, CalendarID = F, DaysInTerm = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttendanceTermIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendanceTerm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceTerm
	#'
	#' This function returns a dataframe or json object of an AttendanceTerm
	#' @param AttendanceTermID The ID of the AttendanceTerm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceTerm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceTerm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceTerm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceTerm <- function(AttendanceTermID, Code = F, StartDate = F, EndDate = F, CalendarID = F, DaysInTerm = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttendanceTermIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceTermID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendanceTerm", objectId = AttendanceTermID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceTerm
	#'
	#' This function deletes an AttendanceTerm
	#' @param AttendanceTermID The ID of the AttendanceTerm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceTermID of the deleted AttendanceTerm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceTerm <- function(AttendanceTermID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendanceTerm", objectId = AttendanceTermID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceTerm
	#'
	#' This function creates an AttendanceTerm
	#' @param fieldNames The field values to give the created AttendanceTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceTerm <- function(Code = NULL, StartDate = NULL, EndDate = NULL, CalendarID = NULL, AttendanceTermIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "AttendanceTerm", body = list(DataObject = body), searchFields = append("AttendanceTermID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceTerm
	#'
	#' This function modifies an AttendanceTerm
	#' @param fieldNames The field values to give the modified AttendanceTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceTerm <- function(AttendanceTermID, Code = NULL, StartDate = NULL, EndDate = NULL, CalendarID = NULL, AttendanceTermIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "AttendanceTerm", objectId = AttendanceTermID, body = list(DataObject = body), searchFields = append("AttendanceTermID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of CalendarDaySchedulingPeriodTimesOverrides
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarDaySchedulingPeriodTimesOverrides <- function(searchConditionsList = NULL, CalendarDaySchedulingPeriodTimesOverrideID = F, CalendarDayID = F, SchedulingPeriodID = F, StartTime = F, EndTime = F, DurationInMinutes = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarDaySchedulingPeriodTimesOverride", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarDaySchedulingPeriodTimesOverride
	#'
	#' This function returns a dataframe or json object of a CalendarDaySchedulingPeriodTimesOverride
	#' @param CalendarDaySchedulingPeriodTimesOverrideID The ID of the CalendarDaySchedulingPeriodTimesOverride to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDaySchedulingPeriodTimesOverride. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDaySchedulingPeriodTimesOverride.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDaySchedulingPeriodTimesOverride') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CalendarDaySchedulingPeriodTimesOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarDaySchedulingPeriodTimesOverride <- function(CalendarDaySchedulingPeriodTimesOverrideID, CalendarDayID = F, SchedulingPeriodID = F, StartTime = F, EndTime = F, DurationInMinutes = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarDaySchedulingPeriodTimesOverrideID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarDaySchedulingPeriodTimesOverride", objectId = CalendarDaySchedulingPeriodTimesOverrideID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarDaySchedulingPeriodTimesOverride
	#'
	#' This function deletes a CalendarDaySchedulingPeriodTimesOverride
	#' @param CalendarDaySchedulingPeriodTimesOverrideID The ID of the CalendarDaySchedulingPeriodTimesOverride to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarDaySchedulingPeriodTimesOverrideID of the deleted CalendarDaySchedulingPeriodTimesOverride.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarDaySchedulingPeriodTimesOverride <- function(CalendarDaySchedulingPeriodTimesOverrideID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarDaySchedulingPeriodTimesOverride", objectId = CalendarDaySchedulingPeriodTimesOverrideID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarDaySchedulingPeriodTimesOverride
	#'
	#' This function creates a CalendarDaySchedulingPeriodTimesOverride
	#' @param fieldNames The field values to give the created CalendarDaySchedulingPeriodTimesOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CalendarDaySchedulingPeriodTimesOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarDaySchedulingPeriodTimesOverride <- function(CalendarDayID = NULL, SchedulingPeriodID = NULL, StartTime = NULL, EndTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "CalendarDaySchedulingPeriodTimesOverride", body = list(DataObject = body), searchFields = append("CalendarDaySchedulingPeriodTimesOverrideID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarDaySchedulingPeriodTimesOverride
	#'
	#' This function modifies a CalendarDaySchedulingPeriodTimesOverride
	#' @param fieldNames The field values to give the modified CalendarDaySchedulingPeriodTimesOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CalendarDaySchedulingPeriodTimesOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarDaySchedulingPeriodTimesOverride <- function(CalendarDaySchedulingPeriodTimesOverrideID, CalendarDayID = NULL, SchedulingPeriodID = NULL, StartTime = NULL, EndTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarDaySchedulingPeriodTimesOverride", objectId = CalendarDaySchedulingPeriodTimesOverrideID, body = list(DataObject = body), searchFields = append("CalendarDaySchedulingPeriodTimesOverrideID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of CalendarDayDisplayPeriodOverrides
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarDayDisplayPeriodOverrides <- function(searchConditionsList = NULL, CalendarDayDisplayPeriodOverrideID = F, CalendarDayID = F, DisplayPeriodID = F, RemovePeriod = F, LengthMinutes = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarDayDisplayPeriodOverride", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarDayDisplayPeriodOverride
	#'
	#' This function returns a dataframe or json object of a CalendarDayDisplayPeriodOverride
	#' @param CalendarDayDisplayPeriodOverrideID The ID of the CalendarDayDisplayPeriodOverride to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDayDisplayPeriodOverride. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDayDisplayPeriodOverride.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDayDisplayPeriodOverride') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CalendarDayDisplayPeriodOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarDayDisplayPeriodOverride <- function(CalendarDayDisplayPeriodOverrideID, CalendarDayID = F, DisplayPeriodID = F, RemovePeriod = F, LengthMinutes = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarDayDisplayPeriodOverrideID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarDayDisplayPeriodOverride", objectId = CalendarDayDisplayPeriodOverrideID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarDayDisplayPeriodOverride
	#'
	#' This function deletes a CalendarDayDisplayPeriodOverride
	#' @param CalendarDayDisplayPeriodOverrideID The ID of the CalendarDayDisplayPeriodOverride to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarDayDisplayPeriodOverrideID of the deleted CalendarDayDisplayPeriodOverride.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarDayDisplayPeriodOverride <- function(CalendarDayDisplayPeriodOverrideID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarDayDisplayPeriodOverride", objectId = CalendarDayDisplayPeriodOverrideID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarDayDisplayPeriodOverride
	#'
	#' This function creates a CalendarDayDisplayPeriodOverride
	#' @param fieldNames The field values to give the created CalendarDayDisplayPeriodOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CalendarDayDisplayPeriodOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarDayDisplayPeriodOverride <- function(CalendarDayID = NULL, DisplayPeriodID = NULL, RemovePeriod = NULL, LengthMinutes = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "CalendarDayDisplayPeriodOverride", body = list(DataObject = body), searchFields = append("CalendarDayDisplayPeriodOverrideID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarDayDisplayPeriodOverride
	#'
	#' This function modifies a CalendarDayDisplayPeriodOverride
	#' @param fieldNames The field values to give the modified CalendarDayDisplayPeriodOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CalendarDayDisplayPeriodOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarDayDisplayPeriodOverride <- function(CalendarDayDisplayPeriodOverrideID, CalendarDayID = NULL, DisplayPeriodID = NULL, RemovePeriod = NULL, LengthMinutes = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarDayDisplayPeriodOverride", objectId = CalendarDayDisplayPeriodOverrideID, body = list(DataObject = body), searchFields = append("CalendarDayDisplayPeriodOverrideID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of CalendarDisplayPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarDisplayPeriods <- function(searchConditionsList = NULL, CalendarDisplayPeriodID = F, CalendarID = F, DisplayPeriodID = F, TakeAttendance = F, IncludeInClassCounts = F, IncludeInTotalCounts = F, CalendarDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalendarDisplayPeriodIDClonedTo = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarDisplayPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarDisplayPeriod
	#'
	#' This function returns a dataframe or json object of a CalendarDisplayPeriod
	#' @param CalendarDisplayPeriodID The ID of the CalendarDisplayPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDisplayPeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDisplayPeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDisplayPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CalendarDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarDisplayPeriod <- function(CalendarDisplayPeriodID, CalendarID = F, DisplayPeriodID = F, TakeAttendance = F, IncludeInClassCounts = F, IncludeInTotalCounts = F, CalendarDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalendarDisplayPeriodIDClonedTo = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarDisplayPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarDisplayPeriod", objectId = CalendarDisplayPeriodID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarDisplayPeriod
	#'
	#' This function deletes a CalendarDisplayPeriod
	#' @param CalendarDisplayPeriodID The ID of the CalendarDisplayPeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarDisplayPeriodID of the deleted CalendarDisplayPeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarDisplayPeriod <- function(CalendarDisplayPeriodID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarDisplayPeriod", objectId = CalendarDisplayPeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarDisplayPeriod
	#'
	#' This function creates a CalendarDisplayPeriod
	#' @param fieldNames The field values to give the created CalendarDisplayPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CalendarDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarDisplayPeriod <- function(CalendarID = NULL, DisplayPeriodID = NULL, TakeAttendance = NULL, IncludeInClassCounts = NULL, IncludeInTotalCounts = NULL, CalendarDisplayPeriodIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "CalendarDisplayPeriod", body = list(DataObject = body), searchFields = append("CalendarDisplayPeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarDisplayPeriod
	#'
	#' This function modifies a CalendarDisplayPeriod
	#' @param fieldNames The field values to give the modified CalendarDisplayPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CalendarDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarDisplayPeriod <- function(CalendarDisplayPeriodID, CalendarID = NULL, DisplayPeriodID = NULL, TakeAttendance = NULL, IncludeInClassCounts = NULL, IncludeInTotalCounts = NULL, CalendarDisplayPeriodIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarDisplayPeriod", objectId = CalendarDisplayPeriodID, body = list(DataObject = body), searchFields = append("CalendarDisplayPeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AttendanceCalendarDays
	#'
	#' This function returns a dataframe or json object of AttendanceCalendarDays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceCalendarDays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceCalendarDays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceCalendarDay') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of AttendanceCalendarDays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceCalendarDays <- function(searchConditionsList = NULL, CalendarDayID = F, CalendarID = F, Date = F, DateWithDayOfWeekAbbreviated = F, DayRotationID = F, Comment = F, CountAs = F, AttendanceTerm = F, DayOfTheWeek = F, DayOfTheWeekNumber = F, NumberOfCalendarDayEvents = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BellScheduleID = F, BellScheduleGroupSummary = F, FoodServicePurchaseExists = F, DynamicRelationshipID = F, DoNotSendAttendanceToEdFi = F, ShowCommentOnCalendar = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarDay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceCalendarDay
	#'
	#' This function returns a dataframe or json object of an AttendanceCalendarDay
	#' @param AttendanceCalendarDayID The ID of the AttendanceCalendarDay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceCalendarDay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceCalendarDay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceCalendarDay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceCalendarDay <- function(AttendanceCalendarDayID, CalendarDayID = F, CalendarID = F, Date = F, DateWithDayOfWeekAbbreviated = F, DayRotationID = F, Comment = F, CountAs = F, AttendanceTerm = F, DayOfTheWeek = F, DayOfTheWeekNumber = F, NumberOfCalendarDayEvents = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BellScheduleID = F, BellScheduleGroupSummary = F, FoodServicePurchaseExists = F, DynamicRelationshipID = F, DoNotSendAttendanceToEdFi = F, ShowCommentOnCalendar = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceCalendarDayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarDay", objectId = AttendanceCalendarDayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceCalendarDay
	#'
	#' This function deletes an AttendanceCalendarDay
	#' @param AttendanceCalendarDayID The ID of the AttendanceCalendarDay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceCalendarDayID of the deleted AttendanceCalendarDay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceCalendarDay <- function(AttendanceCalendarDayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarDay", objectId = AttendanceCalendarDayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceCalendarDay
	#'
	#' This function creates an AttendanceCalendarDay
	#' @param fieldNames The field values to give the created AttendanceCalendarDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceCalendarDay <- function(CalendarID = NULL, Date = NULL, DayRotationID = NULL, Comment = NULL, CountAs = NULL, BellScheduleID = NULL, DoNotSendAttendanceToEdFi = NULL, ShowCommentOnCalendar = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "CalendarDay", body = list(DataObject = body), searchFields = append("CalendarDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceCalendarDay
	#'
	#' This function modifies an AttendanceCalendarDay
	#' @param fieldNames The field values to give the modified AttendanceCalendarDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceCalendarDay <- function(CalendarDayID, CalendarID = NULL, Date = NULL, DayRotationID = NULL, Comment = NULL, CountAs = NULL, BellScheduleID = NULL, DoNotSendAttendanceToEdFi = NULL, ShowCommentOnCalendar = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarDay", objectId = CalendarDayID, body = list(DataObject = body), searchFields = append("CalendarDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of DayRotationPatterns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDayRotationPatterns <- function(searchConditionsList = NULL, DayRotationPatternID = F, EntityGroupKey = F, DayRotationID = F, DayNumber = F, DayRotationPatternIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "DayRotationPattern", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DayRotationPattern
	#'
	#' This function returns a dataframe or json object of a DayRotationPattern
	#' @param DayRotationPatternID The ID of the DayRotationPattern to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DayRotationPattern. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DayRotationPattern.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DayRotationPattern') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of DayRotationPattern
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDayRotationPattern <- function(DayRotationPatternID, EntityGroupKey = F, DayRotationID = F, DayNumber = F, DayRotationPatternIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DayRotationPatternID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "DayRotationPattern", objectId = DayRotationPatternID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DayRotationPattern
	#'
	#' This function deletes a DayRotationPattern
	#' @param DayRotationPatternID The ID of the DayRotationPattern to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The DayRotationPatternID of the deleted DayRotationPattern.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDayRotationPattern <- function(DayRotationPatternID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "DayRotationPattern", objectId = DayRotationPatternID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DayRotationPattern
	#'
	#' This function creates a DayRotationPattern
	#' @param fieldNames The field values to give the created DayRotationPattern. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created DayRotationPattern
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDayRotationPattern <- function(EntityGroupKey = NULL, DayRotationID = NULL, DayNumber = NULL, DayRotationPatternIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "DayRotationPattern", body = list(DataObject = body), searchFields = append("DayRotationPatternID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DayRotationPattern
	#'
	#' This function modifies a DayRotationPattern
	#' @param fieldNames The field values to give the modified DayRotationPattern. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified DayRotationPattern
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDayRotationPattern <- function(DayRotationPatternID, EntityGroupKey = NULL, DayRotationID = NULL, DayNumber = NULL, DayRotationPatternIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "DayRotationPattern", objectId = DayRotationPatternID, body = list(DataObject = body), searchFields = append("DayRotationPatternID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of StudentAttendances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendances <- function(searchConditionsList = NULL, StudentAttendanceID = F, StudentID = F, CalendarDayID = F, IsGuardianNotified = F, Comment = F, DaysExcused = F, DaysUnexcused = F, DaysOther = F, TardyCount = F, DaysAbsent = F, CommentsExistForStudentAttendance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityID = F, SchoolYearID = F, HideRecordMA = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendance
	#'
	#' This function returns a dataframe or json object of a StudentAttendance
	#' @param StudentAttendanceID The ID of the StudentAttendance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendance <- function(StudentAttendanceID, StudentID = F, CalendarDayID = F, IsGuardianNotified = F, Comment = F, DaysExcused = F, DaysUnexcused = F, DaysOther = F, TardyCount = F, DaysAbsent = F, CommentsExistForStudentAttendance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityID = F, SchoolYearID = F, HideRecordMA = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendance", objectId = StudentAttendanceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendance
	#'
	#' This function deletes a StudentAttendance
	#' @param StudentAttendanceID The ID of the StudentAttendance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendanceID of the deleted StudentAttendance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendance <- function(StudentAttendanceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendance", objectId = StudentAttendanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentAttendance
	#'
	#' This function creates a StudentAttendance
	#' @param fieldNames The field values to give the created StudentAttendance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentAttendance <- function(StudentID = NULL, CalendarDayID = NULL, IsGuardianNotified = NULL, Comment = NULL, DaysExcused = NULL, DaysUnexcused = NULL, DaysOther = NULL, TardyCount = NULL, DaysAbsent = NULL, EntityID = NULL, SchoolYearID = NULL, HideRecordMA = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "StudentAttendance", body = list(DataObject = body), searchFields = append("StudentAttendanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentAttendance
	#'
	#' This function modifies a StudentAttendance
	#' @param fieldNames The field values to give the modified StudentAttendance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentAttendance <- function(StudentAttendanceID, StudentID = NULL, CalendarDayID = NULL, IsGuardianNotified = NULL, Comment = NULL, DaysExcused = NULL, DaysUnexcused = NULL, DaysOther = NULL, TardyCount = NULL, DaysAbsent = NULL, EntityID = NULL, SchoolYearID = NULL, HideRecordMA = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "StudentAttendance", objectId = StudentAttendanceID, body = list(DataObject = body), searchFields = append("StudentAttendanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of StudentAttendancePeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendancePeriods <- function(searchConditionsList = NULL, StudentAttendancePeriodID = F, StudentAttendanceID = F, AttendanceTypeID = F, AttendancePeriodID = F, AttendanceReasonID = F, Comment = F, StudentSectionID = F, AttendanceTypeWithReason = F, IncidentOffenseNameActionDetailID = F, CrossWalkedAttendanceTypeWithReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ViewingFromAttendanceEntity = F, EntityIDCourse = F, EntityIDAttendancePeriod = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendancePeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendancePeriod
	#'
	#' This function returns a dataframe or json object of a StudentAttendancePeriod
	#' @param StudentAttendancePeriodID The ID of the StudentAttendancePeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendancePeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendancePeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendancePeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendancePeriod <- function(StudentAttendancePeriodID, StudentAttendanceID = F, AttendanceTypeID = F, AttendancePeriodID = F, AttendanceReasonID = F, Comment = F, StudentSectionID = F, AttendanceTypeWithReason = F, IncidentOffenseNameActionDetailID = F, CrossWalkedAttendanceTypeWithReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ViewingFromAttendanceEntity = F, EntityIDCourse = F, EntityIDAttendancePeriod = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendancePeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendancePeriod", objectId = StudentAttendancePeriodID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendancePeriod
	#'
	#' This function deletes a StudentAttendancePeriod
	#' @param StudentAttendancePeriodID The ID of the StudentAttendancePeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendancePeriodID of the deleted StudentAttendancePeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendancePeriod <- function(StudentAttendancePeriodID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendancePeriod", objectId = StudentAttendancePeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentAttendancePeriod
	#'
	#' This function creates a StudentAttendancePeriod
	#' @param fieldNames The field values to give the created StudentAttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentAttendancePeriod <- function(StudentAttendanceID = NULL, AttendanceTypeID = NULL, AttendancePeriodID = NULL, AttendanceReasonID = NULL, Comment = NULL, StudentSectionID = NULL, IncidentOffenseNameActionDetailID = NULL, EntityIDCourse = NULL, EntityIDAttendancePeriod = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "StudentAttendancePeriod", body = list(DataObject = body), searchFields = append("StudentAttendancePeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentAttendancePeriod
	#'
	#' This function modifies a StudentAttendancePeriod
	#' @param fieldNames The field values to give the modified StudentAttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentAttendancePeriod <- function(StudentAttendancePeriodID, StudentAttendanceID = NULL, AttendanceTypeID = NULL, AttendancePeriodID = NULL, AttendanceReasonID = NULL, Comment = NULL, StudentSectionID = NULL, IncidentOffenseNameActionDetailID = NULL, EntityIDCourse = NULL, EntityIDAttendancePeriod = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "StudentAttendancePeriod", objectId = StudentAttendancePeriodID, body = list(DataObject = body), searchFields = append("StudentAttendancePeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TeacherEntries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTeacherEntries <- function(searchConditionsList = NULL, TeacherEntryID = F, SchoolYearID = F, EntityID = F, EntityGroupKey = F, Label = F, DisplayOrder = F, TeacherEntryIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BackgroundColor = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TeacherEntry", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TeacherEntry
	#'
	#' This function returns a dataframe or json object of a TeacherEntry
	#' @param TeacherEntryID The ID of the TeacherEntry to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TeacherEntry. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TeacherEntry.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TeacherEntry') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TeacherEntry
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTeacherEntry <- function(TeacherEntryID, SchoolYearID = F, EntityID = F, EntityGroupKey = F, Label = F, DisplayOrder = F, TeacherEntryIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BackgroundColor = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TeacherEntryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TeacherEntry", objectId = TeacherEntryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TeacherEntry
	#'
	#' This function deletes a TeacherEntry
	#' @param TeacherEntryID The ID of the TeacherEntry to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TeacherEntryID of the deleted TeacherEntry.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTeacherEntry <- function(TeacherEntryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TeacherEntry", objectId = TeacherEntryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TeacherEntry
	#'
	#' This function creates a TeacherEntry
	#' @param fieldNames The field values to give the created TeacherEntry. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TeacherEntry
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTeacherEntry <- function(SchoolYearID = NULL, EntityID = NULL, EntityGroupKey = NULL, Label = NULL, DisplayOrder = NULL, TeacherEntryIDClonedFrom = NULL, BackgroundColor = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TeacherEntry", body = list(DataObject = body), searchFields = append("TeacherEntryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TeacherEntry
	#'
	#' This function modifies a TeacherEntry
	#' @param fieldNames The field values to give the modified TeacherEntry. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TeacherEntry
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTeacherEntry <- function(TeacherEntryID, SchoolYearID = NULL, EntityID = NULL, EntityGroupKey = NULL, Label = NULL, DisplayOrder = NULL, TeacherEntryIDClonedFrom = NULL, BackgroundColor = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TeacherEntry", objectId = TeacherEntryID, body = list(DataObject = body), searchFields = append("TeacherEntryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of BellSchedulingPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBellSchedulingPeriods <- function(searchConditionsList = NULL, BellSchedulingPeriodID = F, BellScheduleID = F, SchedulingPeriodID = F, StartTime = F, EndTime = F, BellSchedulingPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LengthInMinutes = F, StartTimeWithOverride = F, EndTimeWithOverride = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "BellSchedulingPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BellSchedulingPeriod
	#'
	#' This function returns a dataframe or json object of a BellSchedulingPeriod
	#' @param BellSchedulingPeriodID The ID of the BellSchedulingPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BellSchedulingPeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BellSchedulingPeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BellSchedulingPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of BellSchedulingPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBellSchedulingPeriod <- function(BellSchedulingPeriodID, BellScheduleID = F, SchedulingPeriodID = F, StartTime = F, EndTime = F, BellSchedulingPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LengthInMinutes = F, StartTimeWithOverride = F, EndTimeWithOverride = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BellSchedulingPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "BellSchedulingPeriod", objectId = BellSchedulingPeriodID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BellSchedulingPeriod
	#'
	#' This function deletes a BellSchedulingPeriod
	#' @param BellSchedulingPeriodID The ID of the BellSchedulingPeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The BellSchedulingPeriodID of the deleted BellSchedulingPeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBellSchedulingPeriod <- function(BellSchedulingPeriodID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "BellSchedulingPeriod", objectId = BellSchedulingPeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BellSchedulingPeriod
	#'
	#' This function creates a BellSchedulingPeriod
	#' @param fieldNames The field values to give the created BellSchedulingPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created BellSchedulingPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBellSchedulingPeriod <- function(BellScheduleID = NULL, SchedulingPeriodID = NULL, StartTime = NULL, EndTime = NULL, BellSchedulingPeriodIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "BellSchedulingPeriod", body = list(DataObject = body), searchFields = append("BellSchedulingPeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BellSchedulingPeriod
	#'
	#' This function modifies a BellSchedulingPeriod
	#' @param fieldNames The field values to give the modified BellSchedulingPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified BellSchedulingPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBellSchedulingPeriod <- function(BellSchedulingPeriodID, BellScheduleID = NULL, SchedulingPeriodID = NULL, StartTime = NULL, EndTime = NULL, BellSchedulingPeriodIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "BellSchedulingPeriod", objectId = BellSchedulingPeriodID, body = list(DataObject = body), searchFields = append("BellSchedulingPeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of BellSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBellSchedules <- function(searchConditionsList = NULL, BellScheduleID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, CodeDescription = F, IsDefault = F, BellScheduleIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "BellSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BellSchedule
	#'
	#' This function returns a dataframe or json object of a BellSchedule
	#' @param BellScheduleID The ID of the BellSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BellSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BellSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BellSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of BellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBellSchedule <- function(BellScheduleID, EntityID = F, SchoolYearID = F, Code = F, Description = F, CodeDescription = F, IsDefault = F, BellScheduleIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BellScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "BellSchedule", objectId = BellScheduleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BellSchedule
	#'
	#' This function deletes a BellSchedule
	#' @param BellScheduleID The ID of the BellSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The BellScheduleID of the deleted BellSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBellSchedule <- function(BellScheduleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "BellSchedule", objectId = BellScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BellSchedule
	#'
	#' This function creates a BellSchedule
	#' @param fieldNames The field values to give the created BellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created BellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBellSchedule <- function(EntityID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, IsDefault = NULL, BellScheduleIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "BellSchedule", body = list(DataObject = body), searchFields = append("BellScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BellSchedule
	#'
	#' This function modifies a BellSchedule
	#' @param fieldNames The field values to give the modified BellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified BellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBellSchedule <- function(BellScheduleID, EntityID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, IsDefault = NULL, BellScheduleIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "BellSchedule", objectId = BellScheduleID, body = list(DataObject = body), searchFields = append("BellScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempCalendarAttendanceTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalendarAttendanceTerms <- function(searchConditionsList = NULL, TempCalendarAttendanceTermID = F, AttendanceTermID = F, CalendarID = F, Code = F, CodeDescription = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, TableType = F, TableTypeString = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCalendarAttendanceTerm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCalendarAttendanceTerm
	#'
	#' This function returns a dataframe or json object of a TempCalendarAttendanceTerm
	#' @param TempCalendarAttendanceTermID The ID of the TempCalendarAttendanceTerm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarAttendanceTerm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarAttendanceTerm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarAttendanceTerm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCalendarAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalendarAttendanceTerm <- function(TempCalendarAttendanceTermID, AttendanceTermID = F, CalendarID = F, Code = F, CodeDescription = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, TableType = F, TableTypeString = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalendarAttendanceTermID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCalendarAttendanceTerm", objectId = TempCalendarAttendanceTermID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCalendarAttendanceTerm
	#'
	#' This function deletes a TempCalendarAttendanceTerm
	#' @param TempCalendarAttendanceTermID The ID of the TempCalendarAttendanceTerm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCalendarAttendanceTermID of the deleted TempCalendarAttendanceTerm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCalendarAttendanceTerm <- function(TempCalendarAttendanceTermID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCalendarAttendanceTerm", objectId = TempCalendarAttendanceTermID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCalendarAttendanceTerm
	#'
	#' This function creates a TempCalendarAttendanceTerm
	#' @param fieldNames The field values to give the created TempCalendarAttendanceTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCalendarAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCalendarAttendanceTerm <- function(AttendanceTermID = NULL, CalendarID = NULL, Code = NULL, CodeDescription = NULL, StartDate = NULL, EndDate = NULL, OriginalStartDate = NULL, OriginalEndDate = NULL, TableType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempCalendarAttendanceTerm", body = list(DataObject = body), searchFields = append("TempCalendarAttendanceTermID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCalendarAttendanceTerm
	#'
	#' This function modifies a TempCalendarAttendanceTerm
	#' @param fieldNames The field values to give the modified TempCalendarAttendanceTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCalendarAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCalendarAttendanceTerm <- function(TempCalendarAttendanceTermID, AttendanceTermID = NULL, CalendarID = NULL, Code = NULL, CodeDescription = NULL, StartDate = NULL, EndDate = NULL, OriginalStartDate = NULL, OriginalEndDate = NULL, TableType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempCalendarAttendanceTerm", objectId = TempCalendarAttendanceTermID, body = list(DataObject = body), searchFields = append("TempCalendarAttendanceTermID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempCalendarDayCalendarEventRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalendarDayCalendarEventRecords <- function(searchConditionsList = NULL, TempCalendarDayCalendarEventRecordID = F, CalendarDayID = F, CalendarEventID = F, CalendarEvent = F, Date = F, Calendar = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCalendarDayCalendarEventRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCalendarDayCalendarEventRecord
	#'
	#' This function returns a dataframe or json object of a TempCalendarDayCalendarEventRecord
	#' @param TempCalendarDayCalendarEventRecordID The ID of the TempCalendarDayCalendarEventRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarDayCalendarEventRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarDayCalendarEventRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarDayCalendarEventRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCalendarDayCalendarEventRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalendarDayCalendarEventRecord <- function(TempCalendarDayCalendarEventRecordID, CalendarDayID = F, CalendarEventID = F, CalendarEvent = F, Date = F, Calendar = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalendarDayCalendarEventRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCalendarDayCalendarEventRecord", objectId = TempCalendarDayCalendarEventRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCalendarDayCalendarEventRecord
	#'
	#' This function deletes a TempCalendarDayCalendarEventRecord
	#' @param TempCalendarDayCalendarEventRecordID The ID of the TempCalendarDayCalendarEventRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCalendarDayCalendarEventRecordID of the deleted TempCalendarDayCalendarEventRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCalendarDayCalendarEventRecord <- function(TempCalendarDayCalendarEventRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCalendarDayCalendarEventRecord", objectId = TempCalendarDayCalendarEventRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCalendarDayCalendarEventRecord
	#'
	#' This function creates a TempCalendarDayCalendarEventRecord
	#' @param fieldNames The field values to give the created TempCalendarDayCalendarEventRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCalendarDayCalendarEventRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCalendarDayCalendarEventRecord <- function(CalendarDayID = NULL, CalendarEventID = NULL, CalendarEvent = NULL, Date = NULL, Calendar = NULL, FailureReason = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempCalendarDayCalendarEventRecord", body = list(DataObject = body), searchFields = append("TempCalendarDayCalendarEventRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCalendarDayCalendarEventRecord
	#'
	#' This function modifies a TempCalendarDayCalendarEventRecord
	#' @param fieldNames The field values to give the modified TempCalendarDayCalendarEventRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCalendarDayCalendarEventRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCalendarDayCalendarEventRecord <- function(TempCalendarDayCalendarEventRecordID, CalendarDayID = NULL, CalendarEventID = NULL, CalendarEvent = NULL, Date = NULL, Calendar = NULL, FailureReason = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempCalendarDayCalendarEventRecord", objectId = TempCalendarDayCalendarEventRecordID, body = list(DataObject = body), searchFields = append("TempCalendarDayCalendarEventRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of StudentThresholdPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentThresholdPeriods <- function(searchConditionsList = NULL, StudentThresholdPeriodID = F, StudentDisciplineThresholdAttendanceReportRunHistoryID = F, StudentAttendancePeriodID = F, StudentSectionID = F, SectionID = F, AttendancePeriodID = F, AttendanceTypeID = F, CalendarDayID = F, Date = F, CountsTowardsThreshold = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentThresholdPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentThresholdPeriod
	#'
	#' This function returns a dataframe or json object of a StudentThresholdPeriod
	#' @param StudentThresholdPeriodID The ID of the StudentThresholdPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentThresholdPeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentThresholdPeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentThresholdPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentThresholdPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentThresholdPeriod <- function(StudentThresholdPeriodID, StudentDisciplineThresholdAttendanceReportRunHistoryID = F, StudentAttendancePeriodID = F, StudentSectionID = F, SectionID = F, AttendancePeriodID = F, AttendanceTypeID = F, CalendarDayID = F, Date = F, CountsTowardsThreshold = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentThresholdPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentThresholdPeriod", objectId = StudentThresholdPeriodID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentThresholdPeriod
	#'
	#' This function deletes a StudentThresholdPeriod
	#' @param StudentThresholdPeriodID The ID of the StudentThresholdPeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentThresholdPeriodID of the deleted StudentThresholdPeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentThresholdPeriod <- function(StudentThresholdPeriodID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentThresholdPeriod", objectId = StudentThresholdPeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentThresholdPeriod
	#'
	#' This function creates a StudentThresholdPeriod
	#' @param fieldNames The field values to give the created StudentThresholdPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentThresholdPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentThresholdPeriod <- function(StudentDisciplineThresholdAttendanceReportRunHistoryID = NULL, StudentAttendancePeriodID = NULL, StudentSectionID = NULL, SectionID = NULL, AttendancePeriodID = NULL, AttendanceTypeID = NULL, CalendarDayID = NULL, Date = NULL, CountsTowardsThreshold = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "StudentThresholdPeriod", body = list(DataObject = body), searchFields = append("StudentThresholdPeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentThresholdPeriod
	#'
	#' This function modifies a StudentThresholdPeriod
	#' @param fieldNames The field values to give the modified StudentThresholdPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentThresholdPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentThresholdPeriod <- function(StudentThresholdPeriodID, StudentDisciplineThresholdAttendanceReportRunHistoryID = NULL, StudentAttendancePeriodID = NULL, StudentSectionID = NULL, SectionID = NULL, AttendancePeriodID = NULL, AttendanceTypeID = NULL, CalendarDayID = NULL, Date = NULL, CountsTowardsThreshold = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "StudentThresholdPeriod", objectId = StudentThresholdPeriodID, body = list(DataObject = body), searchFields = append("StudentThresholdPeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of StudentDisciplineThresholdAttendanceReportRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentDisciplineThresholdAttendanceReportRunHistories <- function(searchConditionsList = NULL, StudentDisciplineThresholdAttendanceReportRunHistoryID = F, StudentID = F, AttendanceReportRunHistoryID = F, DisciplineThresholdID = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentID = F, Header = F, Body = F, Footer = F, HeaderForReport = F, BodyForReport = F, FooterForReport = F, AttachmentDisplayName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentDisciplineThresholdAttendanceReportRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentDisciplineThresholdAttendanceReportRunHistory
	#'
	#' This function returns a dataframe or json object of a StudentDisciplineThresholdAttendanceReportRunHistory
	#' @param StudentDisciplineThresholdAttendanceReportRunHistoryID The ID of the StudentDisciplineThresholdAttendanceReportRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentDisciplineThresholdAttendanceReportRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentDisciplineThresholdAttendanceReportRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentDisciplineThresholdAttendanceReportRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentDisciplineThresholdAttendanceReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentDisciplineThresholdAttendanceReportRunHistory <- function(StudentDisciplineThresholdAttendanceReportRunHistoryID, StudentID = F, AttendanceReportRunHistoryID = F, DisciplineThresholdID = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentID = F, Header = F, Body = F, Footer = F, HeaderForReport = F, BodyForReport = F, FooterForReport = F, AttachmentDisplayName = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentDisciplineThresholdAttendanceReportRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentDisciplineThresholdAttendanceReportRunHistory", objectId = StudentDisciplineThresholdAttendanceReportRunHistoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentDisciplineThresholdAttendanceReportRunHistory
	#'
	#' This function deletes a StudentDisciplineThresholdAttendanceReportRunHistory
	#' @param StudentDisciplineThresholdAttendanceReportRunHistoryID The ID of the StudentDisciplineThresholdAttendanceReportRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentDisciplineThresholdAttendanceReportRunHistoryID of the deleted StudentDisciplineThresholdAttendanceReportRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentDisciplineThresholdAttendanceReportRunHistory <- function(StudentDisciplineThresholdAttendanceReportRunHistoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentDisciplineThresholdAttendanceReportRunHistory", objectId = StudentDisciplineThresholdAttendanceReportRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentDisciplineThresholdAttendanceReportRunHistory
	#'
	#' This function creates a StudentDisciplineThresholdAttendanceReportRunHistory
	#' @param fieldNames The field values to give the created StudentDisciplineThresholdAttendanceReportRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentDisciplineThresholdAttendanceReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentDisciplineThresholdAttendanceReportRunHistory <- function(StudentID = NULL, AttendanceReportRunHistoryID = NULL, DisciplineThresholdID = NULL, IsActive = NULL, AttachmentID = NULL, Header = NULL, Body = NULL, Footer = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "StudentDisciplineThresholdAttendanceReportRunHistory", body = list(DataObject = body), searchFields = append("StudentDisciplineThresholdAttendanceReportRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentDisciplineThresholdAttendanceReportRunHistory
	#'
	#' This function modifies a StudentDisciplineThresholdAttendanceReportRunHistory
	#' @param fieldNames The field values to give the modified StudentDisciplineThresholdAttendanceReportRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentDisciplineThresholdAttendanceReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentDisciplineThresholdAttendanceReportRunHistory <- function(StudentDisciplineThresholdAttendanceReportRunHistoryID, StudentID = NULL, AttendanceReportRunHistoryID = NULL, DisciplineThresholdID = NULL, IsActive = NULL, AttachmentID = NULL, Header = NULL, Body = NULL, Footer = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "StudentDisciplineThresholdAttendanceReportRunHistory", objectId = StudentDisciplineThresholdAttendanceReportRunHistoryID, body = list(DataObject = body), searchFields = append("StudentDisciplineThresholdAttendanceReportRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of AttendanceReportRunHistoryThresholdResetRanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceReportRunHistoryThresholdResetRanges <- function(searchConditionsList = NULL, AttendanceReportRunHistoryThresholdResetRangeID = F, AttendanceReportRunHistoryID = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendanceReportRunHistoryThresholdResetRange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceReportRunHistoryThresholdResetRange
	#'
	#' This function returns a dataframe or json object of an AttendanceReportRunHistoryThresholdResetRange
	#' @param AttendanceReportRunHistoryThresholdResetRangeID The ID of the AttendanceReportRunHistoryThresholdResetRange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceReportRunHistoryThresholdResetRange. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceReportRunHistoryThresholdResetRange.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceReportRunHistoryThresholdResetRange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceReportRunHistoryThresholdResetRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceReportRunHistoryThresholdResetRange <- function(AttendanceReportRunHistoryThresholdResetRangeID, AttendanceReportRunHistoryID = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceReportRunHistoryThresholdResetRangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendanceReportRunHistoryThresholdResetRange", objectId = AttendanceReportRunHistoryThresholdResetRangeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceReportRunHistoryThresholdResetRange
	#'
	#' This function deletes an AttendanceReportRunHistoryThresholdResetRange
	#' @param AttendanceReportRunHistoryThresholdResetRangeID The ID of the AttendanceReportRunHistoryThresholdResetRange to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceReportRunHistoryThresholdResetRangeID of the deleted AttendanceReportRunHistoryThresholdResetRange.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceReportRunHistoryThresholdResetRange <- function(AttendanceReportRunHistoryThresholdResetRangeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendanceReportRunHistoryThresholdResetRange", objectId = AttendanceReportRunHistoryThresholdResetRangeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceReportRunHistoryThresholdResetRange
	#'
	#' This function creates an AttendanceReportRunHistoryThresholdResetRange
	#' @param fieldNames The field values to give the created AttendanceReportRunHistoryThresholdResetRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceReportRunHistoryThresholdResetRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceReportRunHistoryThresholdResetRange <- function(AttendanceReportRunHistoryID = NULL, ThresholdResetRangeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "AttendanceReportRunHistoryThresholdResetRange", body = list(DataObject = body), searchFields = append("AttendanceReportRunHistoryThresholdResetRangeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceReportRunHistoryThresholdResetRange
	#'
	#' This function modifies an AttendanceReportRunHistoryThresholdResetRange
	#' @param fieldNames The field values to give the modified AttendanceReportRunHistoryThresholdResetRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceReportRunHistoryThresholdResetRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceReportRunHistoryThresholdResetRange <- function(AttendanceReportRunHistoryThresholdResetRangeID, AttendanceReportRunHistoryID = NULL, ThresholdResetRangeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "AttendanceReportRunHistoryThresholdResetRange", objectId = AttendanceReportRunHistoryThresholdResetRangeID, body = list(DataObject = body), searchFields = append("AttendanceReportRunHistoryThresholdResetRangeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of ThresholdResetRangeAttendancePeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThresholdResetRangeAttendancePeriods <- function(searchConditionsList = NULL, ThresholdResetRangeAttendancePeriodID = F, AttendancePeriodID = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "ThresholdResetRangeAttendancePeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThresholdResetRangeAttendancePeriod
	#'
	#' This function returns a dataframe or json object of a ThresholdResetRangeAttendancePeriod
	#' @param ThresholdResetRangeAttendancePeriodID The ID of the ThresholdResetRangeAttendancePeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdResetRangeAttendancePeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdResetRangeAttendancePeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdResetRangeAttendancePeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of ThresholdResetRangeAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThresholdResetRangeAttendancePeriod <- function(ThresholdResetRangeAttendancePeriodID, AttendancePeriodID = F, ThresholdResetRangeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThresholdResetRangeAttendancePeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendancePeriod", objectId = ThresholdResetRangeAttendancePeriodID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThresholdResetRangeAttendancePeriod
	#'
	#' This function deletes a ThresholdResetRangeAttendancePeriod
	#' @param ThresholdResetRangeAttendancePeriodID The ID of the ThresholdResetRangeAttendancePeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The ThresholdResetRangeAttendancePeriodID of the deleted ThresholdResetRangeAttendancePeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThresholdResetRangeAttendancePeriod <- function(ThresholdResetRangeAttendancePeriodID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendancePeriod", objectId = ThresholdResetRangeAttendancePeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThresholdResetRangeAttendancePeriod
	#'
	#' This function creates a ThresholdResetRangeAttendancePeriod
	#' @param fieldNames The field values to give the created ThresholdResetRangeAttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created ThresholdResetRangeAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThresholdResetRangeAttendancePeriod <- function(AttendancePeriodID = NULL, ThresholdResetRangeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendancePeriod", body = list(DataObject = body), searchFields = append("ThresholdResetRangeAttendancePeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThresholdResetRangeAttendancePeriod
	#'
	#' This function modifies a ThresholdResetRangeAttendancePeriod
	#' @param fieldNames The field values to give the modified ThresholdResetRangeAttendancePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified ThresholdResetRangeAttendancePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThresholdResetRangeAttendancePeriod <- function(ThresholdResetRangeAttendancePeriodID, AttendancePeriodID = NULL, ThresholdResetRangeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "ThresholdResetRangeAttendancePeriod", objectId = ThresholdResetRangeAttendancePeriodID, body = list(DataObject = body), searchFields = append("ThresholdResetRangeAttendancePeriodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempStudentThresholdPeriodRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentThresholdPeriodRecords <- function(searchConditionsList = NULL, TempStudentThresholdPeriodRecordID = F, TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID = F, StudentAttendancePeriodID = F, AttendanceTypeID = F, CalendarDayID = F, AttendancePeriodID = F, StudentSectionID = F, SectionID = F, Date = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempStudentThresholdPeriodRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentThresholdPeriodRecord
	#'
	#' This function returns a dataframe or json object of a TempStudentThresholdPeriodRecord
	#' @param TempStudentThresholdPeriodRecordID The ID of the TempStudentThresholdPeriodRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentThresholdPeriodRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentThresholdPeriodRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentThresholdPeriodRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempStudentThresholdPeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentThresholdPeriodRecord <- function(TempStudentThresholdPeriodRecordID, TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID = F, StudentAttendancePeriodID = F, AttendanceTypeID = F, CalendarDayID = F, AttendancePeriodID = F, StudentSectionID = F, SectionID = F, Date = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentThresholdPeriodRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempStudentThresholdPeriodRecord", objectId = TempStudentThresholdPeriodRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentThresholdPeriodRecord
	#'
	#' This function deletes a TempStudentThresholdPeriodRecord
	#' @param TempStudentThresholdPeriodRecordID The ID of the TempStudentThresholdPeriodRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempStudentThresholdPeriodRecordID of the deleted TempStudentThresholdPeriodRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentThresholdPeriodRecord <- function(TempStudentThresholdPeriodRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempStudentThresholdPeriodRecord", objectId = TempStudentThresholdPeriodRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentThresholdPeriodRecord
	#'
	#' This function creates a TempStudentThresholdPeriodRecord
	#' @param fieldNames The field values to give the created TempStudentThresholdPeriodRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempStudentThresholdPeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentThresholdPeriodRecord <- function(TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID = NULL, StudentAttendancePeriodID = NULL, AttendanceTypeID = NULL, CalendarDayID = NULL, AttendancePeriodID = NULL, StudentSectionID = NULL, SectionID = NULL, Date = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempStudentThresholdPeriodRecord", body = list(DataObject = body), searchFields = append("TempStudentThresholdPeriodRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentThresholdPeriodRecord
	#'
	#' This function modifies a TempStudentThresholdPeriodRecord
	#' @param fieldNames The field values to give the modified TempStudentThresholdPeriodRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempStudentThresholdPeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentThresholdPeriodRecord <- function(TempStudentThresholdPeriodRecordID, TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID = NULL, StudentAttendancePeriodID = NULL, AttendanceTypeID = NULL, CalendarDayID = NULL, AttendancePeriodID = NULL, StudentSectionID = NULL, SectionID = NULL, Date = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempStudentThresholdPeriodRecord", objectId = TempStudentThresholdPeriodRecordID, body = list(DataObject = body), searchFields = append("TempStudentThresholdPeriodRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempStudentDisciplineThresholdAttendanceReportRunHistoryRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentDisciplineThresholdAttendanceReportRunHistoryRecords <- function(searchConditionsList = NULL, TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID = F, StudentID = F, StudentName = F, DisciplineThresholdID = F, DateLow = F, DateHigh = F, ThresholdValue = F, ResetRangeAttendanceTypes = F, CountType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfDays = F, DateType = F, DayCountType = F, AttachmentDisplayName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#'
	#' This function returns a dataframe or json object of a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' @param TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID The ID of the TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentDisciplineThresholdAttendanceReportRunHistoryRecord <- function(TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID, StudentID = F, StudentName = F, DisciplineThresholdID = F, DateLow = F, DateHigh = F, ThresholdValue = F, ResetRangeAttendanceTypes = F, CountType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfDays = F, DateType = F, DayCountType = F, AttachmentDisplayName = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord", objectId = TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#'
	#' This function deletes a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' @param TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID The ID of the TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID of the deleted TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentDisciplineThresholdAttendanceReportRunHistoryRecord <- function(TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord", objectId = TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#'
	#' This function creates a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' @param fieldNames The field values to give the created TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentDisciplineThresholdAttendanceReportRunHistoryRecord <- function(StudentID = NULL, StudentName = NULL, DisciplineThresholdID = NULL, DateLow = NULL, DateHigh = NULL, ThresholdValue = NULL, ResetRangeAttendanceTypes = NULL, CountType = NULL, NumberOfDays = NULL, DateType = NULL, DayCountType = NULL, AttachmentDisplayName = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord", body = list(DataObject = body), searchFields = append("TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#'
	#' This function modifies a TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' @param fieldNames The field values to give the modified TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentDisciplineThresholdAttendanceReportRunHistoryRecord <- function(TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID, StudentID = NULL, StudentName = NULL, DisciplineThresholdID = NULL, DateLow = NULL, DateHigh = NULL, ThresholdValue = NULL, ResetRangeAttendanceTypes = NULL, CountType = NULL, NumberOfDays = NULL, DateType = NULL, DayCountType = NULL, AttachmentDisplayName = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempStudentDisciplineThresholdAttendanceReportRunHistoryRecord", objectId = TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID, body = list(DataObject = body), searchFields = append("TempStudentDisciplineThresholdAttendanceReportRunHistoryRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of AttendanceReportRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendanceReportRunHistories <- function(searchConditionsList = NULL, AttendanceReportRunHistoryID = F, EntityID = F, SchoolYearID = F, RunDescription = F, ParameterData = F, ParameterDescription = F, Date = F, FilterType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GracePeriod = F, IsActive = F, CountType = F, ReportRunInfoID = F, PostToFASA = F, AttachmentDisplayName = F, PrintAttendanceLetterForWindowedEnvelope = F, FiscalYearID = F, CachedFiscalYear = F, SectionID = F, EntityIDList = F, SchoolYearNumericYearOrCurrent = F, CachedSchoolYear = F, CachedEntity = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendanceReportRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendanceReportRunHistory
	#'
	#' This function returns a dataframe or json object of an AttendanceReportRunHistory
	#' @param AttendanceReportRunHistoryID The ID of the AttendanceReportRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendanceReportRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendanceReportRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendanceReportRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendanceReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendanceReportRunHistory <- function(AttendanceReportRunHistoryID, EntityID = F, SchoolYearID = F, RunDescription = F, ParameterData = F, ParameterDescription = F, Date = F, FilterType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GracePeriod = F, IsActive = F, CountType = F, ReportRunInfoID = F, PostToFASA = F, AttachmentDisplayName = F, PrintAttendanceLetterForWindowedEnvelope = F, FiscalYearID = F, CachedFiscalYear = F, SectionID = F, EntityIDList = F, SchoolYearNumericYearOrCurrent = F, CachedSchoolYear = F, CachedEntity = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendanceReportRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendanceReportRunHistory", objectId = AttendanceReportRunHistoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendanceReportRunHistory
	#'
	#' This function deletes an AttendanceReportRunHistory
	#' @param AttendanceReportRunHistoryID The ID of the AttendanceReportRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendanceReportRunHistoryID of the deleted AttendanceReportRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendanceReportRunHistory <- function(AttendanceReportRunHistoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendanceReportRunHistory", objectId = AttendanceReportRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendanceReportRunHistory
	#'
	#' This function creates an AttendanceReportRunHistory
	#' @param fieldNames The field values to give the created AttendanceReportRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendanceReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendanceReportRunHistory <- function(EntityID = NULL, SchoolYearID = NULL, RunDescription = NULL, Date = NULL, FilterType = NULL, GracePeriod = NULL, IsActive = NULL, ReportRunInfoID = NULL, PostToFASA = NULL, AttachmentDisplayName = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "AttendanceReportRunHistory", body = list(DataObject = body), searchFields = append("AttendanceReportRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendanceReportRunHistory
	#'
	#' This function modifies an AttendanceReportRunHistory
	#' @param fieldNames The field values to give the modified AttendanceReportRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendanceReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendanceReportRunHistory <- function(AttendanceReportRunHistoryID, EntityID = NULL, SchoolYearID = NULL, RunDescription = NULL, Date = NULL, FilterType = NULL, GracePeriod = NULL, IsActive = NULL, ReportRunInfoID = NULL, PostToFASA = NULL, AttachmentDisplayName = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "AttendanceReportRunHistory", objectId = AttendanceReportRunHistoryID, body = list(DataObject = body), searchFields = append("AttendanceReportRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of StudentInOutTimes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentInOutTimes <- function(searchConditionsList = NULL, StudentInOutTimeID = F, StudentAttendanceID = F, TimeIn = F, TimeOut = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MinutesPresent = F, PeriodTimes = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentInOutTime", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentInOutTime
	#'
	#' This function returns a dataframe or json object of a StudentInOutTime
	#' @param StudentInOutTimeID The ID of the StudentInOutTime to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentInOutTime. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentInOutTime.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentInOutTime') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentInOutTime
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentInOutTime <- function(StudentInOutTimeID, StudentAttendanceID = F, TimeIn = F, TimeOut = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MinutesPresent = F, PeriodTimes = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentInOutTimeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentInOutTime", objectId = StudentInOutTimeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentInOutTime
	#'
	#' This function deletes a StudentInOutTime
	#' @param StudentInOutTimeID The ID of the StudentInOutTime to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentInOutTimeID of the deleted StudentInOutTime.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentInOutTime <- function(StudentInOutTimeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentInOutTime", objectId = StudentInOutTimeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentInOutTime
	#'
	#' This function creates a StudentInOutTime
	#' @param fieldNames The field values to give the created StudentInOutTime. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentInOutTime
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentInOutTime <- function(StudentAttendanceID = NULL, TimeIn = NULL, TimeOut = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "StudentInOutTime", body = list(DataObject = body), searchFields = append("StudentInOutTimeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentInOutTime
	#'
	#' This function modifies a StudentInOutTime
	#' @param fieldNames The field values to give the modified StudentInOutTime. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentInOutTime
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentInOutTime <- function(StudentInOutTimeID, StudentAttendanceID = NULL, TimeIn = NULL, TimeOut = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "StudentInOutTime", objectId = StudentInOutTimeID, body = list(DataObject = body), searchFields = append("StudentInOutTimeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of StudentAttendancePeriodGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendancePeriodGroups <- function(searchConditionsList = NULL, StudentAttendanceID = F, StudentAttendancePeriodID = F, AttendancePeriodID = F, StudentID = F, EntityID = F, SchoolYearID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendancePeriodGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendancePeriodGroup
	#'
	#' This function returns a dataframe or json object of a StudentAttendancePeriodGroup
	#' @param StudentAttendancePeriodGroupID The ID of the StudentAttendancePeriodGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendancePeriodGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendancePeriodGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendancePeriodGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendancePeriodGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendancePeriodGroup <- function(StudentAttendancePeriodGroupID, StudentAttendanceID = F, StudentAttendancePeriodID = F, AttendancePeriodID = F, StudentID = F, EntityID = F, SchoolYearID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendancePeriodGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendancePeriodGroup", objectId = StudentAttendancePeriodGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendancePeriodGroup
	#'
	#' This function deletes a StudentAttendancePeriodGroup
	#' @param StudentAttendancePeriodGroupID The ID of the StudentAttendancePeriodGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendancePeriodGroupID of the deleted StudentAttendancePeriodGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendancePeriodGroup <- function(StudentAttendancePeriodGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendancePeriodGroup", objectId = StudentAttendancePeriodGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of SeatingChartUsedLasts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSeatingChartUsedLasts <- function(searchConditionsList = NULL, SeatingChartUsedLastID = F, StaffID = F, RoomID = F, DisplayPeriodID = F, SeatingChartID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "SeatingChartUsedLast", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SeatingChartUsedLast
	#'
	#' This function returns a dataframe or json object of a SeatingChartUsedLast
	#' @param SeatingChartUsedLastID The ID of the SeatingChartUsedLast to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SeatingChartUsedLast. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SeatingChartUsedLast.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SeatingChartUsedLast') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of SeatingChartUsedLast
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSeatingChartUsedLast <- function(SeatingChartUsedLastID, StaffID = F, RoomID = F, DisplayPeriodID = F, SeatingChartID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SeatingChartUsedLastID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "SeatingChartUsedLast", objectId = SeatingChartUsedLastID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SeatingChartUsedLast
	#'
	#' This function deletes a SeatingChartUsedLast
	#' @param SeatingChartUsedLastID The ID of the SeatingChartUsedLast to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The SeatingChartUsedLastID of the deleted SeatingChartUsedLast.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSeatingChartUsedLast <- function(SeatingChartUsedLastID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "SeatingChartUsedLast", objectId = SeatingChartUsedLastID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SeatingChartUsedLast
	#'
	#' This function creates a SeatingChartUsedLast
	#' @param fieldNames The field values to give the created SeatingChartUsedLast. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created SeatingChartUsedLast
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSeatingChartUsedLast <- function(StaffID = NULL, RoomID = NULL, DisplayPeriodID = NULL, SeatingChartID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "SeatingChartUsedLast", body = list(DataObject = body), searchFields = append("SeatingChartUsedLastID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SeatingChartUsedLast
	#'
	#' This function modifies a SeatingChartUsedLast
	#' @param fieldNames The field values to give the modified SeatingChartUsedLast. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified SeatingChartUsedLast
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySeatingChartUsedLast <- function(SeatingChartUsedLastID, StaffID = NULL, RoomID = NULL, DisplayPeriodID = NULL, SeatingChartID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "SeatingChartUsedLast", objectId = SeatingChartUsedLastID, body = list(DataObject = body), searchFields = append("SeatingChartUsedLastID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempCloneCalendarRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCloneCalendarRecords <- function(searchConditionsList = NULL, TempCloneCalendarRecordID = F, AffectedPrimaryKey = F, EntityID = F, Entity = F, SchoolYearID = F, Code = F, Description = F, StartDate = F, EndDate = F, IsDefault = F, DefaultDayLengthMinutes = F, AttendanceCalculationMethod = F, ZeroDayHighPeriodCount = F, HalfDayHighPeriodCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCloneCalendarRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCloneCalendarRecord
	#'
	#' This function returns a dataframe or json object of a TempCloneCalendarRecord
	#' @param TempCloneCalendarRecordID The ID of the TempCloneCalendarRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCloneCalendarRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCloneCalendarRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCloneCalendarRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCloneCalendarRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCloneCalendarRecord <- function(TempCloneCalendarRecordID, AffectedPrimaryKey = F, EntityID = F, Entity = F, SchoolYearID = F, Code = F, Description = F, StartDate = F, EndDate = F, IsDefault = F, DefaultDayLengthMinutes = F, AttendanceCalculationMethod = F, ZeroDayHighPeriodCount = F, HalfDayHighPeriodCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCloneCalendarRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCloneCalendarRecord", objectId = TempCloneCalendarRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCloneCalendarRecord
	#'
	#' This function deletes a TempCloneCalendarRecord
	#' @param TempCloneCalendarRecordID The ID of the TempCloneCalendarRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCloneCalendarRecordID of the deleted TempCloneCalendarRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCloneCalendarRecord <- function(TempCloneCalendarRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCloneCalendarRecord", objectId = TempCloneCalendarRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCloneCalendarRecord
	#'
	#' This function creates a TempCloneCalendarRecord
	#' @param fieldNames The field values to give the created TempCloneCalendarRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCloneCalendarRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCloneCalendarRecord <- function(AffectedPrimaryKey = NULL, EntityID = NULL, Entity = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, StartDate = NULL, EndDate = NULL, IsDefault = NULL, DefaultDayLengthMinutes = NULL, AttendanceCalculationMethod = NULL, ZeroDayHighPeriodCount = NULL, HalfDayHighPeriodCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempCloneCalendarRecord", body = list(DataObject = body), searchFields = append("TempCloneCalendarRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCloneCalendarRecord
	#'
	#' This function modifies a TempCloneCalendarRecord
	#' @param fieldNames The field values to give the modified TempCloneCalendarRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCloneCalendarRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCloneCalendarRecord <- function(TempCloneCalendarRecordID, AffectedPrimaryKey = NULL, EntityID = NULL, Entity = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, StartDate = NULL, EndDate = NULL, IsDefault = NULL, DefaultDayLengthMinutes = NULL, AttendanceCalculationMethod = NULL, ZeroDayHighPeriodCount = NULL, HalfDayHighPeriodCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempCloneCalendarRecord", objectId = TempCloneCalendarRecordID, body = list(DataObject = body), searchFields = append("TempCloneCalendarRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempCloneCalendarErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCloneCalendarErrors <- function(searchConditionsList = NULL, TempCloneCalendarErrorID = F, EntityName = F, RecordType = F, Description = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCloneCalendarError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCloneCalendarError
	#'
	#' This function returns a dataframe or json object of a TempCloneCalendarError
	#' @param TempCloneCalendarErrorID The ID of the TempCloneCalendarError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCloneCalendarError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCloneCalendarError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCloneCalendarError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCloneCalendarError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCloneCalendarError <- function(TempCloneCalendarErrorID, EntityName = F, RecordType = F, Description = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCloneCalendarErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCloneCalendarError", objectId = TempCloneCalendarErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCloneCalendarError
	#'
	#' This function deletes a TempCloneCalendarError
	#' @param TempCloneCalendarErrorID The ID of the TempCloneCalendarError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCloneCalendarErrorID of the deleted TempCloneCalendarError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCloneCalendarError <- function(TempCloneCalendarErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCloneCalendarError", objectId = TempCloneCalendarErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCloneCalendarError
	#'
	#' This function creates a TempCloneCalendarError
	#' @param fieldNames The field values to give the created TempCloneCalendarError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCloneCalendarError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCloneCalendarError <- function(EntityName = NULL, RecordType = NULL, Description = NULL, FailureReason = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempCloneCalendarError", body = list(DataObject = body), searchFields = append("TempCloneCalendarErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCloneCalendarError
	#'
	#' This function modifies a TempCloneCalendarError
	#' @param fieldNames The field values to give the modified TempCloneCalendarError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCloneCalendarError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCloneCalendarError <- function(TempCloneCalendarErrorID, EntityName = NULL, RecordType = NULL, Description = NULL, FailureReason = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempCloneCalendarError", objectId = TempCloneCalendarErrorID, body = list(DataObject = body), searchFields = append("TempCloneCalendarErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempAttendanceTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAttendanceTerms <- function(searchConditionsList = NULL, TempAttendanceTermID = F, AttendanceTermID = F, AttendanceTermCode = F, CalendarID = F, CalendarCode = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, IsUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalendarStartDate = F, CalendarEndDate = F, ProcessAction = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempAttendanceTerm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAttendanceTerm
	#'
	#' This function returns a dataframe or json object of a TempAttendanceTerm
	#' @param TempAttendanceTermID The ID of the TempAttendanceTerm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAttendanceTerm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAttendanceTerm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAttendanceTerm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAttendanceTerm <- function(TempAttendanceTermID, AttendanceTermID = F, AttendanceTermCode = F, CalendarID = F, CalendarCode = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, IsUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CalendarStartDate = F, CalendarEndDate = F, ProcessAction = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAttendanceTermID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempAttendanceTerm", objectId = TempAttendanceTermID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAttendanceTerm
	#'
	#' This function deletes a TempAttendanceTerm
	#' @param TempAttendanceTermID The ID of the TempAttendanceTerm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempAttendanceTermID of the deleted TempAttendanceTerm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAttendanceTerm <- function(TempAttendanceTermID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempAttendanceTerm", objectId = TempAttendanceTermID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAttendanceTerm
	#'
	#' This function creates a TempAttendanceTerm
	#' @param fieldNames The field values to give the created TempAttendanceTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAttendanceTerm <- function(AttendanceTermID = NULL, AttendanceTermCode = NULL, CalendarID = NULL, CalendarCode = NULL, StartDate = NULL, EndDate = NULL, OriginalStartDate = NULL, OriginalEndDate = NULL, IsUpdated = NULL, CalendarStartDate = NULL, CalendarEndDate = NULL, ProcessAction = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempAttendanceTerm", body = list(DataObject = body), searchFields = append("TempAttendanceTermID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAttendanceTerm
	#'
	#' This function modifies a TempAttendanceTerm
	#' @param fieldNames The field values to give the modified TempAttendanceTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempAttendanceTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAttendanceTerm <- function(TempAttendanceTermID, AttendanceTermID = NULL, AttendanceTermCode = NULL, CalendarID = NULL, CalendarCode = NULL, StartDate = NULL, EndDate = NULL, OriginalStartDate = NULL, OriginalEndDate = NULL, IsUpdated = NULL, CalendarStartDate = NULL, CalendarEndDate = NULL, ProcessAction = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempAttendanceTerm", objectId = TempAttendanceTermID, body = list(DataObject = body), searchFields = append("TempAttendanceTermID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of AttendancePeriodConfigEntityGroupYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAttendancePeriodConfigEntityGroupYears <- function(searchConditionsList = NULL, AttendancePeriodConfigEntityGroupYearID = F, EntityGroupKey = F, AttendancePeriodID = F, ConfigEntityGroupYearID = F, AttendancePeriodConfigEntityGroupYearIDClonedFrom = F, TeacherEntryCutoffTime = F, TeacherEntrySpecificCutoffTime = F, TeacherEntryCutoffNumberOfMinutesAfter = F, TeacherEntryCutoffWindowStartTime = F, TeacherEntryCutoffWindowEndTime = F, TeacherEntryCutoffDuration = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowPreviousDayTeacherEntry = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "AttendancePeriodConfigEntityGroupYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AttendancePeriodConfigEntityGroupYear
	#'
	#' This function returns a dataframe or json object of an AttendancePeriodConfigEntityGroupYear
	#' @param AttendancePeriodConfigEntityGroupYearID The ID of the AttendancePeriodConfigEntityGroupYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AttendancePeriodConfigEntityGroupYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AttendancePeriodConfigEntityGroupYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AttendancePeriodConfigEntityGroupYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of AttendancePeriodConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAttendancePeriodConfigEntityGroupYear <- function(AttendancePeriodConfigEntityGroupYearID, EntityGroupKey = F, AttendancePeriodID = F, ConfigEntityGroupYearID = F, AttendancePeriodConfigEntityGroupYearIDClonedFrom = F, TeacherEntryCutoffTime = F, TeacherEntrySpecificCutoffTime = F, TeacherEntryCutoffNumberOfMinutesAfter = F, TeacherEntryCutoffWindowStartTime = F, TeacherEntryCutoffWindowEndTime = F, TeacherEntryCutoffDuration = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowPreviousDayTeacherEntry = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AttendancePeriodConfigEntityGroupYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "AttendancePeriodConfigEntityGroupYear", objectId = AttendancePeriodConfigEntityGroupYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AttendancePeriodConfigEntityGroupYear
	#'
	#' This function deletes an AttendancePeriodConfigEntityGroupYear
	#' @param AttendancePeriodConfigEntityGroupYearID The ID of the AttendancePeriodConfigEntityGroupYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The AttendancePeriodConfigEntityGroupYearID of the deleted AttendancePeriodConfigEntityGroupYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAttendancePeriodConfigEntityGroupYear <- function(AttendancePeriodConfigEntityGroupYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "AttendancePeriodConfigEntityGroupYear", objectId = AttendancePeriodConfigEntityGroupYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AttendancePeriodConfigEntityGroupYear
	#'
	#' This function creates an AttendancePeriodConfigEntityGroupYear
	#' @param fieldNames The field values to give the created AttendancePeriodConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created AttendancePeriodConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAttendancePeriodConfigEntityGroupYear <- function(EntityGroupKey = NULL, AttendancePeriodID = NULL, ConfigEntityGroupYearID = NULL, AttendancePeriodConfigEntityGroupYearIDClonedFrom = NULL, TeacherEntryCutoffTime = NULL, TeacherEntrySpecificCutoffTime = NULL, TeacherEntryCutoffNumberOfMinutesAfter = NULL, TeacherEntryCutoffWindowStartTime = NULL, TeacherEntryCutoffWindowEndTime = NULL, TeacherEntryCutoffDuration = NULL, AllowPreviousDayTeacherEntry = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "AttendancePeriodConfigEntityGroupYear", body = list(DataObject = body), searchFields = append("AttendancePeriodConfigEntityGroupYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AttendancePeriodConfigEntityGroupYear
	#'
	#' This function modifies an AttendancePeriodConfigEntityGroupYear
	#' @param fieldNames The field values to give the modified AttendancePeriodConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified AttendancePeriodConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAttendancePeriodConfigEntityGroupYear <- function(AttendancePeriodConfigEntityGroupYearID, EntityGroupKey = NULL, AttendancePeriodID = NULL, ConfigEntityGroupYearID = NULL, AttendancePeriodConfigEntityGroupYearIDClonedFrom = NULL, TeacherEntryCutoffTime = NULL, TeacherEntrySpecificCutoffTime = NULL, TeacherEntryCutoffNumberOfMinutesAfter = NULL, TeacherEntryCutoffWindowStartTime = NULL, TeacherEntryCutoffWindowEndTime = NULL, TeacherEntryCutoffDuration = NULL, AllowPreviousDayTeacherEntry = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "AttendancePeriodConfigEntityGroupYear", objectId = AttendancePeriodConfigEntityGroupYearID, body = list(DataObject = body), searchFields = append("AttendancePeriodConfigEntityGroupYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of BellScheduleGroupBellSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBellScheduleGroupBellSchedules <- function(searchConditionsList = NULL, BellScheduleGroupBellScheduleID = F, BellScheduleID = F, BellScheduleGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "BellScheduleGroupBellSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BellScheduleGroupBellSchedule
	#'
	#' This function returns a dataframe or json object of a BellScheduleGroupBellSchedule
	#' @param BellScheduleGroupBellScheduleID The ID of the BellScheduleGroupBellSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BellScheduleGroupBellSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BellScheduleGroupBellSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BellScheduleGroupBellSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of BellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBellScheduleGroupBellSchedule <- function(BellScheduleGroupBellScheduleID, BellScheduleID = F, BellScheduleGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BellScheduleGroupBellScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "BellScheduleGroupBellSchedule", objectId = BellScheduleGroupBellScheduleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BellScheduleGroupBellSchedule
	#'
	#' This function deletes a BellScheduleGroupBellSchedule
	#' @param BellScheduleGroupBellScheduleID The ID of the BellScheduleGroupBellSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The BellScheduleGroupBellScheduleID of the deleted BellScheduleGroupBellSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBellScheduleGroupBellSchedule <- function(BellScheduleGroupBellScheduleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "BellScheduleGroupBellSchedule", objectId = BellScheduleGroupBellScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BellScheduleGroupBellSchedule
	#'
	#' This function creates a BellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the created BellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created BellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBellScheduleGroupBellSchedule <- function(BellScheduleID = NULL, BellScheduleGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "BellScheduleGroupBellSchedule", body = list(DataObject = body), searchFields = append("BellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BellScheduleGroupBellSchedule
	#'
	#' This function modifies a BellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the modified BellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified BellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBellScheduleGroupBellSchedule <- function(BellScheduleGroupBellScheduleID, BellScheduleID = NULL, BellScheduleGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "BellScheduleGroupBellSchedule", objectId = BellScheduleGroupBellScheduleID, body = list(DataObject = body), searchFields = append("BellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of CalendarDayBellScheduleGroupBellSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarDayBellScheduleGroupBellSchedules <- function(searchConditionsList = NULL, CalendarDayBellScheduleGroupBellScheduleID = F, CalendarDayID = F, BellScheduleGroupID = F, BellScheduleGroupBellScheduleID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "CalendarDayBellScheduleGroupBellSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function returns a dataframe or json object of a CalendarDayBellScheduleGroupBellSchedule
	#' @param CalendarDayBellScheduleGroupBellScheduleID The ID of the CalendarDayBellScheduleGroupBellSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarDayBellScheduleGroupBellSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarDayBellScheduleGroupBellSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarDayBellScheduleGroupBellSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of CalendarDayBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarDayBellScheduleGroupBellSchedule <- function(CalendarDayBellScheduleGroupBellScheduleID, CalendarDayID = F, BellScheduleGroupID = F, BellScheduleGroupBellScheduleID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarDayBellScheduleGroupBellScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "CalendarDayBellScheduleGroupBellSchedule", objectId = CalendarDayBellScheduleGroupBellScheduleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function deletes a CalendarDayBellScheduleGroupBellSchedule
	#' @param CalendarDayBellScheduleGroupBellScheduleID The ID of the CalendarDayBellScheduleGroupBellSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The CalendarDayBellScheduleGroupBellScheduleID of the deleted CalendarDayBellScheduleGroupBellSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarDayBellScheduleGroupBellSchedule <- function(CalendarDayBellScheduleGroupBellScheduleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "CalendarDayBellScheduleGroupBellSchedule", objectId = CalendarDayBellScheduleGroupBellScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function creates a CalendarDayBellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the created CalendarDayBellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created CalendarDayBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarDayBellScheduleGroupBellSchedule <- function(CalendarDayID = NULL, BellScheduleGroupID = NULL, BellScheduleGroupBellScheduleID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "CalendarDayBellScheduleGroupBellSchedule", body = list(DataObject = body), searchFields = append("CalendarDayBellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function modifies a CalendarDayBellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the modified CalendarDayBellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified CalendarDayBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarDayBellScheduleGroupBellSchedule <- function(CalendarDayBellScheduleGroupBellScheduleID, CalendarDayID = NULL, BellScheduleGroupID = NULL, BellScheduleGroupBellScheduleID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "CalendarDayBellScheduleGroupBellSchedule", objectId = CalendarDayBellScheduleGroupBellScheduleID, body = list(DataObject = body), searchFields = append("CalendarDayBellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of BellScheduleGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBellScheduleGroups <- function(searchConditionsList = NULL, BellScheduleGroupID = F, Code = F, Description = F, SchoolYearID = F, EntityID = F, IsDefault = F, CodeDescription = F, BellScheduleGroupIDClonedFrom = F, AttendancePeriodIDAsOfDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "BellScheduleGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BellScheduleGroup
	#'
	#' This function returns a dataframe or json object of a BellScheduleGroup
	#' @param BellScheduleGroupID The ID of the BellScheduleGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BellScheduleGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BellScheduleGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BellScheduleGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of BellScheduleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBellScheduleGroup <- function(BellScheduleGroupID, Code = F, Description = F, SchoolYearID = F, EntityID = F, IsDefault = F, CodeDescription = F, BellScheduleGroupIDClonedFrom = F, AttendancePeriodIDAsOfDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BellScheduleGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "BellScheduleGroup", objectId = BellScheduleGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BellScheduleGroup
	#'
	#' This function deletes a BellScheduleGroup
	#' @param BellScheduleGroupID The ID of the BellScheduleGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The BellScheduleGroupID of the deleted BellScheduleGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBellScheduleGroup <- function(BellScheduleGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "BellScheduleGroup", objectId = BellScheduleGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BellScheduleGroup
	#'
	#' This function creates a BellScheduleGroup
	#' @param fieldNames The field values to give the created BellScheduleGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created BellScheduleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBellScheduleGroup <- function(Code = NULL, Description = NULL, SchoolYearID = NULL, EntityID = NULL, IsDefault = NULL, BellScheduleGroupIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "BellScheduleGroup", body = list(DataObject = body), searchFields = append("BellScheduleGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BellScheduleGroup
	#'
	#' This function modifies a BellScheduleGroup
	#' @param fieldNames The field values to give the modified BellScheduleGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified BellScheduleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBellScheduleGroup <- function(BellScheduleGroupID, Code = NULL, Description = NULL, SchoolYearID = NULL, EntityID = NULL, IsDefault = NULL, BellScheduleGroupIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "BellScheduleGroup", objectId = BellScheduleGroupID, body = list(DataObject = body), searchFields = append("BellScheduleGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of MassCreateAttendanceByClassActivityRangeRuns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMassCreateAttendanceByClassActivityRangeRuns <- function(searchConditionsList = NULL, MassCreateAttendanceByClassActivityRangeRunID = F, RunTime = F, IsActive = F, UserIDRunBy = F, EntityID = F, SchoolYearID = F, AffectedStudentAttendanceCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "MassCreateAttendanceByClassActivityRangeRun", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MassCreateAttendanceByClassActivityRangeRun
	#'
	#' This function returns a dataframe or json object of a MassCreateAttendanceByClassActivityRangeRun
	#' @param MassCreateAttendanceByClassActivityRangeRunID The ID of the MassCreateAttendanceByClassActivityRangeRun to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassCreateAttendanceByClassActivityRangeRun. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassCreateAttendanceByClassActivityRangeRun.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassCreateAttendanceByClassActivityRangeRun') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of MassCreateAttendanceByClassActivityRangeRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMassCreateAttendanceByClassActivityRangeRun <- function(MassCreateAttendanceByClassActivityRangeRunID, RunTime = F, IsActive = F, UserIDRunBy = F, EntityID = F, SchoolYearID = F, AffectedStudentAttendanceCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MassCreateAttendanceByClassActivityRangeRunID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "MassCreateAttendanceByClassActivityRangeRun", objectId = MassCreateAttendanceByClassActivityRangeRunID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MassCreateAttendanceByClassActivityRangeRun
	#'
	#' This function deletes a MassCreateAttendanceByClassActivityRangeRun
	#' @param MassCreateAttendanceByClassActivityRangeRunID The ID of the MassCreateAttendanceByClassActivityRangeRun to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The MassCreateAttendanceByClassActivityRangeRunID of the deleted MassCreateAttendanceByClassActivityRangeRun.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMassCreateAttendanceByClassActivityRangeRun <- function(MassCreateAttendanceByClassActivityRangeRunID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "MassCreateAttendanceByClassActivityRangeRun", objectId = MassCreateAttendanceByClassActivityRangeRunID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MassCreateAttendanceByClassActivityRangeRun
	#'
	#' This function creates a MassCreateAttendanceByClassActivityRangeRun
	#' @param fieldNames The field values to give the created MassCreateAttendanceByClassActivityRangeRun. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created MassCreateAttendanceByClassActivityRangeRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMassCreateAttendanceByClassActivityRangeRun <- function(RunTime = NULL, IsActive = NULL, UserIDRunBy = NULL, EntityID = NULL, SchoolYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "MassCreateAttendanceByClassActivityRangeRun", body = list(DataObject = body), searchFields = append("MassCreateAttendanceByClassActivityRangeRunID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MassCreateAttendanceByClassActivityRangeRun
	#'
	#' This function modifies a MassCreateAttendanceByClassActivityRangeRun
	#' @param fieldNames The field values to give the modified MassCreateAttendanceByClassActivityRangeRun. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified MassCreateAttendanceByClassActivityRangeRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMassCreateAttendanceByClassActivityRangeRun <- function(MassCreateAttendanceByClassActivityRangeRunID, RunTime = NULL, IsActive = NULL, UserIDRunBy = NULL, EntityID = NULL, SchoolYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "MassCreateAttendanceByClassActivityRangeRun", objectId = MassCreateAttendanceByClassActivityRangeRunID, body = list(DataObject = body), searchFields = append("MassCreateAttendanceByClassActivityRangeRunID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of StudentAttendanceRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendanceRunHistories <- function(searchConditionsList = NULL, StudentAttendanceRunHistoryID = F, StudentAttendanceID = F, MassCreateAttendanceByClassActivityRangeRunID = F, IsActive = F, IsInsert = F, OriginalIsGuardianNotified = F, NewIsGuardianNotified = F, OriginalComment = F, NewComment = F, StudentID = F, CalendarDayID = F, Procedure = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendanceRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendanceRunHistory
	#'
	#' This function returns a dataframe or json object of a StudentAttendanceRunHistory
	#' @param StudentAttendanceRunHistoryID The ID of the StudentAttendanceRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendanceRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendanceRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendanceRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendanceRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendanceRunHistory <- function(StudentAttendanceRunHistoryID, StudentAttendanceID = F, MassCreateAttendanceByClassActivityRangeRunID = F, IsActive = F, IsInsert = F, OriginalIsGuardianNotified = F, NewIsGuardianNotified = F, OriginalComment = F, NewComment = F, StudentID = F, CalendarDayID = F, Procedure = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendanceRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendanceRunHistory", objectId = StudentAttendanceRunHistoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendanceRunHistory
	#'
	#' This function deletes a StudentAttendanceRunHistory
	#' @param StudentAttendanceRunHistoryID The ID of the StudentAttendanceRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendanceRunHistoryID of the deleted StudentAttendanceRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendanceRunHistory <- function(StudentAttendanceRunHistoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendanceRunHistory", objectId = StudentAttendanceRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentAttendanceRunHistory
	#'
	#' This function creates a StudentAttendanceRunHistory
	#' @param fieldNames The field values to give the created StudentAttendanceRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentAttendanceRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentAttendanceRunHistory <- function(StudentAttendanceID = NULL, MassCreateAttendanceByClassActivityRangeRunID = NULL, IsActive = NULL, IsInsert = NULL, OriginalIsGuardianNotified = NULL, NewIsGuardianNotified = NULL, OriginalComment = NULL, NewComment = NULL, StudentID = NULL, CalendarDayID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "StudentAttendanceRunHistory", body = list(DataObject = body), searchFields = append("StudentAttendanceRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentAttendanceRunHistory
	#'
	#' This function modifies a StudentAttendanceRunHistory
	#' @param fieldNames The field values to give the modified StudentAttendanceRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentAttendanceRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentAttendanceRunHistory <- function(StudentAttendanceRunHistoryID, StudentAttendanceID = NULL, MassCreateAttendanceByClassActivityRangeRunID = NULL, IsActive = NULL, IsInsert = NULL, OriginalIsGuardianNotified = NULL, NewIsGuardianNotified = NULL, OriginalComment = NULL, NewComment = NULL, StudentID = NULL, CalendarDayID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "StudentAttendanceRunHistory", objectId = StudentAttendanceRunHistoryID, body = list(DataObject = body), searchFields = append("StudentAttendanceRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of StudentAttendancePeriodRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendancePeriodRunHistories <- function(searchConditionsList = NULL, StudentAttendancePeriodRunHistoryID = F, StudentAttendancePeriodID = F, StudentAttendanceRunHistoryID = F, IsActive = F, IsInsert = F, AttendancePeriodID = F, OriginalAttendanceTypeID = F, OriginalAttendanceReasonID = F, OriginalComment = F, NewAttendanceTypeID = F, NewAttendanceReasonID = F, NewComment = F, Procedure = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendancePeriodRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendancePeriodRunHistory
	#'
	#' This function returns a dataframe or json object of a StudentAttendancePeriodRunHistory
	#' @param StudentAttendancePeriodRunHistoryID The ID of the StudentAttendancePeriodRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendancePeriodRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendancePeriodRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendancePeriodRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendancePeriodRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendancePeriodRunHistory <- function(StudentAttendancePeriodRunHistoryID, StudentAttendancePeriodID = F, StudentAttendanceRunHistoryID = F, IsActive = F, IsInsert = F, AttendancePeriodID = F, OriginalAttendanceTypeID = F, OriginalAttendanceReasonID = F, OriginalComment = F, NewAttendanceTypeID = F, NewAttendanceReasonID = F, NewComment = F, Procedure = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendancePeriodRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendancePeriodRunHistory", objectId = StudentAttendancePeriodRunHistoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendancePeriodRunHistory
	#'
	#' This function deletes a StudentAttendancePeriodRunHistory
	#' @param StudentAttendancePeriodRunHistoryID The ID of the StudentAttendancePeriodRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendancePeriodRunHistoryID of the deleted StudentAttendancePeriodRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendancePeriodRunHistory <- function(StudentAttendancePeriodRunHistoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendancePeriodRunHistory", objectId = StudentAttendancePeriodRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentAttendancePeriodRunHistory
	#'
	#' This function creates a StudentAttendancePeriodRunHistory
	#' @param fieldNames The field values to give the created StudentAttendancePeriodRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentAttendancePeriodRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentAttendancePeriodRunHistory <- function(StudentAttendancePeriodID = NULL, StudentAttendanceRunHistoryID = NULL, IsActive = NULL, IsInsert = NULL, AttendancePeriodID = NULL, OriginalAttendanceTypeID = NULL, OriginalAttendanceReasonID = NULL, OriginalComment = NULL, NewAttendanceTypeID = NULL, NewAttendanceReasonID = NULL, NewComment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "StudentAttendancePeriodRunHistory", body = list(DataObject = body), searchFields = append("StudentAttendancePeriodRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentAttendancePeriodRunHistory
	#'
	#' This function modifies a StudentAttendancePeriodRunHistory
	#' @param fieldNames The field values to give the modified StudentAttendancePeriodRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentAttendancePeriodRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentAttendancePeriodRunHistory <- function(StudentAttendancePeriodRunHistoryID, StudentAttendancePeriodID = NULL, StudentAttendanceRunHistoryID = NULL, IsActive = NULL, IsInsert = NULL, AttendancePeriodID = NULL, OriginalAttendanceTypeID = NULL, OriginalAttendanceReasonID = NULL, OriginalComment = NULL, NewAttendanceTypeID = NULL, NewAttendanceReasonID = NULL, NewComment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "StudentAttendancePeriodRunHistory", objectId = StudentAttendancePeriodRunHistoryID, body = list(DataObject = body), searchFields = append("StudentAttendancePeriodRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of StudentAttendanceEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendanceEntities <- function(searchConditionsList = NULL, StudentAttendanceID = F, EntityID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendanceEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendanceEntity
	#'
	#' This function returns a dataframe or json object of a StudentAttendanceEntity
	#' @param StudentAttendanceEntityID The ID of the StudentAttendanceEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendanceEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendanceEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendanceEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendanceEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendanceEntity <- function(StudentAttendanceEntityID, StudentAttendanceID = F, EntityID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendanceEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendanceEntity", objectId = StudentAttendanceEntityID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendanceEntity
	#'
	#' This function deletes a StudentAttendanceEntity
	#' @param StudentAttendanceEntityID The ID of the StudentAttendanceEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendanceEntityID of the deleted StudentAttendanceEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendanceEntity <- function(StudentAttendanceEntityID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendanceEntity", objectId = StudentAttendanceEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempBellScheduleGroupBellSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempBellScheduleGroupBellSchedules <- function(searchConditionsList = NULL, TempBellScheduleGroupBellScheduleID = F, ShouldUpdate = F, BellScheduleGroupID = F, BellScheduleID = F, BellScheduleGroupBellScheduleID = F, BellScheduleGroupCodeDescription = F, IsDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempBellScheduleGroupBellSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempBellScheduleGroupBellSchedule
	#'
	#' This function returns a dataframe or json object of a TempBellScheduleGroupBellSchedule
	#' @param TempBellScheduleGroupBellScheduleID The ID of the TempBellScheduleGroupBellSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBellScheduleGroupBellSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBellScheduleGroupBellSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBellScheduleGroupBellSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempBellScheduleGroupBellSchedule <- function(TempBellScheduleGroupBellScheduleID, ShouldUpdate = F, BellScheduleGroupID = F, BellScheduleID = F, BellScheduleGroupBellScheduleID = F, BellScheduleGroupCodeDescription = F, IsDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempBellScheduleGroupBellScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempBellScheduleGroupBellSchedule", objectId = TempBellScheduleGroupBellScheduleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempBellScheduleGroupBellSchedule
	#'
	#' This function deletes a TempBellScheduleGroupBellSchedule
	#' @param TempBellScheduleGroupBellScheduleID The ID of the TempBellScheduleGroupBellSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempBellScheduleGroupBellScheduleID of the deleted TempBellScheduleGroupBellSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempBellScheduleGroupBellSchedule <- function(TempBellScheduleGroupBellScheduleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempBellScheduleGroupBellSchedule", objectId = TempBellScheduleGroupBellScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempBellScheduleGroupBellSchedule
	#'
	#' This function creates a TempBellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the created TempBellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempBellScheduleGroupBellSchedule <- function(ShouldUpdate = NULL, BellScheduleGroupID = NULL, BellScheduleID = NULL, BellScheduleGroupBellScheduleID = NULL, BellScheduleGroupCodeDescription = NULL, IsDefault = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempBellScheduleGroupBellSchedule", body = list(DataObject = body), searchFields = append("TempBellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempBellScheduleGroupBellSchedule
	#'
	#' This function modifies a TempBellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the modified TempBellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempBellScheduleGroupBellSchedule <- function(TempBellScheduleGroupBellScheduleID, ShouldUpdate = NULL, BellScheduleGroupID = NULL, BellScheduleID = NULL, BellScheduleGroupBellScheduleID = NULL, BellScheduleGroupCodeDescription = NULL, IsDefault = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempBellScheduleGroupBellSchedule", objectId = TempBellScheduleGroupBellScheduleID, body = list(DataObject = body), searchFields = append("TempBellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempCalendarDayBellScheduleGroupBellSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalendarDayBellScheduleGroupBellSchedules <- function(searchConditionsList = NULL, TempCalendarDayBellScheduleGroupBellScheduleID = F, CalendarDayID = F, BellScheduleGroupBellScheduleID = F, BellScheduleGroupID = F, BellScheduleID = F, Date = F, CountAs = F, DayRotationCode = F, ExistingCalendarDayBellScheduleGroupBellScheduleID = F, ExistingBellScheduleGroupBellScheduleID = F, ExistingBellScheduleCode = F, BellScheduleDescription = F, BellScheduleGroupCodeDescription = F, IsDefault = F, Calendar = F, UpdateBellSchedule = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCalendarDayBellScheduleGroupBellSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function returns a dataframe or json object of a TempCalendarDayBellScheduleGroupBellSchedule
	#' @param TempCalendarDayBellScheduleGroupBellScheduleID The ID of the TempCalendarDayBellScheduleGroupBellSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarDayBellScheduleGroupBellSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarDayBellScheduleGroupBellSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarDayBellScheduleGroupBellSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCalendarDayBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalendarDayBellScheduleGroupBellSchedule <- function(TempCalendarDayBellScheduleGroupBellScheduleID, CalendarDayID = F, BellScheduleGroupBellScheduleID = F, BellScheduleGroupID = F, BellScheduleID = F, Date = F, CountAs = F, DayRotationCode = F, ExistingCalendarDayBellScheduleGroupBellScheduleID = F, ExistingBellScheduleGroupBellScheduleID = F, ExistingBellScheduleCode = F, BellScheduleDescription = F, BellScheduleGroupCodeDescription = F, IsDefault = F, Calendar = F, UpdateBellSchedule = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalendarDayBellScheduleGroupBellScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCalendarDayBellScheduleGroupBellSchedule", objectId = TempCalendarDayBellScheduleGroupBellScheduleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function deletes a TempCalendarDayBellScheduleGroupBellSchedule
	#' @param TempCalendarDayBellScheduleGroupBellScheduleID The ID of the TempCalendarDayBellScheduleGroupBellSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCalendarDayBellScheduleGroupBellScheduleID of the deleted TempCalendarDayBellScheduleGroupBellSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCalendarDayBellScheduleGroupBellSchedule <- function(TempCalendarDayBellScheduleGroupBellScheduleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCalendarDayBellScheduleGroupBellSchedule", objectId = TempCalendarDayBellScheduleGroupBellScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function creates a TempCalendarDayBellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the created TempCalendarDayBellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCalendarDayBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCalendarDayBellScheduleGroupBellSchedule <- function(CalendarDayID = NULL, BellScheduleGroupBellScheduleID = NULL, BellScheduleGroupID = NULL, BellScheduleID = NULL, Date = NULL, CountAs = NULL, DayRotationCode = NULL, ExistingCalendarDayBellScheduleGroupBellScheduleID = NULL, ExistingBellScheduleGroupBellScheduleID = NULL, ExistingBellScheduleCode = NULL, BellScheduleDescription = NULL, BellScheduleGroupCodeDescription = NULL, IsDefault = NULL, Calendar = NULL, UpdateBellSchedule = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempCalendarDayBellScheduleGroupBellSchedule", body = list(DataObject = body), searchFields = append("TempCalendarDayBellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCalendarDayBellScheduleGroupBellSchedule
	#'
	#' This function modifies a TempCalendarDayBellScheduleGroupBellSchedule
	#' @param fieldNames The field values to give the modified TempCalendarDayBellScheduleGroupBellSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCalendarDayBellScheduleGroupBellSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCalendarDayBellScheduleGroupBellSchedule <- function(TempCalendarDayBellScheduleGroupBellScheduleID, CalendarDayID = NULL, BellScheduleGroupBellScheduleID = NULL, BellScheduleGroupID = NULL, BellScheduleID = NULL, Date = NULL, CountAs = NULL, DayRotationCode = NULL, ExistingCalendarDayBellScheduleGroupBellScheduleID = NULL, ExistingBellScheduleGroupBellScheduleID = NULL, ExistingBellScheduleCode = NULL, BellScheduleDescription = NULL, BellScheduleGroupCodeDescription = NULL, IsDefault = NULL, Calendar = NULL, UpdateBellSchedule = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempCalendarDayBellScheduleGroupBellSchedule", objectId = TempCalendarDayBellScheduleGroupBellScheduleID, body = list(DataObject = body), searchFields = append("TempCalendarDayBellScheduleGroupBellScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of StudentAttendanceCounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAttendanceCounts <- function(searchConditionsList = NULL, StudentEntityYearID = F, DaysExcused = F, DaysUnexcused = F, DaysOther = F, TardyCount = F, DaysAbsent = F, DaysEnrolled = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "StudentAttendanceCounts", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAttendanceCounts
	#'
	#' This function returns a dataframe or json object of a StudentAttendanceCounts
	#' @param StudentAttendanceCountsID The ID of the StudentAttendanceCounts to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAttendanceCounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAttendanceCounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAttendanceCounts') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of StudentAttendanceCounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAttendanceCounts <- function(StudentAttendanceCountsID, StudentEntityYearID = F, DaysExcused = F, DaysUnexcused = F, DaysOther = F, TardyCount = F, DaysAbsent = F, DaysEnrolled = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAttendanceCountsID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "StudentAttendanceCounts", objectId = StudentAttendanceCountsID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAttendanceCounts
	#'
	#' This function deletes a StudentAttendanceCounts
	#' @param StudentAttendanceCountsID The ID of the StudentAttendanceCounts to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The StudentAttendanceCountsID of the deleted StudentAttendanceCounts.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAttendanceCounts <- function(StudentAttendanceCountsID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "StudentAttendanceCounts", objectId = StudentAttendanceCountsID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentAttendanceCounts
	#'
	#' This function creates a StudentAttendanceCounts
	#' @param fieldNames The field values to give the created StudentAttendanceCounts. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created StudentAttendanceCounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentAttendanceCounts <- function(DaysExcused = NULL, DaysUnexcused = NULL, DaysOther = NULL, TardyCount = NULL, DaysAbsent = NULL, DaysEnrolled = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "StudentAttendanceCounts", body = list(DataObject = body), searchFields = append("StudentAttendanceCountsID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentAttendanceCounts
	#'
	#' This function modifies a StudentAttendanceCounts
	#' @param fieldNames The field values to give the modified StudentAttendanceCounts. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified StudentAttendanceCounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentAttendanceCounts <- function(StudentAttendanceCountsID, DaysExcused = NULL, DaysUnexcused = NULL, DaysOther = NULL, TardyCount = NULL, DaysAbsent = NULL, DaysEnrolled = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "StudentAttendanceCounts", objectId = StudentAttendanceCountsID, body = list(DataObject = body), searchFields = append("StudentAttendanceCountsID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempDroppedStudentAttendancePeriodErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDroppedStudentAttendancePeriodErrors <- function(searchConditionsList = NULL, TempDroppedStudentAttendancePeriodErrorID = F, TempDroppedStudentAttendancePeriodRecordID = F, ErrorNumber = F, ErrorDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDroppedStudentAttendancePeriodError
	#'
	#' This function returns a dataframe or json object of a TempDroppedStudentAttendancePeriodError
	#' @param TempDroppedStudentAttendancePeriodErrorID The ID of the TempDroppedStudentAttendancePeriodError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDroppedStudentAttendancePeriodError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDroppedStudentAttendancePeriodError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDroppedStudentAttendancePeriodError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempDroppedStudentAttendancePeriodError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDroppedStudentAttendancePeriodError <- function(TempDroppedStudentAttendancePeriodErrorID, TempDroppedStudentAttendancePeriodRecordID = F, ErrorNumber = F, ErrorDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDroppedStudentAttendancePeriodErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodError", objectId = TempDroppedStudentAttendancePeriodErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDroppedStudentAttendancePeriodError
	#'
	#' This function deletes a TempDroppedStudentAttendancePeriodError
	#' @param TempDroppedStudentAttendancePeriodErrorID The ID of the TempDroppedStudentAttendancePeriodError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempDroppedStudentAttendancePeriodErrorID of the deleted TempDroppedStudentAttendancePeriodError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDroppedStudentAttendancePeriodError <- function(TempDroppedStudentAttendancePeriodErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodError", objectId = TempDroppedStudentAttendancePeriodErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDroppedStudentAttendancePeriodError
	#'
	#' This function creates a TempDroppedStudentAttendancePeriodError
	#' @param fieldNames The field values to give the created TempDroppedStudentAttendancePeriodError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempDroppedStudentAttendancePeriodError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDroppedStudentAttendancePeriodError <- function(ErrorNumber = NULL, ErrorDescription = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodError", body = list(DataObject = body), searchFields = append("TempDroppedStudentAttendancePeriodErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDroppedStudentAttendancePeriodError
	#'
	#' This function modifies a TempDroppedStudentAttendancePeriodError
	#' @param fieldNames The field values to give the modified TempDroppedStudentAttendancePeriodError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempDroppedStudentAttendancePeriodError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDroppedStudentAttendancePeriodError <- function(TempDroppedStudentAttendancePeriodErrorID, ErrorNumber = NULL, ErrorDescription = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodError", objectId = TempDroppedStudentAttendancePeriodErrorID, body = list(DataObject = body), searchFields = append("TempDroppedStudentAttendancePeriodErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempDroppedStudentAttendancePeriodRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDroppedStudentAttendancePeriodRecords <- function(searchConditionsList = NULL, TempDroppedStudentAttendancePeriodRecordID = F, AffectedPrimaryKey = F, StudentName = F, Date = F, CourseDescription = F, AttendanceTypeCode = F, AttendancePeriodCode = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDroppedStudentAttendancePeriodRecord
	#'
	#' This function returns a dataframe or json object of a TempDroppedStudentAttendancePeriodRecord
	#' @param TempDroppedStudentAttendancePeriodRecordID The ID of the TempDroppedStudentAttendancePeriodRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDroppedStudentAttendancePeriodRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDroppedStudentAttendancePeriodRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDroppedStudentAttendancePeriodRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempDroppedStudentAttendancePeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDroppedStudentAttendancePeriodRecord <- function(TempDroppedStudentAttendancePeriodRecordID, AffectedPrimaryKey = F, StudentName = F, Date = F, CourseDescription = F, AttendanceTypeCode = F, AttendancePeriodCode = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDroppedStudentAttendancePeriodRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodRecord", objectId = TempDroppedStudentAttendancePeriodRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDroppedStudentAttendancePeriodRecord
	#'
	#' This function deletes a TempDroppedStudentAttendancePeriodRecord
	#' @param TempDroppedStudentAttendancePeriodRecordID The ID of the TempDroppedStudentAttendancePeriodRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempDroppedStudentAttendancePeriodRecordID of the deleted TempDroppedStudentAttendancePeriodRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDroppedStudentAttendancePeriodRecord <- function(TempDroppedStudentAttendancePeriodRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodRecord", objectId = TempDroppedStudentAttendancePeriodRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDroppedStudentAttendancePeriodRecord
	#'
	#' This function creates a TempDroppedStudentAttendancePeriodRecord
	#' @param fieldNames The field values to give the created TempDroppedStudentAttendancePeriodRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempDroppedStudentAttendancePeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDroppedStudentAttendancePeriodRecord <- function(AffectedPrimaryKey = NULL, StudentName = NULL, Date = NULL, CourseDescription = NULL, AttendanceTypeCode = NULL, AttendancePeriodCode = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodRecord", body = list(DataObject = body), searchFields = append("TempDroppedStudentAttendancePeriodRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDroppedStudentAttendancePeriodRecord
	#'
	#' This function modifies a TempDroppedStudentAttendancePeriodRecord
	#' @param fieldNames The field values to give the modified TempDroppedStudentAttendancePeriodRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempDroppedStudentAttendancePeriodRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDroppedStudentAttendancePeriodRecord <- function(TempDroppedStudentAttendancePeriodRecordID, AffectedPrimaryKey = NULL, StudentName = NULL, Date = NULL, CourseDescription = NULL, AttendanceTypeCode = NULL, AttendancePeriodCode = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempDroppedStudentAttendancePeriodRecord", objectId = TempDroppedStudentAttendancePeriodRecordID, body = list(DataObject = body), searchFields = append("TempDroppedStudentAttendancePeriodRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempCalendarDayFieldRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalendarDayFieldRecords <- function(searchConditionsList = NULL, TempCalendarDayFieldRecordID = F, FieldName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempCalendarDayFieldRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCalendarDayFieldRecord
	#'
	#' This function returns a dataframe or json object of a TempCalendarDayFieldRecord
	#' @param TempCalendarDayFieldRecordID The ID of the TempCalendarDayFieldRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarDayFieldRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarDayFieldRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarDayFieldRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempCalendarDayFieldRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalendarDayFieldRecord <- function(TempCalendarDayFieldRecordID, FieldName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalendarDayFieldRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempCalendarDayFieldRecord", objectId = TempCalendarDayFieldRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCalendarDayFieldRecord
	#'
	#' This function deletes a TempCalendarDayFieldRecord
	#' @param TempCalendarDayFieldRecordID The ID of the TempCalendarDayFieldRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempCalendarDayFieldRecordID of the deleted TempCalendarDayFieldRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCalendarDayFieldRecord <- function(TempCalendarDayFieldRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempCalendarDayFieldRecord", objectId = TempCalendarDayFieldRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCalendarDayFieldRecord
	#'
	#' This function creates a TempCalendarDayFieldRecord
	#' @param fieldNames The field values to give the created TempCalendarDayFieldRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempCalendarDayFieldRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCalendarDayFieldRecord <- function(FieldName = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempCalendarDayFieldRecord", body = list(DataObject = body), searchFields = append("TempCalendarDayFieldRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCalendarDayFieldRecord
	#'
	#' This function modifies a TempCalendarDayFieldRecord
	#' @param fieldNames The field values to give the modified TempCalendarDayFieldRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempCalendarDayFieldRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCalendarDayFieldRecord <- function(TempCalendarDayFieldRecordID, FieldName = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempCalendarDayFieldRecord", objectId = TempCalendarDayFieldRecordID, body = list(DataObject = body), searchFields = append("TempCalendarDayFieldRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A list of TempMassUpdateCalendarDayEntryRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassUpdateCalendarDayEntryRecords <- function(searchConditionsList = NULL, TempMassUpdateCalendarDayEntryRecordID = F, Date = F, AddUpdateDate = F, CountAs = F, Comment = F, ShowCommentOnCalendar = F, HasFailureReasons = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Attendance", objectName = "TempMassUpdateCalendarDayEntryRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassUpdateCalendarDayEntryRecord
	#'
	#' This function returns a dataframe or json object of a TempMassUpdateCalendarDayEntryRecord
	#' @param TempMassUpdateCalendarDayEntryRecordID The ID of the TempMassUpdateCalendarDayEntryRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassUpdateCalendarDayEntryRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassUpdateCalendarDayEntryRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassUpdateCalendarDayEntryRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A dataframe or of TempMassUpdateCalendarDayEntryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassUpdateCalendarDayEntryRecord <- function(TempMassUpdateCalendarDayEntryRecordID, Date = F, AddUpdateDate = F, CountAs = F, Comment = F, ShowCommentOnCalendar = F, HasFailureReasons = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassUpdateCalendarDayEntryRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Attendance", objectName = "TempMassUpdateCalendarDayEntryRecord", objectId = TempMassUpdateCalendarDayEntryRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassUpdateCalendarDayEntryRecord
	#'
	#' This function deletes a TempMassUpdateCalendarDayEntryRecord
	#' @param TempMassUpdateCalendarDayEntryRecordID The ID of the TempMassUpdateCalendarDayEntryRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The TempMassUpdateCalendarDayEntryRecordID of the deleted TempMassUpdateCalendarDayEntryRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassUpdateCalendarDayEntryRecord <- function(TempMassUpdateCalendarDayEntryRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Attendance", objectName = "TempMassUpdateCalendarDayEntryRecord", objectId = TempMassUpdateCalendarDayEntryRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassUpdateCalendarDayEntryRecord
	#'
	#' This function creates a TempMassUpdateCalendarDayEntryRecord
	#' @param fieldNames The field values to give the created TempMassUpdateCalendarDayEntryRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return A newly created TempMassUpdateCalendarDayEntryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassUpdateCalendarDayEntryRecord <- function(Date = NULL, AddUpdateDate = NULL, CountAs = NULL, Comment = NULL, ShowCommentOnCalendar = NULL, HasFailureReasons = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Attendance", objectName = "TempMassUpdateCalendarDayEntryRecord", body = list(DataObject = body), searchFields = append("TempMassUpdateCalendarDayEntryRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassUpdateCalendarDayEntryRecord
	#'
	#' This function modifies a TempMassUpdateCalendarDayEntryRecord
	#' @param fieldNames The field values to give the modified TempMassUpdateCalendarDayEntryRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Attendance
	#' @return The modified TempMassUpdateCalendarDayEntryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassUpdateCalendarDayEntryRecord <- function(TempMassUpdateCalendarDayEntryRecordID, Date = NULL, AddUpdateDate = NULL, CountAs = NULL, Comment = NULL, ShowCommentOnCalendar = NULL, HasFailureReasons = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Attendance", objectName = "TempMassUpdateCalendarDayEntryRecord", objectId = TempMassUpdateCalendarDayEntryRecordID, body = list(DataObject = body), searchFields = append("TempMassUpdateCalendarDayEntryRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
