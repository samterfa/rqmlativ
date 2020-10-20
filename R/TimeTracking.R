
	#' List AssignmentTimesheets
	#'
	#' This function returns a dataframe or json object of AssignmentTimesheets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTimesheets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTimesheets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTimesheet') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of AssignmentTimesheets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentTimesheets <- function(searchConditionsList = NULL, AssignmentTimesheetWeekIDFirst = F, AssignmentID = F, TimesheetWeekID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentTimesheet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentTimesheet
	#'
	#' This function returns a dataframe or json object of an AssignmentTimesheet
	#' @param AssignmentTimesheetID The ID of the AssignmentTimesheet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTimesheet. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTimesheet.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTimesheet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of AssignmentTimesheet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentTimesheet <- function(AssignmentTimesheetID, AssignmentTimesheetWeekIDFirst = F, AssignmentID = F, TimesheetWeekID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentTimesheetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "AssignmentTimesheet", objectId = AssignmentTimesheetID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentTimesheet
	#'
	#' This function deletes an AssignmentTimesheet
	#' @param AssignmentTimesheetID The ID of the AssignmentTimesheet to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The AssignmentTimesheetID of the deleted AssignmentTimesheet.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentTimesheet <- function(AssignmentTimesheetID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "AssignmentTimesheet", objectId = AssignmentTimesheetID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentTimesheet
	#'
	#' This function creates an AssignmentTimesheet
	#' @param fieldNames The field values to give the created AssignmentTimesheet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created AssignmentTimesheet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentTimesheet <- function(AssignmentID = NULL, TimesheetWeekID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "AssignmentTimesheet", body = list(DataObject = body), searchFields = append("AssignmentTimesheetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentTimesheet
	#'
	#' This function modifies an AssignmentTimesheet
	#' @param fieldNames The field values to give the modified AssignmentTimesheet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified AssignmentTimesheet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentTimesheet <- function(AssignmentTimesheetID, AssignmentID = NULL, TimesheetWeekID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "AssignmentTimesheet", objectId = AssignmentTimesheetID, body = list(DataObject = body), searchFields = append("AssignmentTimesheetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeTrackingConfigFiscalYears
	#'
	#' This function returns a dataframe or json object of TimeTrackingConfigFiscalYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTrackingConfigFiscalYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTrackingConfigFiscalYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTrackingConfigFiscalYear') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimeTrackingConfigFiscalYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeTrackingConfigFiscalYears <- function(searchConditionsList = NULL, ConfigFiscalYearID = F, FiscalYearID = F, DistrictID = F, OrganizationChartID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "ConfigFiscalYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeTrackingConfigFiscalYear
	#'
	#' This function returns a dataframe or json object of a TimeTrackingConfigFiscalYear
	#' @param TimeTrackingConfigFiscalYearID The ID of the TimeTrackingConfigFiscalYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTrackingConfigFiscalYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTrackingConfigFiscalYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTrackingConfigFiscalYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimeTrackingConfigFiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeTrackingConfigFiscalYear <- function(TimeTrackingConfigFiscalYearID, ConfigFiscalYearID = F, FiscalYearID = F, DistrictID = F, OrganizationChartID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeTrackingConfigFiscalYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "ConfigFiscalYear", objectId = TimeTrackingConfigFiscalYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeTrackingConfigFiscalYear
	#'
	#' This function deletes a TimeTrackingConfigFiscalYear
	#' @param TimeTrackingConfigFiscalYearID The ID of the TimeTrackingConfigFiscalYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimeTrackingConfigFiscalYearID of the deleted TimeTrackingConfigFiscalYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeTrackingConfigFiscalYear <- function(TimeTrackingConfigFiscalYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "ConfigFiscalYear", objectId = TimeTrackingConfigFiscalYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeTrackingConfigFiscalYear
	#'
	#' This function creates a TimeTrackingConfigFiscalYear
	#' @param fieldNames The field values to give the created TimeTrackingConfigFiscalYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimeTrackingConfigFiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeTrackingConfigFiscalYear <- function(FiscalYearID = NULL, DistrictID = NULL, OrganizationChartID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "ConfigFiscalYear", body = list(DataObject = body), searchFields = append("ConfigFiscalYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeTrackingConfigFiscalYear
	#'
	#' This function modifies a TimeTrackingConfigFiscalYear
	#' @param fieldNames The field values to give the modified TimeTrackingConfigFiscalYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimeTrackingConfigFiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeTrackingConfigFiscalYear <- function(ConfigFiscalYearID, FiscalYearID = NULL, DistrictID = NULL, OrganizationChartID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "ConfigFiscalYear", objectId = ConfigFiscalYearID, body = list(DataObject = body), searchFields = append("ConfigFiscalYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeTrackingTempAccountDistributions
	#'
	#' This function returns a dataframe or json object of TimeTrackingTempAccountDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTrackingTempAccountDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTrackingTempAccountDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTrackingTempAccountDistribution') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimeTrackingTempAccountDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeTrackingTempAccountDistributions <- function(searchConditionsList = NULL, TempAccountDistributionID = F, TempTimesheetID = F, AccountID = F, DistributionPercent = F, StateConcordDepartmentTNID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempAccountDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeTrackingTempAccountDistribution
	#'
	#' This function returns a dataframe or json object of a TimeTrackingTempAccountDistribution
	#' @param TimeTrackingTempAccountDistributionID The ID of the TimeTrackingTempAccountDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTrackingTempAccountDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTrackingTempAccountDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTrackingTempAccountDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimeTrackingTempAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeTrackingTempAccountDistribution <- function(TimeTrackingTempAccountDistributionID, TempAccountDistributionID = F, TempTimesheetID = F, AccountID = F, DistributionPercent = F, StateConcordDepartmentTNID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeTrackingTempAccountDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TempAccountDistribution", objectId = TimeTrackingTempAccountDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeTrackingTempAccountDistribution
	#'
	#' This function deletes a TimeTrackingTempAccountDistribution
	#' @param TimeTrackingTempAccountDistributionID The ID of the TimeTrackingTempAccountDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimeTrackingTempAccountDistributionID of the deleted TimeTrackingTempAccountDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeTrackingTempAccountDistribution <- function(TimeTrackingTempAccountDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TempAccountDistribution", objectId = TimeTrackingTempAccountDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeTrackingTempAccountDistribution
	#'
	#' This function creates a TimeTrackingTempAccountDistribution
	#' @param fieldNames The field values to give the created TimeTrackingTempAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimeTrackingTempAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeTrackingTempAccountDistribution <- function(TempTimesheetID = NULL, AccountID = NULL, DistributionPercent = NULL, StateConcordDepartmentTNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TempAccountDistribution", body = list(DataObject = body), searchFields = append("TempAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeTrackingTempAccountDistribution
	#'
	#' This function modifies a TimeTrackingTempAccountDistribution
	#' @param fieldNames The field values to give the modified TimeTrackingTempAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimeTrackingTempAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeTrackingTempAccountDistribution <- function(TempAccountDistributionID, TempTimesheetID = NULL, AccountID = NULL, DistributionPercent = NULL, StateConcordDepartmentTNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TempAccountDistribution", objectId = TempAccountDistributionID, body = list(DataObject = body), searchFields = append("TempAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeTrackingTempTimesheets
	#'
	#' This function returns a dataframe or json object of TimeTrackingTempTimesheets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTrackingTempTimesheets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTrackingTempTimesheets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTrackingTempTimesheet') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimeTrackingTempTimesheets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeTrackingTempTimesheets <- function(searchConditionsList = NULL, TempTimesheetID = F, Factor = F, Rate = F, HoursWorked = F, WorkStartDate = F, WorkEndDate = F, AssignmentPayTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempAssignmentPayTypeTimesheetWeekID = F, GroupOnPayCheck = F, CommentOnPayCheck = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempTimesheet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeTrackingTempTimesheet
	#'
	#' This function returns a dataframe or json object of a TimeTrackingTempTimesheet
	#' @param TimeTrackingTempTimesheetID The ID of the TimeTrackingTempTimesheet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTrackingTempTimesheet. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTrackingTempTimesheet.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTrackingTempTimesheet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimeTrackingTempTimesheet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeTrackingTempTimesheet <- function(TimeTrackingTempTimesheetID, TempTimesheetID = F, Factor = F, Rate = F, HoursWorked = F, WorkStartDate = F, WorkEndDate = F, AssignmentPayTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempAssignmentPayTypeTimesheetWeekID = F, GroupOnPayCheck = F, CommentOnPayCheck = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeTrackingTempTimesheetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TempTimesheet", objectId = TimeTrackingTempTimesheetID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeTrackingTempTimesheet
	#'
	#' This function deletes a TimeTrackingTempTimesheet
	#' @param TimeTrackingTempTimesheetID The ID of the TimeTrackingTempTimesheet to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimeTrackingTempTimesheetID of the deleted TimeTrackingTempTimesheet.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeTrackingTempTimesheet <- function(TimeTrackingTempTimesheetID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TempTimesheet", objectId = TimeTrackingTempTimesheetID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeTrackingTempTimesheet
	#'
	#' This function creates a TimeTrackingTempTimesheet
	#' @param fieldNames The field values to give the created TimeTrackingTempTimesheet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimeTrackingTempTimesheet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeTrackingTempTimesheet <- function(Factor = NULL, Rate = NULL, HoursWorked = NULL, WorkStartDate = NULL, WorkEndDate = NULL, AssignmentPayTypeID = NULL, TempAssignmentPayTypeTimesheetWeekID = NULL, GroupOnPayCheck = NULL, CommentOnPayCheck = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TempTimesheet", body = list(DataObject = body), searchFields = append("TempTimesheetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeTrackingTempTimesheet
	#'
	#' This function modifies a TimeTrackingTempTimesheet
	#' @param fieldNames The field values to give the modified TimeTrackingTempTimesheet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimeTrackingTempTimesheet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeTrackingTempTimesheet <- function(TempTimesheetID, Factor = NULL, Rate = NULL, HoursWorked = NULL, WorkStartDate = NULL, WorkEndDate = NULL, AssignmentPayTypeID = NULL, TempAssignmentPayTypeTimesheetWeekID = NULL, GroupOnPayCheck = NULL, CommentOnPayCheck = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TempTimesheet", objectId = TempTimesheetID, body = list(DataObject = body), searchFields = append("TempTimesheetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimesheetApprovalTasks
	#'
	#' This function returns a dataframe or json object of TimesheetApprovalTasks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimesheetApprovalTasks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimesheetApprovalTasks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimesheetApprovalTask') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimesheetApprovalTasks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimesheetApprovalTasks <- function(searchConditionsList = NULL, TimesheetApprovalTaskID = F, DistrictID = F, IsConditional = F, Level = F, Description = F, FilterData = F, StandardFilterCollectionData = F, UseOrganizationChart = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimesheetApprovalTask", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimesheetApprovalTask
	#'
	#' This function returns a dataframe or json object of a TimesheetApprovalTask
	#' @param TimesheetApprovalTaskID The ID of the TimesheetApprovalTask to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimesheetApprovalTask. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimesheetApprovalTask.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimesheetApprovalTask') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimesheetApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimesheetApprovalTask <- function(TimesheetApprovalTaskID, DistrictID = F, IsConditional = F, Level = F, Description = F, FilterData = F, StandardFilterCollectionData = F, UseOrganizationChart = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimesheetApprovalTaskID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TimesheetApprovalTask", objectId = TimesheetApprovalTaskID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimesheetApprovalTask
	#'
	#' This function deletes a TimesheetApprovalTask
	#' @param TimesheetApprovalTaskID The ID of the TimesheetApprovalTask to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimesheetApprovalTaskID of the deleted TimesheetApprovalTask.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimesheetApprovalTask <- function(TimesheetApprovalTaskID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TimesheetApprovalTask", objectId = TimesheetApprovalTaskID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimesheetApprovalTask
	#'
	#' This function creates a TimesheetApprovalTask
	#' @param fieldNames The field values to give the created TimesheetApprovalTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimesheetApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimesheetApprovalTask <- function(DistrictID = NULL, IsConditional = NULL, Level = NULL, Description = NULL, FilterData = NULL, UseOrganizationChart = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TimesheetApprovalTask", body = list(DataObject = body), searchFields = append("TimesheetApprovalTaskID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimesheetApprovalTask
	#'
	#' This function modifies a TimesheetApprovalTask
	#' @param fieldNames The field values to give the modified TimesheetApprovalTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimesheetApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimesheetApprovalTask <- function(TimesheetApprovalTaskID, DistrictID = NULL, IsConditional = NULL, Level = NULL, Description = NULL, FilterData = NULL, UseOrganizationChart = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TimesheetApprovalTask", objectId = TimesheetApprovalTaskID, body = list(DataObject = body), searchFields = append("TimesheetApprovalTaskID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimesheetApprovalTaskSecurityGroups
	#'
	#' This function returns a dataframe or json object of TimesheetApprovalTaskSecurityGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimesheetApprovalTaskSecurityGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimesheetApprovalTaskSecurityGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimesheetApprovalTaskSecurityGroup') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimesheetApprovalTaskSecurityGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimesheetApprovalTaskSecurityGroups <- function(searchConditionsList = NULL, TimesheetApprovalTaskSecurityGroupID = F, TimesheetApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimesheetApprovalTaskSecurityGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimesheetApprovalTaskSecurityGroup
	#'
	#' This function returns a dataframe or json object of a TimesheetApprovalTaskSecurityGroup
	#' @param TimesheetApprovalTaskSecurityGroupID The ID of the TimesheetApprovalTaskSecurityGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimesheetApprovalTaskSecurityGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimesheetApprovalTaskSecurityGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimesheetApprovalTaskSecurityGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimesheetApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimesheetApprovalTaskSecurityGroup <- function(TimesheetApprovalTaskSecurityGroupID, TimesheetApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimesheetApprovalTaskSecurityGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TimesheetApprovalTaskSecurityGroup", objectId = TimesheetApprovalTaskSecurityGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimesheetApprovalTaskSecurityGroup
	#'
	#' This function deletes a TimesheetApprovalTaskSecurityGroup
	#' @param TimesheetApprovalTaskSecurityGroupID The ID of the TimesheetApprovalTaskSecurityGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimesheetApprovalTaskSecurityGroupID of the deleted TimesheetApprovalTaskSecurityGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimesheetApprovalTaskSecurityGroup <- function(TimesheetApprovalTaskSecurityGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TimesheetApprovalTaskSecurityGroup", objectId = TimesheetApprovalTaskSecurityGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimesheetApprovalTaskSecurityGroup
	#'
	#' This function creates a TimesheetApprovalTaskSecurityGroup
	#' @param fieldNames The field values to give the created TimesheetApprovalTaskSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimesheetApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimesheetApprovalTaskSecurityGroup <- function(TimesheetApprovalTaskID = NULL, GroupIDSecurity = NULL, IsAlternate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TimesheetApprovalTaskSecurityGroup", body = list(DataObject = body), searchFields = append("TimesheetApprovalTaskSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimesheetApprovalTaskSecurityGroup
	#'
	#' This function modifies a TimesheetApprovalTaskSecurityGroup
	#' @param fieldNames The field values to give the modified TimesheetApprovalTaskSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimesheetApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimesheetApprovalTaskSecurityGroup <- function(TimesheetApprovalTaskSecurityGroupID, TimesheetApprovalTaskID = NULL, GroupIDSecurity = NULL, IsAlternate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TimesheetApprovalTaskSecurityGroup", objectId = TimesheetApprovalTaskSecurityGroupID, body = list(DataObject = body), searchFields = append("TimesheetApprovalTaskSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeTrackingTempExceptions
	#'
	#' This function returns a dataframe or json object of TimeTrackingTempExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTrackingTempExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTrackingTempExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTrackingTempException') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimeTrackingTempExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeTrackingTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeTrackingTempException
	#'
	#' This function returns a dataframe or json object of a TimeTrackingTempException
	#' @param TimeTrackingTempExceptionID The ID of the TimeTrackingTempException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTrackingTempException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTrackingTempException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTrackingTempException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimeTrackingTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeTrackingTempException <- function(TimeTrackingTempExceptionID, TempExceptionID = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeTrackingTempExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TempException", objectId = TimeTrackingTempExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeTrackingTempException
	#'
	#' This function deletes a TimeTrackingTempException
	#' @param TimeTrackingTempExceptionID The ID of the TimeTrackingTempException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimeTrackingTempExceptionID of the deleted TimeTrackingTempException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeTrackingTempException <- function(TimeTrackingTempExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TempException", objectId = TimeTrackingTempExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeTrackingTempException
	#'
	#' This function creates a TimeTrackingTempException
	#' @param fieldNames The field values to give the created TimeTrackingTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimeTrackingTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeTrackingTempException <- function(Message = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TempException", body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeTrackingTempException
	#'
	#' This function modifies a TimeTrackingTempException
	#' @param fieldNames The field values to give the modified TimeTrackingTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimeTrackingTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeTrackingTempException <- function(TempExceptionID, Message = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TempException", objectId = TempExceptionID, body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeTrackingConfigDistricts
	#'
	#' This function returns a dataframe or json object of TimeTrackingConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTrackingConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTrackingConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTrackingConfigDistrict') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimeTrackingConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeTrackingConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, SendApprovedMessage = F, TimesheetSubmissionApprovedMessageSubject = F, TimesheetSubmissionApprovedMessageContent = F, SendDeniedMessage = F, TimesheetSubmissionDeniedMessageSubject = F, TimesheetSubmissionDeniedMessageContent = F, SendWaitingMessage = F, TimesheetSubmissionWaitingMessageSubject = F, TimesheetSubmissionWaitingMessageContent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeTrackingConfigDistrict
	#'
	#' This function returns a dataframe or json object of a TimeTrackingConfigDistrict
	#' @param TimeTrackingConfigDistrictID The ID of the TimeTrackingConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTrackingConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTrackingConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTrackingConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimeTrackingConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeTrackingConfigDistrict <- function(TimeTrackingConfigDistrictID, ConfigDistrictID = F, DistrictID = F, SendApprovedMessage = F, TimesheetSubmissionApprovedMessageSubject = F, TimesheetSubmissionApprovedMessageContent = F, SendDeniedMessage = F, TimesheetSubmissionDeniedMessageSubject = F, TimesheetSubmissionDeniedMessageContent = F, SendWaitingMessage = F, TimesheetSubmissionWaitingMessageSubject = F, TimesheetSubmissionWaitingMessageContent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeTrackingConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "ConfigDistrict", objectId = TimeTrackingConfigDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeTrackingConfigDistrict
	#'
	#' This function deletes a TimeTrackingConfigDistrict
	#' @param TimeTrackingConfigDistrictID The ID of the TimeTrackingConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimeTrackingConfigDistrictID of the deleted TimeTrackingConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeTrackingConfigDistrict <- function(TimeTrackingConfigDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "ConfigDistrict", objectId = TimeTrackingConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeTrackingConfigDistrict
	#'
	#' This function creates a TimeTrackingConfigDistrict
	#' @param fieldNames The field values to give the created TimeTrackingConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimeTrackingConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeTrackingConfigDistrict <- function(DistrictID = NULL, SendApprovedMessage = NULL, TimesheetSubmissionApprovedMessageSubject = NULL, TimesheetSubmissionApprovedMessageContent = NULL, SendDeniedMessage = NULL, TimesheetSubmissionDeniedMessageSubject = NULL, TimesheetSubmissionDeniedMessageContent = NULL, SendWaitingMessage = NULL, TimesheetSubmissionWaitingMessageSubject = NULL, TimesheetSubmissionWaitingMessageContent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeTrackingConfigDistrict
	#'
	#' This function modifies a TimeTrackingConfigDistrict
	#' @param fieldNames The field values to give the modified TimeTrackingConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimeTrackingConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeTrackingConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, SendApprovedMessage = NULL, TimesheetSubmissionApprovedMessageSubject = NULL, TimesheetSubmissionApprovedMessageContent = NULL, SendDeniedMessage = NULL, TimesheetSubmissionDeniedMessageSubject = NULL, TimesheetSubmissionDeniedMessageContent = NULL, SendWaitingMessage = NULL, TimesheetSubmissionWaitingMessageSubject = NULL, TimesheetSubmissionWaitingMessageContent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentTimesheetWeeks
	#'
	#' This function returns a dataframe or json object of AssignmentTimesheetWeeks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTimesheetWeeks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTimesheetWeeks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTimesheetWeek') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of AssignmentTimesheetWeeks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentTimesheetWeeks <- function(searchConditionsList = NULL, AssignmentTimesheetWeekID = F, AssignmentID = F, TimesheetWeekID = F, PayScheduleDetailID = F, StartDate = F, EndDate = F, TotalIn = F, TotalWorkOutOfOffice = F, TotalWork = F, TotalBreak = F, TotalLunch = F, TotalPaid = F, TotalPaidEligibleForOvertime = F, TotalOvertime = F, TotalUnpaidBreak = F, TotalUnroundedPaid = F, TotalHoliday = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalTimeOff = F, TotalInSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, TotalPaidSeconds = F, TotalPaidEligibleForOvertimeSeconds = F, TotalOvertimeSeconds = F, TotalUnpaidBreakSeconds = F, TotalUnroundedPaidSeconds = F, TotalHolidaySeconds = F, TotalTimeOffSeconds = F, TimesheetSubmissionID = F, HasNonTimeTransactionSource = F, TotalSalaryPaid = F, TotalSalaryPaidSeconds = F, TotalSalaryOverage = F, TotalSalaryOverageSeconds = F, TotalSalaryUnderage = F, TotalSalaryUnderageSeconds = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentTimesheetWeek", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentTimesheetWeek
	#'
	#' This function returns a dataframe or json object of an AssignmentTimesheetWeek
	#' @param AssignmentTimesheetWeekID The ID of the AssignmentTimesheetWeek to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTimesheetWeek. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTimesheetWeek.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTimesheetWeek') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of AssignmentTimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentTimesheetWeek <- function(AssignmentTimesheetWeekID, AssignmentID = F, TimesheetWeekID = F, PayScheduleDetailID = F, StartDate = F, EndDate = F, TotalIn = F, TotalWorkOutOfOffice = F, TotalWork = F, TotalBreak = F, TotalLunch = F, TotalPaid = F, TotalPaidEligibleForOvertime = F, TotalOvertime = F, TotalUnpaidBreak = F, TotalUnroundedPaid = F, TotalHoliday = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalTimeOff = F, TotalInSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, TotalPaidSeconds = F, TotalPaidEligibleForOvertimeSeconds = F, TotalOvertimeSeconds = F, TotalUnpaidBreakSeconds = F, TotalUnroundedPaidSeconds = F, TotalHolidaySeconds = F, TotalTimeOffSeconds = F, TimesheetSubmissionID = F, HasNonTimeTransactionSource = F, TotalSalaryPaid = F, TotalSalaryPaidSeconds = F, TotalSalaryOverage = F, TotalSalaryOverageSeconds = F, TotalSalaryUnderage = F, TotalSalaryUnderageSeconds = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentTimesheetWeekID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "AssignmentTimesheetWeek", objectId = AssignmentTimesheetWeekID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentTimesheetWeek
	#'
	#' This function deletes an AssignmentTimesheetWeek
	#' @param AssignmentTimesheetWeekID The ID of the AssignmentTimesheetWeek to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The AssignmentTimesheetWeekID of the deleted AssignmentTimesheetWeek.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentTimesheetWeek <- function(AssignmentTimesheetWeekID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "AssignmentTimesheetWeek", objectId = AssignmentTimesheetWeekID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentTimesheetWeek
	#'
	#' This function creates an AssignmentTimesheetWeek
	#' @param fieldNames The field values to give the created AssignmentTimesheetWeek. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created AssignmentTimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentTimesheetWeek <- function(AssignmentID = NULL, TimesheetWeekID = NULL, PayScheduleDetailID = NULL, StartDate = NULL, EndDate = NULL, TimesheetSubmissionID = NULL, HasNonTimeTransactionSource = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "AssignmentTimesheetWeek", body = list(DataObject = body), searchFields = append("AssignmentTimesheetWeekID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentTimesheetWeek
	#'
	#' This function modifies an AssignmentTimesheetWeek
	#' @param fieldNames The field values to give the modified AssignmentTimesheetWeek. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified AssignmentTimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentTimesheetWeek <- function(AssignmentTimesheetWeekID, AssignmentID = NULL, TimesheetWeekID = NULL, PayScheduleDetailID = NULL, StartDate = NULL, EndDate = NULL, TimesheetSubmissionID = NULL, HasNonTimeTransactionSource = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "AssignmentTimesheetWeek", objectId = AssignmentTimesheetWeekID, body = list(DataObject = body), searchFields = append("AssignmentTimesheetWeekID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentTimeTrackingGroupDetailPayTypes
	#'
	#' This function returns a dataframe or json object of AssignmentTimeTrackingGroupDetailPayTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTimeTrackingGroupDetailPayTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTimeTrackingGroupDetailPayTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTimeTrackingGroupDetailPayType') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of AssignmentTimeTrackingGroupDetailPayTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentTimeTrackingGroupDetailPayTypes <- function(searchConditionsList = NULL, AssignmentTimeTrackingGroupDetailPayTypeID = F, PayTypeID = F, AssignmentTimeTrackingGroupDetailID = F, AllowTimeTracking = F, IncludeInOvertimeCalculation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowTimeTrackingOutsideAssignment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetailPayType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentTimeTrackingGroupDetailPayType
	#'
	#' This function returns a dataframe or json object of an AssignmentTimeTrackingGroupDetailPayType
	#' @param AssignmentTimeTrackingGroupDetailPayTypeID The ID of the AssignmentTimeTrackingGroupDetailPayType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTimeTrackingGroupDetailPayType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTimeTrackingGroupDetailPayType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTimeTrackingGroupDetailPayType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of AssignmentTimeTrackingGroupDetailPayType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentTimeTrackingGroupDetailPayType <- function(AssignmentTimeTrackingGroupDetailPayTypeID, PayTypeID = F, AssignmentTimeTrackingGroupDetailID = F, AllowTimeTracking = F, IncludeInOvertimeCalculation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowTimeTrackingOutsideAssignment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentTimeTrackingGroupDetailPayTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetailPayType", objectId = AssignmentTimeTrackingGroupDetailPayTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentTimeTrackingGroupDetailPayType
	#'
	#' This function deletes an AssignmentTimeTrackingGroupDetailPayType
	#' @param AssignmentTimeTrackingGroupDetailPayTypeID The ID of the AssignmentTimeTrackingGroupDetailPayType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The AssignmentTimeTrackingGroupDetailPayTypeID of the deleted AssignmentTimeTrackingGroupDetailPayType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentTimeTrackingGroupDetailPayType <- function(AssignmentTimeTrackingGroupDetailPayTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetailPayType", objectId = AssignmentTimeTrackingGroupDetailPayTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentTimeTrackingGroupDetailPayType
	#'
	#' This function creates an AssignmentTimeTrackingGroupDetailPayType
	#' @param fieldNames The field values to give the created AssignmentTimeTrackingGroupDetailPayType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created AssignmentTimeTrackingGroupDetailPayType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentTimeTrackingGroupDetailPayType <- function(PayTypeID = NULL, AssignmentTimeTrackingGroupDetailID = NULL, AllowTimeTracking = NULL, IncludeInOvertimeCalculation = NULL, AllowTimeTrackingOutsideAssignment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetailPayType", body = list(DataObject = body), searchFields = append("AssignmentTimeTrackingGroupDetailPayTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentTimeTrackingGroupDetailPayType
	#'
	#' This function modifies an AssignmentTimeTrackingGroupDetailPayType
	#' @param fieldNames The field values to give the modified AssignmentTimeTrackingGroupDetailPayType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified AssignmentTimeTrackingGroupDetailPayType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentTimeTrackingGroupDetailPayType <- function(AssignmentTimeTrackingGroupDetailPayTypeID, PayTypeID = NULL, AssignmentTimeTrackingGroupDetailID = NULL, AllowTimeTracking = NULL, IncludeInOvertimeCalculation = NULL, AllowTimeTrackingOutsideAssignment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetailPayType", objectId = AssignmentTimeTrackingGroupDetailPayTypeID, body = list(DataObject = body), searchFields = append("AssignmentTimeTrackingGroupDetailPayTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimesheetWeekDays
	#'
	#' This function returns a dataframe or json object of TimesheetWeekDays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimesheetWeekDays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimesheetWeekDays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimesheetWeekDay') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimesheetWeekDays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimesheetWeekDays <- function(searchConditionsList = NULL, TimesheetWeekDayID = F, TimesheetWeekID = F, Date = F, TotalIn = F, TotalBreak = F, TotalLunch = F, TotalWorkOutOfOffice = F, TotalWork = F, TimeIn = F, TimeOut = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalInSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalPaid = F, TotalUnpaidBreak = F, TotalUnroundedPaid = F, TotalPaidEligibleForOvertime = F, TotalOvertime = F, TotalHoliday = F, TotalTimeOff = F, ManuallyEdited = F, TotalDailyOvertime = F, TotalWeeklyOvertime = F, TotalSalaryPaid = F, TotalSalaryOverage = F, TotalSalaryUnderage = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimesheetWeekDay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimesheetWeekDay
	#'
	#' This function returns a dataframe or json object of a TimesheetWeekDay
	#' @param TimesheetWeekDayID The ID of the TimesheetWeekDay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimesheetWeekDay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimesheetWeekDay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimesheetWeekDay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimesheetWeekDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimesheetWeekDay <- function(TimesheetWeekDayID, TimesheetWeekID = F, Date = F, TotalIn = F, TotalBreak = F, TotalLunch = F, TotalWorkOutOfOffice = F, TotalWork = F, TimeIn = F, TimeOut = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalInSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalPaid = F, TotalUnpaidBreak = F, TotalUnroundedPaid = F, TotalPaidEligibleForOvertime = F, TotalOvertime = F, TotalHoliday = F, TotalTimeOff = F, ManuallyEdited = F, TotalDailyOvertime = F, TotalWeeklyOvertime = F, TotalSalaryPaid = F, TotalSalaryOverage = F, TotalSalaryUnderage = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimesheetWeekDayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TimesheetWeekDay", objectId = TimesheetWeekDayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimesheetWeekDay
	#'
	#' This function deletes a TimesheetWeekDay
	#' @param TimesheetWeekDayID The ID of the TimesheetWeekDay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimesheetWeekDayID of the deleted TimesheetWeekDay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimesheetWeekDay <- function(TimesheetWeekDayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TimesheetWeekDay", objectId = TimesheetWeekDayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimesheetWeekDay
	#'
	#' This function creates a TimesheetWeekDay
	#' @param fieldNames The field values to give the created TimesheetWeekDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimesheetWeekDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimesheetWeekDay <- function(TimesheetWeekID = NULL, Date = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TimesheetWeekDay", body = list(DataObject = body), searchFields = append("TimesheetWeekDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimesheetWeekDay
	#'
	#' This function modifies a TimesheetWeekDay
	#' @param fieldNames The field values to give the modified TimesheetWeekDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimesheetWeekDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimesheetWeekDay <- function(TimesheetWeekDayID, TimesheetWeekID = NULL, Date = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TimesheetWeekDay", objectId = TimesheetWeekDayID, body = list(DataObject = body), searchFields = append("TimesheetWeekDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeTransactions
	#'
	#' This function returns a dataframe or json object of TimeTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTransaction') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimeTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeTransactions <- function(searchConditionsList = NULL, TimeTransactionID = F, StartDateTime = F, StartDateTimeEntered = F, EndDateTime = F, EndDateTimeEntered = F, Status = F, IPAddressStart = F, IPAddressEnd = F, Comment = F, TimesheetWeekDayID = F, AssignmentPayTypeID = F, StartDate = F, StartTime = F, StartTimeFormatted = F, EndDate = F, EndTimeFormatted = F, EndTime = F, Duration = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DurationInSeconds = F, AssignmentTimesheetWeekID = F, ManuallyEdited = F, TimeTransactionAccountDistributionString = F, BuildingID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimeTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeTransaction
	#'
	#' This function returns a dataframe or json object of a TimeTransaction
	#' @param TimeTransactionID The ID of the TimeTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimeTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeTransaction <- function(TimeTransactionID, StartDateTime = F, StartDateTimeEntered = F, EndDateTime = F, EndDateTimeEntered = F, Status = F, IPAddressStart = F, IPAddressEnd = F, Comment = F, TimesheetWeekDayID = F, AssignmentPayTypeID = F, StartDate = F, StartTime = F, StartTimeFormatted = F, EndDate = F, EndTimeFormatted = F, EndTime = F, Duration = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DurationInSeconds = F, AssignmentTimesheetWeekID = F, ManuallyEdited = F, TimeTransactionAccountDistributionString = F, BuildingID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TimeTransaction", objectId = TimeTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeTransaction
	#'
	#' This function deletes a TimeTransaction
	#' @param TimeTransactionID The ID of the TimeTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimeTransactionID of the deleted TimeTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeTransaction <- function(TimeTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TimeTransaction", objectId = TimeTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeTransaction
	#'
	#' This function creates a TimeTransaction
	#' @param fieldNames The field values to give the created TimeTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimeTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeTransaction <- function(StartDateTime = NULL, StartDateTimeEntered = NULL, EndDateTime = NULL, EndDateTimeEntered = NULL, Status = NULL, IPAddressStart = NULL, IPAddressEnd = NULL, Comment = NULL, TimesheetWeekDayID = NULL, AssignmentPayTypeID = NULL, AssignmentTimesheetWeekID = NULL, ManuallyEdited = NULL, BuildingID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TimeTransaction", body = list(DataObject = body), searchFields = append("TimeTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeTransaction
	#'
	#' This function modifies a TimeTransaction
	#' @param fieldNames The field values to give the modified TimeTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimeTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeTransaction <- function(TimeTransactionID, StartDateTime = NULL, StartDateTimeEntered = NULL, EndDateTime = NULL, EndDateTimeEntered = NULL, Status = NULL, IPAddressStart = NULL, IPAddressEnd = NULL, Comment = NULL, TimesheetWeekDayID = NULL, AssignmentPayTypeID = NULL, AssignmentTimesheetWeekID = NULL, ManuallyEdited = NULL, BuildingID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TimeTransaction", objectId = TimeTransactionID, body = list(DataObject = body), searchFields = append("TimeTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeTimeTrackingGroups
	#'
	#' This function returns a dataframe or json object of EmployeeTimeTrackingGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTimeTrackingGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTimeTrackingGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTimeTrackingGroup') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of EmployeeTimeTrackingGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeTimeTrackingGroups <- function(searchConditionsList = NULL, EmployeeTimeTrackingGroupID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, WorkWeekStartDay = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeTimeTrackingGroup
	#'
	#' This function returns a dataframe or json object of an EmployeeTimeTrackingGroup
	#' @param EmployeeTimeTrackingGroupID The ID of the EmployeeTimeTrackingGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTimeTrackingGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTimeTrackingGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTimeTrackingGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of EmployeeTimeTrackingGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeTimeTrackingGroup <- function(EmployeeTimeTrackingGroupID, DistrictID = F, Code = F, Description = F, CodeDescription = F, WorkWeekStartDay = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeTimeTrackingGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroup", objectId = EmployeeTimeTrackingGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeTimeTrackingGroup
	#'
	#' This function deletes an EmployeeTimeTrackingGroup
	#' @param EmployeeTimeTrackingGroupID The ID of the EmployeeTimeTrackingGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The EmployeeTimeTrackingGroupID of the deleted EmployeeTimeTrackingGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeTimeTrackingGroup <- function(EmployeeTimeTrackingGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroup", objectId = EmployeeTimeTrackingGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeTimeTrackingGroup
	#'
	#' This function creates an EmployeeTimeTrackingGroup
	#' @param fieldNames The field values to give the created EmployeeTimeTrackingGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created EmployeeTimeTrackingGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeTimeTrackingGroup <- function(DistrictID = NULL, Code = NULL, Description = NULL, WorkWeekStartDay = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroup", body = list(DataObject = body), searchFields = append("EmployeeTimeTrackingGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeTimeTrackingGroup
	#'
	#' This function modifies an EmployeeTimeTrackingGroup
	#' @param fieldNames The field values to give the modified EmployeeTimeTrackingGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified EmployeeTimeTrackingGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeTimeTrackingGroup <- function(EmployeeTimeTrackingGroupID, DistrictID = NULL, Code = NULL, Description = NULL, WorkWeekStartDay = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroup", objectId = EmployeeTimeTrackingGroupID, body = list(DataObject = body), searchFields = append("EmployeeTimeTrackingGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeTimeTrackingGroupDetails
	#'
	#' This function returns a dataframe or json object of EmployeeTimeTrackingGroupDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTimeTrackingGroupDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTimeTrackingGroupDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTimeTrackingGroupDetail') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of EmployeeTimeTrackingGroupDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeTimeTrackingGroupDetails <- function(searchConditionsList = NULL, EmployeeTimeTrackingGroupDetailID = F, EmployeeTimeTrackingGroupID = F, StartDate = F, EndDate = F, WeeklyOvertimeThreshold = F, DailyOvertimeThreshold = F, OvertimeMethod = F, PaidTimeRoundingType = F, NearestTimeIncrement = F, RoundingValue = F, OvertimeDistributionType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OvertimePaymentType = F, AllowEmployeeAccessEdits = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroupDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeTimeTrackingGroupDetail
	#'
	#' This function returns a dataframe or json object of an EmployeeTimeTrackingGroupDetail
	#' @param EmployeeTimeTrackingGroupDetailID The ID of the EmployeeTimeTrackingGroupDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTimeTrackingGroupDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTimeTrackingGroupDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTimeTrackingGroupDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of EmployeeTimeTrackingGroupDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeTimeTrackingGroupDetail <- function(EmployeeTimeTrackingGroupDetailID, EmployeeTimeTrackingGroupID = F, StartDate = F, EndDate = F, WeeklyOvertimeThreshold = F, DailyOvertimeThreshold = F, OvertimeMethod = F, PaidTimeRoundingType = F, NearestTimeIncrement = F, RoundingValue = F, OvertimeDistributionType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OvertimePaymentType = F, AllowEmployeeAccessEdits = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeTimeTrackingGroupDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroupDetail", objectId = EmployeeTimeTrackingGroupDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeTimeTrackingGroupDetail
	#'
	#' This function deletes an EmployeeTimeTrackingGroupDetail
	#' @param EmployeeTimeTrackingGroupDetailID The ID of the EmployeeTimeTrackingGroupDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The EmployeeTimeTrackingGroupDetailID of the deleted EmployeeTimeTrackingGroupDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeTimeTrackingGroupDetail <- function(EmployeeTimeTrackingGroupDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroupDetail", objectId = EmployeeTimeTrackingGroupDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeTimeTrackingGroupDetail
	#'
	#' This function creates an EmployeeTimeTrackingGroupDetail
	#' @param fieldNames The field values to give the created EmployeeTimeTrackingGroupDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created EmployeeTimeTrackingGroupDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeTimeTrackingGroupDetail <- function(EmployeeTimeTrackingGroupID = NULL, StartDate = NULL, EndDate = NULL, WeeklyOvertimeThreshold = NULL, DailyOvertimeThreshold = NULL, OvertimeMethod = NULL, PaidTimeRoundingType = NULL, NearestTimeIncrement = NULL, OvertimeDistributionType = NULL, OvertimePaymentType = NULL, AllowEmployeeAccessEdits = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroupDetail", body = list(DataObject = body), searchFields = append("EmployeeTimeTrackingGroupDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeTimeTrackingGroupDetail
	#'
	#' This function modifies an EmployeeTimeTrackingGroupDetail
	#' @param fieldNames The field values to give the modified EmployeeTimeTrackingGroupDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified EmployeeTimeTrackingGroupDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeTimeTrackingGroupDetail <- function(EmployeeTimeTrackingGroupDetailID, EmployeeTimeTrackingGroupID = NULL, StartDate = NULL, EndDate = NULL, WeeklyOvertimeThreshold = NULL, DailyOvertimeThreshold = NULL, OvertimeMethod = NULL, PaidTimeRoundingType = NULL, NearestTimeIncrement = NULL, OvertimeDistributionType = NULL, OvertimePaymentType = NULL, AllowEmployeeAccessEdits = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroupDetail", objectId = EmployeeTimeTrackingGroupDetailID, body = list(DataObject = body), searchFields = append("EmployeeTimeTrackingGroupDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimesheetWeeks
	#'
	#' This function returns a dataframe or json object of TimesheetWeeks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimesheetWeeks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimesheetWeeks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimesheetWeek') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimesheetWeeks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimesheetWeeks <- function(searchConditionsList = NULL, TimesheetWeekID = F, DistrictID = F, EmployeeID = F, StartDate = F, EndDate = F, TotalIn = F, TotalWorkOutOfOffice = F, TotalWork = F, TotalBreak = F, TotalLunch = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalInSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, RenderMoveToHistoryWithoutPayroll = F, RenderRestoreToUnsubmitted = F, TotalPaid = F, TotalUnpaidBreak = F, TotalUnroundedPaid = F, TotalPaidEligibleForOvertime = F, TotalOvertime = F, TotalHoliday = F, TotalTimeOff = F, HasUnsubmitted = F, HasWaiting = F, HasApproved = F, ManuallyEdited = F, TotalOvertimeWorked = F, TotalWorkHours = F, TotalSalaryPaid = F, TotalSalaryOverage = F, TotalSalaryUnderage = F, RenderSalary = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimesheetWeek", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimesheetWeek
	#'
	#' This function returns a dataframe or json object of a TimesheetWeek
	#' @param TimesheetWeekID The ID of the TimesheetWeek to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimesheetWeek. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimesheetWeek.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimesheetWeek') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimesheetWeek <- function(TimesheetWeekID, DistrictID = F, EmployeeID = F, StartDate = F, EndDate = F, TotalIn = F, TotalWorkOutOfOffice = F, TotalWork = F, TotalBreak = F, TotalLunch = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalInSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, RenderMoveToHistoryWithoutPayroll = F, RenderRestoreToUnsubmitted = F, TotalPaid = F, TotalUnpaidBreak = F, TotalUnroundedPaid = F, TotalPaidEligibleForOvertime = F, TotalOvertime = F, TotalHoliday = F, TotalTimeOff = F, HasUnsubmitted = F, HasWaiting = F, HasApproved = F, ManuallyEdited = F, TotalOvertimeWorked = F, TotalWorkHours = F, TotalSalaryPaid = F, TotalSalaryOverage = F, TotalSalaryUnderage = F, RenderSalary = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimesheetWeekID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TimesheetWeek", objectId = TimesheetWeekID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimesheetWeek
	#'
	#' This function deletes a TimesheetWeek
	#' @param TimesheetWeekID The ID of the TimesheetWeek to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimesheetWeekID of the deleted TimesheetWeek.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimesheetWeek <- function(TimesheetWeekID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TimesheetWeek", objectId = TimesheetWeekID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimesheetWeek
	#'
	#' This function creates a TimesheetWeek
	#' @param fieldNames The field values to give the created TimesheetWeek. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimesheetWeek <- function(DistrictID = NULL, EmployeeID = NULL, StartDate = NULL, EndDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TimesheetWeek", body = list(DataObject = body), searchFields = append("TimesheetWeekID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimesheetWeek
	#'
	#' This function modifies a TimesheetWeek
	#' @param fieldNames The field values to give the modified TimesheetWeek. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimesheetWeek <- function(TimesheetWeekID, DistrictID = NULL, EmployeeID = NULL, StartDate = NULL, EndDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TimesheetWeek", objectId = TimesheetWeekID, body = list(DataObject = body), searchFields = append("TimesheetWeekID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentTimeTrackingGroups
	#'
	#' This function returns a dataframe or json object of AssignmentTimeTrackingGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTimeTrackingGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTimeTrackingGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTimeTrackingGroup') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of AssignmentTimeTrackingGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentTimeTrackingGroups <- function(searchConditionsList = NULL, AssignmentTimeTrackingGroupID = F, DistrictID = F, Code = F, FiscalYearID = F, Description = F, CodeDescription = F, AssignmentTimeTrackingGroupIDClonedFrom = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentTimeTrackingGroup
	#'
	#' This function returns a dataframe or json object of an AssignmentTimeTrackingGroup
	#' @param AssignmentTimeTrackingGroupID The ID of the AssignmentTimeTrackingGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTimeTrackingGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTimeTrackingGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTimeTrackingGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of AssignmentTimeTrackingGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentTimeTrackingGroup <- function(AssignmentTimeTrackingGroupID, DistrictID = F, Code = F, FiscalYearID = F, Description = F, CodeDescription = F, AssignmentTimeTrackingGroupIDClonedFrom = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentTimeTrackingGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroup", objectId = AssignmentTimeTrackingGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentTimeTrackingGroup
	#'
	#' This function deletes an AssignmentTimeTrackingGroup
	#' @param AssignmentTimeTrackingGroupID The ID of the AssignmentTimeTrackingGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The AssignmentTimeTrackingGroupID of the deleted AssignmentTimeTrackingGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentTimeTrackingGroup <- function(AssignmentTimeTrackingGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroup", objectId = AssignmentTimeTrackingGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentTimeTrackingGroup
	#'
	#' This function creates an AssignmentTimeTrackingGroup
	#' @param fieldNames The field values to give the created AssignmentTimeTrackingGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created AssignmentTimeTrackingGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentTimeTrackingGroup <- function(DistrictID = NULL, Code = NULL, FiscalYearID = NULL, Description = NULL, AssignmentTimeTrackingGroupIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroup", body = list(DataObject = body), searchFields = append("AssignmentTimeTrackingGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentTimeTrackingGroup
	#'
	#' This function modifies an AssignmentTimeTrackingGroup
	#' @param fieldNames The field values to give the modified AssignmentTimeTrackingGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified AssignmentTimeTrackingGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentTimeTrackingGroup <- function(AssignmentTimeTrackingGroupID, DistrictID = NULL, Code = NULL, FiscalYearID = NULL, Description = NULL, AssignmentTimeTrackingGroupIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroup", objectId = AssignmentTimeTrackingGroupID, body = list(DataObject = body), searchFields = append("AssignmentTimeTrackingGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentTimeTrackingGroupDetails
	#'
	#' This function returns a dataframe or json object of AssignmentTimeTrackingGroupDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTimeTrackingGroupDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTimeTrackingGroupDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTimeTrackingGroupDetail') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of AssignmentTimeTrackingGroupDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentTimeTrackingGroupDetails <- function(searchConditionsList = NULL, AssignmentTimeTrackingGroupDetailID = F, AssignmentTimeTrackingGroupID = F, PayScheduleID = F, StartDate = F, EndDate = F, AllowBreak = F, AllowLunch = F, AllowWorkOutOfOffice = F, OvertimeFactor = F, PayTypeIDOvertime = F, UnpaidBreakThresholdPercent = F, PayTypeIDHoliday = F, HolidayOvertime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowOverrideTimeTransactionAccountDistributions = F, SelectBuildingOnTimeTransactionType = F, PaySalaryOverages = F, PayTypeIDOverage = F, OverageThreshold = F, DockSalaryUnderages = F, PayTypeIDUnderage = F, UnderageThreshold = F, UseOverageAssignmentPayTypeRate = F, UseUnderageAssignmentPayTypeRate = F, UseOvertimeAssignmentPayTypeRate = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentTimeTrackingGroupDetail
	#'
	#' This function returns a dataframe or json object of an AssignmentTimeTrackingGroupDetail
	#' @param AssignmentTimeTrackingGroupDetailID The ID of the AssignmentTimeTrackingGroupDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTimeTrackingGroupDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTimeTrackingGroupDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTimeTrackingGroupDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of AssignmentTimeTrackingGroupDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentTimeTrackingGroupDetail <- function(AssignmentTimeTrackingGroupDetailID, AssignmentTimeTrackingGroupID = F, PayScheduleID = F, StartDate = F, EndDate = F, AllowBreak = F, AllowLunch = F, AllowWorkOutOfOffice = F, OvertimeFactor = F, PayTypeIDOvertime = F, UnpaidBreakThresholdPercent = F, PayTypeIDHoliday = F, HolidayOvertime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowOverrideTimeTransactionAccountDistributions = F, SelectBuildingOnTimeTransactionType = F, PaySalaryOverages = F, PayTypeIDOverage = F, OverageThreshold = F, DockSalaryUnderages = F, PayTypeIDUnderage = F, UnderageThreshold = F, UseOverageAssignmentPayTypeRate = F, UseUnderageAssignmentPayTypeRate = F, UseOvertimeAssignmentPayTypeRate = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentTimeTrackingGroupDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetail", objectId = AssignmentTimeTrackingGroupDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentTimeTrackingGroupDetail
	#'
	#' This function deletes an AssignmentTimeTrackingGroupDetail
	#' @param AssignmentTimeTrackingGroupDetailID The ID of the AssignmentTimeTrackingGroupDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The AssignmentTimeTrackingGroupDetailID of the deleted AssignmentTimeTrackingGroupDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentTimeTrackingGroupDetail <- function(AssignmentTimeTrackingGroupDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetail", objectId = AssignmentTimeTrackingGroupDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentTimeTrackingGroupDetail
	#'
	#' This function creates an AssignmentTimeTrackingGroupDetail
	#' @param fieldNames The field values to give the created AssignmentTimeTrackingGroupDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created AssignmentTimeTrackingGroupDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentTimeTrackingGroupDetail <- function(AssignmentTimeTrackingGroupID = NULL, PayScheduleID = NULL, StartDate = NULL, EndDate = NULL, AllowBreak = NULL, AllowLunch = NULL, AllowWorkOutOfOffice = NULL, OvertimeFactor = NULL, PayTypeIDOvertime = NULL, UnpaidBreakThresholdPercent = NULL, PayTypeIDHoliday = NULL, HolidayOvertime = NULL, AllowOverrideTimeTransactionAccountDistributions = NULL, SelectBuildingOnTimeTransactionType = NULL, PaySalaryOverages = NULL, PayTypeIDOverage = NULL, OverageThreshold = NULL, DockSalaryUnderages = NULL, PayTypeIDUnderage = NULL, UnderageThreshold = NULL, UseOverageAssignmentPayTypeRate = NULL, UseUnderageAssignmentPayTypeRate = NULL, UseOvertimeAssignmentPayTypeRate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetail", body = list(DataObject = body), searchFields = append("AssignmentTimeTrackingGroupDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentTimeTrackingGroupDetail
	#'
	#' This function modifies an AssignmentTimeTrackingGroupDetail
	#' @param fieldNames The field values to give the modified AssignmentTimeTrackingGroupDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified AssignmentTimeTrackingGroupDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentTimeTrackingGroupDetail <- function(AssignmentTimeTrackingGroupDetailID, AssignmentTimeTrackingGroupID = NULL, PayScheduleID = NULL, StartDate = NULL, EndDate = NULL, AllowBreak = NULL, AllowLunch = NULL, AllowWorkOutOfOffice = NULL, OvertimeFactor = NULL, PayTypeIDOvertime = NULL, UnpaidBreakThresholdPercent = NULL, PayTypeIDHoliday = NULL, HolidayOvertime = NULL, AllowOverrideTimeTransactionAccountDistributions = NULL, SelectBuildingOnTimeTransactionType = NULL, PaySalaryOverages = NULL, PayTypeIDOverage = NULL, OverageThreshold = NULL, DockSalaryUnderages = NULL, PayTypeIDUnderage = NULL, UnderageThreshold = NULL, UseOverageAssignmentPayTypeRate = NULL, UseUnderageAssignmentPayTypeRate = NULL, UseOvertimeAssignmentPayTypeRate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetail", objectId = AssignmentTimeTrackingGroupDetailID, body = list(DataObject = body), searchFields = append("AssignmentTimeTrackingGroupDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentTimeTrackingGroupDetailTimeOffTypes
	#'
	#' This function returns a dataframe or json object of AssignmentTimeTrackingGroupDetailTimeOffTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTimeTrackingGroupDetailTimeOffTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTimeTrackingGroupDetailTimeOffTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTimeTrackingGroupDetailTimeOffType') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of AssignmentTimeTrackingGroupDetailTimeOffTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentTimeTrackingGroupDetailTimeOffTypes <- function(searchConditionsList = NULL, AssignmentTimeTrackingGroupDetailTimeOffTypeID = F, AssignmentTimeTrackingGroupDetailID = F, TimeOffTypeID = F, PayTypeID = F, IncludeInOvertimeCalculation = F, IsPaidTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetailTimeOffType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentTimeTrackingGroupDetailTimeOffType
	#'
	#' This function returns a dataframe or json object of an AssignmentTimeTrackingGroupDetailTimeOffType
	#' @param AssignmentTimeTrackingGroupDetailTimeOffTypeID The ID of the AssignmentTimeTrackingGroupDetailTimeOffType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTimeTrackingGroupDetailTimeOffType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTimeTrackingGroupDetailTimeOffType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTimeTrackingGroupDetailTimeOffType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of AssignmentTimeTrackingGroupDetailTimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentTimeTrackingGroupDetailTimeOffType <- function(AssignmentTimeTrackingGroupDetailTimeOffTypeID, AssignmentTimeTrackingGroupDetailID = F, TimeOffTypeID = F, PayTypeID = F, IncludeInOvertimeCalculation = F, IsPaidTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentTimeTrackingGroupDetailTimeOffTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetailTimeOffType", objectId = AssignmentTimeTrackingGroupDetailTimeOffTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentTimeTrackingGroupDetailTimeOffType
	#'
	#' This function deletes an AssignmentTimeTrackingGroupDetailTimeOffType
	#' @param AssignmentTimeTrackingGroupDetailTimeOffTypeID The ID of the AssignmentTimeTrackingGroupDetailTimeOffType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The AssignmentTimeTrackingGroupDetailTimeOffTypeID of the deleted AssignmentTimeTrackingGroupDetailTimeOffType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentTimeTrackingGroupDetailTimeOffType <- function(AssignmentTimeTrackingGroupDetailTimeOffTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetailTimeOffType", objectId = AssignmentTimeTrackingGroupDetailTimeOffTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentTimeTrackingGroupDetailTimeOffType
	#'
	#' This function creates an AssignmentTimeTrackingGroupDetailTimeOffType
	#' @param fieldNames The field values to give the created AssignmentTimeTrackingGroupDetailTimeOffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created AssignmentTimeTrackingGroupDetailTimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentTimeTrackingGroupDetailTimeOffType <- function(AssignmentTimeTrackingGroupDetailID = NULL, TimeOffTypeID = NULL, PayTypeID = NULL, IncludeInOvertimeCalculation = NULL, IsPaidTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetailTimeOffType", body = list(DataObject = body), searchFields = append("AssignmentTimeTrackingGroupDetailTimeOffTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentTimeTrackingGroupDetailTimeOffType
	#'
	#' This function modifies an AssignmentTimeTrackingGroupDetailTimeOffType
	#' @param fieldNames The field values to give the modified AssignmentTimeTrackingGroupDetailTimeOffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified AssignmentTimeTrackingGroupDetailTimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentTimeTrackingGroupDetailTimeOffType <- function(AssignmentTimeTrackingGroupDetailTimeOffTypeID, AssignmentTimeTrackingGroupDetailID = NULL, TimeOffTypeID = NULL, PayTypeID = NULL, IncludeInOvertimeCalculation = NULL, IsPaidTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetailTimeOffType", objectId = AssignmentTimeTrackingGroupDetailTimeOffTypeID, body = list(DataObject = body), searchFields = append("AssignmentTimeTrackingGroupDetailTimeOffTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempTimeTransactions
	#'
	#' This function returns a dataframe or json object of TempTimeTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimeTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimeTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimeTransaction') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TempTimeTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempTimeTransactions <- function(searchConditionsList = NULL, TempTimeTransactionID = F, StartDateTime = F, EndDateTime = F, Status = F, TimesheetWeekDayID = F, AssignmentPayTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempTimeTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempTimeTransaction
	#'
	#' This function returns a dataframe or json object of a TempTimeTransaction
	#' @param TempTimeTransactionID The ID of the TempTimeTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimeTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimeTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimeTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TempTimeTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempTimeTransaction <- function(TempTimeTransactionID, StartDateTime = F, EndDateTime = F, Status = F, TimesheetWeekDayID = F, AssignmentPayTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempTimeTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TempTimeTransaction", objectId = TempTimeTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempTimeTransaction
	#'
	#' This function deletes a TempTimeTransaction
	#' @param TempTimeTransactionID The ID of the TempTimeTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TempTimeTransactionID of the deleted TempTimeTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempTimeTransaction <- function(TempTimeTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TempTimeTransaction", objectId = TempTimeTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempTimeTransaction
	#'
	#' This function creates a TempTimeTransaction
	#' @param fieldNames The field values to give the created TempTimeTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TempTimeTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempTimeTransaction <- function(StartDateTime = NULL, EndDateTime = NULL, Status = NULL, TimesheetWeekDayID = NULL, AssignmentPayTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TempTimeTransaction", body = list(DataObject = body), searchFields = append("TempTimeTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempTimeTransaction
	#'
	#' This function modifies a TempTimeTransaction
	#' @param fieldNames The field values to give the modified TempTimeTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TempTimeTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempTimeTransaction <- function(TempTimeTransactionID, StartDateTime = NULL, EndDateTime = NULL, Status = NULL, TimesheetWeekDayID = NULL, AssignmentPayTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TempTimeTransaction", objectId = TempTimeTransactionID, body = list(DataObject = body), searchFields = append("TempTimeTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassCalculateExceptions
	#'
	#' This function returns a dataframe or json object of TempMassCalculateExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassCalculateExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassCalculateExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassCalculateException') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TempMassCalculateExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassCalculateExceptions <- function(searchConditionsList = NULL, TempMassCalculateExceptionID = F, EmployeeNameLFM = F, StartDate = F, EndDate = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempMassCalculateException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassCalculateException
	#'
	#' This function returns a dataframe or json object of a TempMassCalculateException
	#' @param TempMassCalculateExceptionID The ID of the TempMassCalculateException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassCalculateException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassCalculateException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassCalculateException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TempMassCalculateException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassCalculateException <- function(TempMassCalculateExceptionID, EmployeeNameLFM = F, StartDate = F, EndDate = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassCalculateExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TempMassCalculateException", objectId = TempMassCalculateExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassCalculateException
	#'
	#' This function deletes a TempMassCalculateException
	#' @param TempMassCalculateExceptionID The ID of the TempMassCalculateException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TempMassCalculateExceptionID of the deleted TempMassCalculateException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassCalculateException <- function(TempMassCalculateExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TempMassCalculateException", objectId = TempMassCalculateExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassCalculateException
	#'
	#' This function creates a TempMassCalculateException
	#' @param fieldNames The field values to give the created TempMassCalculateException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TempMassCalculateException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassCalculateException <- function(EmployeeNameLFM = NULL, StartDate = NULL, EndDate = NULL, Message = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TempMassCalculateException", body = list(DataObject = body), searchFields = append("TempMassCalculateExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassCalculateException
	#'
	#' This function modifies a TempMassCalculateException
	#' @param fieldNames The field values to give the modified TempMassCalculateException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TempMassCalculateException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassCalculateException <- function(TempMassCalculateExceptionID, EmployeeNameLFM = NULL, StartDate = NULL, EndDate = NULL, Message = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TempMassCalculateException", objectId = TempMassCalculateExceptionID, body = list(DataObject = body), searchFields = append("TempMassCalculateExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempTimesheetSubmissions
	#'
	#' This function returns a dataframe or json object of TempTimesheetSubmissions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimesheetSubmissions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimesheetSubmissions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimesheetSubmission') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TempTimesheetSubmissions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempTimesheetSubmissions <- function(searchConditionsList = NULL, TempTimesheetSubmissionID = F, TimesheetSubmissionID = F, EmployeeName = F, StartDate = F, EndDate = F, TotalInSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, TotalPaidSeconds = F, TotalPaidEligibleForOvertimeSeconds = F, TotalOvertimeSeconds = F, TotalUnpaidBreakSeconds = F, TotalUnroundedPaidSeconds = F, TotalHolidaySeconds = F, TotalTimeOffSeconds = F, LatestApprover = F, LatestApprovalLevelDescription = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalSalaryPaidSeconds = F, TotalSalaryOverageSeconds = F, TotalSalaryUnderageSeconds = F, HasSupervisors = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempTimesheetSubmission", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempTimesheetSubmission
	#'
	#' This function returns a dataframe or json object of a TempTimesheetSubmission
	#' @param TempTimesheetSubmissionID The ID of the TempTimesheetSubmission to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimesheetSubmission. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimesheetSubmission.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimesheetSubmission') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TempTimesheetSubmission
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempTimesheetSubmission <- function(TempTimesheetSubmissionID, TimesheetSubmissionID = F, EmployeeName = F, StartDate = F, EndDate = F, TotalInSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, TotalPaidSeconds = F, TotalPaidEligibleForOvertimeSeconds = F, TotalOvertimeSeconds = F, TotalUnpaidBreakSeconds = F, TotalUnroundedPaidSeconds = F, TotalHolidaySeconds = F, TotalTimeOffSeconds = F, LatestApprover = F, LatestApprovalLevelDescription = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalSalaryPaidSeconds = F, TotalSalaryOverageSeconds = F, TotalSalaryUnderageSeconds = F, HasSupervisors = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempTimesheetSubmissionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TempTimesheetSubmission", objectId = TempTimesheetSubmissionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempTimesheetSubmission
	#'
	#' This function deletes a TempTimesheetSubmission
	#' @param TempTimesheetSubmissionID The ID of the TempTimesheetSubmission to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TempTimesheetSubmissionID of the deleted TempTimesheetSubmission.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempTimesheetSubmission <- function(TempTimesheetSubmissionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TempTimesheetSubmission", objectId = TempTimesheetSubmissionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempTimesheetSubmission
	#'
	#' This function creates a TempTimesheetSubmission
	#' @param fieldNames The field values to give the created TempTimesheetSubmission. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TempTimesheetSubmission
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempTimesheetSubmission <- function(TimesheetSubmissionID = NULL, EmployeeName = NULL, StartDate = NULL, EndDate = NULL, LatestApprover = NULL, LatestApprovalLevelDescription = NULL, HasErrors = NULL, HasSupervisors = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TempTimesheetSubmission", body = list(DataObject = body), searchFields = append("TempTimesheetSubmissionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempTimesheetSubmission
	#'
	#' This function modifies a TempTimesheetSubmission
	#' @param fieldNames The field values to give the modified TempTimesheetSubmission. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TempTimesheetSubmission
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempTimesheetSubmission <- function(TempTimesheetSubmissionID, TimesheetSubmissionID = NULL, EmployeeName = NULL, StartDate = NULL, EndDate = NULL, LatestApprover = NULL, LatestApprovalLevelDescription = NULL, HasErrors = NULL, HasSupervisors = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TempTimesheetSubmission", objectId = TempTimesheetSubmissionID, body = list(DataObject = body), searchFields = append("TempTimesheetSubmissionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempTimesheetSubmissionErrors
	#'
	#' This function returns a dataframe or json object of TempTimesheetSubmissionErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimesheetSubmissionErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimesheetSubmissionErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimesheetSubmissionError') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TempTimesheetSubmissionErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempTimesheetSubmissionErrors <- function(searchConditionsList = NULL, TempTimesheetSubmissionErrorID = F, TempTimesheetSubmissionID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempTimesheetSubmissionError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempTimesheetSubmissionError
	#'
	#' This function returns a dataframe or json object of a TempTimesheetSubmissionError
	#' @param TempTimesheetSubmissionErrorID The ID of the TempTimesheetSubmissionError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimesheetSubmissionError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimesheetSubmissionError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimesheetSubmissionError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TempTimesheetSubmissionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempTimesheetSubmissionError <- function(TempTimesheetSubmissionErrorID, TempTimesheetSubmissionID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempTimesheetSubmissionErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TempTimesheetSubmissionError", objectId = TempTimesheetSubmissionErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempTimesheetSubmissionError
	#'
	#' This function deletes a TempTimesheetSubmissionError
	#' @param TempTimesheetSubmissionErrorID The ID of the TempTimesheetSubmissionError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TempTimesheetSubmissionErrorID of the deleted TempTimesheetSubmissionError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempTimesheetSubmissionError <- function(TempTimesheetSubmissionErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TempTimesheetSubmissionError", objectId = TempTimesheetSubmissionErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempTimesheetSubmissionError
	#'
	#' This function creates a TempTimesheetSubmissionError
	#' @param fieldNames The field values to give the created TempTimesheetSubmissionError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TempTimesheetSubmissionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempTimesheetSubmissionError <- function(TempTimesheetSubmissionID = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TempTimesheetSubmissionError", body = list(DataObject = body), searchFields = append("TempTimesheetSubmissionErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempTimesheetSubmissionError
	#'
	#' This function modifies a TempTimesheetSubmissionError
	#' @param fieldNames The field values to give the modified TempTimesheetSubmissionError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TempTimesheetSubmissionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempTimesheetSubmissionError <- function(TempTimesheetSubmissionErrorID, TempTimesheetSubmissionID = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TempTimesheetSubmissionError", objectId = TempTimesheetSubmissionErrorID, body = list(DataObject = body), searchFields = append("TempTimesheetSubmissionErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimesheetSubmissions
	#'
	#' This function returns a dataframe or json object of TimesheetSubmissions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimesheetSubmissions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimesheetSubmissions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimesheetSubmission') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimesheetSubmissions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimesheetSubmissions <- function(searchConditionsList = NULL, TimesheetSubmissionID = F, Status = F, StartDate = F, EndDate = F, TimesheetWeekID = F, AllowMoveToHistoryWithoutPayroll = F, AllowRestoreToUnsubmitted = F, TotalInSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, TotalPaidSeconds = F, TotalPaidEligibleForOvertimeSeconds = F, TotalOvertimeSeconds = F, TotalUnpaidBreakSeconds = F, TotalUnroundedPaidSeconds = F, TotalHolidaySeconds = F, TotalTimeOffSeconds = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalSalaryPaidSeconds = F, TotalSalaryOverageSeconds = F, TotalSalaryUnderageSeconds = F, RenderSalaryTotals = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimesheetSubmission", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimesheetSubmission
	#'
	#' This function returns a dataframe or json object of a TimesheetSubmission
	#' @param TimesheetSubmissionID The ID of the TimesheetSubmission to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimesheetSubmission. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimesheetSubmission.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimesheetSubmission') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimesheetSubmission
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimesheetSubmission <- function(TimesheetSubmissionID, Status = F, StartDate = F, EndDate = F, TimesheetWeekID = F, AllowMoveToHistoryWithoutPayroll = F, AllowRestoreToUnsubmitted = F, TotalInSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, TotalPaidSeconds = F, TotalPaidEligibleForOvertimeSeconds = F, TotalOvertimeSeconds = F, TotalUnpaidBreakSeconds = F, TotalUnroundedPaidSeconds = F, TotalHolidaySeconds = F, TotalTimeOffSeconds = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalSalaryPaidSeconds = F, TotalSalaryOverageSeconds = F, TotalSalaryUnderageSeconds = F, RenderSalaryTotals = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimesheetSubmissionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TimesheetSubmission", objectId = TimesheetSubmissionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimesheetSubmission
	#'
	#' This function deletes a TimesheetSubmission
	#' @param TimesheetSubmissionID The ID of the TimesheetSubmission to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimesheetSubmissionID of the deleted TimesheetSubmission.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimesheetSubmission <- function(TimesheetSubmissionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TimesheetSubmission", objectId = TimesheetSubmissionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimesheetSubmission
	#'
	#' This function creates a TimesheetSubmission
	#' @param fieldNames The field values to give the created TimesheetSubmission. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimesheetSubmission
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimesheetSubmission <- function(Status = NULL, StartDate = NULL, EndDate = NULL, TimesheetWeekID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TimesheetSubmission", body = list(DataObject = body), searchFields = append("TimesheetSubmissionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimesheetSubmission
	#'
	#' This function modifies a TimesheetSubmission
	#' @param fieldNames The field values to give the modified TimesheetSubmission. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimesheetSubmission
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimesheetSubmission <- function(TimesheetSubmissionID, Status = NULL, StartDate = NULL, EndDate = NULL, TimesheetWeekID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TimesheetSubmission", objectId = TimesheetSubmissionID, body = list(DataObject = body), searchFields = append("TimesheetSubmissionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimesheetSubmissionApprovals
	#'
	#' This function returns a dataframe or json object of TimesheetSubmissionApprovals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimesheetSubmissionApprovals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimesheetSubmissionApprovals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimesheetSubmissionApproval') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimesheetSubmissionApprovals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimesheetSubmissionApprovals <- function(searchConditionsList = NULL, TimesheetSubmissionApprovalID = F, TimesheetSubmissionID = F, Status = F, Comment = F, UserIDApprover = F, OrganizationChartRelationshipID = F, Level = F, LevelDescription = F, ApprovalActionTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimesheetSubmissionApproval", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimesheetSubmissionApproval
	#'
	#' This function returns a dataframe or json object of a TimesheetSubmissionApproval
	#' @param TimesheetSubmissionApprovalID The ID of the TimesheetSubmissionApproval to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimesheetSubmissionApproval. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimesheetSubmissionApproval.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimesheetSubmissionApproval') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimesheetSubmissionApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimesheetSubmissionApproval <- function(TimesheetSubmissionApprovalID, TimesheetSubmissionID = F, Status = F, Comment = F, UserIDApprover = F, OrganizationChartRelationshipID = F, Level = F, LevelDescription = F, ApprovalActionTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimesheetSubmissionApprovalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TimesheetSubmissionApproval", objectId = TimesheetSubmissionApprovalID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimesheetSubmissionApproval
	#'
	#' This function deletes a TimesheetSubmissionApproval
	#' @param TimesheetSubmissionApprovalID The ID of the TimesheetSubmissionApproval to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimesheetSubmissionApprovalID of the deleted TimesheetSubmissionApproval.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimesheetSubmissionApproval <- function(TimesheetSubmissionApprovalID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TimesheetSubmissionApproval", objectId = TimesheetSubmissionApprovalID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimesheetSubmissionApproval
	#'
	#' This function creates a TimesheetSubmissionApproval
	#' @param fieldNames The field values to give the created TimesheetSubmissionApproval. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimesheetSubmissionApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimesheetSubmissionApproval <- function(TimesheetSubmissionID = NULL, Status = NULL, Comment = NULL, UserIDApprover = NULL, OrganizationChartRelationshipID = NULL, Level = NULL, LevelDescription = NULL, ApprovalActionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TimesheetSubmissionApproval", body = list(DataObject = body), searchFields = append("TimesheetSubmissionApprovalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimesheetSubmissionApproval
	#'
	#' This function modifies a TimesheetSubmissionApproval
	#' @param fieldNames The field values to give the modified TimesheetSubmissionApproval. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimesheetSubmissionApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimesheetSubmissionApproval <- function(TimesheetSubmissionApprovalID, TimesheetSubmissionID = NULL, Status = NULL, Comment = NULL, UserIDApprover = NULL, OrganizationChartRelationshipID = NULL, Level = NULL, LevelDescription = NULL, ApprovalActionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TimesheetSubmissionApproval", objectId = TimesheetSubmissionApprovalID, body = list(DataObject = body), searchFields = append("TimesheetSubmissionApprovalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentPayTypeTimesheetWeeks
	#'
	#' This function returns a dataframe or json object of AssignmentPayTypeTimesheetWeeks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentPayTypeTimesheetWeeks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentPayTypeTimesheetWeeks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentPayTypeTimesheetWeek') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of AssignmentPayTypeTimesheetWeeks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentPayTypeTimesheetWeeks <- function(searchConditionsList = NULL, AssignmentPayTypeTimesheetWeekID = F, AssignmentPayTypeID = F, TimesheetWeekID = F, AssignmentTimesheetWeekID = F, StartDate = F, EndDate = F, TotalPaid = F, TotalUnpaidBreak = F, TotalUnroundedPaid = F, TotalPaidEligibleForOvertime = F, TotalOvertime = F, TotalHoliday = F, TotalTimeOff = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalDailyOvertime = F, TotalWeeklyOvertime = F, TotalSalaryPaid = F, TotalSalaryOverage = F, TotalSalaryUnderage = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentPayTypeTimesheetWeek", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentPayTypeTimesheetWeek
	#'
	#' This function returns a dataframe or json object of an AssignmentPayTypeTimesheetWeek
	#' @param AssignmentPayTypeTimesheetWeekID The ID of the AssignmentPayTypeTimesheetWeek to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentPayTypeTimesheetWeek. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentPayTypeTimesheetWeek.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentPayTypeTimesheetWeek') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of AssignmentPayTypeTimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentPayTypeTimesheetWeek <- function(AssignmentPayTypeTimesheetWeekID, AssignmentPayTypeID = F, TimesheetWeekID = F, AssignmentTimesheetWeekID = F, StartDate = F, EndDate = F, TotalPaid = F, TotalUnpaidBreak = F, TotalUnroundedPaid = F, TotalPaidEligibleForOvertime = F, TotalOvertime = F, TotalHoliday = F, TotalTimeOff = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalDailyOvertime = F, TotalWeeklyOvertime = F, TotalSalaryPaid = F, TotalSalaryOverage = F, TotalSalaryUnderage = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentPayTypeTimesheetWeekID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "AssignmentPayTypeTimesheetWeek", objectId = AssignmentPayTypeTimesheetWeekID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentPayTypeTimesheetWeek
	#'
	#' This function deletes an AssignmentPayTypeTimesheetWeek
	#' @param AssignmentPayTypeTimesheetWeekID The ID of the AssignmentPayTypeTimesheetWeek to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The AssignmentPayTypeTimesheetWeekID of the deleted AssignmentPayTypeTimesheetWeek.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentPayTypeTimesheetWeek <- function(AssignmentPayTypeTimesheetWeekID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "AssignmentPayTypeTimesheetWeek", objectId = AssignmentPayTypeTimesheetWeekID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentPayTypeTimesheetWeek
	#'
	#' This function creates an AssignmentPayTypeTimesheetWeek
	#' @param fieldNames The field values to give the created AssignmentPayTypeTimesheetWeek. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created AssignmentPayTypeTimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentPayTypeTimesheetWeek <- function(AssignmentPayTypeID = NULL, TimesheetWeekID = NULL, AssignmentTimesheetWeekID = NULL, StartDate = NULL, EndDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "AssignmentPayTypeTimesheetWeek", body = list(DataObject = body), searchFields = append("AssignmentPayTypeTimesheetWeekID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentPayTypeTimesheetWeek
	#'
	#' This function modifies an AssignmentPayTypeTimesheetWeek
	#' @param fieldNames The field values to give the modified AssignmentPayTypeTimesheetWeek. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified AssignmentPayTypeTimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentPayTypeTimesheetWeek <- function(AssignmentPayTypeTimesheetWeekID, AssignmentPayTypeID = NULL, TimesheetWeekID = NULL, AssignmentTimesheetWeekID = NULL, StartDate = NULL, EndDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "AssignmentPayTypeTimesheetWeek", objectId = AssignmentPayTypeTimesheetWeekID, body = list(DataObject = body), searchFields = append("AssignmentPayTypeTimesheetWeekID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentPayTypeTimesheetWeekDays
	#'
	#' This function returns a dataframe or json object of AssignmentPayTypeTimesheetWeekDays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentPayTypeTimesheetWeekDays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentPayTypeTimesheetWeekDays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentPayTypeTimesheetWeekDay') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of AssignmentPayTypeTimesheetWeekDays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentPayTypeTimesheetWeekDays <- function(searchConditionsList = NULL, AssignmentPayTypeTimesheetWeekDayID = F, AssignmentPayTypeTimesheetWeekID = F, TimesheetWeekDayID = F, Date = F, Rate = F, Paid = F, UnpaidBreak = F, UnroundedPaid = F, PaidEligibleForOvertime = F, DailyOvertime = F, WeeklyOvertime = F, Holiday = F, TimeOff = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OvertimePaid = F, SalaryPaid = F, SalaryOverage = F, SalaryUnderage = F, OvertimeRateOverride = F, OverageRateOverride = F, UnderageRateOverride = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentPayTypeTimesheetWeekDay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentPayTypeTimesheetWeekDay
	#'
	#' This function returns a dataframe or json object of an AssignmentPayTypeTimesheetWeekDay
	#' @param AssignmentPayTypeTimesheetWeekDayID The ID of the AssignmentPayTypeTimesheetWeekDay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentPayTypeTimesheetWeekDay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentPayTypeTimesheetWeekDay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentPayTypeTimesheetWeekDay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of AssignmentPayTypeTimesheetWeekDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentPayTypeTimesheetWeekDay <- function(AssignmentPayTypeTimesheetWeekDayID, AssignmentPayTypeTimesheetWeekID = F, TimesheetWeekDayID = F, Date = F, Rate = F, Paid = F, UnpaidBreak = F, UnroundedPaid = F, PaidEligibleForOvertime = F, DailyOvertime = F, WeeklyOvertime = F, Holiday = F, TimeOff = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OvertimePaid = F, SalaryPaid = F, SalaryOverage = F, SalaryUnderage = F, OvertimeRateOverride = F, OverageRateOverride = F, UnderageRateOverride = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentPayTypeTimesheetWeekDayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "AssignmentPayTypeTimesheetWeekDay", objectId = AssignmentPayTypeTimesheetWeekDayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentPayTypeTimesheetWeekDay
	#'
	#' This function deletes an AssignmentPayTypeTimesheetWeekDay
	#' @param AssignmentPayTypeTimesheetWeekDayID The ID of the AssignmentPayTypeTimesheetWeekDay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The AssignmentPayTypeTimesheetWeekDayID of the deleted AssignmentPayTypeTimesheetWeekDay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentPayTypeTimesheetWeekDay <- function(AssignmentPayTypeTimesheetWeekDayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "AssignmentPayTypeTimesheetWeekDay", objectId = AssignmentPayTypeTimesheetWeekDayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentPayTypeTimesheetWeekDay
	#'
	#' This function creates an AssignmentPayTypeTimesheetWeekDay
	#' @param fieldNames The field values to give the created AssignmentPayTypeTimesheetWeekDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created AssignmentPayTypeTimesheetWeekDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentPayTypeTimesheetWeekDay <- function(AssignmentPayTypeTimesheetWeekID = NULL, TimesheetWeekDayID = NULL, Date = NULL, Rate = NULL, Paid = NULL, UnpaidBreak = NULL, UnroundedPaid = NULL, PaidEligibleForOvertime = NULL, DailyOvertime = NULL, WeeklyOvertime = NULL, Holiday = NULL, TimeOff = NULL, OvertimePaid = NULL, SalaryPaid = NULL, SalaryOverage = NULL, SalaryUnderage = NULL, OvertimeRateOverride = NULL, OverageRateOverride = NULL, UnderageRateOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "AssignmentPayTypeTimesheetWeekDay", body = list(DataObject = body), searchFields = append("AssignmentPayTypeTimesheetWeekDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentPayTypeTimesheetWeekDay
	#'
	#' This function modifies an AssignmentPayTypeTimesheetWeekDay
	#' @param fieldNames The field values to give the modified AssignmentPayTypeTimesheetWeekDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified AssignmentPayTypeTimesheetWeekDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentPayTypeTimesheetWeekDay <- function(AssignmentPayTypeTimesheetWeekDayID, AssignmentPayTypeTimesheetWeekID = NULL, TimesheetWeekDayID = NULL, Date = NULL, Rate = NULL, Paid = NULL, UnpaidBreak = NULL, UnroundedPaid = NULL, PaidEligibleForOvertime = NULL, DailyOvertime = NULL, WeeklyOvertime = NULL, Holiday = NULL, TimeOff = NULL, OvertimePaid = NULL, SalaryPaid = NULL, SalaryOverage = NULL, SalaryUnderage = NULL, OvertimeRateOverride = NULL, OverageRateOverride = NULL, UnderageRateOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "AssignmentPayTypeTimesheetWeekDay", objectId = AssignmentPayTypeTimesheetWeekDayID, body = list(DataObject = body), searchFields = append("AssignmentPayTypeTimesheetWeekDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAssignmentPayTypeTimesheetWeeks
	#'
	#' This function returns a dataframe or json object of TempAssignmentPayTypeTimesheetWeeks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAssignmentPayTypeTimesheetWeeks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAssignmentPayTypeTimesheetWeeks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAssignmentPayTypeTimesheetWeek') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TempAssignmentPayTypeTimesheetWeeks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAssignmentPayTypeTimesheetWeeks <- function(searchConditionsList = NULL, TempAssignmentPayTypeTimesheetWeekID = F, AssignmentPayTypeID = F, TimesheetWeekID = F, AssignmentTimesheetWeekID = F, APTIdentifier = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempAssignmentPayTypeTimesheetWeek", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAssignmentPayTypeTimesheetWeek
	#'
	#' This function returns a dataframe or json object of a TempAssignmentPayTypeTimesheetWeek
	#' @param TempAssignmentPayTypeTimesheetWeekID The ID of the TempAssignmentPayTypeTimesheetWeek to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAssignmentPayTypeTimesheetWeek. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAssignmentPayTypeTimesheetWeek.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAssignmentPayTypeTimesheetWeek') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TempAssignmentPayTypeTimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAssignmentPayTypeTimesheetWeek <- function(TempAssignmentPayTypeTimesheetWeekID, AssignmentPayTypeID = F, TimesheetWeekID = F, AssignmentTimesheetWeekID = F, APTIdentifier = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAssignmentPayTypeTimesheetWeekID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TempAssignmentPayTypeTimesheetWeek", objectId = TempAssignmentPayTypeTimesheetWeekID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAssignmentPayTypeTimesheetWeek
	#'
	#' This function deletes a TempAssignmentPayTypeTimesheetWeek
	#' @param TempAssignmentPayTypeTimesheetWeekID The ID of the TempAssignmentPayTypeTimesheetWeek to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TempAssignmentPayTypeTimesheetWeekID of the deleted TempAssignmentPayTypeTimesheetWeek.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAssignmentPayTypeTimesheetWeek <- function(TempAssignmentPayTypeTimesheetWeekID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TempAssignmentPayTypeTimesheetWeek", objectId = TempAssignmentPayTypeTimesheetWeekID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAssignmentPayTypeTimesheetWeek
	#'
	#' This function creates a TempAssignmentPayTypeTimesheetWeek
	#' @param fieldNames The field values to give the created TempAssignmentPayTypeTimesheetWeek. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TempAssignmentPayTypeTimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAssignmentPayTypeTimesheetWeek <- function(AssignmentPayTypeID = NULL, TimesheetWeekID = NULL, AssignmentTimesheetWeekID = NULL, APTIdentifier = NULL, StartDate = NULL, EndDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TempAssignmentPayTypeTimesheetWeek", body = list(DataObject = body), searchFields = append("TempAssignmentPayTypeTimesheetWeekID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAssignmentPayTypeTimesheetWeek
	#'
	#' This function modifies a TempAssignmentPayTypeTimesheetWeek
	#' @param fieldNames The field values to give the modified TempAssignmentPayTypeTimesheetWeek. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TempAssignmentPayTypeTimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAssignmentPayTypeTimesheetWeek <- function(TempAssignmentPayTypeTimesheetWeekID, AssignmentPayTypeID = NULL, TimesheetWeekID = NULL, AssignmentTimesheetWeekID = NULL, APTIdentifier = NULL, StartDate = NULL, EndDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TempAssignmentPayTypeTimesheetWeek", objectId = TempAssignmentPayTypeTimesheetWeekID, body = list(DataObject = body), searchFields = append("TempAssignmentPayTypeTimesheetWeekID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAssignmentPayTypeTimesheetWeekDays
	#'
	#' This function returns a dataframe or json object of TempAssignmentPayTypeTimesheetWeekDays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAssignmentPayTypeTimesheetWeekDays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAssignmentPayTypeTimesheetWeekDays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAssignmentPayTypeTimesheetWeekDay') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TempAssignmentPayTypeTimesheetWeekDays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAssignmentPayTypeTimesheetWeekDays <- function(searchConditionsList = NULL, TempAssignmentPayTypeTimesheetWeekDayID = F, TempAssignmentPayTypeTimesheetWeekID = F, APTIdentifier = F, TimesheetWeekDayID = F, Date = F, Rate = F, Paid = F, UnpaidBreak = F, UnroundedPaid = F, PaidEligibleForOvertime = F, DailyOvertime = F, WeeklyOvertime = F, Holiday = F, TimeOff = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OvertimePaid = F, SalaryPaid = F, SalaryOverage = F, SalaryUnderage = F, UseOverageAssignmentPayTypeRate = F, UseUnderageAssignmentPayTypeRate = F, UseOvertimeAssignmentPayTypeRate = F, OverageRateOverride = F, UnderageRateOverride = F, OvertimeRateOverride = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempAssignmentPayTypeTimesheetWeekDay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAssignmentPayTypeTimesheetWeekDay
	#'
	#' This function returns a dataframe or json object of a TempAssignmentPayTypeTimesheetWeekDay
	#' @param TempAssignmentPayTypeTimesheetWeekDayID The ID of the TempAssignmentPayTypeTimesheetWeekDay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAssignmentPayTypeTimesheetWeekDay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAssignmentPayTypeTimesheetWeekDay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAssignmentPayTypeTimesheetWeekDay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TempAssignmentPayTypeTimesheetWeekDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAssignmentPayTypeTimesheetWeekDay <- function(TempAssignmentPayTypeTimesheetWeekDayID, TempAssignmentPayTypeTimesheetWeekID = F, APTIdentifier = F, TimesheetWeekDayID = F, Date = F, Rate = F, Paid = F, UnpaidBreak = F, UnroundedPaid = F, PaidEligibleForOvertime = F, DailyOvertime = F, WeeklyOvertime = F, Holiday = F, TimeOff = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OvertimePaid = F, SalaryPaid = F, SalaryOverage = F, SalaryUnderage = F, UseOverageAssignmentPayTypeRate = F, UseUnderageAssignmentPayTypeRate = F, UseOvertimeAssignmentPayTypeRate = F, OverageRateOverride = F, UnderageRateOverride = F, OvertimeRateOverride = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAssignmentPayTypeTimesheetWeekDayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TempAssignmentPayTypeTimesheetWeekDay", objectId = TempAssignmentPayTypeTimesheetWeekDayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAssignmentPayTypeTimesheetWeekDay
	#'
	#' This function deletes a TempAssignmentPayTypeTimesheetWeekDay
	#' @param TempAssignmentPayTypeTimesheetWeekDayID The ID of the TempAssignmentPayTypeTimesheetWeekDay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TempAssignmentPayTypeTimesheetWeekDayID of the deleted TempAssignmentPayTypeTimesheetWeekDay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAssignmentPayTypeTimesheetWeekDay <- function(TempAssignmentPayTypeTimesheetWeekDayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TempAssignmentPayTypeTimesheetWeekDay", objectId = TempAssignmentPayTypeTimesheetWeekDayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAssignmentPayTypeTimesheetWeekDay
	#'
	#' This function creates a TempAssignmentPayTypeTimesheetWeekDay
	#' @param fieldNames The field values to give the created TempAssignmentPayTypeTimesheetWeekDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TempAssignmentPayTypeTimesheetWeekDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAssignmentPayTypeTimesheetWeekDay <- function(TempAssignmentPayTypeTimesheetWeekID = NULL, APTIdentifier = NULL, TimesheetWeekDayID = NULL, Date = NULL, Rate = NULL, UseOverageAssignmentPayTypeRate = NULL, UseUnderageAssignmentPayTypeRate = NULL, UseOvertimeAssignmentPayTypeRate = NULL, OverageRateOverride = NULL, UnderageRateOverride = NULL, OvertimeRateOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TempAssignmentPayTypeTimesheetWeekDay", body = list(DataObject = body), searchFields = append("TempAssignmentPayTypeTimesheetWeekDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAssignmentPayTypeTimesheetWeekDay
	#'
	#' This function modifies a TempAssignmentPayTypeTimesheetWeekDay
	#' @param fieldNames The field values to give the modified TempAssignmentPayTypeTimesheetWeekDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TempAssignmentPayTypeTimesheetWeekDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAssignmentPayTypeTimesheetWeekDay <- function(TempAssignmentPayTypeTimesheetWeekDayID, TempAssignmentPayTypeTimesheetWeekID = NULL, APTIdentifier = NULL, TimesheetWeekDayID = NULL, Date = NULL, Rate = NULL, UseOverageAssignmentPayTypeRate = NULL, UseUnderageAssignmentPayTypeRate = NULL, UseOvertimeAssignmentPayTypeRate = NULL, OverageRateOverride = NULL, UnderageRateOverride = NULL, OvertimeRateOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TempAssignmentPayTypeTimesheetWeekDay", objectId = TempAssignmentPayTypeTimesheetWeekDayID, body = list(DataObject = body), searchFields = append("TempAssignmentPayTypeTimesheetWeekDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempTimesheetWeeks
	#'
	#' This function returns a dataframe or json object of TempTimesheetWeeks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimesheetWeeks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimesheetWeeks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimesheetWeek') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TempTimesheetWeeks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempTimesheetWeeks <- function(searchConditionsList = NULL, TempTimesheetWeekID = F, TimesheetWeekID = F, EmployeeID = F, EmployeeFullNameLFM = F, EmployeeNumber = F, StartDate = F, EndDate = F, OldPaid = F, FormattedOldPaid = F, HasUnsubmitted = F, HasWaiting = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionTypeCodeDescription = F, AssignmentTypeCodes = F, BuildingCodes = F, HasApproved = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempTimesheetWeek", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempTimesheetWeek
	#'
	#' This function returns a dataframe or json object of a TempTimesheetWeek
	#' @param TempTimesheetWeekID The ID of the TempTimesheetWeek to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimesheetWeek. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimesheetWeek.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimesheetWeek') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TempTimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempTimesheetWeek <- function(TempTimesheetWeekID, TimesheetWeekID = F, EmployeeID = F, EmployeeFullNameLFM = F, EmployeeNumber = F, StartDate = F, EndDate = F, OldPaid = F, FormattedOldPaid = F, HasUnsubmitted = F, HasWaiting = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionTypeCodeDescription = F, AssignmentTypeCodes = F, BuildingCodes = F, HasApproved = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempTimesheetWeekID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TempTimesheetWeek", objectId = TempTimesheetWeekID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempTimesheetWeek
	#'
	#' This function deletes a TempTimesheetWeek
	#' @param TempTimesheetWeekID The ID of the TempTimesheetWeek to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TempTimesheetWeekID of the deleted TempTimesheetWeek.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempTimesheetWeek <- function(TempTimesheetWeekID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TempTimesheetWeek", objectId = TempTimesheetWeekID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempTimesheetWeek
	#'
	#' This function creates a TempTimesheetWeek
	#' @param fieldNames The field values to give the created TempTimesheetWeek. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TempTimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempTimesheetWeek <- function(TimesheetWeekID = NULL, EmployeeID = NULL, EmployeeFullNameLFM = NULL, EmployeeNumber = NULL, StartDate = NULL, EndDate = NULL, HasUnsubmitted = NULL, HasWaiting = NULL, PositionTypeCodeDescription = NULL, AssignmentTypeCodes = NULL, BuildingCodes = NULL, HasApproved = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TempTimesheetWeek", body = list(DataObject = body), searchFields = append("TempTimesheetWeekID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempTimesheetWeek
	#'
	#' This function modifies a TempTimesheetWeek
	#' @param fieldNames The field values to give the modified TempTimesheetWeek. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TempTimesheetWeek
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempTimesheetWeek <- function(TempTimesheetWeekID, TimesheetWeekID = NULL, EmployeeID = NULL, EmployeeFullNameLFM = NULL, EmployeeNumber = NULL, StartDate = NULL, EndDate = NULL, HasUnsubmitted = NULL, HasWaiting = NULL, PositionTypeCodeDescription = NULL, AssignmentTypeCodes = NULL, BuildingCodes = NULL, HasApproved = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TempTimesheetWeek", objectId = TempTimesheetWeekID, body = list(DataObject = body), searchFields = append("TempTimesheetWeekID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeTimeTrackingGroupDetailIPRanges
	#'
	#' This function returns a dataframe or json object of EmployeeTimeTrackingGroupDetailIPRanges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTimeTrackingGroupDetailIPRanges. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTimeTrackingGroupDetailIPRanges.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTimeTrackingGroupDetailIPRange') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of EmployeeTimeTrackingGroupDetailIPRanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeTimeTrackingGroupDetailIPRanges <- function(searchConditionsList = NULL, EmployeeTimeTrackingGroupDetailIPRangeID = F, EmployeeTimeTrackingGroupDetailID = F, IPRangeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroupDetailIPRange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeTimeTrackingGroupDetailIPRange
	#'
	#' This function returns a dataframe or json object of an EmployeeTimeTrackingGroupDetailIPRange
	#' @param EmployeeTimeTrackingGroupDetailIPRangeID The ID of the EmployeeTimeTrackingGroupDetailIPRange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTimeTrackingGroupDetailIPRange. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTimeTrackingGroupDetailIPRange.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTimeTrackingGroupDetailIPRange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of EmployeeTimeTrackingGroupDetailIPRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeTimeTrackingGroupDetailIPRange <- function(EmployeeTimeTrackingGroupDetailIPRangeID, EmployeeTimeTrackingGroupDetailID = F, IPRangeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeTimeTrackingGroupDetailIPRangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroupDetailIPRange", objectId = EmployeeTimeTrackingGroupDetailIPRangeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeTimeTrackingGroupDetailIPRange
	#'
	#' This function deletes an EmployeeTimeTrackingGroupDetailIPRange
	#' @param EmployeeTimeTrackingGroupDetailIPRangeID The ID of the EmployeeTimeTrackingGroupDetailIPRange to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The EmployeeTimeTrackingGroupDetailIPRangeID of the deleted EmployeeTimeTrackingGroupDetailIPRange.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeTimeTrackingGroupDetailIPRange <- function(EmployeeTimeTrackingGroupDetailIPRangeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroupDetailIPRange", objectId = EmployeeTimeTrackingGroupDetailIPRangeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeTimeTrackingGroupDetailIPRange
	#'
	#' This function creates an EmployeeTimeTrackingGroupDetailIPRange
	#' @param fieldNames The field values to give the created EmployeeTimeTrackingGroupDetailIPRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created EmployeeTimeTrackingGroupDetailIPRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeTimeTrackingGroupDetailIPRange <- function(EmployeeTimeTrackingGroupDetailID = NULL, IPRangeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroupDetailIPRange", body = list(DataObject = body), searchFields = append("EmployeeTimeTrackingGroupDetailIPRangeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeTimeTrackingGroupDetailIPRange
	#'
	#' This function modifies an EmployeeTimeTrackingGroupDetailIPRange
	#' @param fieldNames The field values to give the modified EmployeeTimeTrackingGroupDetailIPRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified EmployeeTimeTrackingGroupDetailIPRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeTimeTrackingGroupDetailIPRange <- function(EmployeeTimeTrackingGroupDetailIPRangeID, EmployeeTimeTrackingGroupDetailID = NULL, IPRangeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroupDetailIPRange", objectId = EmployeeTimeTrackingGroupDetailIPRangeID, body = list(DataObject = body), searchFields = append("EmployeeTimeTrackingGroupDetailIPRangeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeTransactionAccountDistributionOverrides
	#'
	#' This function returns a dataframe or json object of TimeTransactionAccountDistributionOverrides
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTransactionAccountDistributionOverrides. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTransactionAccountDistributionOverrides.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTransactionAccountDistributionOverride') to get more field paths.
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
	#' @concept Time Tracking
	#' @return A list of TimeTransactionAccountDistributionOverrides
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeTransactionAccountDistributionOverrides <- function(searchConditionsList = NULL, TimeTransactionAccountDistributionOverrideID = F, TimeTransactionID = F, AccountID = F, Percent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimeTransactionAccountDistributionOverride", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeTransactionAccountDistributionOverride
	#'
	#' This function returns a dataframe or json object of a TimeTransactionAccountDistributionOverride
	#' @param TimeTransactionAccountDistributionOverrideID The ID of the TimeTransactionAccountDistributionOverride to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeTransactionAccountDistributionOverride. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeTransactionAccountDistributionOverride.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeTransactionAccountDistributionOverride') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A dataframe or of TimeTransactionAccountDistributionOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeTransactionAccountDistributionOverride <- function(TimeTransactionAccountDistributionOverrideID, TimeTransactionID = F, AccountID = F, Percent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeTransactionAccountDistributionOverrideID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeTracking", objectName = "TimeTransactionAccountDistributionOverride", objectId = TimeTransactionAccountDistributionOverrideID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeTransactionAccountDistributionOverride
	#'
	#' This function deletes a TimeTransactionAccountDistributionOverride
	#' @param TimeTransactionAccountDistributionOverrideID The ID of the TimeTransactionAccountDistributionOverride to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The TimeTransactionAccountDistributionOverrideID of the deleted TimeTransactionAccountDistributionOverride.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeTransactionAccountDistributionOverride <- function(TimeTransactionAccountDistributionOverrideID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeTracking", objectName = "TimeTransactionAccountDistributionOverride", objectId = TimeTransactionAccountDistributionOverrideID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeTransactionAccountDistributionOverride
	#'
	#' This function creates a TimeTransactionAccountDistributionOverride
	#' @param fieldNames The field values to give the created TimeTransactionAccountDistributionOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return A newly created TimeTransactionAccountDistributionOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeTransactionAccountDistributionOverride <- function(TimeTransactionID = NULL, AccountID = NULL, Percent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "TimeTracking", objectName = "TimeTransactionAccountDistributionOverride", body = list(DataObject = body), searchFields = append("TimeTransactionAccountDistributionOverrideID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeTransactionAccountDistributionOverride
	#'
	#' This function modifies a TimeTransactionAccountDistributionOverride
	#' @param fieldNames The field values to give the modified TimeTransactionAccountDistributionOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Tracking
	#' @return The modified TimeTransactionAccountDistributionOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeTransactionAccountDistributionOverride <- function(TimeTransactionAccountDistributionOverrideID, TimeTransactionID = NULL, AccountID = NULL, Percent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "TimeTracking", objectName = "TimeTransactionAccountDistributionOverride", objectId = TimeTransactionAccountDistributionOverrideID, body = list(DataObject = body), searchFields = append("TimeTransactionAccountDistributionOverrideID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
