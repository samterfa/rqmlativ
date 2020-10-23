
	#' List TimeOffConfigFiscalYears
	#'
	#' This function returns a dataframe or json object of TimeOffConfigFiscalYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffConfigFiscalYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffConfigFiscalYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffConfigFiscalYear') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TimeOffConfigFiscalYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeOffConfigFiscalYears <- function(searchConditionsList = NULL, ConfigFiscalYearID = F, FiscalYearID = F, DistrictID = F, OrganizationChartID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "ConfigFiscalYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeOffConfigFiscalYear
	#'
	#' This function returns a dataframe or json object of a TimeOffConfigFiscalYear
	#' @param TimeOffConfigFiscalYearID The ID of the TimeOffConfigFiscalYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffConfigFiscalYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffConfigFiscalYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffConfigFiscalYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TimeOffConfigFiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeOffConfigFiscalYear <- function(TimeOffConfigFiscalYearID, ConfigFiscalYearID = F, FiscalYearID = F, DistrictID = F, OrganizationChartID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeOffConfigFiscalYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "ConfigFiscalYear", objectId = TimeOffConfigFiscalYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeOffConfigFiscalYear
	#'
	#' This function deletes a TimeOffConfigFiscalYear
	#' @param TimeOffConfigFiscalYearID The ID of the TimeOffConfigFiscalYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TimeOffConfigFiscalYearID of the deleted TimeOffConfigFiscalYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeOffConfigFiscalYear <- function(TimeOffConfigFiscalYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "ConfigFiscalYear", objectId = TimeOffConfigFiscalYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeOffConfigFiscalYear
	#'
	#' This function creates a TimeOffConfigFiscalYear
	#' @param fieldNames The field values to give the created TimeOffConfigFiscalYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TimeOffConfigFiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeOffConfigFiscalYear <- function(FiscalYearID = NULL, DistrictID = NULL, OrganizationChartID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "ConfigFiscalYear", body = list(DataObject = body), searchFields = append("ConfigFiscalYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeOffConfigFiscalYear
	#'
	#' This function modifies a TimeOffConfigFiscalYear
	#' @param fieldNames The field values to give the modified TimeOffConfigFiscalYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TimeOffConfigFiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeOffConfigFiscalYear <- function(ConfigFiscalYearID, FiscalYearID = NULL, DistrictID = NULL, OrganizationChartID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "ConfigFiscalYear", objectId = ConfigFiscalYearID, body = list(DataObject = body), searchFields = append("ConfigFiscalYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeOffTempExceptions
	#'
	#' This function returns a dataframe or json object of TimeOffTempExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTempExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTempExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffTempException') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TimeOffTempExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeOffTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, TimeOffTransactionID = F, StartDateTime = F, TimeOffTransactionDescription = F, EmployeeNameLFM = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TempException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeOffTempException
	#'
	#' This function returns a dataframe or json object of a TimeOffTempException
	#' @param TimeOffTempExceptionID The ID of the TimeOffTempException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTempException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTempException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffTempException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TimeOffTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeOffTempException <- function(TimeOffTempExceptionID, TempExceptionID = F, TimeOffTransactionID = F, StartDateTime = F, TimeOffTransactionDescription = F, EmployeeNameLFM = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeOffTempExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TempException", objectId = TimeOffTempExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeOffTempException
	#'
	#' This function deletes a TimeOffTempException
	#' @param TimeOffTempExceptionID The ID of the TimeOffTempException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TimeOffTempExceptionID of the deleted TimeOffTempException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeOffTempException <- function(TimeOffTempExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TempException", objectId = TimeOffTempExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeOffTempException
	#'
	#' This function creates a TimeOffTempException
	#' @param fieldNames The field values to give the created TimeOffTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TimeOffTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeOffTempException <- function(TimeOffTransactionID = NULL, StartDateTime = NULL, TimeOffTransactionDescription = NULL, EmployeeNameLFM = NULL, Message = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TempException", body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeOffTempException
	#'
	#' This function modifies a TimeOffTempException
	#' @param fieldNames The field values to give the modified TimeOffTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TimeOffTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeOffTempException <- function(TempExceptionID, TimeOffTransactionID = NULL, StartDateTime = NULL, TimeOffTransactionDescription = NULL, EmployeeNameLFM = NULL, Message = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TempException", objectId = TempExceptionID, body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeOffApprovalTaskSecurityGroups
	#'
	#' This function returns a dataframe or json object of TimeOffApprovalTaskSecurityGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffApprovalTaskSecurityGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffApprovalTaskSecurityGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffApprovalTaskSecurityGroup') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TimeOffApprovalTaskSecurityGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeOffApprovalTaskSecurityGroups <- function(searchConditionsList = NULL, TimeOffApprovalTaskSecurityGroupID = F, TimeOffApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TimeOffApprovalTaskSecurityGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeOffApprovalTaskSecurityGroup
	#'
	#' This function returns a dataframe or json object of a TimeOffApprovalTaskSecurityGroup
	#' @param TimeOffApprovalTaskSecurityGroupID The ID of the TimeOffApprovalTaskSecurityGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffApprovalTaskSecurityGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffApprovalTaskSecurityGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffApprovalTaskSecurityGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TimeOffApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeOffApprovalTaskSecurityGroup <- function(TimeOffApprovalTaskSecurityGroupID, TimeOffApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeOffApprovalTaskSecurityGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TimeOffApprovalTaskSecurityGroup", objectId = TimeOffApprovalTaskSecurityGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeOffApprovalTaskSecurityGroup
	#'
	#' This function deletes a TimeOffApprovalTaskSecurityGroup
	#' @param TimeOffApprovalTaskSecurityGroupID The ID of the TimeOffApprovalTaskSecurityGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TimeOffApprovalTaskSecurityGroupID of the deleted TimeOffApprovalTaskSecurityGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeOffApprovalTaskSecurityGroup <- function(TimeOffApprovalTaskSecurityGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TimeOffApprovalTaskSecurityGroup", objectId = TimeOffApprovalTaskSecurityGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeOffApprovalTaskSecurityGroup
	#'
	#' This function creates a TimeOffApprovalTaskSecurityGroup
	#' @param fieldNames The field values to give the created TimeOffApprovalTaskSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TimeOffApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeOffApprovalTaskSecurityGroup <- function(TimeOffApprovalTaskID = NULL, GroupIDSecurity = NULL, IsAlternate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TimeOffApprovalTaskSecurityGroup", body = list(DataObject = body), searchFields = append("TimeOffApprovalTaskSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeOffApprovalTaskSecurityGroup
	#'
	#' This function modifies a TimeOffApprovalTaskSecurityGroup
	#' @param fieldNames The field values to give the modified TimeOffApprovalTaskSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TimeOffApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeOffApprovalTaskSecurityGroup <- function(TimeOffApprovalTaskSecurityGroupID, TimeOffApprovalTaskID = NULL, GroupIDSecurity = NULL, IsAlternate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TimeOffApprovalTaskSecurityGroup", objectId = TimeOffApprovalTaskSecurityGroupID, body = list(DataObject = body), searchFields = append("TimeOffApprovalTaskSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeOffApprovalTasks
	#'
	#' This function returns a dataframe or json object of TimeOffApprovalTasks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffApprovalTasks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffApprovalTasks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffApprovalTask') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TimeOffApprovalTasks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeOffApprovalTasks <- function(searchConditionsList = NULL, TimeOffApprovalTaskID = F, DistrictID = F, IsConditional = F, Level = F, Description = F, UseOrganizationChart = F, FilterData = F, StandardFilterCollectionData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TimeOffApprovalTask", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeOffApprovalTask
	#'
	#' This function returns a dataframe or json object of a TimeOffApprovalTask
	#' @param TimeOffApprovalTaskID The ID of the TimeOffApprovalTask to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffApprovalTask. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffApprovalTask.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffApprovalTask') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TimeOffApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeOffApprovalTask <- function(TimeOffApprovalTaskID, DistrictID = F, IsConditional = F, Level = F, Description = F, UseOrganizationChart = F, FilterData = F, StandardFilterCollectionData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeOffApprovalTaskID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TimeOffApprovalTask", objectId = TimeOffApprovalTaskID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeOffApprovalTask
	#'
	#' This function deletes a TimeOffApprovalTask
	#' @param TimeOffApprovalTaskID The ID of the TimeOffApprovalTask to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TimeOffApprovalTaskID of the deleted TimeOffApprovalTask.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeOffApprovalTask <- function(TimeOffApprovalTaskID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TimeOffApprovalTask", objectId = TimeOffApprovalTaskID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeOffApprovalTask
	#'
	#' This function creates a TimeOffApprovalTask
	#' @param fieldNames The field values to give the created TimeOffApprovalTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TimeOffApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeOffApprovalTask <- function(DistrictID = NULL, IsConditional = NULL, Level = NULL, Description = NULL, UseOrganizationChart = NULL, FilterData = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TimeOffApprovalTask", body = list(DataObject = body), searchFields = append("TimeOffApprovalTaskID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeOffApprovalTask
	#'
	#' This function modifies a TimeOffApprovalTask
	#' @param fieldNames The field values to give the modified TimeOffApprovalTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TimeOffApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeOffApprovalTask <- function(TimeOffApprovalTaskID, DistrictID = NULL, IsConditional = NULL, Level = NULL, Description = NULL, UseOrganizationChart = NULL, FilterData = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TimeOffApprovalTask", objectId = TimeOffApprovalTaskID, body = list(DataObject = body), searchFields = append("TimeOffApprovalTaskID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AllocationDefinitions
	#'
	#' This function returns a dataframe or json object of AllocationDefinitions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AllocationDefinitions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AllocationDefinitions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AllocationDefinition') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of AllocationDefinitions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAllocationDefinitions <- function(searchConditionsList = NULL, AllocationDefinitionID = F, Description = F, TimeOffTypeIDMoveTo = F, AllocationTypeID = F, AllocationUnit = F, RollDisposition = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "AllocationDefinition", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AllocationDefinition
	#'
	#' This function returns a dataframe or json object of an AllocationDefinition
	#' @param AllocationDefinitionID The ID of the AllocationDefinition to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AllocationDefinition. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AllocationDefinition.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AllocationDefinition') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of AllocationDefinition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAllocationDefinition <- function(AllocationDefinitionID, Description = F, TimeOffTypeIDMoveTo = F, AllocationTypeID = F, AllocationUnit = F, RollDisposition = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AllocationDefinitionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "AllocationDefinition", objectId = AllocationDefinitionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AllocationDefinition
	#'
	#' This function deletes an AllocationDefinition
	#' @param AllocationDefinitionID The ID of the AllocationDefinition to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The AllocationDefinitionID of the deleted AllocationDefinition.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAllocationDefinition <- function(AllocationDefinitionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "AllocationDefinition", objectId = AllocationDefinitionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AllocationDefinition
	#'
	#' This function creates an AllocationDefinition
	#' @param fieldNames The field values to give the created AllocationDefinition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created AllocationDefinition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAllocationDefinition <- function(Description = NULL, TimeOffTypeIDMoveTo = NULL, AllocationTypeID = NULL, AllocationUnit = NULL, RollDisposition = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "AllocationDefinition", body = list(DataObject = body), searchFields = append("AllocationDefinitionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AllocationDefinition
	#'
	#' This function modifies an AllocationDefinition
	#' @param fieldNames The field values to give the modified AllocationDefinition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified AllocationDefinition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAllocationDefinition <- function(AllocationDefinitionID, Description = NULL, TimeOffTypeIDMoveTo = NULL, AllocationTypeID = NULL, AllocationUnit = NULL, RollDisposition = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "AllocationDefinition", objectId = AllocationDefinitionID, body = list(DataObject = body), searchFields = append("AllocationDefinitionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AllocationDefinitionDetails
	#'
	#' This function returns a dataframe or json object of AllocationDefinitionDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AllocationDefinitionDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AllocationDefinitionDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AllocationDefinitionDetail') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of AllocationDefinitionDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAllocationDefinitionDetails <- function(searchConditionsList = NULL, AllocationDefinitionDetailID = F, AllocationDefinitionID = F, EmployeeYearExperienceLow = F, EmployeeYearExperienceHigh = F, AllocationAmount = F, MaximumAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "AllocationDefinitionDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AllocationDefinitionDetail
	#'
	#' This function returns a dataframe or json object of an AllocationDefinitionDetail
	#' @param AllocationDefinitionDetailID The ID of the AllocationDefinitionDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AllocationDefinitionDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AllocationDefinitionDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AllocationDefinitionDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of AllocationDefinitionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAllocationDefinitionDetail <- function(AllocationDefinitionDetailID, AllocationDefinitionID = F, EmployeeYearExperienceLow = F, EmployeeYearExperienceHigh = F, AllocationAmount = F, MaximumAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AllocationDefinitionDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "AllocationDefinitionDetail", objectId = AllocationDefinitionDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AllocationDefinitionDetail
	#'
	#' This function deletes an AllocationDefinitionDetail
	#' @param AllocationDefinitionDetailID The ID of the AllocationDefinitionDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The AllocationDefinitionDetailID of the deleted AllocationDefinitionDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAllocationDefinitionDetail <- function(AllocationDefinitionDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "AllocationDefinitionDetail", objectId = AllocationDefinitionDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AllocationDefinitionDetail
	#'
	#' This function creates an AllocationDefinitionDetail
	#' @param fieldNames The field values to give the created AllocationDefinitionDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created AllocationDefinitionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAllocationDefinitionDetail <- function(AllocationDefinitionID = NULL, EmployeeYearExperienceLow = NULL, EmployeeYearExperienceHigh = NULL, AllocationAmount = NULL, MaximumAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "AllocationDefinitionDetail", body = list(DataObject = body), searchFields = append("AllocationDefinitionDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AllocationDefinitionDetail
	#'
	#' This function modifies an AllocationDefinitionDetail
	#' @param fieldNames The field values to give the modified AllocationDefinitionDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified AllocationDefinitionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAllocationDefinitionDetail <- function(AllocationDefinitionDetailID, AllocationDefinitionID = NULL, EmployeeYearExperienceLow = NULL, EmployeeYearExperienceHigh = NULL, AllocationAmount = NULL, MaximumAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "AllocationDefinitionDetail", objectId = AllocationDefinitionDetailID, body = list(DataObject = body), searchFields = append("AllocationDefinitionDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AllocationSchedules
	#'
	#' This function returns a dataframe or json object of AllocationSchedules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AllocationSchedules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AllocationSchedules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AllocationSchedule') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of AllocationSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAllocationSchedules <- function(searchConditionsList = NULL, AllocationScheduleID = F, DistrictID = F, Code = F, Description = F, ExpectedRunDate = F, CodeDescription = F, AllocationCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "AllocationSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AllocationSchedule
	#'
	#' This function returns a dataframe or json object of an AllocationSchedule
	#' @param AllocationScheduleID The ID of the AllocationSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AllocationSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AllocationSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AllocationSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of AllocationSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAllocationSchedule <- function(AllocationScheduleID, DistrictID = F, Code = F, Description = F, ExpectedRunDate = F, CodeDescription = F, AllocationCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AllocationScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "AllocationSchedule", objectId = AllocationScheduleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AllocationSchedule
	#'
	#' This function deletes an AllocationSchedule
	#' @param AllocationScheduleID The ID of the AllocationSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The AllocationScheduleID of the deleted AllocationSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAllocationSchedule <- function(AllocationScheduleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "AllocationSchedule", objectId = AllocationScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AllocationSchedule
	#'
	#' This function creates an AllocationSchedule
	#' @param fieldNames The field values to give the created AllocationSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created AllocationSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAllocationSchedule <- function(DistrictID = NULL, Code = NULL, Description = NULL, ExpectedRunDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "AllocationSchedule", body = list(DataObject = body), searchFields = append("AllocationScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AllocationSchedule
	#'
	#' This function modifies an AllocationSchedule
	#' @param fieldNames The field values to give the modified AllocationSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified AllocationSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAllocationSchedule <- function(AllocationScheduleID, DistrictID = NULL, Code = NULL, Description = NULL, ExpectedRunDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "AllocationSchedule", objectId = AllocationScheduleID, body = list(DataObject = body), searchFields = append("AllocationScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AllocationTypes
	#'
	#' This function returns a dataframe or json object of AllocationTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AllocationTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AllocationTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AllocationType') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of AllocationTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAllocationTypes <- function(searchConditionsList = NULL, AllocationTypeID = F, DistrictID = F, Code = F, Description = F, TimeOffReasonID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UseAssignmentHoursPerDay = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "AllocationType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AllocationType
	#'
	#' This function returns a dataframe or json object of an AllocationType
	#' @param AllocationTypeID The ID of the AllocationType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AllocationType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AllocationType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AllocationType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of AllocationType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAllocationType <- function(AllocationTypeID, DistrictID = F, Code = F, Description = F, TimeOffReasonID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UseAssignmentHoursPerDay = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AllocationTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "AllocationType", objectId = AllocationTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AllocationType
	#'
	#' This function deletes an AllocationType
	#' @param AllocationTypeID The ID of the AllocationType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The AllocationTypeID of the deleted AllocationType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAllocationType <- function(AllocationTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "AllocationType", objectId = AllocationTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AllocationType
	#'
	#' This function creates an AllocationType
	#' @param fieldNames The field values to give the created AllocationType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created AllocationType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAllocationType <- function(DistrictID = NULL, Code = NULL, Description = NULL, TimeOffReasonID = NULL, UseAssignmentHoursPerDay = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "AllocationType", body = list(DataObject = body), searchFields = append("AllocationTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AllocationType
	#'
	#' This function modifies an AllocationType
	#' @param fieldNames The field values to give the modified AllocationType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified AllocationType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAllocationType <- function(AllocationTypeID, DistrictID = NULL, Code = NULL, Description = NULL, TimeOffReasonID = NULL, UseAssignmentHoursPerDay = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "AllocationType", objectId = AllocationTypeID, body = list(DataObject = body), searchFields = append("AllocationTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AllocationTypeSchedules
	#'
	#' This function returns a dataframe or json object of AllocationTypeSchedules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AllocationTypeSchedules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AllocationTypeSchedules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AllocationTypeSchedule') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of AllocationTypeSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAllocationTypeSchedules <- function(searchConditionsList = NULL, AllocationTypeScheduleID = F, AllocationScheduleID = F, AllocationDefinitionID = F, Scope = F, TransactionDate = F, AnniversaryLowDate = F, AnniversaryHighDate = F, HoursAllocated = F, AllocationCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DaysAllocated = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "AllocationTypeSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AllocationTypeSchedule
	#'
	#' This function returns a dataframe or json object of an AllocationTypeSchedule
	#' @param AllocationTypeScheduleID The ID of the AllocationTypeSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AllocationTypeSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AllocationTypeSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AllocationTypeSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of AllocationTypeSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAllocationTypeSchedule <- function(AllocationTypeScheduleID, AllocationScheduleID = F, AllocationDefinitionID = F, Scope = F, TransactionDate = F, AnniversaryLowDate = F, AnniversaryHighDate = F, HoursAllocated = F, AllocationCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DaysAllocated = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AllocationTypeScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "AllocationTypeSchedule", objectId = AllocationTypeScheduleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AllocationTypeSchedule
	#'
	#' This function deletes an AllocationTypeSchedule
	#' @param AllocationTypeScheduleID The ID of the AllocationTypeSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The AllocationTypeScheduleID of the deleted AllocationTypeSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAllocationTypeSchedule <- function(AllocationTypeScheduleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "AllocationTypeSchedule", objectId = AllocationTypeScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AllocationTypeSchedule
	#'
	#' This function creates an AllocationTypeSchedule
	#' @param fieldNames The field values to give the created AllocationTypeSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created AllocationTypeSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAllocationTypeSchedule <- function(AllocationScheduleID = NULL, AllocationDefinitionID = NULL, Scope = NULL, TransactionDate = NULL, AnniversaryLowDate = NULL, AnniversaryHighDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "AllocationTypeSchedule", body = list(DataObject = body), searchFields = append("AllocationTypeScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AllocationTypeSchedule
	#'
	#' This function modifies an AllocationTypeSchedule
	#' @param fieldNames The field values to give the modified AllocationTypeSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified AllocationTypeSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAllocationTypeSchedule <- function(AllocationTypeScheduleID, AllocationScheduleID = NULL, AllocationDefinitionID = NULL, Scope = NULL, TransactionDate = NULL, AnniversaryLowDate = NULL, AnniversaryHighDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "AllocationTypeSchedule", objectId = AllocationTypeScheduleID, body = list(DataObject = body), searchFields = append("AllocationTypeScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeOffConfigDistricts
	#'
	#' This function returns a dataframe or json object of TimeOffConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffConfigDistrict') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TimeOffConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeOffConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, TimeOffAnniversary = F, SendApprovedMessage = F, TimeOffTransactionRequestApprovedMessageSubject = F, TimeOffTransactionRequestApprovedMessageContent = F, SendDeniedMessage = F, TimeOffTransactionRequestDeniedMessageSubject = F, TimeOffTransactionRequestDeniedMessageContent = F, SendWaitingMessage = F, TimeOffTransactionRequestWaitingMessageSubject = F, TimeOffTransactionRequestWaitingMessageContent = F, LaunchThirdPartyAbsenceRequest = F, FilterData = F, StandardFilterCollectionData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ThirdPartyAbsenceRequestURL = F, AppendThirdPartyQueryParameters = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeOffConfigDistrict
	#'
	#' This function returns a dataframe or json object of a TimeOffConfigDistrict
	#' @param TimeOffConfigDistrictID The ID of the TimeOffConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TimeOffConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeOffConfigDistrict <- function(TimeOffConfigDistrictID, ConfigDistrictID = F, DistrictID = F, TimeOffAnniversary = F, SendApprovedMessage = F, TimeOffTransactionRequestApprovedMessageSubject = F, TimeOffTransactionRequestApprovedMessageContent = F, SendDeniedMessage = F, TimeOffTransactionRequestDeniedMessageSubject = F, TimeOffTransactionRequestDeniedMessageContent = F, SendWaitingMessage = F, TimeOffTransactionRequestWaitingMessageSubject = F, TimeOffTransactionRequestWaitingMessageContent = F, LaunchThirdPartyAbsenceRequest = F, FilterData = F, StandardFilterCollectionData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ThirdPartyAbsenceRequestURL = F, AppendThirdPartyQueryParameters = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeOffConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "ConfigDistrict", objectId = TimeOffConfigDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeOffConfigDistrict
	#'
	#' This function deletes a TimeOffConfigDistrict
	#' @param TimeOffConfigDistrictID The ID of the TimeOffConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TimeOffConfigDistrictID of the deleted TimeOffConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeOffConfigDistrict <- function(TimeOffConfigDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "ConfigDistrict", objectId = TimeOffConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeOffConfigDistrict
	#'
	#' This function creates a TimeOffConfigDistrict
	#' @param fieldNames The field values to give the created TimeOffConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TimeOffConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeOffConfigDistrict <- function(DistrictID = NULL, TimeOffAnniversary = NULL, SendApprovedMessage = NULL, TimeOffTransactionRequestApprovedMessageSubject = NULL, TimeOffTransactionRequestApprovedMessageContent = NULL, SendDeniedMessage = NULL, TimeOffTransactionRequestDeniedMessageSubject = NULL, TimeOffTransactionRequestDeniedMessageContent = NULL, SendWaitingMessage = NULL, TimeOffTransactionRequestWaitingMessageSubject = NULL, TimeOffTransactionRequestWaitingMessageContent = NULL, LaunchThirdPartyAbsenceRequest = NULL, FilterData = NULL, ThirdPartyAbsenceRequestURL = NULL, AppendThirdPartyQueryParameters = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeOffConfigDistrict
	#'
	#' This function modifies a TimeOffConfigDistrict
	#' @param fieldNames The field values to give the modified TimeOffConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TimeOffConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeOffConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, TimeOffAnniversary = NULL, SendApprovedMessage = NULL, TimeOffTransactionRequestApprovedMessageSubject = NULL, TimeOffTransactionRequestApprovedMessageContent = NULL, SendDeniedMessage = NULL, TimeOffTransactionRequestDeniedMessageSubject = NULL, TimeOffTransactionRequestDeniedMessageContent = NULL, SendWaitingMessage = NULL, TimeOffTransactionRequestWaitingMessageSubject = NULL, TimeOffTransactionRequestWaitingMessageContent = NULL, LaunchThirdPartyAbsenceRequest = NULL, FilterData = NULL, ThirdPartyAbsenceRequestURL = NULL, AppendThirdPartyQueryParameters = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeTimeOffTypes
	#'
	#' This function returns a dataframe or json object of EmployeeTimeOffTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTimeOffTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTimeOffTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTimeOffType') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of EmployeeTimeOffTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeTimeOffTypes <- function(searchConditionsList = NULL, EmployeeTimeOffTypeID = F, TimeOffTypeID = F, EmployeeID = F, SecondsPerDayOverride = F, AllocationTypeIDOverride = F, NextAllocationCycleDate = F, NextCycleDateYearsExperience = F, CurrentYearEndingBalanceSeconds = F, CurrentYearEndingBalance = F, NextYearEndingBalanceSeconds = F, NextYearEndingBalance = F, PriorYearEndingBalanceSeconds = F, PriorYearEndingBalance = F, CurrentYearWaitingForApprovalSeconds = F, CurrentYearWaitingForApproval = F, CurrentYearAllocatedSeconds = F, CurrentYearAllocated = F, CurrentYearUsedSeconds = F, CurrentYearUsed = F, TotalEndingBalanceSeconds = F, TotalEndingBalance = F, TotalWaitingForApprovalSeconds = F, TotalWaitingForApproval = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SecondsPerDay = F, NextYearEndingBalanceDays = F, PriorYearEndingBalanceDays = F, CurrentYearWaitingForApprovalDays = F, CurrentYearUsedDays = F, TotalWaitingForApprovalDays = F, FormattedOverrideSecondsDecimal = F, CurrentYearEndingBalanceDecimal = F, NextYearEndingBalanceDecimal = F, PriorYearEndingBalanceDecimal = F, CurrentYearWaitingForApprovalDecimal = F, CurrentYearAllocatedDecimal = F, CurrentYearUsedDecimal = F, TotalEndingBalanceDecimal = F, TotalWaitingForApprovalDecimal = F, TimeOffAnniversaryDate = F, TimeOffAnniversary = F, TimeOffAnniversaryYears = F, PriorFiscalYearEndingBalanceDays = F, CurrentFiscalYearAllocatedDays = F, CurrentFiscalYearUsedDays = F, CurrentYearAllocatedDays = F, CurrentYearEndingBalanceDays = F, TotalEndingBalanceDays = F, CurrentYearUnpaidDecimal = F, CurrentYearUnpaidDays = F, CurrentYearUnpaidSeconds = F, CurrentYearUnpaid = F, EmployeeSecondsPerDay = F, EmployeeHoursPerDay = F, TypedTotalEndingBalance = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "EmployeeTimeOffType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeTimeOffType
	#'
	#' This function returns a dataframe or json object of an EmployeeTimeOffType
	#' @param EmployeeTimeOffTypeID The ID of the EmployeeTimeOffType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTimeOffType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTimeOffType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTimeOffType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of EmployeeTimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeTimeOffType <- function(EmployeeTimeOffTypeID, TimeOffTypeID = F, EmployeeID = F, SecondsPerDayOverride = F, AllocationTypeIDOverride = F, NextAllocationCycleDate = F, NextCycleDateYearsExperience = F, CurrentYearEndingBalanceSeconds = F, CurrentYearEndingBalance = F, NextYearEndingBalanceSeconds = F, NextYearEndingBalance = F, PriorYearEndingBalanceSeconds = F, PriorYearEndingBalance = F, CurrentYearWaitingForApprovalSeconds = F, CurrentYearWaitingForApproval = F, CurrentYearAllocatedSeconds = F, CurrentYearAllocated = F, CurrentYearUsedSeconds = F, CurrentYearUsed = F, TotalEndingBalanceSeconds = F, TotalEndingBalance = F, TotalWaitingForApprovalSeconds = F, TotalWaitingForApproval = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SecondsPerDay = F, NextYearEndingBalanceDays = F, PriorYearEndingBalanceDays = F, CurrentYearWaitingForApprovalDays = F, CurrentYearUsedDays = F, TotalWaitingForApprovalDays = F, FormattedOverrideSecondsDecimal = F, CurrentYearEndingBalanceDecimal = F, NextYearEndingBalanceDecimal = F, PriorYearEndingBalanceDecimal = F, CurrentYearWaitingForApprovalDecimal = F, CurrentYearAllocatedDecimal = F, CurrentYearUsedDecimal = F, TotalEndingBalanceDecimal = F, TotalWaitingForApprovalDecimal = F, TimeOffAnniversaryDate = F, TimeOffAnniversary = F, TimeOffAnniversaryYears = F, PriorFiscalYearEndingBalanceDays = F, CurrentFiscalYearAllocatedDays = F, CurrentFiscalYearUsedDays = F, CurrentYearAllocatedDays = F, CurrentYearEndingBalanceDays = F, TotalEndingBalanceDays = F, CurrentYearUnpaidDecimal = F, CurrentYearUnpaidDays = F, CurrentYearUnpaidSeconds = F, CurrentYearUnpaid = F, EmployeeSecondsPerDay = F, EmployeeHoursPerDay = F, TypedTotalEndingBalance = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeTimeOffTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "EmployeeTimeOffType", objectId = EmployeeTimeOffTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeTimeOffType
	#'
	#' This function deletes an EmployeeTimeOffType
	#' @param EmployeeTimeOffTypeID The ID of the EmployeeTimeOffType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The EmployeeTimeOffTypeID of the deleted EmployeeTimeOffType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeTimeOffType <- function(EmployeeTimeOffTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "EmployeeTimeOffType", objectId = EmployeeTimeOffTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeTimeOffType
	#'
	#' This function creates an EmployeeTimeOffType
	#' @param fieldNames The field values to give the created EmployeeTimeOffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created EmployeeTimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeTimeOffType <- function(TimeOffTypeID = NULL, EmployeeID = NULL, SecondsPerDayOverride = NULL, AllocationTypeIDOverride = NULL, TimeOffAnniversaryDate = NULL, TimeOffAnniversary = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "EmployeeTimeOffType", body = list(DataObject = body), searchFields = append("EmployeeTimeOffTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeTimeOffType
	#'
	#' This function modifies an EmployeeTimeOffType
	#' @param fieldNames The field values to give the modified EmployeeTimeOffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified EmployeeTimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeTimeOffType <- function(EmployeeTimeOffTypeID, TimeOffTypeID = NULL, EmployeeID = NULL, SecondsPerDayOverride = NULL, AllocationTypeIDOverride = NULL, TimeOffAnniversaryDate = NULL, TimeOffAnniversary = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "EmployeeTimeOffType", objectId = EmployeeTimeOffTypeID, body = list(DataObject = body), searchFields = append("EmployeeTimeOffTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Entitlements
	#'
	#' This function returns a dataframe or json object of Entitlements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Entitlements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Entitlements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Entitlement') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of Entitlements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntitlements <- function(searchConditionsList = NULL, EntitlementID = F, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "Entitlement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Entitlement
	#'
	#' This function returns a dataframe or json object of an Entitlement
	#' @param EntitlementID The ID of the Entitlement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Entitlement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Entitlement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Entitlement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of Entitlement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntitlement <- function(EntitlementID, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntitlementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "Entitlement", objectId = EntitlementID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Entitlement
	#'
	#' This function deletes an Entitlement
	#' @param EntitlementID The ID of the Entitlement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The EntitlementID of the deleted Entitlement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntitlement <- function(EntitlementID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "Entitlement", objectId = EntitlementID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Entitlement
	#'
	#' This function creates an Entitlement
	#' @param fieldNames The field values to give the created Entitlement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created Entitlement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntitlement <- function(Code = NULL, Description = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "Entitlement", body = list(DataObject = body), searchFields = append("EntitlementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Entitlement
	#'
	#' This function modifies an Entitlement
	#' @param fieldNames The field values to give the modified Entitlement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified Entitlement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntitlement <- function(EntitlementID, Code = NULL, Description = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "Entitlement", objectId = EntitlementID, body = list(DataObject = body), searchFields = append("EntitlementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntitlementDetails
	#'
	#' This function returns a dataframe or json object of EntitlementDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntitlementDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntitlementDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntitlementDetail') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of EntitlementDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntitlementDetails <- function(searchConditionsList = NULL, EntitlementDetailID = F, EntitlementID = F, TimeOffTypeID = F, AllocationTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "EntitlementDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntitlementDetail
	#'
	#' This function returns a dataframe or json object of an EntitlementDetail
	#' @param EntitlementDetailID The ID of the EntitlementDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntitlementDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntitlementDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntitlementDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of EntitlementDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntitlementDetail <- function(EntitlementDetailID, EntitlementID = F, TimeOffTypeID = F, AllocationTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntitlementDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "EntitlementDetail", objectId = EntitlementDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntitlementDetail
	#'
	#' This function deletes an EntitlementDetail
	#' @param EntitlementDetailID The ID of the EntitlementDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The EntitlementDetailID of the deleted EntitlementDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntitlementDetail <- function(EntitlementDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "EntitlementDetail", objectId = EntitlementDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntitlementDetail
	#'
	#' This function creates an EntitlementDetail
	#' @param fieldNames The field values to give the created EntitlementDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created EntitlementDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntitlementDetail <- function(EntitlementID = NULL, TimeOffTypeID = NULL, AllocationTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "EntitlementDetail", body = list(DataObject = body), searchFields = append("EntitlementDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntitlementDetail
	#'
	#' This function modifies an EntitlementDetail
	#' @param fieldNames The field values to give the modified EntitlementDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified EntitlementDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntitlementDetail <- function(EntitlementDetailID, EntitlementID = NULL, TimeOffTypeID = NULL, AllocationTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "EntitlementDetail", objectId = EntitlementDetailID, body = list(DataObject = body), searchFields = append("EntitlementDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeeTimeOffTypes
	#'
	#' This function returns a dataframe or json object of TempEmployeeTimeOffTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeTimeOffTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeTimeOffTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeTimeOffType') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TempEmployeeTimeOffTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeeTimeOffTypes <- function(searchConditionsList = NULL, TempEmployeeTimeOffTypeID = F, EmployeeID = F, EmployeeNameLFM = F, TimeOffTypeID = F, TimeOffTypeCode = F, TimeOffTypeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, TimeOffAnniversary = F, TimeOffAnniversaryDate = F, EmployeeTimeOffTypeID = F, TimeOffTypeCodeDescription = F, EmployeeSecondsPerDay = F, OldTotalEndingBalanceSeconds = F, NewTotalEndingBalanceSeconds = F, OldTotalEndingBalanceDays = F, NewTotalEndingBalanceDays = F, OldSecondsPerDayOverride = F, NewSecondsPerDayOverride = F, EmployeeIsActive = F, ErrorCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TempEmployeeTimeOffType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeeTimeOffType
	#'
	#' This function returns a dataframe or json object of a TempEmployeeTimeOffType
	#' @param TempEmployeeTimeOffTypeID The ID of the TempEmployeeTimeOffType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeTimeOffType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeTimeOffType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeTimeOffType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TempEmployeeTimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeeTimeOffType <- function(TempEmployeeTimeOffTypeID, EmployeeID = F, EmployeeNameLFM = F, TimeOffTypeID = F, TimeOffTypeCode = F, TimeOffTypeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, TimeOffAnniversary = F, TimeOffAnniversaryDate = F, EmployeeTimeOffTypeID = F, TimeOffTypeCodeDescription = F, EmployeeSecondsPerDay = F, OldTotalEndingBalanceSeconds = F, NewTotalEndingBalanceSeconds = F, OldTotalEndingBalanceDays = F, NewTotalEndingBalanceDays = F, OldSecondsPerDayOverride = F, NewSecondsPerDayOverride = F, EmployeeIsActive = F, ErrorCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeeTimeOffTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TempEmployeeTimeOffType", objectId = TempEmployeeTimeOffTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeeTimeOffType
	#'
	#' This function deletes a TempEmployeeTimeOffType
	#' @param TempEmployeeTimeOffTypeID The ID of the TempEmployeeTimeOffType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TempEmployeeTimeOffTypeID of the deleted TempEmployeeTimeOffType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeeTimeOffType <- function(TempEmployeeTimeOffTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TempEmployeeTimeOffType", objectId = TempEmployeeTimeOffTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeeTimeOffType
	#'
	#' This function creates a TempEmployeeTimeOffType
	#' @param fieldNames The field values to give the created TempEmployeeTimeOffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TempEmployeeTimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeeTimeOffType <- function(EmployeeID = NULL, EmployeeNameLFM = NULL, TimeOffTypeID = NULL, TimeOffTypeCode = NULL, TimeOffTypeDescription = NULL, EmployeeNumber = NULL, TimeOffAnniversary = NULL, TimeOffAnniversaryDate = NULL, EmployeeTimeOffTypeID = NULL, TimeOffTypeCodeDescription = NULL, OldTotalEndingBalanceDays = NULL, NewTotalEndingBalanceDays = NULL, EmployeeIsActive = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TempEmployeeTimeOffType", body = list(DataObject = body), searchFields = append("TempEmployeeTimeOffTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeeTimeOffType
	#'
	#' This function modifies a TempEmployeeTimeOffType
	#' @param fieldNames The field values to give the modified TempEmployeeTimeOffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TempEmployeeTimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeeTimeOffType <- function(TempEmployeeTimeOffTypeID, EmployeeID = NULL, EmployeeNameLFM = NULL, TimeOffTypeID = NULL, TimeOffTypeCode = NULL, TimeOffTypeDescription = NULL, EmployeeNumber = NULL, TimeOffAnniversary = NULL, TimeOffAnniversaryDate = NULL, EmployeeTimeOffTypeID = NULL, TimeOffTypeCodeDescription = NULL, OldTotalEndingBalanceDays = NULL, NewTotalEndingBalanceDays = NULL, EmployeeIsActive = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TempEmployeeTimeOffType", objectId = TempEmployeeTimeOffTypeID, body = list(DataObject = body), searchFields = append("TempEmployeeTimeOffTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempTimeOffTransactions
	#'
	#' This function returns a dataframe or json object of TempTimeOffTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimeOffTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimeOffTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimeOffTransaction') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TempTimeOffTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempTimeOffTransactions <- function(searchConditionsList = NULL, TempTimeOffTransactionID = F, EmployeeTimeOffTypeID = F, TempEmployeeTimeOffTypeID = F, EmployeeID = F, EmployeeNameLFM = F, EmployeeServiceYears = F, TimeOffTypeID = F, TimeOffTypeCode = F, TimeOffTypeDescription = F, TransactionType = F, AssignmentID = F, AssignmentPositionTypeCode = F, AssignmentAssignmentTypeCodes = F, AssignmentBuildingCodes = F, TimeOffReasonID = F, TimeOffReasonCode = F, TimeOffReasonDescription = F, Description = F, TransactionSeconds = F, StartDateTime = F, EndDateTime = F, SecondsPerDay = F, FormattedSecondsPerDay = F, AllocationTypeScheduleID = F, AllocationTypeCode = F, IsUnpaidTimeOffDock = F, TimesheetIDUnpaidDock = F, TimeOffTransactionID = F, UniqueImportRecordIdentifier = F, YearEndUpdateType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LineNumber = F, TimeOffTypeCodeDescription = F, TimeOffReasonCodeDescription = F, HasErrors = F, LatestApprover = F, LatestApprovalLevelDescription = F, EmployeeNumber = F, TransactionDays = F, ErrorCount = F, Status = F, Exception = F, HasSupervisors = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TempTimeOffTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempTimeOffTransaction
	#'
	#' This function returns a dataframe or json object of a TempTimeOffTransaction
	#' @param TempTimeOffTransactionID The ID of the TempTimeOffTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimeOffTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimeOffTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimeOffTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TempTimeOffTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempTimeOffTransaction <- function(TempTimeOffTransactionID, EmployeeTimeOffTypeID = F, TempEmployeeTimeOffTypeID = F, EmployeeID = F, EmployeeNameLFM = F, EmployeeServiceYears = F, TimeOffTypeID = F, TimeOffTypeCode = F, TimeOffTypeDescription = F, TransactionType = F, AssignmentID = F, AssignmentPositionTypeCode = F, AssignmentAssignmentTypeCodes = F, AssignmentBuildingCodes = F, TimeOffReasonID = F, TimeOffReasonCode = F, TimeOffReasonDescription = F, Description = F, TransactionSeconds = F, StartDateTime = F, EndDateTime = F, SecondsPerDay = F, FormattedSecondsPerDay = F, AllocationTypeScheduleID = F, AllocationTypeCode = F, IsUnpaidTimeOffDock = F, TimesheetIDUnpaidDock = F, TimeOffTransactionID = F, UniqueImportRecordIdentifier = F, YearEndUpdateType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LineNumber = F, TimeOffTypeCodeDescription = F, TimeOffReasonCodeDescription = F, HasErrors = F, LatestApprover = F, LatestApprovalLevelDescription = F, EmployeeNumber = F, TransactionDays = F, ErrorCount = F, Status = F, Exception = F, HasSupervisors = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempTimeOffTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TempTimeOffTransaction", objectId = TempTimeOffTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempTimeOffTransaction
	#'
	#' This function deletes a TempTimeOffTransaction
	#' @param TempTimeOffTransactionID The ID of the TempTimeOffTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TempTimeOffTransactionID of the deleted TempTimeOffTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempTimeOffTransaction <- function(TempTimeOffTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TempTimeOffTransaction", objectId = TempTimeOffTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempTimeOffTransaction
	#'
	#' This function creates a TempTimeOffTransaction
	#' @param fieldNames The field values to give the created TempTimeOffTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TempTimeOffTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempTimeOffTransaction <- function(EmployeeTimeOffTypeID = NULL, TempEmployeeTimeOffTypeID = NULL, EmployeeID = NULL, EmployeeNameLFM = NULL, EmployeeServiceYears = NULL, TimeOffTypeID = NULL, TimeOffTypeCode = NULL, TimeOffTypeDescription = NULL, TransactionType = NULL, AssignmentID = NULL, AssignmentPositionTypeCode = NULL, AssignmentAssignmentTypeCodes = NULL, AssignmentBuildingCodes = NULL, TimeOffReasonID = NULL, TimeOffReasonCode = NULL, TimeOffReasonDescription = NULL, Description = NULL, TransactionSeconds = NULL, StartDateTime = NULL, EndDateTime = NULL, SecondsPerDay = NULL, AllocationTypeScheduleID = NULL, AllocationTypeCode = NULL, IsUnpaidTimeOffDock = NULL, TimesheetIDUnpaidDock = NULL, TimeOffTransactionID = NULL, UniqueImportRecordIdentifier = NULL, YearEndUpdateType = NULL, LineNumber = NULL, TimeOffTypeCodeDescription = NULL, TimeOffReasonCodeDescription = NULL, HasErrors = NULL, LatestApprover = NULL, LatestApprovalLevelDescription = NULL, EmployeeNumber = NULL, TransactionDays = NULL, ErrorCount = NULL, Status = NULL, Exception = NULL, HasSupervisors = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TempTimeOffTransaction", body = list(DataObject = body), searchFields = append("TempTimeOffTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempTimeOffTransaction
	#'
	#' This function modifies a TempTimeOffTransaction
	#' @param fieldNames The field values to give the modified TempTimeOffTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TempTimeOffTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempTimeOffTransaction <- function(TempTimeOffTransactionID, EmployeeTimeOffTypeID = NULL, TempEmployeeTimeOffTypeID = NULL, EmployeeID = NULL, EmployeeNameLFM = NULL, EmployeeServiceYears = NULL, TimeOffTypeID = NULL, TimeOffTypeCode = NULL, TimeOffTypeDescription = NULL, TransactionType = NULL, AssignmentID = NULL, AssignmentPositionTypeCode = NULL, AssignmentAssignmentTypeCodes = NULL, AssignmentBuildingCodes = NULL, TimeOffReasonID = NULL, TimeOffReasonCode = NULL, TimeOffReasonDescription = NULL, Description = NULL, TransactionSeconds = NULL, StartDateTime = NULL, EndDateTime = NULL, SecondsPerDay = NULL, AllocationTypeScheduleID = NULL, AllocationTypeCode = NULL, IsUnpaidTimeOffDock = NULL, TimesheetIDUnpaidDock = NULL, TimeOffTransactionID = NULL, UniqueImportRecordIdentifier = NULL, YearEndUpdateType = NULL, LineNumber = NULL, TimeOffTypeCodeDescription = NULL, TimeOffReasonCodeDescription = NULL, HasErrors = NULL, LatestApprover = NULL, LatestApprovalLevelDescription = NULL, EmployeeNumber = NULL, TransactionDays = NULL, ErrorCount = NULL, Status = NULL, Exception = NULL, HasSupervisors = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TempTimeOffTransaction", objectId = TempTimeOffTransactionID, body = list(DataObject = body), searchFields = append("TempTimeOffTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempTimeOffTransactionErrors
	#'
	#' This function returns a dataframe or json object of TempTimeOffTransactionErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimeOffTransactionErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimeOffTransactionErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimeOffTransactionError') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TempTimeOffTransactionErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempTimeOffTransactionErrors <- function(searchConditionsList = NULL, TempTimeOffTransactionErrorID = F, EmployeeNameLFM = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempTimeOffTransactionID = F, EmployeeNumber = F, ErrorNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TempTimeOffTransactionError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempTimeOffTransactionError
	#'
	#' This function returns a dataframe or json object of a TempTimeOffTransactionError
	#' @param TempTimeOffTransactionErrorID The ID of the TempTimeOffTransactionError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimeOffTransactionError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimeOffTransactionError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimeOffTransactionError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TempTimeOffTransactionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempTimeOffTransactionError <- function(TempTimeOffTransactionErrorID, EmployeeNameLFM = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempTimeOffTransactionID = F, EmployeeNumber = F, ErrorNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempTimeOffTransactionErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TempTimeOffTransactionError", objectId = TempTimeOffTransactionErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempTimeOffTransactionError
	#'
	#' This function deletes a TempTimeOffTransactionError
	#' @param TempTimeOffTransactionErrorID The ID of the TempTimeOffTransactionError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TempTimeOffTransactionErrorID of the deleted TempTimeOffTransactionError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempTimeOffTransactionError <- function(TempTimeOffTransactionErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TempTimeOffTransactionError", objectId = TempTimeOffTransactionErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempTimeOffTransactionError
	#'
	#' This function creates a TempTimeOffTransactionError
	#' @param fieldNames The field values to give the created TempTimeOffTransactionError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TempTimeOffTransactionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempTimeOffTransactionError <- function(EmployeeNameLFM = NULL, Error = NULL, TempTimeOffTransactionID = NULL, EmployeeNumber = NULL, ErrorNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TempTimeOffTransactionError", body = list(DataObject = body), searchFields = append("TempTimeOffTransactionErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempTimeOffTransactionError
	#'
	#' This function modifies a TempTimeOffTransactionError
	#' @param fieldNames The field values to give the modified TempTimeOffTransactionError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TempTimeOffTransactionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempTimeOffTransactionError <- function(TempTimeOffTransactionErrorID, EmployeeNameLFM = NULL, Error = NULL, TempTimeOffTransactionID = NULL, EmployeeNumber = NULL, ErrorNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TempTimeOffTransactionError", objectId = TempTimeOffTransactionErrorID, body = list(DataObject = body), searchFields = append("TempTimeOffTransactionErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeOffReasons
	#'
	#' This function returns a dataframe or json object of TimeOffReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffReasons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffReasons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffReason') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TimeOffReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeOffReasons <- function(searchConditionsList = NULL, TimeOffReasonID = F, DistrictID = F, Code = F, Description = F, AllowEmployeeAccess = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsCRDCAbsence = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TimeOffReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeOffReason
	#'
	#' This function returns a dataframe or json object of a TimeOffReason
	#' @param TimeOffReasonID The ID of the TimeOffReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TimeOffReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeOffReason <- function(TimeOffReasonID, DistrictID = F, Code = F, Description = F, AllowEmployeeAccess = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsCRDCAbsence = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeOffReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TimeOffReason", objectId = TimeOffReasonID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeOffReason
	#'
	#' This function deletes a TimeOffReason
	#' @param TimeOffReasonID The ID of the TimeOffReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TimeOffReasonID of the deleted TimeOffReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeOffReason <- function(TimeOffReasonID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TimeOffReason", objectId = TimeOffReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeOffReason
	#'
	#' This function creates a TimeOffReason
	#' @param fieldNames The field values to give the created TimeOffReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TimeOffReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeOffReason <- function(DistrictID = NULL, Code = NULL, Description = NULL, AllowEmployeeAccess = NULL, IsCRDCAbsence = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TimeOffReason", body = list(DataObject = body), searchFields = append("TimeOffReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeOffReason
	#'
	#' This function modifies a TimeOffReason
	#' @param fieldNames The field values to give the modified TimeOffReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TimeOffReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeOffReason <- function(TimeOffReasonID, DistrictID = NULL, Code = NULL, Description = NULL, AllowEmployeeAccess = NULL, IsCRDCAbsence = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TimeOffReason", objectId = TimeOffReasonID, body = list(DataObject = body), searchFields = append("TimeOffReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeOffTransactions
	#'
	#' This function returns a dataframe or json object of TimeOffTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffTransaction') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TimeOffTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeOffTransactions <- function(searchConditionsList = NULL, TimeOffTransactionID = F, EmployeeTimeOffTypeID = F, TimeOffReasonID = F, Description = F, TransactionType = F, StartDateTime = F, EndDateTime = F, TransactionSeconds = F, AllocationTypeScheduleID = F, SecondsPerDay = F, AssignmentID = F, Status = F, TransactionIdentifier = F, SubstituteCoverage = F, ThirdPartyImportID = F, UniqueImportRecordIdentifier = F, AttachmentCount = F, TransactionDays = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FormattedTransactionSecondsDecimal = F, FormattedSecondsPerDayDecimal = F, TimesheetIDUnpaidDock = F, StartDateSortable = F, IncludeInUnpaidDockTimesheetBuild = F, RenderExcludeFromUnpaidDockTimesheetBuild = F, RenderDenyFromAdministration = F, AbleToDelete = F, AbleToReverse = F, CalculatedTransactionSeconds = F, CalculatedTransactionDays = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TimeOffTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeOffTransaction
	#'
	#' This function returns a dataframe or json object of a TimeOffTransaction
	#' @param TimeOffTransactionID The ID of the TimeOffTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TimeOffTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeOffTransaction <- function(TimeOffTransactionID, EmployeeTimeOffTypeID = F, TimeOffReasonID = F, Description = F, TransactionType = F, StartDateTime = F, EndDateTime = F, TransactionSeconds = F, AllocationTypeScheduleID = F, SecondsPerDay = F, AssignmentID = F, Status = F, TransactionIdentifier = F, SubstituteCoverage = F, ThirdPartyImportID = F, UniqueImportRecordIdentifier = F, AttachmentCount = F, TransactionDays = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FormattedTransactionSecondsDecimal = F, FormattedSecondsPerDayDecimal = F, TimesheetIDUnpaidDock = F, StartDateSortable = F, IncludeInUnpaidDockTimesheetBuild = F, RenderExcludeFromUnpaidDockTimesheetBuild = F, RenderDenyFromAdministration = F, AbleToDelete = F, AbleToReverse = F, CalculatedTransactionSeconds = F, CalculatedTransactionDays = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeOffTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TimeOffTransaction", objectId = TimeOffTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeOffTransaction
	#'
	#' This function deletes a TimeOffTransaction
	#' @param TimeOffTransactionID The ID of the TimeOffTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TimeOffTransactionID of the deleted TimeOffTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeOffTransaction <- function(TimeOffTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TimeOffTransaction", objectId = TimeOffTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeOffTransaction
	#'
	#' This function creates a TimeOffTransaction
	#' @param fieldNames The field values to give the created TimeOffTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TimeOffTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeOffTransaction <- function(EmployeeTimeOffTypeID = NULL, TimeOffReasonID = NULL, Description = NULL, TransactionType = NULL, StartDateTime = NULL, EndDateTime = NULL, TransactionSeconds = NULL, AllocationTypeScheduleID = NULL, SecondsPerDay = NULL, AssignmentID = NULL, Status = NULL, ThirdPartyImportID = NULL, UniqueImportRecordIdentifier = NULL, TransactionDays = NULL, TimesheetIDUnpaidDock = NULL, IncludeInUnpaidDockTimesheetBuild = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TimeOffTransaction", body = list(DataObject = body), searchFields = append("TimeOffTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeOffTransaction
	#'
	#' This function modifies a TimeOffTransaction
	#' @param fieldNames The field values to give the modified TimeOffTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TimeOffTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeOffTransaction <- function(TimeOffTransactionID, EmployeeTimeOffTypeID = NULL, TimeOffReasonID = NULL, Description = NULL, TransactionType = NULL, StartDateTime = NULL, EndDateTime = NULL, TransactionSeconds = NULL, AllocationTypeScheduleID = NULL, SecondsPerDay = NULL, AssignmentID = NULL, Status = NULL, ThirdPartyImportID = NULL, UniqueImportRecordIdentifier = NULL, TransactionDays = NULL, TimesheetIDUnpaidDock = NULL, IncludeInUnpaidDockTimesheetBuild = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TimeOffTransaction", objectId = TimeOffTransactionID, body = list(DataObject = body), searchFields = append("TimeOffTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeOffTransactionApprovals
	#'
	#' This function returns a dataframe or json object of TimeOffTransactionApprovals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTransactionApprovals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTransactionApprovals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffTransactionApproval') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TimeOffTransactionApprovals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeOffTransactionApprovals <- function(searchConditionsList = NULL, TimeOffTransactionApprovalID = F, TimeOffTransactionID = F, UserIDApprover = F, Comment = F, Status = F, OrganizationChartRelationshipID = F, Level = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApprovalActionTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TimeOffTransactionApproval", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeOffTransactionApproval
	#'
	#' This function returns a dataframe or json object of a TimeOffTransactionApproval
	#' @param TimeOffTransactionApprovalID The ID of the TimeOffTransactionApproval to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTransactionApproval. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTransactionApproval.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffTransactionApproval') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TimeOffTransactionApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeOffTransactionApproval <- function(TimeOffTransactionApprovalID, TimeOffTransactionID = F, UserIDApprover = F, Comment = F, Status = F, OrganizationChartRelationshipID = F, Level = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApprovalActionTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeOffTransactionApprovalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TimeOffTransactionApproval", objectId = TimeOffTransactionApprovalID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeOffTransactionApproval
	#'
	#' This function deletes a TimeOffTransactionApproval
	#' @param TimeOffTransactionApprovalID The ID of the TimeOffTransactionApproval to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TimeOffTransactionApprovalID of the deleted TimeOffTransactionApproval.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeOffTransactionApproval <- function(TimeOffTransactionApprovalID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TimeOffTransactionApproval", objectId = TimeOffTransactionApprovalID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeOffTransactionApproval
	#'
	#' This function creates a TimeOffTransactionApproval
	#' @param fieldNames The field values to give the created TimeOffTransactionApproval. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TimeOffTransactionApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeOffTransactionApproval <- function(TimeOffTransactionID = NULL, UserIDApprover = NULL, Comment = NULL, Status = NULL, OrganizationChartRelationshipID = NULL, Level = NULL, LevelDescription = NULL, ApprovalActionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TimeOffTransactionApproval", body = list(DataObject = body), searchFields = append("TimeOffTransactionApprovalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeOffTransactionApproval
	#'
	#' This function modifies a TimeOffTransactionApproval
	#' @param fieldNames The field values to give the modified TimeOffTransactionApproval. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TimeOffTransactionApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeOffTransactionApproval <- function(TimeOffTransactionApprovalID, TimeOffTransactionID = NULL, UserIDApprover = NULL, Comment = NULL, Status = NULL, OrganizationChartRelationshipID = NULL, Level = NULL, LevelDescription = NULL, ApprovalActionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TimeOffTransactionApproval", objectId = TimeOffTransactionApprovalID, body = list(DataObject = body), searchFields = append("TimeOffTransactionApprovalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeOffTransactionCalculations
	#'
	#' This function returns a dataframe or json object of TimeOffTransactionCalculations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTransactionCalculations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTransactionCalculations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffTransactionCalculation') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TimeOffTransactionCalculations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeOffTransactionCalculations <- function(searchConditionsList = NULL, TimeOffTransactionCalculationID = F, TimeOffTransactionID = F, CheckTransactionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TimeOffTransactionCalculation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeOffTransactionCalculation
	#'
	#' This function returns a dataframe or json object of a TimeOffTransactionCalculation
	#' @param TimeOffTransactionCalculationID The ID of the TimeOffTransactionCalculation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTransactionCalculation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTransactionCalculation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffTransactionCalculation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TimeOffTransactionCalculation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeOffTransactionCalculation <- function(TimeOffTransactionCalculationID, TimeOffTransactionID = F, CheckTransactionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeOffTransactionCalculationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TimeOffTransactionCalculation", objectId = TimeOffTransactionCalculationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeOffTransactionCalculation
	#'
	#' This function deletes a TimeOffTransactionCalculation
	#' @param TimeOffTransactionCalculationID The ID of the TimeOffTransactionCalculation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TimeOffTransactionCalculationID of the deleted TimeOffTransactionCalculation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeOffTransactionCalculation <- function(TimeOffTransactionCalculationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TimeOffTransactionCalculation", objectId = TimeOffTransactionCalculationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeOffTransactionCalculation
	#'
	#' This function creates a TimeOffTransactionCalculation
	#' @param fieldNames The field values to give the created TimeOffTransactionCalculation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TimeOffTransactionCalculation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeOffTransactionCalculation <- function(TimeOffTransactionID = NULL, CheckTransactionID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TimeOffTransactionCalculation", body = list(DataObject = body), searchFields = append("TimeOffTransactionCalculationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeOffTransactionCalculation
	#'
	#' This function modifies a TimeOffTransactionCalculation
	#' @param fieldNames The field values to give the modified TimeOffTransactionCalculation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TimeOffTransactionCalculation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeOffTransactionCalculation <- function(TimeOffTransactionCalculationID, TimeOffTransactionID = NULL, CheckTransactionID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TimeOffTransactionCalculation", objectId = TimeOffTransactionCalculationID, body = list(DataObject = body), searchFields = append("TimeOffTransactionCalculationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeOffTypes
	#'
	#' This function returns a dataframe or json object of TimeOffTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffType') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TimeOffTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeOffTypes <- function(searchConditionsList = NULL, TimeOffTypeID = F, DistrictID = F, Code = F, Description = F, NegativeBalance = F, AllocationCycle = F, AllocationCycleCalendarDate = F, CheckStubRank = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowEmployeeAccess = F, DisplayOnCheckStub = F, UnitType = F, HourRestriction = F, DayRestriction = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TimeOffType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeOffType
	#'
	#' This function returns a dataframe or json object of a TimeOffType
	#' @param TimeOffTypeID The ID of the TimeOffType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeOffType <- function(TimeOffTypeID, DistrictID = F, Code = F, Description = F, NegativeBalance = F, AllocationCycle = F, AllocationCycleCalendarDate = F, CheckStubRank = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowEmployeeAccess = F, DisplayOnCheckStub = F, UnitType = F, HourRestriction = F, DayRestriction = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeOffTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TimeOffType", objectId = TimeOffTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeOffType
	#'
	#' This function deletes a TimeOffType
	#' @param TimeOffTypeID The ID of the TimeOffType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TimeOffTypeID of the deleted TimeOffType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeOffType <- function(TimeOffTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TimeOffType", objectId = TimeOffTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeOffType
	#'
	#' This function creates a TimeOffType
	#' @param fieldNames The field values to give the created TimeOffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeOffType <- function(DistrictID = NULL, Code = NULL, Description = NULL, NegativeBalance = NULL, AllocationCycle = NULL, AllocationCycleCalendarDate = NULL, CheckStubRank = NULL, AllowEmployeeAccess = NULL, DisplayOnCheckStub = NULL, UnitType = NULL, HourRestriction = NULL, DayRestriction = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TimeOffType", body = list(DataObject = body), searchFields = append("TimeOffTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeOffType
	#'
	#' This function modifies a TimeOffType
	#' @param fieldNames The field values to give the modified TimeOffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeOffType <- function(TimeOffTypeID, DistrictID = NULL, Code = NULL, Description = NULL, NegativeBalance = NULL, AllocationCycle = NULL, AllocationCycleCalendarDate = NULL, CheckStubRank = NULL, AllowEmployeeAccess = NULL, DisplayOnCheckStub = NULL, UnitType = NULL, HourRestriction = NULL, DayRestriction = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TimeOffType", objectId = TimeOffTypeID, body = list(DataObject = body), searchFields = append("TimeOffTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeOffTypeReasons
	#'
	#' This function returns a dataframe or json object of TimeOffTypeReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTypeReasons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTypeReasons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffTypeReason') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TimeOffTypeReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeOffTypeReasons <- function(searchConditionsList = NULL, TimeOffTypeReasonID = F, TimeOffTypeID = F, TimeOffReasonID = F, AllowForAllocated = F, AllowForUsed = F, AllowForUnpaid = F, MaxStorage = F, MaxAmountSeconds = F, MaxAmountDays = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FormattedMaxAmountSecondsDecimal = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TimeOffTypeReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeOffTypeReason
	#'
	#' This function returns a dataframe or json object of a TimeOffTypeReason
	#' @param TimeOffTypeReasonID The ID of the TimeOffTypeReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTypeReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTypeReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffTypeReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TimeOffTypeReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeOffTypeReason <- function(TimeOffTypeReasonID, TimeOffTypeID = F, TimeOffReasonID = F, AllowForAllocated = F, AllowForUsed = F, AllowForUnpaid = F, MaxStorage = F, MaxAmountSeconds = F, MaxAmountDays = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FormattedMaxAmountSecondsDecimal = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeOffTypeReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TimeOffTypeReason", objectId = TimeOffTypeReasonID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeOffTypeReason
	#'
	#' This function deletes a TimeOffTypeReason
	#' @param TimeOffTypeReasonID The ID of the TimeOffTypeReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TimeOffTypeReasonID of the deleted TimeOffTypeReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeOffTypeReason <- function(TimeOffTypeReasonID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TimeOffTypeReason", objectId = TimeOffTypeReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeOffTypeReason
	#'
	#' This function creates a TimeOffTypeReason
	#' @param fieldNames The field values to give the created TimeOffTypeReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TimeOffTypeReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeOffTypeReason <- function(TimeOffTypeID = NULL, TimeOffReasonID = NULL, AllowForAllocated = NULL, AllowForUsed = NULL, AllowForUnpaid = NULL, MaxStorage = NULL, MaxAmountSeconds = NULL, MaxAmountDays = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TimeOffTypeReason", body = list(DataObject = body), searchFields = append("TimeOffTypeReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeOffTypeReason
	#'
	#' This function modifies a TimeOffTypeReason
	#' @param fieldNames The field values to give the modified TimeOffTypeReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TimeOffTypeReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeOffTypeReason <- function(TimeOffTypeReasonID, TimeOffTypeID = NULL, TimeOffReasonID = NULL, AllowForAllocated = NULL, AllowForUsed = NULL, AllowForUnpaid = NULL, MaxStorage = NULL, MaxAmountSeconds = NULL, MaxAmountDays = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TimeOffTypeReason", objectId = TimeOffTypeReasonID, body = list(DataObject = body), searchFields = append("TimeOffTypeReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAllocationSchedules
	#'
	#' This function returns a dataframe or json object of TempAllocationSchedules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAllocationSchedules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAllocationSchedules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAllocationSchedule') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TempAllocationSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAllocationSchedules <- function(searchConditionsList = NULL, TempAllocationScheduleID = F, DistrictID = F, Code = F, Description = F, ExpectedRunDate = F, HasErrors = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TempAllocationSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAllocationSchedule
	#'
	#' This function returns a dataframe or json object of a TempAllocationSchedule
	#' @param TempAllocationScheduleID The ID of the TempAllocationSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAllocationSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAllocationSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAllocationSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TempAllocationSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAllocationSchedule <- function(TempAllocationScheduleID, DistrictID = F, Code = F, Description = F, ExpectedRunDate = F, HasErrors = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAllocationScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TempAllocationSchedule", objectId = TempAllocationScheduleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAllocationSchedule
	#'
	#' This function deletes a TempAllocationSchedule
	#' @param TempAllocationScheduleID The ID of the TempAllocationSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TempAllocationScheduleID of the deleted TempAllocationSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAllocationSchedule <- function(TempAllocationScheduleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TempAllocationSchedule", objectId = TempAllocationScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAllocationSchedule
	#'
	#' This function creates a TempAllocationSchedule
	#' @param fieldNames The field values to give the created TempAllocationSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TempAllocationSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAllocationSchedule <- function(DistrictID = NULL, Code = NULL, Description = NULL, ExpectedRunDate = NULL, HasErrors = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TempAllocationSchedule", body = list(DataObject = body), searchFields = append("TempAllocationScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAllocationSchedule
	#'
	#' This function modifies a TempAllocationSchedule
	#' @param fieldNames The field values to give the modified TempAllocationSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TempAllocationSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAllocationSchedule <- function(TempAllocationScheduleID, DistrictID = NULL, Code = NULL, Description = NULL, ExpectedRunDate = NULL, HasErrors = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TempAllocationSchedule", objectId = TempAllocationScheduleID, body = list(DataObject = body), searchFields = append("TempAllocationScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAllocationTypeSchedules
	#'
	#' This function returns a dataframe or json object of TempAllocationTypeSchedules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAllocationTypeSchedules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAllocationTypeSchedules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAllocationTypeSchedule') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TempAllocationTypeSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAllocationTypeSchedules <- function(searchConditionsList = NULL, TempAllocationTypeScheduleID = F, AllocationScheduleID = F, TempAllocationScheduleID = F, AllocationDefinitionID = F, AllocationDefinitionDescription = F, Scope = F, TransactionDate = F, AnniversaryLowDate = F, AnniversaryHighDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TempAllocationTypeSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAllocationTypeSchedule
	#'
	#' This function returns a dataframe or json object of a TempAllocationTypeSchedule
	#' @param TempAllocationTypeScheduleID The ID of the TempAllocationTypeSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAllocationTypeSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAllocationTypeSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAllocationTypeSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TempAllocationTypeSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAllocationTypeSchedule <- function(TempAllocationTypeScheduleID, AllocationScheduleID = F, TempAllocationScheduleID = F, AllocationDefinitionID = F, AllocationDefinitionDescription = F, Scope = F, TransactionDate = F, AnniversaryLowDate = F, AnniversaryHighDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAllocationTypeScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TempAllocationTypeSchedule", objectId = TempAllocationTypeScheduleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAllocationTypeSchedule
	#'
	#' This function deletes a TempAllocationTypeSchedule
	#' @param TempAllocationTypeScheduleID The ID of the TempAllocationTypeSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TempAllocationTypeScheduleID of the deleted TempAllocationTypeSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAllocationTypeSchedule <- function(TempAllocationTypeScheduleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TempAllocationTypeSchedule", objectId = TempAllocationTypeScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAllocationTypeSchedule
	#'
	#' This function creates a TempAllocationTypeSchedule
	#' @param fieldNames The field values to give the created TempAllocationTypeSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TempAllocationTypeSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAllocationTypeSchedule <- function(AllocationScheduleID = NULL, TempAllocationScheduleID = NULL, AllocationDefinitionID = NULL, AllocationDefinitionDescription = NULL, Scope = NULL, TransactionDate = NULL, AnniversaryLowDate = NULL, AnniversaryHighDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TempAllocationTypeSchedule", body = list(DataObject = body), searchFields = append("TempAllocationTypeScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAllocationTypeSchedule
	#'
	#' This function modifies a TempAllocationTypeSchedule
	#' @param fieldNames The field values to give the modified TempAllocationTypeSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TempAllocationTypeSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAllocationTypeSchedule <- function(TempAllocationTypeScheduleID, AllocationScheduleID = NULL, TempAllocationScheduleID = NULL, AllocationDefinitionID = NULL, AllocationDefinitionDescription = NULL, Scope = NULL, TransactionDate = NULL, AnniversaryLowDate = NULL, AnniversaryHighDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TempAllocationTypeSchedule", objectId = TempAllocationTypeScheduleID, body = list(DataObject = body), searchFields = append("TempAllocationTypeScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TimeOffTempErrors
	#'
	#' This function returns a dataframe or json object of TimeOffTempErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTempErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTempErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffTempError') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TimeOffTempErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTimeOffTempErrors <- function(searchConditionsList = NULL, TempErrorID = F, TempAllocationScheduleID = F, ErrorField = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TempError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TimeOffTempError
	#'
	#' This function returns a dataframe or json object of a TimeOffTempError
	#' @param TimeOffTempErrorID The ID of the TimeOffTempError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TimeOffTempError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TimeOffTempError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TimeOffTempError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TimeOffTempError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTimeOffTempError <- function(TimeOffTempErrorID, TempErrorID = F, TempAllocationScheduleID = F, ErrorField = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TimeOffTempErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TempError", objectId = TimeOffTempErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TimeOffTempError
	#'
	#' This function deletes a TimeOffTempError
	#' @param TimeOffTempErrorID The ID of the TimeOffTempError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TimeOffTempErrorID of the deleted TimeOffTempError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTimeOffTempError <- function(TimeOffTempErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TempError", objectId = TimeOffTempErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TimeOffTempError
	#'
	#' This function creates a TimeOffTempError
	#' @param fieldNames The field values to give the created TimeOffTempError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TimeOffTempError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTimeOffTempError <- function(TempAllocationScheduleID = NULL, ErrorField = NULL, Message = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TempError", body = list(DataObject = body), searchFields = append("TempErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TimeOffTempError
	#'
	#' This function modifies a TimeOffTempError
	#' @param fieldNames The field values to give the modified TimeOffTempError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TimeOffTempError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTimeOffTempError <- function(TempErrorID, TempAllocationScheduleID = NULL, ErrorField = NULL, Message = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TempError", objectId = TempErrorID, body = list(DataObject = body), searchFields = append("TempErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionTypeTimeOffTypes
	#'
	#' This function returns a dataframe or json object of PositionTypeTimeOffTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionTypeTimeOffTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionTypeTimeOffTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionTypeTimeOffType') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of PositionTypeTimeOffTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionTypeTimeOffTypes <- function(searchConditionsList = NULL, PositionTypeTimeOffTypeID = F, PositionTypeID = F, TimeOffTypeID = F, CreateEmployeeTimeOffType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OverrideRestriction = F, HourRestrictionOverride = F, HourRestrictionCode = F, DayRestrictionOverride = F, DayRestrictionCode = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "PositionTypeTimeOffType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionTypeTimeOffType
	#'
	#' This function returns a dataframe or json object of a PositionTypeTimeOffType
	#' @param PositionTypeTimeOffTypeID The ID of the PositionTypeTimeOffType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionTypeTimeOffType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionTypeTimeOffType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionTypeTimeOffType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of PositionTypeTimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionTypeTimeOffType <- function(PositionTypeTimeOffTypeID, PositionTypeID = F, TimeOffTypeID = F, CreateEmployeeTimeOffType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OverrideRestriction = F, HourRestrictionOverride = F, HourRestrictionCode = F, DayRestrictionOverride = F, DayRestrictionCode = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionTypeTimeOffTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "PositionTypeTimeOffType", objectId = PositionTypeTimeOffTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionTypeTimeOffType
	#'
	#' This function deletes a PositionTypeTimeOffType
	#' @param PositionTypeTimeOffTypeID The ID of the PositionTypeTimeOffType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The PositionTypeTimeOffTypeID of the deleted PositionTypeTimeOffType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionTypeTimeOffType <- function(PositionTypeTimeOffTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "PositionTypeTimeOffType", objectId = PositionTypeTimeOffTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionTypeTimeOffType
	#'
	#' This function creates a PositionTypeTimeOffType
	#' @param fieldNames The field values to give the created PositionTypeTimeOffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created PositionTypeTimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionTypeTimeOffType <- function(PositionTypeID = NULL, TimeOffTypeID = NULL, CreateEmployeeTimeOffType = NULL, OverrideRestriction = NULL, HourRestrictionOverride = NULL, DayRestrictionOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "PositionTypeTimeOffType", body = list(DataObject = body), searchFields = append("PositionTypeTimeOffTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionTypeTimeOffType
	#'
	#' This function modifies a PositionTypeTimeOffType
	#' @param fieldNames The field values to give the modified PositionTypeTimeOffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified PositionTypeTimeOffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionTypeTimeOffType <- function(PositionTypeTimeOffTypeID, PositionTypeID = NULL, TimeOffTypeID = NULL, CreateEmployeeTimeOffType = NULL, OverrideRestriction = NULL, HourRestrictionOverride = NULL, DayRestrictionOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "PositionTypeTimeOffType", objectId = PositionTypeTimeOffTypeID, body = list(DataObject = body), searchFields = append("PositionTypeTimeOffTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempTimeOffTypeReasons
	#'
	#' This function returns a dataframe or json object of TempTimeOffTypeReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimeOffTypeReasons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimeOffTypeReasons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimeOffTypeReason') to get more field paths.
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
	#' @concept Time Off
	#' @return A list of TempTimeOffTypeReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempTimeOffTypeReasons <- function(searchConditionsList = NULL, TempTimeOffTypeReasonID = F, TimeOffTypeReasonID = F, TimeOffTypeID = F, TimeOffReasonID = F, TimeOffReasonCodeDescription = F, MaxStorage = F, IsMaxAmountDaysReadOnly = F, IsMaxAmountSecondsReadOnly = F, MaxDays = F, PreviousMaxDays = F, MaxSeconds = F, PreviousMaxSeconds = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeOff", objectName = "TempTimeOffTypeReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempTimeOffTypeReason
	#'
	#' This function returns a dataframe or json object of a TempTimeOffTypeReason
	#' @param TempTimeOffTypeReasonID The ID of the TempTimeOffTypeReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTimeOffTypeReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTimeOffTypeReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTimeOffTypeReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A dataframe or of TempTimeOffTypeReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempTimeOffTypeReason <- function(TempTimeOffTypeReasonID, TimeOffTypeReasonID = F, TimeOffTypeID = F, TimeOffReasonID = F, TimeOffReasonCodeDescription = F, MaxStorage = F, IsMaxAmountDaysReadOnly = F, IsMaxAmountSecondsReadOnly = F, MaxDays = F, PreviousMaxDays = F, MaxSeconds = F, PreviousMaxSeconds = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempTimeOffTypeReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "TimeOff", objectName = "TempTimeOffTypeReason", objectId = TempTimeOffTypeReasonID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempTimeOffTypeReason
	#'
	#' This function deletes a TempTimeOffTypeReason
	#' @param TempTimeOffTypeReasonID The ID of the TempTimeOffTypeReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The TempTimeOffTypeReasonID of the deleted TempTimeOffTypeReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempTimeOffTypeReason <- function(TempTimeOffTypeReasonID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "TimeOff", objectName = "TempTimeOffTypeReason", objectId = TempTimeOffTypeReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempTimeOffTypeReason
	#'
	#' This function creates a TempTimeOffTypeReason
	#' @param fieldNames The field values to give the created TempTimeOffTypeReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return A newly created TempTimeOffTypeReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempTimeOffTypeReason <- function(TimeOffTypeReasonID = NULL, TimeOffTypeID = NULL, TimeOffReasonID = NULL, TimeOffReasonCodeDescription = NULL, MaxStorage = NULL, MaxDays = NULL, PreviousMaxDays = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "TimeOff", objectName = "TempTimeOffTypeReason", body = list(DataObject = body), searchFields = append("TempTimeOffTypeReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempTimeOffTypeReason
	#'
	#' This function modifies a TempTimeOffTypeReason
	#' @param fieldNames The field values to give the modified TempTimeOffTypeReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Time Off
	#' @return The modified TempTimeOffTypeReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempTimeOffTypeReason <- function(TempTimeOffTypeReasonID, TimeOffTypeReasonID = NULL, TimeOffTypeID = NULL, TimeOffReasonID = NULL, TimeOffReasonCodeDescription = NULL, MaxStorage = NULL, MaxDays = NULL, PreviousMaxDays = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "TimeOff", objectName = "TempTimeOffTypeReason", objectId = TempTimeOffTypeReasonID, body = list(DataObject = body), searchFields = append("TempTimeOffTypeReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
